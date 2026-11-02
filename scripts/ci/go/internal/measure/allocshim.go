package measure

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"

	"github.com/hhh-pa/clojure/scripts/ci/go/internal/snipcore"
)

// allocShimSource is the LD_PRELOAD counting shim (Linux/glibc
// only). It interposes the libc allocation entry points, sums
// requested bytes, and a destructor reports the total on fd 2 as
// "snip_alloc_total_bytes=N". Freed bytes are never subtracted, so
// the total matches EpsilonGC heap-used-at-exit semantics:
// cumulative allocation for the whole process run. Deterministic
// programs yield deterministic totals; exit-path allocations after
// the destructor (if any) are missed identically every run, so
// they shift the total by a constant, never by noise.
const allocShimSource = `
#define _GNU_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <dlfcn.h>
#include <stdatomic.h>
#include <sys/types.h>
#include <errno.h>

static atomic_ullong total_bytes = 0;

/* Bootstrap pool: dlsym(RTLD_NEXT, ...) itself allocates, so
 * allocations arriving before init completes are served from here
 * (uncounted) instead of recursing. 64KB dwarfs dlsym's needs. */
#define POOL_SIZE (64u * 1024u)
_Alignas(64) static char bootstrap_pool[POOL_SIZE];
static atomic_size_t pool_used = 0;
static volatile int resolving = 0;
static void *(*real_malloc)(size_t);
static void (*real_free)(void *);
static void *(*real_calloc)(size_t, size_t);
static void *(*real_realloc)(void *, size_t);
static void *(*real_reallocarray)(void *, size_t, size_t);
static int (*real_posix_memalign)(void **, size_t, size_t);
static void *(*real_aligned_alloc)(size_t, size_t);

static void *pool_alloc(size_t size) {
	if (size == 0)
		size = 1;
	size = (size + 15) & ~(size_t)15;
	size_t off = atomic_fetch_add(&pool_used, size);
	if (off + size > POOL_SIZE)
		return NULL;
	return bootstrap_pool + off;
}

static int in_pool(const void *p) {
	const char *c = (const char *)p;
	return c >= bootstrap_pool && c < bootstrap_pool + POOL_SIZE;
}

static size_t pool_tail(const void *p) {
	return (size_t)(bootstrap_pool + POOL_SIZE - (const char *)p);
}

static void resolve_all(void) {
	real_malloc = dlsym(RTLD_NEXT, "malloc");
	real_free = dlsym(RTLD_NEXT, "free");
	real_calloc = dlsym(RTLD_NEXT, "calloc");
	real_realloc = dlsym(RTLD_NEXT, "realloc");
	real_reallocarray = dlsym(RTLD_NEXT, "reallocarray");
	real_posix_memalign = dlsym(RTLD_NEXT, "posix_memalign");
	real_aligned_alloc = dlsym(RTLD_NEXT, "aligned_alloc");
}

/* True once the real entry points are resolved. Reentrant calls
 * during resolution (dlsym allocates) take the pool path. */
static int ensure_init(void) {
	if (real_malloc)
		return 1;
	if (resolving)
		return 0;
	resolving = 1;
	resolve_all();
	resolving = 0;
	return real_malloc != 0;
}

__attribute__((constructor)) static void shim_init(void) {
	resolving = 1;
	resolve_all();
	resolving = 0;
}

void *malloc(size_t size) {
	if (!ensure_init())
		return pool_alloc(size);
	void *p = real_malloc(size);
	if (p)
		atomic_fetch_add(&total_bytes, (unsigned long long)size);
	return p;
}

void free(void *p) {
	if (!p || in_pool(p))
		return;
	if (!ensure_init())
		return;
	real_free(p);
}

void *calloc(size_t n, size_t size) {
	if (!ensure_init()) {
		if (size != 0 && n > POOL_SIZE / size)
			return NULL;
		void *p = pool_alloc(n * size);
		if (p)
			memset(p, 0, n * size);
		return p;
	}
	void *p = real_calloc(n, size);
	if (p)
		atomic_fetch_add(&total_bytes, (unsigned long long)n * (unsigned long long)size);
	return p;
}

void *realloc(void *ptr, size_t size) {
	if (ptr && in_pool(ptr)) {
		/* Bootstrap block outliving init: re-home it. The old
		 * size is unknown, so the copy clamps to the pool tail;
		 * glibc never reallocs its bootstrap blocks, so this
		 * path is unreachable in practice. */
		void *p = ensure_init() ? real_malloc(size ? size : 1) : pool_alloc(size);
		if (p) {
			size_t tail = pool_tail(ptr);
			memcpy(p, ptr, size < tail ? size : tail);
			if (real_malloc)
				atomic_fetch_add(&total_bytes, (unsigned long long)size);
		}
		return p;
	}
	if (!ensure_init())
		return pool_alloc(size);
	void *p = real_realloc(ptr, size);
	if (p)
		atomic_fetch_add(&total_bytes, (unsigned long long)size);
	return p;
}

void *reallocarray(void *ptr, size_t n, size_t size) {
	if (size != 0 && n > (size_t)-1 / size) {
		errno = ENOMEM;
		return NULL;
	}
	if (real_reallocarray || ensure_init()) {
		if (real_reallocarray && !(ptr && in_pool(ptr))) {
			void *p = real_reallocarray(ptr, n, size);
			if (p)
				atomic_fetch_add(&total_bytes, (unsigned long long)n * (unsigned long long)size);
			return p;
		}
	}
	return realloc(ptr, n * size);
}

int posix_memalign(void **memptr, size_t alignment, size_t size) {
	if (!ensure_init() || !real_posix_memalign) {
		if (alignment < sizeof(void *) || (alignment & (alignment - 1)) != 0)
			return 22; /* EINVAL */
		if (size + alignment < size)
			return 12; /* ENOMEM */
		void *raw = pool_alloc(size + alignment);
		if (!raw)
			return 12; /* ENOMEM */
		uintptr_t a = ((uintptr_t)raw + alignment - 1) & ~(uintptr_t)(alignment - 1);
		*memptr = (void *)a;
		return 0;
	}
	int r = real_posix_memalign(memptr, alignment, size);
	if (r == 0)
		atomic_fetch_add(&total_bytes, (unsigned long long)size);
	return r;
}

void *aligned_alloc(size_t alignment, size_t size) {
	if (!ensure_init() || !real_aligned_alloc) {
		void *p = NULL;
		int r = posix_memalign(&p, alignment, size);
		if (r != 0) {
			errno = r;
			return NULL;
		}
		return p;
	}
	void *p = real_aligned_alloc(alignment, size);
	if (p)
		atomic_fetch_add(&total_bytes, (unsigned long long)size);
	return p;
}

__attribute__((destructor)) static void shim_fini(void) {
	static const char marker[] = "snip_alloc_total_bytes=";
	char buf[64];
	unsigned long long v = atomic_load(&total_bytes);
	size_t n = sizeof(marker) - 1;
	memcpy(buf, marker, n);
	char rev[24];
	int r = 0;
	if (v == 0)
		rev[r++] = '0';
	while (v > 0) {
		rev[r++] = (char)('0' + (v % 10));
		v /= 10;
	}
	while (r > 0)
		buf[n++] = rev[--r];
	buf[n++] = '\n';
	ssize_t w = write(2, buf, n);
	(void)w;
}
`

// InnerIsJava reports whether a resolved snippet argv runs the JVM.
func InnerIsJava(inner []string) bool {
	return len(inner) > 0 && filepath.Base(inner[0]) == "java"
}

// TemplateHeadIsJava reports whether a command template's argv0 is
// the JVM. Holes are cp/script/stem (never the binary), so the
// literal head classifies the leg without expansion; used only for
// provenance when no case resolved an argv.
func TemplateHeadIsJava(template string) bool {
	fields := strings.Fields(template)
	if len(fields) == 0 {
		return false
	}
	return filepath.Base(strings.Trim(fields[0], `"'`)) == "java"
}

var (
	allocShimOnce sync.Once
	allocShimPath string
	allocShimErr  error
)

// EnsureAllocShim compiles the counting shim once per process and
// returns the .so path. Linux-only: LD_PRELOAD/glibc
// interposition has no macOS equivalent (and valgrind, the shim's
// partner, is Linux-only in practice too).
func EnsureAllocShim() (string, error) {
	if runtime.GOOS != "linux" {
		return "", errors.New("alloc shim needs Linux (LD_PRELOAD)")
	}
	allocShimOnce.Do(func() {
		allocShimPath, allocShimErr = buildAllocShim()
	})
	return allocShimPath, allocShimErr
}

func buildAllocShim() (string, error) {
	cc, err := exec.LookPath("cc")
	if err != nil {
		return "", errors.New("alloc shim needs a C compiler (cc not found)")
	}
	dir, err := os.MkdirTemp("", "snip-alloc-shim-")
	if err != nil {
		return "", err
	}
	src := filepath.Join(dir, "alloc_shim.c")
	if err := os.WriteFile(src, []byte(allocShimSource), 0o644); err != nil {
		return "", err
	}
	out := filepath.Join(dir, "alloc_shim.so")
	_, stderr, err := snipcore.Run([]string{cc, "-shared", "-fPIC", "-O2", "-o", out, src, "-ldl"}, nil, 120)
	if err != nil {
		return "", fmt.Errorf("alloc shim build failed: %s", snipcore.LastLine(stderr))
	}
	return out, nil
}

// ParseAllocShimTotal extracts the shim's total from child stderr.
// The destructor prints exactly one line; the last marker match
// wins, so a program that echoes the marker cannot shadow the
// real total.
func ParseAllocShimTotal(text string) (int64, error) {
	const marker = "snip_alloc_total_bytes="
	var value int64
	var ok bool
	var bad string
	for _, line := range strings.Split(text, "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.HasPrefix(trimmed, marker) {
			continue
		}
		n, err := strconv.ParseInt(strings.TrimPrefix(trimmed, marker), 10, 64)
		if err != nil || n < 0 {
			bad = trimmed
			continue
		}
		value, ok = n, true
	}
	if !ok {
		if bad != "" {
			return 0, fmt.Errorf("alloc-shim total unreadable: %s", snipcore.PyRepr(bad))
		}
		return 0, fmt.Errorf("alloc-shim total unreadable: no snip_alloc_total_bytes line")
	}
	return value, nil
}
