/* snip Pin tool: dynamic instruction count plus cumulative
 * malloc-family bytes.
 *
 * Output (-o file) is exactly two lines, the contract that
 * scripts/ci/go/internal/measure/parsers.go parses
 * (ParsePinCount/ParsePinAlloc take the last Count/Alloc line):
 *
 *   Count <total dynamic instructions, all threads>
 *   Alloc <cumulative malloc-family requested bytes, all threads>
 *
 * Alloc mirrors the LD_PRELOAD shim semantics (cumulative requested
 * bytes, frees never subtracted): malloc adds its size arg,
 * calloc/reallocarray add n*size, realloc/memalign/
 * aligned_alloc/posix_memalign add size. All probes read arguments
 * at IPOINT_BEFORE (entrypoint registers are clobbered by function
 * exit, so AFTER-point args would be garbage); a failed
 * posix_memalign therefore overcounts by its size, but failures
 * are deterministic per input, so the offset is a constant, never
 * noise. Like any native-heap counter it cannot see the JVM's
 * bulk-mmaped Java heap on the JVM leg (same caveat as the
 * callgrind docs); there it measures native allocation, its own
 * comparable signal.
 *
 * Counting is per-call-site exact: the main executable is skipped
 * (its malloc symbols are PLT stubs that would double-count the
 * libc implementation every call funnels through), and each
 * routine address is instrumented at most once (libc aliases such
 * as __libc_malloc share malloc's address). Addition commutes, so
 * the totals are thread-count independent.
 */

#include <cstdio>
#include <iostream>
#include <string>

#include "pin.H"

KNOB<std::string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool", "o", "inscount_alloc.out",
                                 "specify output file name");

static UINT64 icount = 0;
static UINT64 allocBytes = 0;

static FILE* out = 0;

// Instrumented routine entry addresses (libc aliases share one
// address; image loads take the lock).
static PIN_LOCK imgLock;
static ADDRINT doneAddrs[64];
static unsigned doneCount = 0;

static BOOL alreadyDone(ADDRINT addr)
{
    for (unsigned i = 0; i < doneCount; i++)
    {
        if (doneAddrs[i] == addr) return TRUE;
    }
    return FALSE;
}

static VOID markDone(ADDRINT addr)
{
    if (doneCount < sizeof(doneAddrs) / sizeof(doneAddrs[0])) doneAddrs[doneCount++] = addr;
}

// Hot path: one LOCK XADD per basic block.
VOID PIN_FAST_ANALYSIS_CALL CountIns(UINT32 c) { __sync_fetch_and_add(&icount, (UINT64)c); }

VOID RecordAlloc(ADDRINT size) { __sync_fetch_and_add(&allocBytes, (UINT64)size); }

VOID RecordCalloc(ADDRINT n, ADDRINT size) { __sync_fetch_and_add(&allocBytes, (UINT64)n * (UINT64)size); }

static VOID instrumentSizeFn(IMG img, const char* name, unsigned arg)
{
    RTN rtn = RTN_FindByName(img, name);
    if (!RTN_Valid(rtn)) return;
    PIN_GetLock(&imgLock, 1);
    BOOL dup = alreadyDone(RTN_Address(rtn));
    if (!dup) markDone(RTN_Address(rtn));
    PIN_ReleaseLock(&imgLock);
    if (dup) return;
    RTN_Open(rtn);
    RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)RecordAlloc, IARG_FUNCARG_ENTRYPOINT_VALUE, arg, IARG_END);
    RTN_Close(rtn);
}

static VOID instrumentCallocFn(IMG img, const char* name, unsigned nArg, unsigned sizeArg)
{
    RTN rtn = RTN_FindByName(img, name);
    if (!RTN_Valid(rtn)) return;
    PIN_GetLock(&imgLock, 1);
    BOOL dup = alreadyDone(RTN_Address(rtn));
    if (!dup) markDone(RTN_Address(rtn));
    PIN_ReleaseLock(&imgLock);
    if (dup) return;
    RTN_Open(rtn);
    RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)RecordCalloc, IARG_FUNCARG_ENTRYPOINT_VALUE, nArg,
                   IARG_FUNCARG_ENTRYPOINT_VALUE, sizeArg, IARG_END);
    RTN_Close(rtn);
}

VOID Image(IMG img, VOID* v)
{
    (void)v;
    if (IMG_IsMainExecutable(img)) return;
    instrumentSizeFn(img, "malloc", 0);
    instrumentSizeFn(img, "__libc_malloc", 0);
    instrumentCallocFn(img, "calloc", 0, 1);
    instrumentCallocFn(img, "__libc_calloc", 0, 1);
    instrumentSizeFn(img, "realloc", 1);
    instrumentSizeFn(img, "__libc_realloc", 1);
    instrumentCallocFn(img, "reallocarray", 1, 2);
    instrumentSizeFn(img, "memalign", 1);
    instrumentSizeFn(img, "__libc_memalign", 1);
    instrumentSizeFn(img, "aligned_alloc", 1);
    instrumentSizeFn(img, "posix_memalign", 2);
}

// Pin calls this function every time a new trace is encountered.
VOID Trace(TRACE trace, VOID* v)
{
    (void)v;
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        BBL_InsertCall(bbl, IPOINT_ANYWHERE, MAKE_AFUNPTR(CountIns), IARG_FAST_ANALYSIS_CALL, IARG_UINT32,
                       BBL_NumIns(bbl), IARG_END);
    }
}

VOID Fini(INT32 code, VOID* v)
{
    (void)code;
    (void)v;
    if (out == 0) return;
    fprintf(out, "Count %llu\n", (unsigned long long)icount);
    fprintf(out, "Alloc %llu\n", (unsigned long long)allocBytes);
    fclose(out);
    out = 0;
}

INT32 Usage()
{
    std::cerr << "snip tool: counts dynamic instructions and malloc-family bytes" << std::endl;
    std::cerr << std::endl << KNOB_BASE::StringKnobSummary() << std::endl;
    return -1;
}

int main(int argc, char* argv[])
{
    PIN_InitLock(&imgLock);
    if (PIN_Init(argc, argv)) return Usage();
    PIN_InitSymbols();

    out = fopen(KnobOutputFile.Value().c_str(), "w");

    TRACE_AddInstrumentFunction(Trace, 0);
    IMG_AddInstrumentFunction(Image, 0);
    PIN_AddFiniFunction(Fini, 0);

    PIN_StartProgram();

    return 0;
}
