package snipcore

import (
	"errors"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// ResolveClasspath ports run_tests.resolve_classpath: explicit --cp
// wins, then $CLOJURE_CP, then .ci/jars/*.jar, then $CLOJURE_JAR plus
// its sibling spec jars; otherwise the same fatal message.
func ResolveClasspath(root, explicit string) (string, error) {
	if explicit != "" {
		return explicit, nil
	}
	if env := strings.TrimSpace(os.Getenv("CLOJURE_CP")); env != "" {
		return env, nil
	}
	jars, err := filepath.Glob(filepath.Join(root, ".ci", "jars", "*.jar"))
	if err == nil && len(jars) > 0 {
		sort.Strings(jars)
		return strings.Join(jars, string(os.PathListSeparator)), nil
	}
	if jar := strings.TrimSpace(os.Getenv("CLOJURE_JAR")); jar != "" {
		dir := filepath.Dir(jar)
		candidates := []string{jar}
		for _, pattern := range []string{"spec.alpha*.jar", "core.specs*.jar"} {
			extra, err := filepath.Glob(filepath.Join(dir, pattern))
			if err == nil {
				sort.Strings(extra)
				candidates = append(candidates, extra...)
			}
		}
		return strings.Join(candidates, string(os.PathListSeparator)), nil
	}
	return "", errors.New("no Clojure classpath: pass --cp, set CLOJURE_CP/CLOJURE_JAR, or put jars under .ci/jars/")
}

// TemplateNeedsClasspath reports whether a command template
// references the {cp} hole, honoring {{/}} escapes like
// ExpandTemplate. snip measure tolerates a missing Clojure
// classpath when the template never uses it (native legs).
func TemplateNeedsClasspath(template string) bool {
	for i := 0; i < len(template); {
		if template[i] == '{' {
			if i+1 < len(template) && template[i+1] == '{' {
				i += 2
				continue
			}
			if strings.HasPrefix(template[i:], "{cp}") {
				return true
			}
		}
		i++
	}
	return false
}
