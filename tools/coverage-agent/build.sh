#!/bin/bash
# Builds coverage-agent.jar (the agent and ASM, which it needs) next to this script. Reads only the sources and the ASM jar; writes the jar and a temp dir.
set -eu
HERE=$(cd "$(dirname "$0")" && pwd)
# The ASM jar: $ASM_JAR, or the Coursier cache on macOS or Linux.
ASM=${ASM_JAR:-}
for candidate in "$HOME/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm/9.8/asm-9.8.jar" \
                 "$HOME/.cache/coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm/9.8/asm-9.8.jar"; do
  [ -n "$ASM" ] || { [ -f "$candidate" ] && ASM=$candidate; }
done
[ -f "$ASM" ] || { echo "ASM jar not found: set ASM_JAR to an asm-9.x jar" >&2; exit 1; }
WORK=$(mktemp -d)
mkdir "$WORK/classes"
javac --release 17 -nowarn -cp "$ASM" -d "$WORK/classes" "$HERE"/rl/*.java
(cd "$WORK/classes" && jar xf "$ASM" org)   # the agent carries ASM, so the application's own classpath is not touched
printf 'Premain-Class: rl.CoverageAgent\n' > "$WORK/manifest.txt"
jar cfm "$HERE/coverage-agent.jar" "$WORK/manifest.txt" -C "$WORK/classes" .
echo "built $HERE/coverage-agent.jar"
