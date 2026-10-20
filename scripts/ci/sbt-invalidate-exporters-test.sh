#!/usr/bin/env bash
# sbt-invalidate-exporters.sh: a source with a wildcard export loses its compiled classes, so zinc recompiles it after an
# older build cache's restore; a source without one, and another source's classes, keep theirs.
set -euo pipefail

script="$(cd "$(dirname "$0")" && pwd)/sbt-invalidate-exporters.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fails=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected '$3', got '$2'"; fails=$((fails + 1)); fi; }

. "$(cd "$(dirname "$0")/../.." && pwd)/scripts/scratch-git.sh"
scratch_repo "$work/repo"
r="$work/repo"
mkdir -p "$r/identity/src/main/scala/services/identity" "$r/web/src/test/scala/controllers" "$r/web/src/main/scala/a"
printf 'package services.identity\n\nobject Measures {\n  export Shape.*\n}\nfinal class Slots\n' \
  > "$r/identity/src/main/scala/services/identity/Measures.scala"
printf 'package services.identity\n\nprivate[identity] object Shape { def x = 1 }\n' \
  > "$r/identity/src/main/scala/services/identity/Shape.scala"
printf 'package controllers\n\nclass ReviewSpec {\n  new Object {\n      export source.{decisions as _, *}\n    }\n}\n' \
  > "$r/web/src/test/scala/controllers/ReviewSpec.scala"
printf 'package a\n\nobject Named {\n  export other.{one, two}\n}\n' > "$r/web/src/main/scala/a/Named.scala"
scratch_git "$r" add -A

classes="$r/identity/target/scala-3.9.0/classes/services/identity"
tests="$r/web/target/scala-3.9.0/test-classes/controllers"
named="$r/web/target/scala-3.9.0/classes/a"
mkdir -p "$classes" "$tests" "$named"
touch "$classes/Measures.class" "$classes/Measures\$.class" "$classes/Measures.tasty" "$classes/Slots.class" \
  "$classes/Shape.class" "$classes/Shape\$.class" "$classes/MeasuresSpec.class" \
  "$tests/ReviewSpec.class" "$tests/ReviewSpec\$\$anon\$1.class" "$named/Named.class"

out=$(cd "$r" && "$script")
left() { (cd "$1" && ls | LC_ALL=C sort | paste -sd' ' -); }
check "the exporter's classes, tasty and its file's other top-level classes go" "$(left "$classes")" \
  "MeasuresSpec.class Shape$.class Shape.class"
check "a test source's exporter goes too, anonymous classes and all" "$(left "$tests")" ""
check "a named (non-wildcard) export is left alone" "$(left "$named")" "Named.class"
check "it names what it invalidated" "$out" "invalidated identity/src/main/scala/services/identity/Measures.scala (4 class files)
invalidated web/src/test/scala/controllers/ReviewSpec.scala (2 class files)"
check "a second run, nothing left to delete, says nothing" "$(cd "$r" && "$script")" ""
check "no build cache at all is no error" "$(rm -rf "$r/identity/target" "$r/web/target" && cd "$r" && "$script"; echo "exit $?")" "exit 0"

[ "$fails" -eq 0 ] && echo "all passed" || { echo "$fails failed"; exit 1; }
