#!/usr/bin/env bash
# Purpose: The stub sbt for the touched-code tools' specs (mutation-check, coverage-touched)
# THE STUB sbt FOR THE TOUCHED-CODE TOOLS' SPECS (scripts/mutation_check_test.py, scripts/coverage_touched_test.py):
# KINOWO_TOUCHED_SBT points at it. Each call is logged to $STUB_CALLS as `<cwd>|<command>|<command>...`, and each command
# answers as the real one would, from the fixtures the spec names:
#   quality/runMain quality.MethodSpans ... --out <f> ...   $STUB_SPANS copied to <f>
#   touchedSpecs <touched> <out>                            $STUB_SPECS copied to <out>
#   <module>/stryker                                        a Stryker report of $STUB_MUTANTS (`line status mutator
#                                                           replacement` rows) for $STUB_SOURCE, under target/
#   <module>/testOnly ...                                   exit $STUB_TEST_RC (default 0)
#   <module>/coverageReport                                 an scoverage.xml of $STUB_STATEMENTS (`line count` rows)
#                                                           for $STUB_SOURCE, under <module>/target/
# $STUB_FAIL_ON=<word>: the call holding a command with that word fails (exit 1).
set -uo pipefail
line="$PWD"
for a in "$@"; do line="$line|$a"; done
echo "$line" >> "$STUB_CALLS"
rc=0
for a in "$@"; do
  case "$a" in *"${STUB_FAIL_ON:-<none>}"*) echo "[error] $a failed"; exit 1 ;; esac
  case "$a" in
    "quality/runMain quality.MethodSpans "*)
      out=$(echo "$a" | sed -E 's/.*--out ([^ ]+).*/\1/'); cp "$STUB_SPANS" "$out" ;;
    "touchedSpecs "*)
      set -- $a; cp "$STUB_SPECS" "$3" ;;
    */stryker)
      mkdir -p target/stryker4s-report/1
      python3 - "$PWD" "$STUB_SOURCE" "$STUB_MUTANTS" > target/stryker4s-report/1/report.json <<'PY'
import json, sys
root, source, rows = sys.argv[1], sys.argv[2], open(sys.argv[3]).read().split("\n")
mutants = [{"mutatorName": m, "replacement": r, "status": s, "location": {"start": {"line": int(l), "column": 1}}}
           for l, s, m, r in (row.split(" ", 3) for row in rows if row.strip())]
print(json.dumps({"projectRoot": root, "files": {source: {"mutants": mutants}}}))
PY
      ;;
    */testOnly*) rc=${STUB_TEST_RC:-0} ;;
    */coverageReport)
      m=${a%%/*}; mkdir -p "$m/target/scala-3.x/scoverage-report"
      { echo "<scoverage><packages><package><classes><class><methods><method><statements>"
        n=0
        while read -r l count; do
          [ -n "$l" ] || continue
          n=$((n + 10))
          echo "<statement source=\"$PWD/$STUB_SOURCE\" line=\"$l\" start=\"$n\" end=\"$((n + 5))\" invocation-count=\"$count\" ignored=\"false\"></statement>"
        done < "$STUB_STATEMENTS"
        echo "</statements></method></methods></class></classes></package></packages></scoverage>"
      } > "$m/target/scala-3.x/scoverage-report/scoverage.xml" ;;
  esac
done
exit "$rc"
