#!/usr/bin/env bash
# scripts/dead-code against a stub sbt (KINOWO_DEAD_CODE_SBT): it asks `deadCode <rows>` once, ranks what the rows say
# (scripts/dead_code.py: unreferenced first), keeps rows, report and log under --out, and a failed sbt step fails the
# tool with the log's tail. Run: bash scripts/dead-code-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m dead-code\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cat > "$work/sbt" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB_CALLS"
[ -n "${STUB_FAIL:-}" ] && { echo "[error] compile broke"; exit 1; }
out=${1#deadCode }
printf 'method\ttest-only\ttools.A.seam\thttp\thttp/src/main/scala/tools/A.scala\n' > "$out"
printf 'method\tunreferenced\ttools.HttpRead.checkJsonArray\thttp\thttp/src/main/scala/tools/HttpRead.scala\n' >> "$out"
STUB
chmod +x "$work/sbt"
export STUB_CALLS="$work/calls" KINOWO_DEAD_CODE_SBT="$work/sbt"

out=$("$REPO_ROOT/scripts/dead-code" --out "$work/keep" 2>&1); status=$?
check "the report: exit 0" "0" "$status"
check "...sbt asked deadCode once, into --out" "deadCode $work/keep/rows.tsv" "$(cat "$work/calls")"
check "...the counts first" "# dead-code: 1 unreferenced, 1 test-only" "$(echo "$out" | head -1)"
check "...the unreferenced method ranked in its own section" "1" \
  "$(echo "$out" | grep -c '^| 1 | method | `tools.HttpRead.checkJsonArray`')"
check "...rows, report and log kept" "rows.tsv report.md sbt.log" \
  "$(for f in rows.tsv report.md sbt.log; do [ -f "$work/keep/$f" ] && printf '%s ' "$f"; done | sed 's/ $//')"

out=$(STUB_FAIL=1 "$REPO_ROOT/scripts/dead-code" 2>&1); status=$?
check "a failed sbt step: exit 1" "1" "$status"
check "...naming the step and showing its log's tail" "2" "$(echo "$out" | grep -c -e 'compile broke' -e 'sbt deadCode failed')"

out=$("$REPO_ROOT/scripts/dead-code" --bogus 2>&1); status=$?
check "an unknown flag: exit 2" "2" "$status"
check "--help prints the header" "1" "$("$REPO_ROOT/scripts/dead-code" --help | grep -c '^THE DEAD-CODE FINDER')"
spec_summary
