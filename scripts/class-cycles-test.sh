#!/usr/bin/env bash
# scripts/class-cycles over a stub scripts/sbt-warm (the real one runs `sbt couplingClassCycles`, ClassCyclesSpec holds
# its numbers): the package and the order reach the task as one sbt command, an order table's path absolute, the report
# reaches stdout and --out; a usage error, an order with a space, a failed or silent task are exit 2.
# Run: bash scripts/class-cycles-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m class-cycles\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/root/scripts"
cp "$REPO_ROOT/scripts/class-cycles" "$REPO_ROOT/scripts/own-checkout" "$work/root/scripts/"
# the stub task: records its command, writes a report to its second word unless told to fail or write nothing
cat > "$work/root/scripts/sbt-warm" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" > "$(dirname "$0")/../called"
[ -z "${STUB_FAIL:-}" ] || { echo "[error] boom"; exit 1; }
set -- $1
[ -n "${STUB_SILENT:-}" ] || printf '# Class cycles in `%s`\n' "$2" > "$3"
STUB
chmod +x "$work/root/scripts/sbt-warm"
cc() { ( cd "$work" && "$work/root/scripts/class-cycles" "$@" ); }

out="$(cc services.identity 2>&1)"; status=$?
check "the report reaches stdout" "0 # Class cycles in \`services.identity\`" "$status $out"
check "...from one couplingClassCycles command naming the package and the report" "couplingClassCycles services.identity" \
  "$(cut -d' ' -f1-2 "$work/root/called")"

cc services.identity --order 'vocabulary.*<*Measures,Listing<*' > /dev/null 2>&1
check "an inline order is the task's third word" "vocabulary.*<*Measures,Listing<*" "$(cut -d' ' -f4 "$work/root/called")"

printf 'Listing\t0\nIdentityMeasures\t1\n' > "$work/tiers.tsv"
cc services.identity --order tiers.tsv --out kept.md > /dev/null 2>&1
check "an order table's path is handed on absolute (the server runs in the root)" "$work/tiers.tsv" "$(cut -d' ' -f4 "$work/root/called")"
check "--out keeps the report, a relative path from where it was run" "# Class cycles in \`services.identity\`" "$(cat "$work/kept.md" 2>/dev/null)"

check "no package is a usage error" "2" "$(cc > /dev/null 2>&1; echo $?)"
check "two packages are a usage error" "2" "$(cc a b > /dev/null 2>&1; echo $?)"
check "an order with a space is refused" "2" "$(cc p --order 'a < b' > /dev/null 2>&1; echo $?)"
out="$(STUB_FAIL=1 cc p 2>&1)"; status=$?
check "a failed task is exit 2, its log's tail shown" "2 1" "$status $(printf '%s\n' "$out" | grep -c 'boom')"
check "a task writing no report is exit 2" "2" "$(STUB_SILENT=1 cc p > /dev/null 2>&1; echo $?)"

spec_summary
