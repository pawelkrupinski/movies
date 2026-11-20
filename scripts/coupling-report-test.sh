#!/usr/bin/env bash
# scripts/coupling-report's --simulate and --exemption-slack over a stub scripts/sbt-warm (the real one runs `sbt
# couplingSimulate` / `couplingExemptionSlack`; CouplingSimulationSpec and ExemptionSlackSpec hold their numbers): each
# runs its own task, never couplingReport, the moves file's path handed on absolute, and prints the task's markdown; a
# missing moves file, a failed or silent task are exit 2.
# Run: bash scripts/coupling-report-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m coupling-report\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/root/scripts"
cp "$REPO_ROOT/scripts/coupling-report" "$work/root/scripts/"
cat > "$work/root/scripts/sbt-warm" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" > "$(dirname "$0")/../called"
[ -z "${STUB_FAIL:-}" ] || { echo "[error] boom"; exit 1; }
set -- $1
[ -n "${STUB_SILENT:-}" ] || printf '# %s\n' "$1" > "${!#}"
STUB
chmod +x "$work/root/scripts/sbt-warm"
report() { ( cd "$work" && "$work/root/scripts/coupling-report" "$@" ); }

printf 'services.movies.Reading\tservices.evidence\n' > "$work/moves.tsv"
out="$(report --simulate moves.tsv 2>&1)"; status=$?
check "--simulate prints the simulation" "0 # couplingSimulate" "$status $out"
check "...from couplingSimulate alone, the moves file's path absolute" "couplingSimulate $work/moves.tsv" "$(cut -d' ' -f1-2 "$work/root/called")"

out="$(report --exemption-slack 2>&1)"; status=$?
check "--exemption-slack prints the slack" "0 # couplingExemptionSlack" "$status $out"
check "...from couplingExemptionSlack alone" "couplingExemptionSlack" "$(cut -d' ' -f1 "$work/root/called")"

check "--simulate without an existing moves file is exit 2" "2 2" \
  "$(report --simulate > /dev/null 2>&1; echo $?) $(report --simulate nothing.tsv > /dev/null 2>&1; echo $?)"
out="$(STUB_FAIL=1 report --exemption-slack 2>&1)"; status=$?
check "a failed task is exit 2, its log's tail shown" "2 1" "$status $(printf '%s\n' "$out" | grep -c 'boom')"
check "a task writing nothing is exit 2" "2" "$(STUB_SILENT=1 report --simulate moves.tsv > /dev/null 2>&1; echo $?)"

spec_summary
