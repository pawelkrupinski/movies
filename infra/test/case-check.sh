# Purpose: The pass/fail counters and `check name expected actual` of the infra spec scripts (sourced by them)
# shellcheck shell=bash
# THE INFRA SPECS' CASE CHECK. Sourced by the infra tests that compare an expected value with what a host's
# evaluated config (or a script) gives: it defines `pass` and `fail` (counters the sourcing script reports and
# exits on) and `check <name> <expected> <actual>`, printing one `ok` or `FAIL` line per case.
pass=0
fail=0
check() {
  local name="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    printf '  ok   %s\n' "$name"
    pass=$((pass + 1))
  else
    printf '  FAIL %s -- expected %s, got %s\n' "$name" "$expected" "$actual"
    fail=$((fail + 1))
  fi
}
