#!/usr/bin/env bash
# infra/bin/backtest-alerts-replay with ssh and gh faked: the host is this machine, the red run's log a canned one,
# the backtest a stub that fails at the revision whose `rule-state` file says "bad".
# Run: bash infra/bin/backtest-alerts-replay-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m backtest-alerts-replay\n'
work="$(mktemp -d)"; work="$(cd "$work" && pwd -P)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"; fakebin="$work/bin"; mkdir -p "$fakebin" "$repo/infra/bin" "$repo/scripts"
scratch_repo "$repo"
cp "$REPO_ROOT/infra/bin/backtest-alerts-replay" "$repo/infra/bin/"
cat > "$repo/infra/bin/backtest-alerts" <<'STUB'
#!/usr/bin/env bash
# accepts --from-log
echo "STUB ARGS: $*"
[ -f red-run.log ] && echo "STUB LOG: $(head -1 red-run.log)"
[ "$(cat rule-state)" = bad ] && { echo "FAILED"; exit 1; }
exit "${STUB_EXIT:-0}"
STUB
chmod +x "$repo/infra/bin/"*
printf '#!/bin/sh\necho "%s"\n' "$work/venv" > "$repo/scripts/python-venv"; chmod +x "$repo/scripts/python-venv"
echo "pyyaml==0" > "$repo/scripts/pyyaml-requirements.txt"
echo bad > "$repo/rule-state"; scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
scratch_git "$repo" tag broken
echo good > "$repo/rule-state"; scratch_git "$repo" commit -qam fix; scratch_git "$repo" tag fixed

cat > "$fakebin/ssh" <<'STUB'
#!/usr/bin/env bash
echo "ssh $1" >> "$FAKE_SSH_LOG"; shift
exec bash -c "$1"
STUB
cat > "$fakebin/gh" <<'STUB'
#!/usr/bin/env bash
printf 'backtest\tBacktest every rule\t2026-11-19T12:50:01.1234567Z   - IdentityIncrementalDrift (identity-regressions.rules): fired 86.9%%\n'
printf 'backtest\tBacktest every rule\t2026-11-19T12:50:01.1234567Z   - incident abc: HostDown should have fired and did not\n'
STUB
chmod +x "$fakebin/ssh" "$fakebin/gh"
export FAKE_SSH_LOG="$work/ssh.log" BACKTEST_REPLAY_PROMETHEUS_BINARY=/bin/true
replay() { (cd "$repo" && env PATH="$fakebin:$PATH" python3 infra/bin/backtest-alerts-replay "$@") > "$work/out" 2>&1; echo $?; }

check "a fix: base fails, head passes -> exit 0" "0" "$(replay --from-run 7 --base broken --head fixed)"
check "...the trailer line, without --base/--head" \
  "Fix-Replay: infra/bin/backtest-alerts-replay --from-run 7 base=FAIL head=PASS" "$(tail -1 "$work/out")"
check "...scoped by the log's alerts with the timestamps stripped" "2" \
  "$(grep -c 'STUB ARGS: --source snapshot --no-write --snapshot .* --from-log red-run.log' "$work/out")"
check "...the log shipped to each host (twice) has the problem lines bare" "2" \
  "$(grep -c '^STUB LOG:   - IdentityIncrementalDrift (identity-regressions.rules): fired 86.9%$' "$work/out")"

check "no fix: both revisions fail -> exit 1" "1" "$(replay --from-run 7 --base broken --head broken)"
check "...base=FAIL head=FAIL" "Fix-Replay: infra/bin/backtest-alerts-replay --from-run 7 base=FAIL head=FAIL" "$(tail -1 "$work/out")"
check "a green base is no proof -> exit 1" "1" "$(replay --alert HostDown --group a.rules --base fixed --head fixed)"
check "...base=PASS head=PASS, the explicit scope passed on" "2 Fix-Replay: infra/bin/backtest-alerts-replay --alert HostDown --group a.rules base=PASS head=PASS" \
  "$(grep -c 'STUB ARGS.*--alert HostDown --group a.rules' "$work/out") $(tail -1 "$work/out")"

check "a harness error (exit 2) is not a verdict -> exit 2, no trailer" "2 0" \
  "$(STUB_EXIT=2 replay --alert HostDown --base fixed --head fixed) $(grep -c '^Fix-Replay' "$work/out")"
check "an unknown revision -> exit 2" "2" "$(replay --alert HostDown --base nope --head fixed)"
check "nothing to replay is a usage error" "2" "$(replay)"
check "the remote directories are removed" "0" "$(find /tmp -maxdepth 1 -name 'backtest-replay-*-*' -newer "$work/ssh.log" 2>/dev/null | wc -l | tr -d ' ')"

spec_summary
