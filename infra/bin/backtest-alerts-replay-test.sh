#!/usr/bin/env bash
# infra/bin/backtest-alerts-replay with ssh and gh faked: the host is this machine, the red run's log a canned one,
# the backtest a stub that fails at the revision whose `infra/rule-state` file says "bad" (under infra/: the replay ships only `git archive <rev> infra scripts`).
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
[ "$(cat infra/rule-state)" = bad ] && { echo "FAILED"; exit 1; }
exit "${STUB_EXIT:-0}"
STUB
chmod +x "$repo/infra/bin/"*
printf '#!/bin/sh\necho "%s"\n' "$work/venv" > "$repo/scripts/python-venv"; chmod +x "$repo/scripts/python-venv"
echo "pyyaml==0" > "$repo/scripts/pyyaml-requirements.txt"
echo bad > "$repo/infra/rule-state"; scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
scratch_git "$repo" tag broken
echo good > "$repo/infra/rule-state"; scratch_git "$repo" commit -qam fix; scratch_git "$repo" tag fixed

cat > "$fakebin/ssh" <<'STUB'
#!/usr/bin/env bash
echo "ssh $1" >> "$FAKE_SSH_LOG"; shift
printf '%s\n' "$1" >> "$FAKE_SSH_COMMANDS"
exec bash -c "$1"
STUB
cat > "$fakebin/gh" <<'STUB'
#!/usr/bin/env bash
printf 'backtest\tBacktest every rule\t2026-11-19T12:50:01.1234567Z   - IdentityIncrementalDrift (identity-regressions.rules): fired 86.9%%\n'
printf 'backtest\tBacktest every rule\t2026-11-19T12:50:01.1234567Z   - incident abc: HostDown should have fired and did not\n'
STUB
chmod +x "$fakebin/ssh" "$fakebin/gh"
export FAKE_SSH_LOG="$work/ssh.log" FAKE_SSH_COMMANDS="$work/ssh-commands.log" BACKTEST_REPLAY_PROMETHEUS_BINARY=/bin/true
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
check "the remote directories are removed" "0" "$(find /tmp -maxdepth 1 -name 'backtest-replay*' -newer "$work/ssh.log" 2>/dev/null | wc -l | tr -d ' ')"
# A name made from the user and pid is one another user on the host can pre-create (a symlink) before the unpack.
check "the host makes the directory itself (mktemp -d), never from the user and pid" "1 0" \
  "$(grep -c 'mktemp -d' "$work/ssh-commands.log" | awk '{print ($1>0)}') $(grep -c "backtest-replay-$(id -un)-" "$work/ssh-commands.log")"

# --local: both revisions replayed HERE, by their own backtest-alerts, over the fixture copy of ci-runner's snapshot
# (infra/test/fixtures/backtest-snapshot-remote, real Prometheus blocks) on the real Prometheus: a rule reading a metric
# that does not exist is DEAD at the base, its fix reads the real one at the head. Needs nix for the flake's
# Prometheus; the Linux container has none, and says so.
nix_bin="$(command -v nix || { [ -x /nix/var/nix/profiles/default/bin/nix ] && echo /nix/var/nix/profiles/default/bin/nix; } || true)"
if [ -n "$nix_bin" ]; then
  local_repo="$work/local"; mkdir -p "$local_repo/infra/bin" "$local_repo/infra/test" "$local_repo/infra/nix/files/monitoring/rules"
  scratch_repo "$local_repo"
  mkdir -p "$local_repo/scripts"; : > "$local_repo/scripts/.keep"
  for tool in backtest-alerts backtest-alerts-local backtest-alerts-replay; do cp "$REPO_ROOT/infra/bin/$tool" "$local_repo/infra/bin/"; done
  cp "$REPO_ROOT/infra/test/promql_selectors.py" "$local_repo/infra/test/"
  echo '{}' > "$local_repo/infra/test/alert-backtest-allowlist.yml"
  echo 'incidents: []' > "$local_repo/infra/test/alert-incidents.yml"
  rules="$local_repo/infra/nix/files/monitoring/rules/host-health.rules"
  memory_rule() { printf 'groups:\n  - name: host-health\n    rules:\n      - alert: MemoryAvailableLow\n        expr: %s / node_memory_MemTotal_bytes < 0.15\n        for: 15m\n' "$1"; }
  memory_rule node_memory_MemAvailable > "$rules"
  scratch_git "$local_repo" add -A; scratch_git "$local_repo" commit -qm base; scratch_git "$local_repo" tag broken
  memory_rule node_memory_MemAvailable_bytes > "$rules"
  scratch_git "$local_repo" commit -qam fix; scratch_git "$local_repo" tag fixed
  prometheus="$("$nix_bin" --extra-experimental-features 'nix-command flakes' build --no-link --print-out-paths \
    --inputs-from "$REPO_ROOT/infra" nixpkgs#prometheus | tail -1)/bin/prometheus"
  yaml_bin="$("$REPO_ROOT/scripts/python-venv" "$REPO_ROOT/scripts/pyyaml-requirements.txt" yaml)"
  local_replay() {
    (cd "$local_repo" && env PATH="$yaml_bin:$PATH" KINOWO_GOVERNOR=0 BACKTEST_PROMETHEUS_BINARY="$prometheus" \
      BACKTEST_SNAPSHOT_REMOTE="$REPO_ROOT/infra/test/fixtures/backtest-snapshot-remote/" \
      BACKTEST_LOCAL_CACHE="$work/local-cache" python3 infra/bin/backtest-alerts-replay --local "$@") > "$work/out" 2>&1
    echo $?
  }
  check "--local: a fix of a DEAD rule, base fails and head passes -> exit 0" "0" \
    "$(local_replay --alert MemoryAvailableLow --days 0.2 --base broken --head fixed)"
  check "...the trailer names --local" \
    "Fix-Replay: infra/bin/backtest-alerts-replay --local --alert MemoryAvailableLow --days 0.2 base=FAIL head=PASS" \
    "$(tail -1 "$work/out")"
  check "...the base's own verdict was DEAD on the metric it misread" "1" \
    "$(grep -c '^  - MemoryAvailableLow (host-health.rules): returned no data for the whole window and reads metrics with no series in it: node_memory_MemAvailable$' "$work/out")"
  check "...no ssh was asked" "0" "$(grep -c . "$FAKE_SSH_LOG.local" 2>/dev/null || echo 0)"
  check "--local, no fix: head still DEAD -> exit 1" "1" \
    "$(local_replay --alert MemoryAvailableLow --days 0.2 --base broken --head broken --offline)"
else
  printf '  (--local cases not run: no nix here to build the flake'"'"'s Prometheus)\n'
fi

spec_summary
