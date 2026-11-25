#!/usr/bin/env bash
# scripts/gov-parallel (THE GOVERNED FAN-OUT for a shell, jvm-gate.sh gov_width) against a stub governor that admits one
# ticket at a time: a second fan-out waits while the first holds its cores and starts by itself once they are given
# back; a fan-out inside one (its commands see AGENTCTL_GOV_WIDTH=1) takes no ticket and never waits on its parent;
# under a ticket of width W it runs at most W wide; ungoverned (KINOWO_GOVERNOR=0) as asked; a refusing governor one wide.
# Run: bash scripts/gov-parallel-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m gov-parallel\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
gp="$REPO_ROOT/scripts/gov-parallel"

# the stub: `gov admit` records its call, then waits (writing <ticket>.waiting) while any OTHER ticket's lock in the
# governor's directory is held -- one ticket at a time -- and takes its own on descriptor 7, as the real one does
cat > "$work/agentctl" <<'STUB'
#!/usr/bin/env bash
[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores route width"; exit 0; }
printf '%s\n' "$*" >> "$GOV_CALLS"
exec python3 -I -B -c '
import fcntl, glob, os, sys, time
me = sys.argv[sys.argv.index("--ticket") + 1]
def held(p):
    if os.path.realpath(p) == os.path.realpath(me):
        return False
    with open(p, "a") as f:
        try:
            fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            return True
        fcntl.flock(f, fcntl.LOCK_UN)
        return False
for _ in range(4800):
    if not any(held(p) for p in glob.glob(os.path.join(os.path.dirname(me), "t-*.lock"))):
        break
    open(me + ".waiting", "w").close()
    time.sleep(0.05)
fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)' "$@"
STUB
chmod +x "$work/agentctl"
export GOV_CALLS="$work/calls"
governed() { spec_governed "$work/agentctl" "$@"; }
# overlap <log> -> the most commands that ran at once, from their +1 / -1 lines
overlap() { awk '{ n += $1; if (n > m) m = n } END { print m + 0 }' "$1"; }
cat > "$work/item" <<'ITEM'
#!/usr/bin/env bash
echo 1 >> "$1"; echo "${AGENTCTL_GOV_WIDTH:-}" >> "$1.width"; sleep 0.3; echo -1 >> "$1"
ITEM
chmod +x "$work/item"

: > "$GOV_CALLS"
( seq 1 1 | governed "$gp" --job first -P 4 bash -c 'touch "$1/first.ran"; while [ ! -e "$1/first.release" ]; do sleep 0.05; done' _ "$work" ) > "$work/first.out" 2>&1 &
first=$!
wait_until "the first fan-out runs" '[ -e "$work/first.ran" ]'
( seq 1 1 | governed "$gp" --job second -P 4 bash -c 'touch "$1/second.ran"' _ "$work" ) > "$work/second.out" 2>&1 &
second=$!
wait_until "the second asked the governor and waits" 'ls "$(dirname "$work/agentctl")"/gov/t-*.waiting > /dev/null 2>&1'
check "two fan-outs started together: the second waits while the first holds the cores, not started" "absent" \
  "$([ -e "$work/second.ran" ] && echo ran || echo absent)"
touch "$work/first.release"; wait "$first"; wait "$second"; status=$?
check "...once the first gave its ticket back, the second starts by itself and passes" "0 ran" \
  "$status $([ -e "$work/second.ran" ] && echo ran)"
check "...each its own ticket, charged its job and the width it asked" "1 1" \
  "$(grep -c -- '--job first --cores 4$' "$GOV_CALLS") $(grep -c -- '--job second --cores 4$' "$GOV_CALLS")"

: > "$GOV_CALLS"; : > "$work/nested"
# under spec_bounded: a nested fan-out waiting on its own parent would hang, and fails the case (124) instead
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 3 | spec_bounded env -u AGENTCTL_GOV_WIDTH -u KINOWO_GOVERNOR_FEATURES -u KINOWO_JVM_SLOT_HELD -u KINOWO_GOV_CPU_HELD \
  KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin AGENTCTL_GOV_DIR="$work/gov" \
  "$gp" --job outer -P 3 -n 1 bash -c 'seq 1 2 | "$1" --job inner -P 4 -n 1 "$2" "$3"' _ "$gp" "$work/item" "$work/nested" > "$work/out" 2>&1
status=$?
check "a fan-out inside a governed one: no deadlock, one ticket (the outer's), each inner command one core" "0 1 1" \
  "$status $(wc -l < "$GOV_CALLS" | tr -d ' ') $(sort -u "$work/nested.width" | paste -sd' ' -)"
check "...the inner fan-outs ran one wide each: all 6 commands, at most the outer's 3 at once" "6 yes" \
  "$(grep -c '^1$' "$work/nested") $([ "$(overlap "$work/nested")" -le 3 ] && echo yes)"

: > "$GOV_CALLS"; : > "$work/under"
seq 1 6 | governed env AGENTCTL_GOV_WIDTH=2 "$gp" --job j -P 8 -n 1 "$work/item" "$work/under" > /dev/null 2>&1
check "under a ticket of width 2 (a check's hook): none asked, -P 8 runs at most 2 at once" "0 6 2" \
  "$(wc -l < "$GOV_CALLS" | tr -d ' ') $(grep -c '^1$' "$work/under") $(overlap "$work/under")"

: > "$GOV_CALLS"; : > "$work/free"
seq 1 3 | KINOWO_GOVERNOR=0 "$gp" --job j -P 3 -n 1 "$work/item" "$work/free" > /dev/null 2>&1
check "ungoverned (KINOWO_GOVERNOR=0, Linux CI): no ticket, every command run" "0 3" \
  "$(wc -l < "$GOV_CALLS" | tr -d ' ') $(grep -c '^1$' "$work/free")"

printf '#!/usr/bin/env bash\n[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores"; exit 0; }\nexit 1\n' > "$work/refusing"
chmod +x "$work/refusing"; : > "$work/refused"
out=$(seq 1 3 | spec_governed "$work/refusing" "$gp" --job j -P 3 -n 1 "$work/item" "$work/refused" 2>&1)
check "a governor that fails: warned, and the fan-out runs one wide, never its full width ticketless" "1 3 1" \
  "$(printf '%s\n' "$out" | grep -c 'running one wide') $(grep -c '^1$' "$work/refused") $(overlap "$work/refused")"
check "usage: no --job or no -P is 64" "64 64" \
  "$(echo | "$gp" -P 2 true > /dev/null 2>&1; echo $?) $(echo | "$gp" --job j true > /dev/null 2>&1; echo $?)"

spec_summary
