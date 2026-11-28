#!/usr/bin/env bash
# ios-simulators: `run` shuts down what its command booted (and only that, on any exit), `sweep` shuts down only agents'
# Simulators up past the idle bound -- never the owner's hand-booted one, never one a test process still names.
# xcrun and ps are stubs. Run: bash scripts/ios-simulators-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
TOOL="$REPO_ROOT/scripts/ios-simulators"

printf '\033[36m▸\033[0m ios-simulators\n'
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
export KINOWO_IOS_SIM_LEDGER="$work/ledger"
# the stub xcrun: devices are files in $work/dev named <udid>=<name>, their state the file's content
mkdir -p "$work/dev"
cat > "$work/xcrun" <<STUB
#!/usr/bin/env bash
dev="$work/dev"
case "\$2" in
  list) python3 - "\$dev" <<'PY'
import json, os, sys
d = sys.argv[1]
print(json.dumps({"devices": {"iOS 27": [
  {"udid": f.split("=")[0], "name": f.split("=", 1)[1], "state": open(os.path.join(d, f)).read().strip()} for f in sorted(os.listdir(d))]}}))
PY
  ;;
  boot) f=\$(ls "\$dev" | grep "^\$3=") && echo Booted > "\$dev/\$f" ;;
  shutdown) f=\$(ls "\$dev" | grep "^\$3=") && echo Shutdown > "\$dev/\$f"; echo "\$3" >> "$work/shutdowns" ;;
esac
STUB
chmod +x "$work/xcrun"; export XCRUN="$work/xcrun"
device() { echo "${3:-Shutdown}" > "$work/dev/$1=$2"; }
state() { cat "$work/dev/$1="*; }
device OWNER "iPhone 17" Booted
: > "$work/no-ps"; export IOS_SIM_PS_COMMAND="cat $work/no-ps"  # no foreign test process, whatever runs on this machine
device NEW "iPhone 16e"

# the stub gov-run: logs its arguments, and waits for $work/admit while $work/busy exists (a busy governor), then execs
cat > "$work/gov-run" <<STUB
#!/usr/bin/env bash
echo "\$*" >> "$work/gov.log"
while [ -e "$work/busy" ] && [ ! -e "$work/admit" ]; do sleep 0.05; done
while [ "\$1" != -- ]; do shift; done; shift
exec "\$@"
STUB
chmod +x "$work/gov-run"; export GOV_RUN="$work/gov-run"

# run: shuts down what its command booted, not the owner's
"$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW" > /dev/null; rc=$?
check "run shuts down the Simulator its command booted" "0 Shutdown" "$rc $(state NEW)"
check "...and leaves the one already booted before it" "Booted" "$(state OWNER)"
# the command fails and is killed: still shut down, and its status is the run's
device NEW "iPhone 16e"
"$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW; exit 7" > /dev/null; rc=$?
check "run keeps the command's exit status and still shuts down" "7 Shutdown" "$rc $(state NEW)"
device NEW "iPhone 16e"
"$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW; kill -TERM \$\$" > /dev/null; rc=$?
check "run shuts down after its command was killed" "Shutdown" "$(state NEW)"

# another agent boots its own Simulator during the run and a test process of its own names it: not ours to shut down
device NEW "iPhone 16e"; device OTHER "iPhone 16 Pro"
echo "999999 00:10 xcodebuild test -destination platform=iOS Simulator,id=OTHER" > "$work/foreign-ps"
IOS_SIM_PS_COMMAND="cat $work/foreign-ps" "$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW; '$work/xcrun' simctl boot OTHER" > /dev/null
check "run shuts down its own Simulator but not one another agent's test process names" "Shutdown Booted" "$(state NEW) $(state OTHER)"
device OTHER "iPhone 16 Pro"

# a run killed hard leaves its Simulator in the ledger, for sweep
device NEW "iPhone 16e"; rm -rf "$KINOWO_IOS_SIM_LEDGER"
IOS_SIM_POLL_SECONDS=0.1 "$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW; echo \$\$ > '$work/child.pid'; sleep 30" > /dev/null 2>&1 &
wrapper=$!
wait_until "the run claimed the Simulator it booted" '[ -e "$KINOWO_IOS_SIM_LEDGER/NEW" ]'
kill -9 "$wrapper" 2>/dev/null; kill "$(cat "$work/child.pid")" 2>/dev/null; wait "$wrapper" 2>/dev/null
check "run writes the Simulator it booted to the ledger while it runs" "yes" "$([ -e "$KINOWO_IOS_SIM_LEDGER/NEW" ] && echo yes || echo no)"
device NEW "iPhone 16e"; rm -rf "$KINOWO_IOS_SIM_LEDGER"

# governor: the command runs under a ticket, sized and named; a busy governor makes the boot wait
: > "$work/gov.log"; device NEW "iPhone 16e"
"$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW" > /dev/null
check "run takes a governor ticket for the boot and the test run" "--job ios-simulator --cores 6 -- bash" "$(sed 's/ -c .*//' "$work/gov.log")"
device NEW "iPhone 16e"; touch "$work/busy"; rm -f "$work/admit" "$work/shutdowns"
"$TOOL" run -- bash -c "'$work/xcrun' simctl boot NEW" > /dev/null &
waiter=$!
sleep 0.5
check "a busy governor makes the boot wait" "Shutdown" "$(state NEW)"
touch "$work/admit"; wait "$waiter"; rm -f "$work/busy" "$work/admit"
check "...and the run goes on once admitted (booted, then shut down by it)" "1" "$(grep -c NEW "$work/shutdowns")"

# reuse: a device of the destination's name already booted is addressed by udid, boots nothing, and is never shut down
device HAND "iPhone 17 Pro" Booted
: > "$work/args"
"$TOOL" run -- bash -c 'echo "$@" > "'"$work"'/args"' _ xcodebuild test -destination "platform=iOS Simulator,name=iPhone 17 Pro,OS=latest" > /dev/null
check "run points a destination at the already-booted device of that name" "xcodebuild test -destination platform=iOS Simulator,id=HAND" "$(cat "$work/args")"
check "...and leaves that device booted" "Booted" "$(state HAND)"
: > "$work/args"
"$TOOL" run -- bash -c 'echo "$@" > "'"$work"'/args"' _ xcodebuild test -destination "platform=iOS Simulator,name=iPhone 16e" > /dev/null
check "a destination whose device is not booted is left alone (xcodebuild boots it, under the ticket)" "xcodebuild test -destination platform=iOS Simulator,name=iPhone 16e" "$(cat "$work/args")"
device HAND "iPhone 17 Pro"

# sweep
device NEW "iPhone 16e"
device OLDRED "red33-iPhone17" Booted
device YOUNGRED "red34-iPhone17" Booted
device BUSYRED "agent-iPhone" Booted
device LEDGER "iPhone 15" Booted
device HAND "iPhone 17 Pro" Booted
mkdir -p "$KINOWO_IOS_SIM_LEDGER"; : > "$KINOWO_IOS_SIM_LEDGER/LEDGER"
cat > "$work/ps" <<PS
1 3-04:00:00 /x/CoreSimulator/Devices/OWNER/launchd_sim
2 05:00:00 /x/CoreSimulator/Devices/OLDRED/launchd_sim
3 00:30:00 /x/CoreSimulator/Devices/YOUNGRED/launchd_sim
4 09:00:00 /x/CoreSimulator/Devices/BUSYRED/launchd_sim
5 09:00:00 xcodebuild test -destination platform=iOS Simulator,id=BUSYRED
6 1-00:00:00 /x/CoreSimulator/Devices/LEDGER/launchd_sim
7 10:00:00 /x/CoreSimulator/Devices/HAND/launchd_sim
PS
export IOS_SIM_PS_COMMAND="cat $work/ps"
rm -f "$work/shutdowns"
"$TOOL" sweep --dry-run > /dev/null
check "sweep --dry-run shuts nothing down" "no" "$([ -f "$work/shutdowns" ] && echo yes || echo no)"
rm -f "$work/shutdowns"
"$TOOL" sweep > /dev/null
check "sweep shuts down an agent-named Simulator up past 2 h, and a ledger one" "LEDGER OLDRED" "$(sort "$work/shutdowns" | tr '\n' ' ' | sed 's/ $//')"
check "...never the owner's, hand-booted for days" "Booted Booted" "$(state OWNER) $(state HAND)"
check "...nor a young agent one, nor one a test process names" "Booted Booted" "$(state YOUNGRED) $(state BUSYRED)"
check "...and the ledger entry goes with the shutdown" "no" "$([ -e "$KINOWO_IOS_SIM_LEDGER/LEDGER" ] && echo yes || echo no)"

spec_summary
