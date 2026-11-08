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
device NEW "iPhone 16e"

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
