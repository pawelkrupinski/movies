#!/usr/bin/env bash
# Print an xcodebuild -destination for an iPhone simulator this runner actually has.
#   xcodebuild ... -destination "$(scripts/ci/ios-sim-destination.sh)"
#
# A hard-coded model ("name=iPhone 15,OS=latest") breaks the day the runner image drops it:
# the order-independence iOS leg failed "Unable to find a device" on an Xcode 26 image that
# shipped iPhone 17s (run 36111347595), and ios.yml's XCUITest step, `continue-on-error`,
# silently stopped running at all. So pick from what `simctl` lists, in this order:
#   1. the pinned model, IOS_SIM_DEVICE (default "iPhone 17"), on the newest runtime carrying it;
#   2. else the newest runtime's plainest numbered iPhone ("iPhone 18" over "iPhone 18 Pro Max");
#   3. else any iPhone at all.
# "Newest runtime" alone is not enough: Xcode 27.1's iOS 27.1 is a device-specific runtime that
# runs only the dual-screen "iPhone Duo", so rule 2 skips unnumbered models (Duo, Air, SE) and
# the run stays on the standard phone the layout tests are written against.
# Addressed by UDID, so two runtimes carrying the same model can't make it ambiguous.
# SIMCTL_JSON (a file) stands in for `xcrun simctl list devices available -j` in tests.
set -euo pipefail

if [ -n "${SIMCTL_JSON:-}" ]; then json="$(cat "$SIMCTL_JSON")"
else json="$(xcrun simctl list devices available -j)"; fi

printf '%s' "$json" | IOS_SIM_DEVICE="${IOS_SIM_DEVICE:-iPhone 17}" python3 -c '
import json, os, re, sys
pinned = os.environ["IOS_SIM_DEVICE"]
iphones = []  # (runtime version, name, udid)
for runtime, sims in json.load(sys.stdin)["devices"].items():
    m = re.search(r"SimRuntime\.iOS-(\d+(?:-\d+)*)$", runtime)
    if not m:
        continue
    version = tuple(int(p) for p in m.group(1).split("-"))
    iphones += [(version, sim["name"], sim["udid"]) for sim in sims
                if sim.get("isAvailable", True) and sim["name"].startswith("iPhone")]

def newest_plainest(candidates):
    return max(candidates, key=lambda c: (c[0], -len(c[1]), c[1]), default=None)

best = (newest_plainest([c for c in iphones if c[1] == pinned])
        or newest_plainest([c for c in iphones if re.match(r"iPhone \d", c[1])])
        or newest_plainest(iphones))
if best is None:
    sys.exit("no available iPhone simulator on any iOS runtime")
print(f"platform=iOS Simulator,id={best[2]}")
'
