#!/usr/bin/env bash
# Print an xcodebuild -destination for an iPhone simulator this runner actually has.
#   xcodebuild ... -destination "$(scripts/ci/ios-sim-destination.sh)"
#
# A hard-coded model ("name=iPhone 15,OS=latest") breaks the day the runner image drops it:
# the order-independence iOS leg failed "Unable to find a device" on an Xcode 26 image that
# shipped iPhone 17s (run 36111347595), and ios.yml's XCUITest step, `continue-on-error`,
# silently stopped running at all. So pick from what `simctl` lists: the newest iOS runtime,
# and on it the plainest iPhone (shortest name -- "iPhone 17" over "iPhone 17 Pro Max").
# Addressed by UDID, so two runtimes carrying the same model can't make it ambiguous.
# SIMCTL_JSON (a file) stands in for `xcrun simctl list devices available -j` in tests.
set -euo pipefail

if [ -n "${SIMCTL_JSON:-}" ]; then json="$(cat "$SIMCTL_JSON")"
else json="$(xcrun simctl list devices available -j)"; fi

printf '%s' "$json" | python3 -c '
import json, re, sys
devices = json.load(sys.stdin)["devices"]
best = None
for runtime, sims in devices.items():
    m = re.search(r"SimRuntime\.iOS-(\d+(?:-\d+)*)$", runtime)
    if not m:
        continue
    version = tuple(int(p) for p in m.group(1).split("-"))
    for sim in sims:
        if sim.get("isAvailable", True) and sim["name"].startswith("iPhone"):
            key = (version, -len(sim["name"]), sim["name"])
            if best is None or key > best[0]:
                best = (key, sim["udid"])
if best is None:
    sys.exit("no available iPhone simulator on any iOS runtime")
print(f"platform=iOS Simulator,id={best[1]}")
'
