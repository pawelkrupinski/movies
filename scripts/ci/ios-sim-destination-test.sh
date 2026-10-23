#!/usr/bin/env bash
# ios-sim-destination.sh against canned `simctl list devices available -j` output, so it runs
# anywhere. The promise: name an iPhone simulator the runner actually HAS, on its newest iOS
# runtime -- never a hard-coded model the image may have dropped (the order-independence iOS
# leg asked for "iPhone 15" on an Xcode 26 image that had none: run 36111347595).
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/../shell-spec.sh"
printf '\033[36m▸\033[0m ios-sim-destination.sh\n'

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

cat >"$tmp/xcode26.json" <<'JSON'
{"devices":{
 "com.apple.CoreSimulator.SimRuntime.iOS-26-2":[
   {"name":"iPad (A16)","udid":"IPAD-262","isAvailable":true},
   {"name":"iPhone 17 Pro","udid":"IP17PRO-262","isAvailable":true},
   {"name":"iPhone 17","udid":"IP17-262","isAvailable":true}],
 "com.apple.CoreSimulator.SimRuntime.iOS-26-5":[
   {"name":"iPhone 17 Pro Max","udid":"IP17PM-265","isAvailable":true},
   {"name":"iPhone 17","udid":"IP17-265","isAvailable":true},
   {"name":"iPhone Air","udid":"IPAIR-265","isAvailable":true}],
 "com.apple.CoreSimulator.SimRuntime.visionOS-26-5":[
   {"name":"Apple Vision Pro","udid":"AVP","isAvailable":true}]}}
JSON
check "the newest iOS runtime's plain iPhone, by id" "platform=iOS Simulator,id=IP17-265" \
  "$(SIMCTL_JSON="$tmp/xcode26.json" "$HERE/ios-sim-destination.sh")"

# Xcode 27.1 ships iOS 27.1 as a device-specific runtime (build 24A94232) that runs ONLY the
# "iPhone Duo"; the newest-runtime rule picked it, and the XCUITest leg ran on a dual-screen
# device whose rotation the simulator renders with the status bar and the app disagreeing.
cat >"$tmp/xcode27.json" <<'JSON'
{"devices":{
 "com.apple.CoreSimulator.SimRuntime.iOS-27-0":[
   {"name":"iPhone 17 Pro","udid":"IP17PRO-270","isAvailable":true},
   {"name":"iPhone 17","udid":"IP17-270","isAvailable":true},
   {"name":"iPhone Air","udid":"IPAIR-270","isAvailable":true}],
 "com.apple.CoreSimulator.SimRuntime.iOS-27-1":[
   {"name":"iPhone Duo","udid":"DUO-271","isAvailable":true}]}}
JSON
check "the pinned iPhone 17 over a newer runtime's iPhone Duo" "platform=iOS Simulator,id=IP17-270" \
  "$(SIMCTL_JSON="$tmp/xcode27.json" "$HERE/ios-sim-destination.sh")"

cat >"$tmp/no17.json" <<'JSON'
{"devices":{
 "com.apple.CoreSimulator.SimRuntime.iOS-28-0":[
   {"name":"iPhone 18 Pro","udid":"IP18PRO-280","isAvailable":true},
   {"name":"iPhone 18","udid":"IP18-280","isAvailable":true},
   {"name":"iPhone Air","udid":"IPAIR-280","isAvailable":true}],
 "com.apple.CoreSimulator.SimRuntime.iOS-28-1":[
   {"name":"iPhone Duo","udid":"DUO-281","isAvailable":true}]}}
JSON
check "no iPhone 17: the newest runtime's plainest numbered iPhone" "platform=iOS Simulator,id=IP18-280" \
  "$(SIMCTL_JSON="$tmp/no17.json" "$HERE/ios-sim-destination.sh")"

cat >"$tmp/duoonly.json" <<'JSON'
{"devices":{"com.apple.CoreSimulator.SimRuntime.iOS-27-1":[{"name":"iPhone Duo","udid":"DUO","isAvailable":true}]}}
JSON
check "only an unnumbered iPhone: still runs, on it" "platform=iOS Simulator,id=DUO" \
  "$(SIMCTL_JSON="$tmp/duoonly.json" "$HERE/ios-sim-destination.sh")"

cat >"$tmp/nophone.json" <<'JSON'
{"devices":{"com.apple.CoreSimulator.SimRuntime.iOS-26-2":[{"name":"iPad (A16)","udid":"IPAD","isAvailable":true}]}}
JSON
SIMCTL_JSON="$tmp/nophone.json" "$HERE/ios-sim-destination.sh" >/dev/null 2>&1; code=$?
check "no iPhone simulator at all fails the step" "1" "$code"

spec_summary
