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

cat >"$tmp/nophone.json" <<'JSON'
{"devices":{"com.apple.CoreSimulator.SimRuntime.iOS-26-2":[{"name":"iPad (A16)","udid":"IPAD","isAvailable":true}]}}
JSON
SIMCTL_JSON="$tmp/nophone.json" "$HERE/ios-sim-destination.sh" >/dev/null 2>&1; code=$?
check "no iPhone simulator at all fails the step" "1" "$code"

spec_summary
