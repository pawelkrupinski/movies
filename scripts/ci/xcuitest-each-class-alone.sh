#!/bin/sh
# Purpose: Every XCUITest class alone, each in its own `xcodebuild test-without-building` on the booted $DESTINATION
# The name order-independence.yml runs: scripts/ci/xcuitest_each_class_alone.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/xcuitest_each_class_alone.py" "$@"
