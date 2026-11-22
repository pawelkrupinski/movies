#!/bin/sh
# Purpose: Every `swift test` case alone, in an order drawn from a seed — the iOS lane of the nightly order-independence workflow
# The name Order independence runs and replays: ios/scripts/swift_test_shuffled.py is the tool, its header says what
# it does, and how.
exec python3 "$(dirname "$0")/swift_test_shuffled.py" "$@"
