#!/bin/sh
# Purpose: Every instrumented (androidTest) class alone, in an order drawn from a seed — the emulator lane of the nightly order-independence workflow
# The name Order independence runs and replays: android/scripts/devtest_shuffled.py is the tool, its header says what
# it does, and how.
exec python3 "$(dirname "$0")/devtest_shuffled.py" "$@"
