#!/usr/bin/env bash
# DevPanel commit gate: the devpanel.py actions against scripted devices, the
# process reapers against real processes, and the Swift panel compiled and
# self-tested. See test_devpanel.py.
cd "$(dirname "$0")" && exec /usr/bin/python3 -m unittest -v test_devpanel
