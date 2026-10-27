#!/bin/bash
# RETIRED (2026-10-27): the stale-JVM reaper's rules (orphan, hog, duplicate / gone-dir sbt servers) are agentctl's,
# `~/bin/agentctl reap`, run every 15 min by launchd net.bitcashier.agentctl-maintain (~/bin/AGENTCTL.md, "maintain");
# every kill is a line of ~/.cache/agentctl/reap.log. This shim keeps old callers (~/.local/bin/reap-stale-jvms.sh links
# here) working: `--dry-run` lists without killing, as before.
set -euo pipefail
agentctl="${AGENTCTL:-$HOME/bin/agentctl}"
[ -x "$agentctl" ] || { echo "reap-stale-jvms.sh: retired; its rules are agentctl's, and $agentctl is not installed" >&2; exit 1; }
exec "$agentctl" reap "$@"
