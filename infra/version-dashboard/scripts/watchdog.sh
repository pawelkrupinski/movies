#!/bin/sh
# Health check for com.kinowo.nixos-dashboard, run every 30s by com.kinowo.nixos-dashboard-watchdog.
#
# Probes /healthz over HTTP -- not a bare TCP connect, which a wedged event loop still accepts at
# the kernel -- and kickstarts the job when two probes 2s apart both fail. A draining server still
# answers /healthz, so a restart waiting on a running switch is never cut short here.

URL=http://127.0.0.1:8788/healthz

check() { /usr/bin/curl -fsS --max-time 3 "$URL" >/dev/null 2>&1; }

if check; then exit 0; fi
sleep 2
if check; then exit 0; fi

echo "$(date '+%Y-%m-%dT%H:%M:%S') $URL not answering -> kickstart com.kinowo.nixos-dashboard"
/bin/launchctl kickstart -k "gui/$(id -u)/com.kinowo.nixos-dashboard"
