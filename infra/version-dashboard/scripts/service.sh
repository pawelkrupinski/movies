#!/bin/sh
# One entry point for the launchd service (the shape of bitcashier's version-dashboard).
#   install    copy both plists to ~/Library/LaunchAgents and load them
#   uninstall  unload and remove them
#   restart    lint + test this checkout, then SIGTERM the job: the server refuses new checks and
#              switches, finishes running ones, exits, and KeepAlive starts the new code
#   status     launchd state + /healthz (including any work in flight)
#   logs       follow the server logs
set -e
cd "$(dirname "$0")/.."
DOMAIN="gui/$(id -u)"
AGENTS="$HOME/Library/LaunchAgents"
JOBS="com.kinowo.nixos-dashboard com.kinowo.nixos-dashboard-watchdog"
HEALTH=http://127.0.0.1:8788/healthz
LOGS="$HOME/.kinowo-dashboard-logs"

case "${1:-}" in
  install)
    mkdir -p "$LOGS"
    for job in $JOBS; do
      cp "$job.plist" "$AGENTS/"
      /bin/launchctl bootout "$DOMAIN/$job" 2>/dev/null || true
      # bootout returns before the job is gone, and a bootstrap that lands first fails with
      # "5: Input/output error" -- wait (bounded) for it to be really unloaded.
      for _ in 1 2 3 4 5 6 7 8 9 10; do
        /bin/launchctl print "$DOMAIN/$job" >/dev/null 2>&1 || break
        sleep 1
      done
      /bin/launchctl bootstrap "$DOMAIN" "$AGENTS/$job.plist"
    done
    echo "installed; $HEALTH should answer within a few seconds"
    ;;
  uninstall)
    for job in $JOBS; do
      /bin/launchctl bootout "$DOMAIN/$job" 2>/dev/null || true
      rm -f "$AGENTS/$job.plist"
    done
    ;;
  restart)
    npm run --silent lint
    npm test --silent
    if curl -fsS --max-time 3 "$HEALTH" | grep -q '"running":\[\]'; then
      echo "nothing in flight; restarting"
    else
      echo "work in flight; the server finishes it before restarting (watch: $0 status)"
    fi
    /bin/launchctl kill SIGTERM "$DOMAIN/com.kinowo.nixos-dashboard"
    ;;
  status)
    /bin/launchctl print "$DOMAIN/com.kinowo.nixos-dashboard" | grep -E "state =|pid =|last exit" || true
    curl -fsS --max-time 3 "$HEALTH" && echo || echo "healthz not answering"
    ;;
  logs)
    tail -F "$LOGS/server.out.log" "$LOGS/server.err.log"
    ;;
  *)
    echo "usage: $0 install|uninstall|restart|status|logs" >&2
    exit 2
    ;;
esac
