#!/usr/bin/env bash
#
# Run the local `/debug` mirror sync as a macOS launchd user agent, so it starts
# at login and restarts on failure — no terminal to babysit. The agent runs
# `mirror.sh`, which self-manages the whole stack: it brings up its own flyctl
# tunnel, (re)ensures the native brew-managed mirror Mongo, seeds/re-seeds, and
# tails prod's change stream. The Mongo is itself a `brew services` agent (see
# start-local-mongo.sh), so it restarts at login too; this agent manages the rest.
#
# Usage:
#   scripts/local-mirror/service.sh install     # install + start, runs at login
#   scripts/local-mirror/service.sh uninstall   # stop + remove the agent
#   scripts/local-mirror/service.sh status      # show state + pid
#   scripts/local-mirror/service.sh logs        # tail the agent log
#
# Prereqs (one-time): MONGODB_MOVIES_MIRROR_URI set in .env.local (see README),
# and flyctl logged in (`flyctl auth login`) so the agent can open the tunnel.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
LABEL="pl.kinowo.local-mirror"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
LOG="$HOME/Library/Logs/kinowo-local-mirror.log"
DOMAIN="gui/$(id -u)"

case "${1:-}" in
  install)
    # Build PATH from where the tools actually live (launchd starts agents with a
    # bare PATH that wouldn't find flyctl/brew/mongosh). Resolve them in this
    # shell — which has the user's full PATH — and keep their dirs, de-duped.
    dirs=""
    for t in bash flyctl brew mongosh nc; do
      p="$(command -v "$t" 2>/dev/null || true)"
      if [ -z "$p" ]; then echo "[service] WARN: '$t' not found in PATH — the agent may fail to find it" >&2
      else dirs="$dirs:$(cd "$(dirname "$p")" && pwd)"; fi
    done
    BIN_PATH="$(printf '%s' "$dirs" | tr ':' '\n' | awk 'NF && !seen[$0]++' | paste -sd: -):/usr/bin:/bin"

    mkdir -p "$HOME/Library/LaunchAgents" "$(dirname "$LOG")"
    cat > "$PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/bash</string>
    <string>$HERE/mirror.sh</string>
  </array>
  <key>WorkingDirectory</key><string>$ROOT</string>
  <key>EnvironmentVariables</key>
  <dict><key>PATH</key><string>$BIN_PATH</string></dict>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>ThrottleInterval</key><integer>10</integer>
  <key>StandardOutPath</key><string>$LOG</string>
  <key>StandardErrorPath</key><string>$LOG</string>
</dict>
</plist>
PLIST
    # Reload cleanly: boot out any prior copy (ignore "not loaded"), then in.
    launchctl bootout "$DOMAIN/$LABEL" 2>/dev/null || true
    launchctl bootstrap "$DOMAIN" "$PLIST"
    launchctl enable "$DOMAIN/$LABEL"
    echo "[service] installed + started: $LABEL"
    echo "[service] plist: $PLIST"
    echo "[service] logs:  $LOG   (tail with: $0 logs)"
    ;;
  uninstall)
    launchctl bootout "$DOMAIN/$LABEL" 2>/dev/null || true
    rm -f "$PLIST"
    echo "[service] uninstalled: $LABEL"
    echo "[service] the native mirror Mongo is left running; stop it with:"
    echo "          brew services stop mongodb-community@7.0"
    ;;
  status)
    launchctl print "$DOMAIN/$LABEL" 2>/dev/null | grep -E '^\s+(state|pid) =' || echo "[service] not loaded"
    ;;
  logs)
    tail -n 40 -f "$LOG"
    ;;
  *)
    echo "usage: $0 {install|uninstall|status|logs}" >&2; exit 1;;
esac
