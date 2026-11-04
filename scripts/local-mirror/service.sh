#!/usr/bin/env bash
#
# Run the local `/debug` mirror sync as a macOS launchd user agent, so it starts
# at login and restarts on failure — no terminal to babysit. The agent runs
# `mirror.sh`, which self-manages the whole stack: it brings up its own ssh
# tunnel to prod Mongo, (re)ensures the native brew-managed mirror Mongo, seeds/re-seeds, and
# tails prod's change stream. The Mongo is itself a `brew services` agent (see
# start-local-mongo.sh), so it restarts at login too; this agent manages the rest.
#
# Usage:
#   scripts/local-mirror/service.sh install     # install + start, runs at login
#   scripts/local-mirror/service.sh uninstall   # stop + remove the agent
#   scripts/local-mirror/service.sh status      # show state + pid
#   scripts/local-mirror/service.sh logs        # tail the agent log
#   scripts/local-mirror/service.sh agent       # what launchd runs (see below)
#
# THE SECRET. mirror.sh needs MONGODB_URI, which lives only in the Touch ID vault
# (`secrets`, namespace `movies`) — there is no .env.local any more. A LaunchAgent
# has no terminal, and mirror.sh deliberately EXITS to be restarted (a database
# supervisor died, its own code changed); if launchd restarted mirror.sh directly,
# every restart would re-exec it under `secrets run` and raise a fresh Touch ID
# dialog, at login and overnight alike. So launchd runs `service.sh agent`, which
# reads MONGODB_URI from the vault ONCE (one Touch ID when the agent starts — at
# login, or on `install`) and then restarts mirror.sh itself, the variable
# inherited. The value is held only in that process's environment: never written
# into the plist, the log, or any file.
#
# Prereqs (one-time): key-based ssh to the prod Mongo host working NON-INTERACTIVELY —
# `ssh -o BatchMode=yes root@<host> true` must succeed, because launchd has no
# terminal to answer a passphrase prompt with (see prod-tunnel.sh).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
LABEL="pl.kinowo.local-mirror"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
. "$HERE/log-path.sh"   # $MIRROR_LOG — the path mirror.sh also rotates
LOG="$MIRROR_LOG"
DOMAIN="gui/$(id -u)"

case "${1:-}" in
  install)
    # Build PATH from where the tools actually live (launchd starts agents with a
    # bare PATH that wouldn't find ssh/brew/mongosh). Resolve them in this
    # shell — which has the user's full PATH — and keep their dirs, de-duped.
    dirs=""
    for t in bash ssh brew mongosh nc secrets; do
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
    <string>$HERE/service.sh</string>
    <string>agent</string>
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
  agent)
    # Not exec: when the vault says no (Touch ID declined, or timed out with
    # nobody at the Mac) launchd's KeepAlive would otherwise re-prompt every
    # ThrottleInterval. Wait a while instead, then let launchd try again.
    if [ -z "${MONGODB_URI:-}" ]; then
      secrets run movies MONGODB_URI -- "$BASH" "$0" agent && exit 0
      echo "[service] could not read MONGODB_URI from the Touch ID vault — asking again in 10 minutes" >&2
      sleep 600
      exit 1
    fi
    child=""
    trap '[ -z "$child" ] || kill "$child" 2>/dev/null; exit 0' TERM INT
    while true; do
      "$BASH" "$HERE/mirror.sh" &
      child=$!
      wait "$child" || true
      child=""
      echo "[service] mirror.sh exited — restarting it in 10s" | stamp_lines
      sleep 10
    done
    ;;
  *)
    echo "usage: $0 {install|uninstall|status|logs|agent}" >&2; exit 1;;
esac
