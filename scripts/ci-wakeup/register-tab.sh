#!/usr/bin/env bash
# Mark the current Terminal.app tab as the designated CI-fixing Claude
# session. Future CI-failure webhooks will queue their prompts into this
# tab via AppleScript instead of opening a fresh tab per failure.
#
# Usage: open a new Terminal tab, then `./scripts/ci-wakeup/register-tab.sh`.
# The script writes this tab's tty to ~/.movies-ci-wakeup-target, cds into
# the repo, and execs `claude`. On exit (Ctrl-C, claude quit) it removes
# the marker so the next webhook falls back to spawning a fresh tab.
#
# Only one tab can be registered at a time — re-running this script in
# another tab silently takes over.

set -euo pipefail

TARGET_FILE="${HOME}/.movies-ci-wakeup-target"
REPO_DIR="${HOME}/projects/movies"

tty_path=$(tty)
echo "$tty_path" > "$TARGET_FILE"
echo "registered $tty_path as CI-fixing Claude tab; releases on exit"

# Release the marker on any exit — Ctrl-C in claude, claude quit, shell
# kill. Guarded with `==` so we don't clobber another tab that took over
# after us.
release() {
    if [[ -f "$TARGET_FILE" ]] && [[ "$(cat "$TARGET_FILE" 2>/dev/null)" == "$tty_path" ]]; then
        rm -f "$TARGET_FILE"
        echo "released $tty_path"
    fi
}
trap release EXIT

cd "$REPO_DIR"
claude
