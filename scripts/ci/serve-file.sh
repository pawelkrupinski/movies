#!/usr/bin/env bash
# Serve <dir> on 127.0.0.1 over HTTP, in the background, and print the URL of <name> in it once the server answers --
# for an action that only takes a download URL (scala-steward-action's `coursier-cli-url`), so it reads the copy CI
# keeps in actions/cache instead of GitHub's CDN, whose 504s failed Scala Steward #146 (2026-11-09).
# Usage: scripts/ci/serve-file.sh <dir> <name>   (the server lives until the job ends; its log is <dir>.http.log, its
# pid in <dir>.http.pid)
# Tested by scripts/ci/serve-file-test.sh.
set -euo pipefail
dir="${1:?usage: serve-file.sh <dir> <name>}"
name="${2:?usage: serve-file.sh <dir> <name>}"
[ -f "$dir/$name" ] || { echo "serve-file: no $dir/$name to serve" >&2; exit 1; }
port="$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
nohup python3 -m http.server "$port" --bind 127.0.0.1 --directory "$dir" > "$dir.http.log" 2>&1 &
echo "$!" > "$dir.http.pid"
url="http://127.0.0.1:$port/$name"
# the server answers within a second; curl's own bounded retry waits for it (refused until it listens)
curl -fs -o /dev/null --retry 30 --retry-connrefused --retry-delay 1 --max-time 60 -I "$url" \
  || { echo "serve-file: $url never answered" >&2; cat "$dir.http.log" >&2; exit 1; }
echo "$url"
