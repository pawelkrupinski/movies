#!/usr/bin/env python3
"""Local HTTP listener that wakes a Claude Code session in Terminal.app
when GitHub Actions reports a CI failure on the movies repo.

Wire-up:

  GH workflow (`if: failure()` step) ──curl──► ngrok static domain ──► this server
                                                                          │
                                                          osascript spawns ▼
                                              Terminal.app new tab in ~/projects/movies
                                              running `claude <prompt-with-failure-context>`

One-time setup
==============

  1. Install ngrok:
       brew install ngrok

  2. Add your authtoken (from https://dashboard.ngrok.com/get-started/your-authtoken):
       ngrok config add-authtoken <token>

  3. Add an `endpoints:` block to ~/Library/Application Support/ngrok/ngrok.yml:
       endpoints:
         - name: ci-wakeup
           description: movies CI failure webhook
           url: https://<your-static-domain>.ngrok-free.dev
           upstream:
             url: http://localhost:9876
     (Reserve a free static domain at https://dashboard.ngrok.com/domains.)

  4. Generate + store a shared secret + push both GH secrets:
       openssl rand -hex 32 > ~/.movies-ci-wakeup-secret
       chmod 600 ~/.movies-ci-wakeup-secret
       gh secret set CLAUDE_WAKEUP_URL    --body "https://<your-static-domain>.ngrok-free.dev/webhook"
       gh secret set CLAUDE_WAKEUP_SECRET --body "$(cat ~/.movies-ci-wakeup-secret)"

  5. Auto-start the server + tunnel at login (see plists in this dir):
       launchctl bootstrap gui/$(id -u) \
         ~/projects/movies/scripts/ci-wakeup/com.kinowo.ci-wakeup.plist
       launchctl bootstrap gui/$(id -u) \
         ~/projects/movies/scripts/ci-wakeup/com.kinowo.ci-wakeup-ngrok.plist

The webhook payload is `{"run_id":..., "sha":..., "branch":..., "job":..., "workflow":...}`,
authenticated by `Authorization: Bearer <secret>`. Anything else is dropped.
"""

from __future__ import annotations

import hmac
import http.server
import json
import os
import re
import shlex
import subprocess
import sys
import threading
import time
from pathlib import Path

PORT = 9876
SECRET_PATH = Path.home() / ".movies-ci-wakeup-secret"
REPO_DIR = Path.home() / "projects" / "movies"
LOG_DIR = Path.home() / ".movies-ci-wakeup-logs"

# A single workflow run usually has multiple jobs (iOS has unit-integration,
# ui-tests, smoke; Deploy has page-tests-chrome and page-tests-webkit), each
# of which fires the wake-on-failure step independently. Dedup on `run_id`
# so we only spawn one Claude session per workflow run — otherwise a single
# bad push opens 3+ tabs that all want to fix the same red CI.
#
# In-memory is fine: a run's failed jobs all arrive within seconds of each
# other, and a launchd restart in that window would just unsuppress the
# second job, which is the safer failure mode.
DEDUP_TTL_SECONDS = 30 * 60
_seen_runs: dict[str, float] = {}
_seen_runs_lock = threading.Lock()


def _should_spawn(run_id: str) -> bool:
    """Atomically check-and-mark `run_id`. Returns True the first time
    a run_id is seen within the TTL window, False for repeats."""
    now = time.time()
    with _seen_runs_lock:
        for rid in [r for r, t in _seen_runs.items() if now - t > DEDUP_TTL_SECONDS]:
            del _seen_runs[rid]
        if run_id in _seen_runs:
            return False
        _seen_runs[run_id] = now
        return True

# Hard-fail at startup rather than silently accepting any request.
if not SECRET_PATH.exists():
    sys.stderr.write(f"missing {SECRET_PATH} — run `openssl rand -hex 32 > {SECRET_PATH}`\n")
    sys.exit(1)
SECRET = SECRET_PATH.read_text().strip().encode()

LOG_DIR.mkdir(exist_ok=True)


def _spawn_claude(run_id: str, sha: str, branch: str, job: str, workflow: str) -> None:
    """Open a Terminal.app tab in the repo and start Claude Code with a
    prompt that includes the failure context. AppleScript is the cleanest
    way to control Terminal.app from a daemon — `open -a Terminal` only
    raises the app, it can't seed a command in the new tab."""
    prompt = (
        f"CI run {run_id} on branch {branch} (workflow {workflow!r}, job {job!r}) "
        f"failed at sha {sha}. Pull the failing logs with "
        f"`gh run view {run_id} --log-failed`, find the failing test/job, "
        f"diagnose the root cause, fix it, and push. Per project CLAUDE.md: "
        f"add a regression test, commit and push without waiting for approval."
    )

    cmd = f"cd {shlex.quote(str(REPO_DIR))} && claude {shlex.quote(prompt)}"

    # `do script` opens a new window if Terminal isn't already running, or a
    # new tab in the front window if it is. The double-quoted string is
    # passed verbatim to the shell — escape backslashes and double-quotes.
    escaped = cmd.replace("\\", "\\\\").replace('"', '\\"')
    applescript = (
        'tell application "Terminal"\n'
        '  activate\n'
        f'  do script "{escaped}"\n'
        'end tell\n'
    )
    subprocess.run(["osascript", "-e", applescript], check=False)


class Handler(http.server.BaseHTTPRequestHandler):
    def _send(self, status: int, body: bytes = b"") -> None:
        self.send_response(status)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if body:
            self.wfile.write(body)

    def do_GET(self) -> None:  # noqa: N802 — stdlib name
        # Health check so cloudflared / a curl test confirms the listener
        # is up without needing the secret.
        if self.path == "/health":
            self._send(200, b'{"ok": true}\n')
            return
        self._send(404)

    def do_POST(self) -> None:  # noqa: N802 — stdlib name
        if self.path != "/webhook":
            self._send(404)
            return

        auth = self.headers.get("Authorization", "")
        m = re.fullmatch(r"Bearer (.+)", auth)
        if not m or not hmac.compare_digest(m.group(1).encode(), SECRET):
            self._send(401, b'{"error": "unauthorized"}\n')
            return

        length = int(self.headers.get("Content-Length", "0"))
        try:
            data = json.loads(self.rfile.read(length))
        except ValueError:
            self._send(400, b'{"error": "bad json"}\n')
            return

        run_id   = str(data.get("run_id",   "?"))
        sha      = str(data.get("sha",      "?"))
        branch   = str(data.get("branch",   "?"))
        job      = str(data.get("job",      "?"))
        workflow = str(data.get("workflow", "?"))

        if not _should_spawn(run_id):
            sys.stderr.write(
                f"skip: run={run_id} job={job} (already spawned for this run)\n"
            )
            self._send(200, b'{"ok": true, "skipped": "duplicate"}\n')
            return

        sys.stderr.write(
            f"wake: run={run_id} sha={sha[:7]} branch={branch} job={job}\n"
        )
        _spawn_claude(run_id, sha, branch, job, workflow)
        self._send(200, b'{"ok": true}\n')

    def log_message(self, fmt: str, *args: object) -> None:
        sys.stderr.write(f"{self.address_string()} {fmt % args}\n")


if __name__ == "__main__":
    print(f"ci-wakeup listening on 127.0.0.1:{PORT}", file=sys.stderr)
    http.server.HTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
