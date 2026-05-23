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

Designated tab
==============

By default each wake spawns a fresh Terminal tab + claude session. To
route every CI failure into one long-lived claude session instead, run
`scripts/ci-wakeup/register-tab.sh` in a Terminal tab — it writes the
tab's tty to `~/.movies-ci-wakeup-target` and execs claude. While that
marker exists, the listener types each prompt into the registered tab
(claude's TUI queues them as separate user messages). When the marker
is missing or the tab is gone, behaviour falls back to the new-tab
spawn.
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
# Set by `register-tab.sh` to a tty path (e.g. `/dev/ttys003`). When the
# file exists AND the tty still belongs to a live Terminal tab, the wake
# routes there; otherwise we fall back to spawning a new tab.
TARGET_TTY_PATH = Path.home() / ".movies-ci-wakeup-target"

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


def _build_prompt(run_id: str, sha: str, branch: str, job: str, workflow: str) -> str:
    return (
        f"CI run {run_id} on branch {branch} (workflow {workflow!r}, job {job!r}) "
        f"failed at sha {sha}. Pull the failing logs with "
        f"`gh run view {run_id} --log-failed`, find the failing test/job, "
        f"diagnose the root cause, fix it, and push. Per project CLAUDE.md: "
        f"add a regression test, commit and push without waiting for approval."
    )


def _as_applescript_string(s: str) -> str:
    """Escape Python string `s` for embedding in an AppleScript double-quoted
    string literal. Newlines become explicit `& linefeed &` joins because
    AppleScript string literals can't span source lines."""
    parts = s.split("\n")
    escaped = ['"' + p.replace("\\", "\\\\").replace('"', '\\"') + '"' for p in parts]
    return " & linefeed & ".join(escaped)


def _read_target_tty() -> str | None:
    """The path written by `register-tab.sh`. Returns None when the marker
    file is absent or empty (no designated tab — fall back to new-tab)."""
    try:
        tty = TARGET_TTY_PATH.read_text().strip()
    except OSError:
        return None
    return tty or None


def _send_into_existing_tab(prompt: str, target_tty: str) -> bool:
    """Type `prompt` into the Terminal tab whose tty matches `target_tty`,
    bringing its window forward. Returns True if the tab was found and the
    keystrokes dispatched, False if no such tab exists (caller falls back)."""
    applescript = f'''
tell application "Terminal"
    set targetTab to missing value
    set targetWindow to missing value
    repeat with w in windows
        repeat with t in tabs of w
            if tty of t is "{target_tty}" then
                set targetTab to t
                set targetWindow to w
                exit repeat
            end if
        end repeat
        if targetTab is not missing value then exit repeat
    end repeat
    if targetTab is missing value then
        error "tab-not-found"
    end if
    do script ({_as_applescript_string(prompt)}) in targetTab
    activate
    set frontmost of targetWindow to true
end tell
'''.strip()
    result = subprocess.run(
        ["osascript", "-e", applescript],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        sys.stderr.write(
            f"send-to-tab failed (tty={target_tty}): {result.stderr.strip()}\n"
        )
        return False
    return True


def _spawn_fresh_tab(prompt: str) -> None:
    """Fallback: open a new Terminal tab in the repo dir running claude with
    the prompt as its first message."""
    cmd = f"cd {shlex.quote(str(REPO_DIR))} && claude {shlex.quote(prompt)}"
    escaped = cmd.replace("\\", "\\\\").replace('"', '\\"')
    applescript = (
        'tell application "Terminal"\n'
        '  activate\n'
        f'  do script "{escaped}"\n'
        'end tell\n'
    )
    subprocess.run(["osascript", "-e", applescript], check=False)


def _spawn_claude(run_id: str, sha: str, branch: str, job: str, workflow: str) -> None:
    """Deliver the failure prompt to claude. If a tab is registered via
    `register-tab.sh`, type the prompt into that tab's running claude session
    (its TUI queues it as the next user message). Otherwise open a fresh
    Terminal tab + claude with the prompt as its first message.

    The HTTPServer is single-threaded so concurrent webhooks are already
    serialised; combined with claude's input queue, "queue them as they
    arrive" needs no explicit queue here."""
    prompt = _build_prompt(run_id, sha, branch, job, workflow)
    target_tty = _read_target_tty()
    if target_tty and _send_into_existing_tab(prompt, target_tty):
        sys.stderr.write(f"queued into designated tab {target_tty}\n")
        return
    if target_tty:
        sys.stderr.write(
            f"designated tab {target_tty} not found — spawning fresh tab\n"
        )
    _spawn_fresh_tab(prompt)


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
