"""THE STUB GITHUB of the tools' end-to-end layer (scripts/e2e/e2e_world.py): a local HTTP server holding a toy
repository's Actions runs, which the `gh` shim (scripts/e2e/gh_shim.py, first on every tool's PATH) asks for every
call. Never the real GitHub.

What it answers, as gh would for the calls the tools make:
  run list --workflow W [--branch B] [--commit C] [--limit N] --json F   the runs of W, newest first
  run view ID --json F                                                    one run (`jobs` included)
  api repos/{owner}/{repo}/actions/runs/ID[/jobs]                         the REST shape of the same run
  api repos/{owner}/{repo}/compare/A...B                                  {"status": ahead|behind|identical|diverged}
Anything else is answered `unknown command` (exit 1), so a tool reaching for a call no scenario expects fails loudly.

A run PROGRESSES deterministically: `add_run(..., steps=[("queued", ""), ("in_progress", ""), ("completed", "success")])`
moves one step each time the run is read, and stays at its last -- a poller sees each state once, without a clock."""
from __future__ import annotations

import json
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

Step = Tuple[str, str]
Json = Dict[str, Any]  # a run, a reply: what gh prints as JSON


class GitHub:
    """The stub's state: runs by id, and the compare answers. Thread-safe (the server answers on threads)."""

    def __init__(self) -> None:
        self.lock = threading.Lock()
        self.runs: Dict[int, Json] = {}
        self.steps: Dict[int, List[Step]] = {}
        self.compare: Dict[str, str] = {}
        self.next_id = 1000
        self.calls: List[List[str]] = []

    def add_run(self, workflow: str, sha: str, steps: Sequence[Step] = (("completed", "success"),),
                branch: str = "main", jobs: Sequence[Tuple[str, str]] = ()) -> int:
        with self.lock:
            self.next_id += 1
            run_id = self.next_id
            self.runs[run_id] = {"databaseId": run_id, "number": run_id - 1000, "headSha": sha, "headBranch": branch,
                                 "workflowName": workflow, "createdAt": f"2026-11-27T10:{run_id % 60:02d}:00Z",
                                 "updatedAt": f"2026-11-27T10:{run_id % 60:02d}:30Z", "url": f"https://stub/runs/{run_id}",
                                 "status": "", "conclusion": "",
                                 "jobs": [{"name": n, "status": "completed", "conclusion": c} for n, c in jobs]}
            self.steps[run_id] = list(steps)
            self._apply(run_id)
            return run_id

    def _apply(self, run_id: int) -> None:
        status, conclusion = self.steps[run_id][0]
        self.runs[run_id].update(status=status, conclusion=conclusion)

    def read(self, run_id: int) -> Json:
        """The run as this read sees it; the next read sees its next step."""
        self._apply(run_id)
        run = dict(self.runs[run_id])
        if len(self.steps[run_id]) > 1:
            self.steps[run_id].pop(0)
        return run

    def answer(self, argv: List[str]) -> Json:
        with self.lock:
            self.calls.append(list(argv))
            for verb, handler in ANSWERS:
                if argv[:len(verb)] == list(verb):
                    return handler(self, argv[len(verb):])
            return {"rc": 1, "stderr": f"unknown command {' '.join(argv[:2])} (the e2e gh stub)\n"}


def flags(args: Sequence[str]) -> Tuple[List[str], Dict[str, str]]:
    """The positional words and the `--flag value` pairs of a gh argv tail."""
    words: List[str] = []
    named: Dict[str, str] = {}
    i = 0
    while i < len(args):
        if args[i].startswith("-") and i + 1 < len(args):
            named[args[i].lstrip("-")] = args[i + 1]
            i += 2
        else:
            words.append(args[i])
            i += 1
    return words, named


def fields(run: Json, wanted: Optional[str]) -> Json:
    return {k: run.get(k) for k in wanted.split(",")} if wanted else run


def run_list(gh: GitHub, args: Sequence[str]) -> Json:
    _, named = flags(args)
    found = [r for _, r in sorted(gh.runs.items(), reverse=True)
             if r["workflowName"] == named.get("workflow")
             and r["headBranch"] == named.get("branch", r["headBranch"])
             and str(r["headSha"]).startswith(named.get("commit", ""))]
    limit = int(named.get("limit", "20"))
    return {"rc": 0, "data": [fields(gh.read(r["databaseId"]), named.get("json")) for r in found[:limit]]}


def run_view(gh: GitHub, args: Sequence[str]) -> Json:
    words, named = flags(args)
    run_id = int(words[0]) if words and words[0].isdigit() else 0
    if run_id not in gh.runs:
        return {"rc": 1, "stderr": f"HTTP 404: no run {words[:1]}\n"}
    return {"rc": 0, "data": fields(gh.read(run_id), named.get("json"))}


def run_rest(gh: GitHub, parts: Sequence[str]) -> Optional[Json]:
    """`repos/{owner}/{repo}/actions/runs/ID[/jobs]`: the run's REST shape, or its jobs."""
    if not (len(parts) >= 6 and list(parts[3:5]) == ["actions", "runs"] and parts[5].isdigit()
            and int(parts[5]) in gh.runs):
        return None
    run = gh.read(int(parts[5]))
    if list(parts[6:]) == ["jobs"]:
        return {"jobs": run["jobs"]}
    return {"id": run["databaseId"], "status": run["status"], "conclusion": run["conclusion"] or None,
            "head_sha": run["headSha"], "path": ".github/workflows/x.yml"}


def compare_rest(gh: GitHub, parts: Sequence[str]) -> Optional[Json]:
    """`repos/{owner}/{repo}/compare/A...B`."""
    if len(parts) == 5 and parts[3] == "compare" and parts[4] in gh.compare:
        return {"status": gh.compare[parts[4]]}
    return None


def rest(gh: GitHub, args: Sequence[str]) -> Json:
    words, _ = flags(args)
    parts = words[0].split("?")[0].split("/") if words else []
    data = run_rest(gh, parts) or compare_rest(gh, parts)
    if data is None:
        return {"rc": 1, "stderr": f"gh: Not Found (HTTP 404) {words[:1]}\n"}
    return {"rc": 0, "data": data}


ANSWERS: Sequence[Tuple[Tuple[str, ...], Callable[[GitHub, Sequence[str]], Json]]] = (
    (("run", "list"), run_list), (("run", "view"), run_view), (("api",), rest))


def serve(gh: GitHub) -> Tuple[ThreadingHTTPServer, str]:
    """The stub on a free local port, answering on a daemon thread: (server, its URL). `server.shutdown()` ends it."""

    class Handler(BaseHTTPRequestHandler):
        def do_POST(self) -> None:  # noqa: N802 (http.server's name)
            body: Json = json.loads(self.rfile.read(int(self.headers.get("Content-Length") or 0)) or b"{}")
            argv: List[str] = [str(a) for a in body.get("argv", [])]
            reply = json.dumps(gh.answer(argv)).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(reply)))
            self.end_headers()
            self.wfile.write(reply)

        def log_message(self, format: str, *args: Any) -> None:  # noqa: A002 (http.server's name)
            pass

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server, f"http://127.0.0.1:{server.server_address[1]}"
