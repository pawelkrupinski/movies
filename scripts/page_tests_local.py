#!/usr/bin/env python3
"""Page tests, locally: CI's Playwright page-test rows, against a FixtureServerMain in this checkout's kept sbt server.

PAGE TESTS, LOCALLY: the Playwright rows CI's `pw` page-test rows run, against a FixtureServerMain booted in this
checkout's kept sbt server, by hand: a UI change's cheap insurance before it lands. No longer the pre-push hook's
`page` area (the owner, 2026-11-17, "B1"): the page tests run after landing, on Main's `page-tests-*` rows.

usage: scripts/page-tests-local [chromium|webkit|all]      (default all)
  chromium  CI's `pw phones` (chromium) and `pw behaviour + desktop` rows: the three Chromium phones' viewport
            specs, the @agnostic specs once on mobile Chromium, chrome-desktop's viewport specs     (~1.5 min warm)
  webkit    CI's two `pw phones` WebKit shards in one run, one worker (WebKit rows run one on every host) (~7 min)
Measured 2026-10-26 on the laptop under a load of 17-130: the page set (web/PageTest/test ~2.7 min + both rows
+ the server's boot) ~11 min, inside the 15-min bound. axe-core stays CI's (continue-on-error there).
Exit: 0 every row passed; 1 a row failed (each row runs, the failing ones said `==> <row> FAILED`); 2 usage, or run from
inside another checkout (scripts/own-checkout); a failed boot, npm ci or browser install: its status.

THE SERVER: `scripts/sbt-warm --serve 'web/PageTest/runMain tools.FixtureServerMain <port file>'` through
.github/scripts/fixture-server.sh (relaunch on an early exit, 600 s ceiling), stopped on exit with the checkout's kept
server (`sbt-warm --stop`): a serving runMain holds it. The serve is nohup'd: this tool killed before its clean-up,
the serve's runMain would hold the checkout's sbt server -- and every later command queued in it -- for good
(2026-10-27, a land's hook 2.5 h), so the serve names this process its owner (KINOWO_SBT_WARM_SERVE_OWNER) and
sbt-warm stops that server the moment it is gone. Needs Node (npm ci once per worktree) and Playwright's browsers
(`npx playwright install chromium webkit`; Google Chrome for chrome-desktop).
ON CI (precommit-check.yml's remote checks): the job image has none of Playwright's browsers, and its Node sees the
host's 32 cores behind a 4-CPU quota. Run 3342 failed every Chromium test in milliseconds (no browser); with the
browsers but 40 workers (playwright.config.ts's 125%) the load passed 170 and 16 tests timed out (diag run 3353). So:
the rows' browsers and their system libraries, CI's worker figure (run-page-test's PW_WORKERS, unless the caller set
one; on the laptop too, THE GOVERNOR below), and one warm render per engine before any test's clock starts
(warm-browsers.mjs).
THE GOVERNOR: each Playwright row runs under a governor ticket of its own on the Mac (scripts/gov-run, `agentctl gov`),
charged its workers' cores -- PW_WORKERS, CI's 5 when unset (Playwright's local default, every core, would charge the
whole laptop), 1 for WebKit: while the laptop is starved a row waits for admission, then starts by itself (the owner,
2026-11-19). The fixture server's sbt-warm takes its own; off the Mac, with KINOWO_GOVERNOR=0 or in a check's hook,
none.
PAGE_TESTS_SBT_WARM / PAGE_TESTS_NPX / PAGE_TESTS_NPM / PAGE_TESTS_INSTALL / PAGE_TESTS_NODE stand in for
scripts/sbt-warm, npx, npm, scripts/ci/install-playwright.sh and node (page_tests_local_test.py's fakes)."""
from __future__ import annotations

import os
import shutil
import signal
import sys
import tempfile
import time
from dataclasses import dataclass
from types import FrameType
from typing import Callable, Dict, List, Mapping, Optional, Sequence, Tuple

ROOT = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import tool_process  # noqa: E402

PLAYWRIGHT = os.path.join(ROOT, "page-tests-playwright")
FIXTURE_SERVER = ".github/scripts/fixture-server.sh"
USAGE = "usage: scripts/page-tests-local [chromium|webkit|all]"
CHOICES = ("chromium", "webkit", "all")
CI_WORKERS = "5"
NOT_AXE = "axe-core|@agnostic"


@dataclass(frozen=True)
class Row:
    name: str
    engine: str
    args: Tuple[str, ...]
    workers: Optional[str] = None  # its own worker figure, else PW_WORKERS


ROWS = (
    Row("chromium phones", "chromium", ("--project", "chromium-galaxy-s10*", "--project", "chromium-galaxy-s25*",
                                        "--project", "chromium-pixel-9-pro*", "--grep-invert", NOT_AXE)),
    Row("chromium behaviour", "chromium", ("--project", "chromium-galaxy-s10", "--grep", "@agnostic",
                                           "--grep-invert", "axe-core")),
    Row("chrome desktop", "chromium", ("--project", "chrome-desktop", "--grep-invert", NOT_AXE)),
    Row("webkit phones", "webkit", ("--project", "webkit-iphone-se*", "--project", "webkit-iphone-13*",
                                    "--project", "webkit-iphone-17-pro-max*", "--project", "webkit-desktop",
                                    "--grep-invert", NOT_AXE), workers="1"),
)


class Failed(Exception):
    """A step the run cannot go on without failed: the tool exits with its status (the shell's `set -e`)."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def engines(which: str) -> List[str]:
    return ["chromium", "webkit"] if which == "all" else [which]


class PageTests:
    """One run: the fixture server up, the rows `which` selects against it, the server down however it ends."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], clock: Callable[[], float],
                 pid: int) -> None:
        self.runner = runner
        self.env: Dict[str, str] = dict(env)
        self.clock = clock
        self.pid = pid
        self.sbt_warm = env.get("PAGE_TESTS_SBT_WARM") or "scripts/sbt-warm"

    def run(self, which: str) -> int:
        state = tempfile.mkdtemp()
        try:
            self.serve(state)
            self.prepare(which)
            return self.rows(which)
        finally:
            self.stop(state)

    def must(self, argv: Sequence[str], cwd: str, env: Optional[Mapping[str, str]] = None) -> None:
        code = self.runner.run(tool_process.Command(argv, env or self.env, cwd=cwd))
        if code != 0:
            raise Failed(code)

    def serve(self, state: str) -> None:
        owned = {**self.env, "KINOWO_SBT_WARM_SERVE_OWNER": str(self.pid)}
        serve = f"web/PageTest/runMain tools.FixtureServerMain {state}/port"
        self.must([FIXTURE_SERVER, "launch", state, self.sbt_warm, "--serve", serve], ROOT, owned)
        self.must([FIXTURE_SERVER, "await", state], ROOT)
        with open(os.path.join(state, "port")) as f:
            self.env["KINOWO_BASE_URL"] = f"http://127.0.0.1:{f.read().strip()}"

    def prepare(self, which: str) -> None:
        """Node's modules once per worktree; on CI the rows' browsers installed and warmed (ON CI, above)."""
        if not os.access(os.path.join(PLAYWRIGHT, "node_modules", ".bin", "playwright"), os.X_OK):
            self.must([self.env.get("PAGE_TESTS_NPM") or "npm", "ci", "--no-audit", "--no-fund", "--silent"],
                      PLAYWRIGHT)
        self.env["PW_WORKERS"] = self.env.get("PW_WORKERS") or CI_WORKERS
        if self.env.get("CI"):
            install = self.env.get("PAGE_TESTS_INSTALL") or "../scripts/ci/install-playwright.sh"
            self.must([install, *engines(which)], PLAYWRIGHT)
            self.must([self.env.get("PAGE_TESTS_NODE") or "node", "warm-browsers.mjs", *engines(which)], PLAYWRIGHT)

    def rows(self, which: str) -> int:
        selected = [row for row in ROWS if which == "all" or row.engine == which]
        return 1 if [row for row in selected if not self.row(row)] else 0

    def row(self, row: Row) -> bool:
        """The row under a governor ticket of its own, charged its workers (THE GOVERNOR, above): whether it passed."""
        workers = row.workers or self.env["PW_WORKERS"]
        npx = self.env.get("PAGE_TESTS_NPX") or "npx"
        started = self.clock()
        print(f"==> {row.name}: playwright test {' '.join(row.args)}", flush=True)
        governed = [os.path.join(ROOT, "scripts", "gov-run"), "--job", "page-tests", "--cores", workers, "--mem", "2",
                    "--label", f"page-tests {row.name}", "--", npx, "playwright", "test", *row.args]
        code = self.runner.run(tool_process.Command(governed, {**self.env, "PW_WORKERS": workers}, cwd=PLAYWRIGHT))
        print(f"==> {row.name} {'ok' if code == 0 else 'FAILED'} ({int(self.clock() - started)}s)", flush=True)
        return code == 0

    def stop(self, state: str) -> None:
        self.runner.run(tool_process.Command([FIXTURE_SERVER, "stop", state], self.env, cwd=ROOT))
        self.runner.run(tool_process.Command([self.sbt_warm, "--stop"], self.env, log=os.devnull, cwd=ROOT))
        shutil.rmtree(state, ignore_errors=True)


def end_on_signal(signum: int, _frame: Optional[FrameType]) -> None:
    """A TERM, INT or HUP ends the run through its clean-up (the shell's EXIT trap): the server stopped."""
    raise SystemExit(128 + signum)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    runner = tool_process.SubprocessRunner()
    own = os.path.join(ROOT, "scripts", "own-checkout")
    if runner.run(tool_process.Command([own, os.path.join(ROOT, "scripts", "page-tests-local")], env)) != 0:
        return 2  # it measures its own checkout: never another one's
    which = argv[0] if argv else "all"
    if which in ("-h", "--help"):
        print(__doc__)
        return 0
    if which not in CHOICES:
        print(USAGE, file=sys.stderr)
        return 2
    for signum in (signal.SIGTERM, signal.SIGINT, signal.SIGHUP):
        signal.signal(signum, end_on_signal)
    try:
        return PageTests(runner, env, time.monotonic, os.getpid()).run(which)
    except Failed as failed:
        return failed.code


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
