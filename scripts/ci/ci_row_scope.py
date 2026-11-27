#!/usr/bin/env python3
"""CAN THIS PUSH MOVE ci.yml's SLOW ROWS? Reads changed paths on stdin and prints `true` when one of them can change
what the named rows test (or, for `deploy`, what main.yml ships), `false` when every one is inert for them.

  ci-row-scope.sh pages    < paths   the Scala page-test row (`page-tests-chrome (stable)`: ReviewPageSpec renders
                                     identity code)
  ci-row-scope.sh browsers < paths   the Playwright page-test rows and mobile-local-server: the public pages, served by
                                     FixtureServerMain from the checked-in read-model snapshot
  ci-row-scope.sh e2e      < paths   the e2e rows: what `pages` takes, and identity's tests too, because the
                                     `e2e (corpus)` row builds the images main.yml ships and its web tier filter takes
                                     all of identity/**
  ci-row-scope.sh --outputs          ci.yml's `scope` job: diffs this commit against the newest green Main run (and,
                                     for e2e, each tier's deployed marker) and writes e2e=, pages=, browsers= to
                                     $GITHUB_OUTPUT; every row on anything it cannot read
(`[repo root]` after a kind: the tree whose scripts are judged, `.` by default.)

Main runs ~65 pushes a day, and about a third touch only identity, the judgements, docs or scripts: each still ran 9
page/mobile/e2e rows (~44 runner-minutes) that could not have moved (2026-10-22). ci.yml's `scope` job asks this tool
and skips those rows; a skipped row counts as a success for main.yml's `needs: ci` and for the run's conclusion the
pre-push red gate reads.

INERT, for every kind:
  docs/**, .claude/**, a top-level *.md       read by no row's code (lint specs reading docs run in `test`)
  test/resources/fixtures/identity-judgements*  read by the convergence legs and `test`, not by these rows
  scripts/** that no row can run               a script is LIVE when its name appears in ci.yml, the composite actions
                                               these rows use, fixture-server.sh, the e2e or page-test sources or the
                                               build, or a live script sources it
and also:
  identity/src/test/**   for pages and browsers (not e2e: main.yml's web tier filter takes all of identity/**)
  identity/src/main/**   for browsers only (the public pages render the snapshot; identity runs in e2e and in the review
                         pages of the Scala page row)
Everything else is LIVE: an unknown path runs every row. (CiRowScopeSpec reads the wiring; ci_row_scope_test.py beside
it the classification and the outputs.) ci.yml runs it by the name scripts/ci/ci-row-scope.sh."""
from __future__ import annotations

import os
import re
import sys
from typing import IO, Dict, Iterable, List, Mapping, Optional, Sequence, Set

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import machine  # noqa: E402
import tool_git  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

KINDS = ("pages", "browsers", "e2e")
USAGE = "usage: ci-row-scope.sh pages|browsers|e2e [repo root] < changed paths | --outputs"
# What the rows themselves read: ci.yml (whole: a script any of its rows names counts; over-running is safe), the
# composite actions and helper those rows use, the e2e and page-test sources, and the build.
CONSUMERS = (".github/workflows/ci.yml", ".github/actions/setup-sbt-build", ".github/actions/sbt-target-cache",
             ".github/actions/setup-jdk", ".github/actions/setup-buildx", ".github/actions/run-page-test",
             ".github/scripts/fixture-server.sh", "e2e/src", "web/src/page", "page-tests-playwright", "build.sbt",
             "project")
SKIPPED_DIRECTORIES = ("node_modules", "target")
# Only what a live script SOURCES: its comments and messages name half the toolbox, and following those made every
# script live. What a live script runs by name is named in ci.yml or the specs too.
SOURCES = re.compile(r"[ \t]*(\.|source)[ \t]")
ZERO = "0" * 40
LISTED = 50


def files_under(path: str) -> List[str]:
    """`path` itself when a file, else every file below it, skipping SKIPPED_DIRECTORIES."""
    if os.path.isfile(path):
        return [path]
    found: List[str] = []
    for directory, subdirectories, names in os.walk(path):
        subdirectories[:] = sorted(d for d in subdirectories if d not in SKIPPED_DIRECTORIES)
        found += [os.path.join(directory, n) for n in sorted(names)]
    return found


def text_of(path: str) -> str:
    """A file's text, or "" for a binary one (grep -I) or one that cannot be read."""
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError:
        return ""
    return "" if b"\0" in data else data.decode(errors="replace")


class Scripts:
    """The scripts of a tree some row can run, by file name, computed once and only when a script changed."""

    def __init__(self, root: str) -> None:
        self.root = root
        self._live: Optional[Set[str]] = None

    def by_name(self) -> Dict[str, List[str]]:
        named: Dict[str, List[str]] = {}
        for path in files_under(os.path.join(self.root, "scripts")):
            named.setdefault(os.path.basename(path), []).append(path)
        return named

    def live(self) -> Set[str]:
        """Breadth first: a script named by the text found so far is live, and the files it sources are searched next."""
        if self._live is None:
            self._live = self.named_from(self.consumers_text())
        return self._live

    def consumers_text(self) -> str:
        present = [os.path.join(self.root, c) for c in CONSUMERS if os.path.exists(os.path.join(self.root, c))]
        return "".join(text_of(f) for c in present for f in files_under(c))

    def named_from(self, text: str) -> Set[str]:
        named = self.by_name()
        left: List[str] = sorted(named)
        found: Set[str] = set()
        while text:
            taken: List[str] = [name for name in left if name in text]
            found.update(taken)
            left = [name for name in left if name not in found]
            text = "".join(sourced(path) for name in taken for path in named[name])
        return found


def sourced(path: str) -> str:
    """The lines of a script that source another."""
    return "".join(line + "\n" for line in text_of(path).split("\n") if SOURCES.match(line))


def inert(path: str, kind: str, scripts: Scripts) -> bool:
    if path.startswith(("docs/", ".claude/", "test/resources/fixtures/identity-judgements")):
        return True
    if "/" not in path:
        return path.endswith(".md")
    if path.startswith("scripts/"):
        return os.path.basename(path) not in scripts.live()
    if path.startswith("identity/src/test/"):
        return kind != "e2e"
    if path.startswith("identity/src/main/"):
        return kind == "browsers"
    return False


def moves(kind: str, paths: Iterable[str], root: str) -> bool:
    scripts = Scripts(root)
    return any(not inert(p, kind, scripts) for p in (line.strip() for line in paths) if p)


class EveryRow(Exception):
    """Something the scope cannot judge: every row runs, saying why."""


class Outputs:
    """--outputs: each row's verdict over what moved since the newest green Main run, into $GITHUB_OUTPUT."""

    def __init__(self, git: tool_git.Git, gh: tool_github.GitHub, env: Mapping[str, str], out: IO[str]) -> None:
        self.git = git
        self.gh = gh
        self.env = env
        self.out = out

    def changed(self, *args: str) -> List[str]:
        done = self.git.call("diff", "--name-only", *args, quiet=True)
        if done.code != 0:
            raise EveryRow(f"could not diff {args[0].replace('refs/tags/', '')}..{args[1]}")
        return [p for p in done.stdout.split("\n") if p]

    def newest_green(self) -> str:
        done = self.gh.call(["run", "list", "--workflow", "main.yml", "--branch", "main", "--status", "success",
                             "--limit", "1", "--json", "headSha", "--jq", ".[0].headSha // empty"], 60, quiet=True)
        if done.code != 0:
            raise EveryRow(f"could not read Main's runs ({done.stderr[:200].rstrip(chr(10))})")
        green = done.stdout.rstrip("\n")
        if not green:
            raise EveryRow("no green Main run to diff from")
        # GIT_NO_LAZY_FETCH: the checkout is a blob:none partial clone, so a plain `cat-file -e` of a commit it lacks
        # asks the promisor remote for it, one lazy fetch spawning the next: 60-140 s of the job's ~88 s (2026-10-22).
        # The explicit depth-1 fetch brings the same commit in under a second.
        unlazy = tool_git.Git(self.git.runner, {**self.git.env, "GIT_NO_LAZY_FETCH": "1"}, self.git.root)
        if not unlazy.ok("cat-file", "-e", f"{green}^{{commit}}", quiet=True) and not self.git.ok(
                "fetch", "--no-tags", "--depth=1", "--filter=blob:none", "origin", green, timeout=120, quiet=True):
            raise EveryRow(f"could not fetch the newest green Main run's commit {green}")
        return green

    def verdicts(self) -> None:
        if self.env.get("PULL_REQUEST"):
            raise EveryRow("a pull request")
        if self.env.get("BEFORE", "") in ("", ZERO):
            raise EveryRow("not a push (a dispatch)")
        head = self.env["GITHUB_SHA"]
        green = self.newest_green()
        since_green = self.changed(green, head)
        e2e = list(since_green)
        # A tier still to ship: whatever moved since its deployed marker (deployed-base) builds in `e2e (corpus)`.
        for tier in ("web", "worker"):
            tag = f"refs/tags/deployed-{tier}"
            if self.git.ok("fetch", "--no-tags", "--depth=1", "--filter=blob:none", "origin", f"+{tag}:{tag}",
                           timeout=60, quiet=True):
                e2e += self.changed(tag, head)
        print(f"since the newest green Main run ({green}): {len(since_green)} paths")
        print("".join(f"  {p}\n" for p in since_green[:LISTED]), end="")
        self.decide(since_green, e2e)

    def decide(self, since_green: List[str], e2e: List[str]) -> None:
        for row in ("e2e", "pages", "browsers"):
            self.emit(f"{row}={'true' if moves(row, e2e if row == 'e2e' else since_green, '.') else 'false'}")

    def run(self) -> int:
        try:
            self.verdicts()
        except EveryRow as why:
            print(f"{why}: every row runs", flush=True)
            self.out.write("e2e=true\npages=true\nbrowsers=true\n")
        return 0

    def emit(self, line: str) -> None:
        """A row's verdict, on stdout and in the outputs (`| tee -a`)."""
        print(line, flush=True)
        self.out.write(line + "\n")
        self.out.flush()


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if not argv or not argv[0]:
        print(f"ci-row-scope.sh: 1: {USAGE}", file=sys.stderr)
        return 1
    if argv[0] == "--outputs":
        if not env.get("GITHUB_SHA"):
            print("ci-row-scope.sh: GITHUB_SHA: parameter null or not set", file=sys.stderr)
            return 1
        runner = tool_process.SubprocessRunner()
        with open(env.get("GITHUB_OUTPUT") or "/dev/stdout", "a") as out:
            return Outputs(tool_git.Git(runner, env, "."), tool_github.GitHub(runner, env), env, out).run()
    if argv[0] not in KINDS:
        print(f"unknown kind '{argv[0]}'", file=sys.stderr)
        return 2
    print("true" if moves(argv[0], sys.stdin.read().split("\n"), argv[1] if len(argv) > 1 else ".") else "false")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
