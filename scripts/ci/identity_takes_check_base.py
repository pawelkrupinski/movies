"""THE BASE of the identity takes check (identity_takes_check.py): the newest commit before the head whose own check
passed (its commit status `identity takes (<cc>)` is `success`), so a failed check stays failed on every later push until
the move is fixed or accepted, however far back that commit is. A check that has only ever failed diffs from its first
run's base -- the parent of the oldest commit it judged; only one that never ran takes the head's parent. A failed read
fails the pick (exit 2), never reads as "no green commit": that would diff from the parent and quietly let an earlier
unaccepted move through -- as a 40-commit window once did, absorbing a day's moves.

GraphQL hands a commit's statuses with it (only the check's own context: all of them ran past github.com's 10 s budget,
Identity takes #114), a page of 100 one call, each page asked up to three times (an HTML error page now and then lost
Identity takes #82 its base). When GraphQL will not answer at all, THE HISTORY FROM GIT: up to
IDENTITY_TAKES_LOCAL_HISTORY (200) of the head's commits, commits only (--filter=tree:0), into a repository of their own
(the checkout's objects and shallow boundary stay as the row fetched them), each one's state asked by REST, newest first,
under ONE deadline, IDENTITY_TAKES_LOCAL_HISTORY_DEADLINE (600 s), for the fetch and every read. It names only a GREEN
commit. THE HISTORY FLOOR: a status older than .github/github-takes-history-floor counts as none."""
from __future__ import annotations

import os
import re
import shutil
from typing import List, Optional, Tuple

import identity_takes_check_context as ctx_module
import tool_git
import tool_process

Context = ctx_module.Context
FLOOR = re.compile(r"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
QUERY = """query($owner: String!, $name: String!, $expr: String!, $context: String!{cursor_var}) {{
      repository(owner: $owner, name: $name) {{ object(expression: $expr) {{ ... on Commit {{
        history(first: 100{after}) {{ pageInfo {{ hasNextPage endCursor }}
          nodes {{ oid parents(first: 1) {{ nodes {{ oid }} }} status {{ context(name: $context) {{ state createdAt }} }} }} }} }} }} }} }}"""
JQ = """.data.repository.object.history | (.nodes[] | .oid + " " +
            ([.status.context // empty | select((.createdAt // "") >= "{floor}") | .state][0] // "-") + " " +
            (.parents.nodes[0].oid // "-")),
          (if .pageInfo.hasNextPage then "NEXT " + .pageInfo.endCursor else empty end)"""


def history_floor(ctx: Context) -> str:
    """The floor (TAKES_HISTORY_FLOOR overrides, even empty); one that is no ISO time stops the check."""
    floor = ctx.env["TAKES_HISTORY_FLOOR"] if "TAKES_HISTORY_FLOOR" in ctx.env else first_line(
        os.path.join(ctx.here, ".github/github-takes-history-floor"))
    if floor and not FLOOR.match(floor):
        ctx.say(f"::error::{ctx.label}: the history floor is an ISO time like 2026-10-31T15:00:00Z, not {floor}")
        raise ctx_module.Stop(2)
    return floor


def first_line(path: str) -> str:
    lines = [line for line in ctx_module.read(path).splitlines() if not re.match(r"^\s*(#|$)", line)]
    return lines[0] if lines else ""


class History:
    def __init__(self, ctx: Context, context: str, floor: str) -> None:
        self.ctx = ctx
        self.context = context
        self.floor = floor
        self.gh_read = ctx.seconds("IDENTITY_TAKES_GH_TIMEOUT", "120")
        self.repository = ctx.env.get("GITHUB_REPOSITORY", "")

    def query(self, cursor: str) -> Optional[str]:
        owner, _, name = self.repository.partition("/")
        args = ["api", "graphql", "-f", f"owner={owner}", "-f", f"name={name or owner}", "-f", f"expr={self.ctx.head}~1",
                "-f", f"context={self.context}"]
        if cursor:
            args += ["-f", f"cursor={cursor}"]
        query = QUERY.format(cursor_var=", $cursor: String" if cursor else "", after=", after: $cursor" if cursor else "")
        args += ["-f", f"query={query}", "--jq", JQ.format(floor=self.floor)]
        return self.ctx.gh.read(args, self.gh_read)

    def page(self, cursor: str) -> Optional[str]:
        backoff = self.ctx.seconds("IDENTITY_TAKES_HISTORY_BACKOFF", "10")
        for attempt in (1, 2, 3):
            page = self.query(cursor)
            if page is not None:
                return page
            self.ctx.warn(f"{self.ctx.label}: history read {attempt} of 3 failed (gh's error above)")
            if attempt < 3:
                self.ctx.sleep(backoff * attempt)
        return None

    def pick(self) -> Optional[str]:
        """The newest green commit, else the first judged one's parent, else the head's parent; None: no base."""
        walk = Walk()
        for _ in range(20):
            page = self.page(walk.cursor)
            if page is None:
                return LocalHistory(self.ctx, self.context, self.floor).base()
            green = walk.read(page)
            if green or not walk.cursor:
                return green or walk.fallback()
        return walk.fallback()


class Walk:
    """What the pages read so far say: the cursor of the next, the first judged commit's parent, the head's parent."""

    def __init__(self) -> None:
        self.cursor = self.first_judged = self.head_parent = ""

    def read(self, page: str) -> str:
        """The green commit this page names, if any."""
        self.cursor = ""
        for sha, state, parent in rows(page):
            if sha == "NEXT":
                self.cursor = f"{state} {parent}".rstrip()  # a history cursor is "<sha> <offset>"
                continue
            self.head_parent = self.head_parent or sha  # the history of head~1 starts with it
            if state == "SUCCESS":
                return sha
            if state != "-":
                self.first_judged = parent
        return ""

    def fallback(self) -> str:
        return self.first_judged or self.head_parent  # never judged in the last 2,000 commits: the check's first run


def rows(page: str) -> List[Tuple[str, str, str]]:
    """`read -r sha state parent` of each line: the last word takes the rest."""
    out: List[Tuple[str, str, str]] = []
    for line in page.splitlines():
        words = line.split(None, 2) + ["", "", ""]
        out.append((words[0], words[1], words[2]))
    return out


class Deadline(Exception):
    pass


class LocalHistory:
    """THE HISTORY FROM GIT (the module's header), under its one deadline."""

    def __init__(self, ctx: Context, context: str, floor: str) -> None:
        self.ctx = ctx
        self.context = context
        self.floor = floor
        self.depth = int(ctx.setting("IDENTITY_TAKES_LOCAL_HISTORY", "200"))
        self.deadline = ctx.seconds("IDENTITY_TAKES_LOCAL_HISTORY_DEADLINE", "600")
        self.gh_read = ctx.seconds("IDENTITY_TAKES_GH_TIMEOUT", "120")
        self.started = ctx.clock()
        self.checked = 0

    def within(self, bound: float) -> float:
        """The seconds the walk has left, at most `bound`; none left raises Deadline, saying so."""
        left = self.deadline - int(self.ctx.clock() - self.started)
        if left <= 0:
            self.ctx.warn(f"{self.ctx.label}: the history-from-git walk ran past its {self.deadline:g} s deadline after "
                          f"{self.checked} commit(s) -- no base named")
            raise Deadline()
        return min(left, bound)

    def base(self) -> Optional[str]:
        self.ctx.warn(f"{self.ctx.label}: GraphQL gave no history; reading the head's from git, each commit's status by REST")
        try:
            history = self.fetch()
            return self.walk(history) if history else None
        except Deadline:
            return None

    def auth(self) -> List[str]:
        """The checkout's own credentials (actions/checkout's http.<server>/.extraheader)."""
        found = self.ctx.git.out("config", "--get-regexp", r"^http\..*extraheader$", quiet=True) or ""
        return [w for line in found.splitlines() if line for w in ("-c", "=".join(line.split(" ", 1)))]

    def fetch(self) -> Optional[str]:
        url = self.ctx.git.out("remote", "get-url", "origin")
        if url is None:
            return None
        history = os.path.join(self.ctx.temp, "takes-history.git")
        shutil.rmtree(history, ignore_errors=True)
        made = self.ctx.git.ok("init", "-q", "--bare", history)
        if made:
            left = self.within(self.ctx.seconds("IDENTITY_TAKES_DOWNLOAD_TIMEOUT", "600"))
            fetched = tool_git.Git(self.ctx.runner, self.ctx.env, history).call(
                "fetch", "-q", "--no-tags", "--filter=tree:0", f"--depth={self.depth + 1}", url.strip(), self.ctx.head,
                timeout=left, options=tuple(self.auth()))
            made = fetched.code == 0
        if not made:
            self.ctx.warn(f"{self.ctx.label}: could not fetch the head's history from git either")
            return None
        return history

    def walk(self, history: str) -> Optional[str]:
        listed = tool_git.Git(self.ctx.runner, self.ctx.env, history).out(
            "rev-list", f"--max-count={self.depth}", f"{self.ctx.head}~1", quiet=True) or ""
        for sha in listed.split():
            left = self.within(self.gh_read)
            self.checked += 1
            state = self.status(sha, left)
            if state is None:
                self.within(1)
                self.ctx.warn(f"{self.ctx.label}: could not read {sha}'s status by REST either")
                return None
            if state == "SUCCESS":
                self.ctx.warn(f"{self.ctx.label}: {sha} is green, {self.checked} commit(s) back (history from git)")
                return sha
        self.ctx.warn(f"{self.ctx.label}: no green commit in the {self.checked} commit(s) of the head's history git gave"
                      " -- no base named")
        return None

    def status(self, sha: str, left: float) -> Optional[str]:
        jq = (f'[.statuses[] | select(.context == "{self.context}" and (.created_at // "") >= "{self.floor}") | .state][0]'
              ' // "-" | ascii_upcase')
        answer = self.ctx.gh.read(["api", f"repos/{self.ctx.env.get('GITHUB_REPOSITORY', '')}/commits/{sha}/status?per_page=100",
                                   "--jq", jq], left)
        return None if answer is None else answer.strip()


def resolve(ctx: Context, aa: bool) -> str:
    """The base: the head itself (THE A/A), TAKES_BASE (named by hand, expanded when abbreviated), or the pick."""
    if aa:
        return ctx.head
    base = ctx.env.get("TAKES_BASE") or History(ctx, ctx.setting("STATUS_CONTEXT", ctx.label), history_floor(ctx)).pick()
    if not base:
        raise ctx.fail(f"::error::{ctx.label}: could not name a base commit")
    if re.match(r"^[0-9a-f]{40}$", base):
        return base
    # a fetch asks the server for a FULL object name: run 38533191835's every leg failed to fetch its short base
    full = ctx.gh.read(["api", f"repos/{ctx.env.get('GITHUB_REPOSITORY', '')}/commits/{base}", "--jq", ".sha"],
                       ctx.seconds("IDENTITY_TAKES_GH_TIMEOUT", "120"))
    if full is None:
        raise ctx.fail(f"::error::{ctx.label}: no commit {base} in {ctx.env.get('GITHUB_REPOSITORY', '')}")
    return full.strip()


def fetch(ctx: Context, base: str) -> None:
    timeout = ctx.seconds("IDENTITY_TAKES_DOWNLOAD_TIMEOUT", "600")
    if ctx.runner.run(tool_process.Command(["git", "fetch", "-q", "--no-tags", "--depth=1", "origin", base], ctx.env,
                                           timeout=timeout)) != 0:
        raise ctx.fail(f"::error::{ctx.label}: could not fetch base {base}")
