"""WHAT A PUSH PUSHES, and the checks its commits add or drop (scripts/hooks/pre-push's header: the ranges git's stdin
names, or a dry run's one range; the paths they changed; the rules a commit's MESSAGE brings -- `Fixes-Red:` the drift
guards, `Pure-Move:` the puremove check and no identity area for a verified move, a drift path the identity drift
check -- and --fast-only / --verify-only's narrowing). Every git question goes through `Repo`."""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from fnmatch import fnmatchcase
from typing import Callable, List, Mapping, Optional, Sequence, Set, Tuple

from pre_push_hook.plan.pre_push_repo import Repo, Say

ZERO = "0" * 40
DRY_RUNS = ("--range", "--checks-only", "--fast-only", "--verify-only")
FAST_CHECKS = ("sbt", "shellcheck", "actionlint")
AREA_CHECKS = ("identity", "itrest", "itidentity", "e2ecorpus")
PURE_MOVE_AREAS = ("identity", "itidentity", "e2ecorpus")
CI_REFS = ("refs/heads/measure/", "refs/heads/check/", "refs/heads/land/")

Plan = Callable[[List[str]], Optional[List[str]]]  # changed paths -> the checks they select (None: it failed)


def no_ranges() -> List[str]:
    return []


@dataclass
class Push:
    """The pushed ranges (`<base>..<sha>`) and how the push is judged."""
    ranges: List[str] = field(default_factory=no_ranges)
    to_main: bool = False
    checks_only: bool = False
    fast_only: bool = False
    verify_only: bool = False
    measure_only: bool = False
    tag_only: bool = False
    other: bool = False
    pushed: str = ""  # git's stdin as it came, for agentctl prepush-check


def dry_run(args: Sequence[str]) -> Push:
    mode = args[0]
    return Push([args[1]], True, mode != "--range", mode in ("--fast-only", "--verify-only"),
                mode == "--verify-only")


def base_for(repo: Repo, say: Say, local: str, remote: str) -> str:
    """A pushed ref's base: the remote's sha when this clone has it, else its merge-base with origin/main."""
    if remote != ZERO and repo.git("cat-file", "-e", f"{remote}^{{commit}}")[0] == 0:
        return remote
    if repo.git("rev-parse", "-q", "--verify", "origin/main")[0] == 0:
        code, base = repo.git("merge-base", local, "origin/main")
        if code == 0 and base.strip():
            return base.strip()
        say.raw(f"\033[33m! pre-push\033[0m {local[:9]} shares no history with origin/main (unrelated or shallow) — "
                "nothing checked (git fetch --unshallow, or rebase onto origin/main)\n")
        return ""
    say.raw(f"\033[33m! pre-push\033[0m no origin/main to find the base of {local[:9]} from — nothing checked "
            "(git fetch origin, then push again)\n")
    return ""


def from_stdin(repo: Repo, say: Say, text: str) -> Push:
    """git's `<local ref> <local sha> <remote ref> <remote sha>` lines."""
    push = Push()
    for line in text.split("\n"):
        words = line.split()
        if not words:
            continue
        words += [""] * (4 - len(words))
        push.pushed += f"{words[0]} {words[1]} {words[2]} {' '.join(words[3:])}\n"
        take_ref(repo, say, push, words)
    return push


def take_ref(repo: Repo, say: Say, push: Push, words: List[str]) -> None:
    local, remote_ref, remote = words[1], words[2], words[3]
    if local == ZERO:
        return
    if remote_ref.startswith("refs/tags/"):
        push.tag_only = True
        return
    if remote_ref.startswith(CI_REFS):
        push.measure_only = True
        return
    push.other = True
    base = base_for(repo, say, local, remote)
    if base:
        push.ranges.append(f"{base}..{local}")
    if remote_ref == "refs/heads/main":
        push.to_main = True


@dataclass
class Changed:
    paths: List[str]   # changed, still there
    drift: List[str]   # and deleted (the drift check's selection counts deletions)


def changed(repo: Repo, say: Say, ranges: Sequence[str]) -> Optional[Changed]:
    """THREE dots: what the pushed commits changed since they left the base. None when a diff fails."""
    names: Set[str] = set()
    drift: Set[str] = set()
    for r in ranges:
        three = r.replace("..", "...", 1)
        kept = repo.git("diff", "--name-only", "--diff-filter=d", three)
        gone = repo.git("diff", "--name-only", "--diff-filter=D", three) if kept[0] == 0 else (1, "")
        if kept[0] != 0 or gone[0] != 0:
            say.raw(f"\033[33m! pre-push\033[0m cannot diff {r} — blocked (bypass with --no-verify)\n")
            return None
        names |= {p for p in kept[1].split("\n") if p}
        drift |= {p for p in kept[1].split("\n") + gone[1].split("\n") if p}
    return Changed(sorted(names), sorted(drift))


def union_merged(repo: Repo, paths: List[str], head: str) -> List[str]:
    """The changed paths the pushed tree merges by union; every changed path when git cannot say (fail closed)."""
    code, attrs = repo.git("check-attr", f"--source={head}", "--stdin", "merge", text_in="".join(p + "\n" for p in paths))
    return paths if code != 0 else [m.group(1) for m in re.finditer(r"(?m)^(.*): merge: union$", attrs)]


def fast_select(repo: Repo, plan: Plan, checks: List[str], paths: List[str], head: str) -> List[str]:
    """--fast-only: the fast checks, and those a union-merged changed path selects (THE COMBINED FILES)."""
    union = union_merged(repo, paths, head)
    combined = [c for c in (plan(union) or []) if c not in AREA_CHECKS] if union else []
    return [c for c in checks if c in FAST_CHECKS or c in combined]


def readers_selected(globs: List[str], paths: List[str]) -> bool:
    """--verify-only: a changed path a repo-wide self-test reads (a declared glob, matched as bash's `case`)."""
    return any(fnmatchcase(path, glob) for path in paths for glob in globs if glob)


def with_trailer_checks(repo: Repo, ranges: Sequence[str], checks: List[str]) -> List[str]:
    """A `Fixes-Red:` brings the drift guards, a `Pure-Move:` the puremove check, whatever the paths."""
    out = list(checks)
    if "driftguards" not in out and any(repo.trailers("Fixes-Red", r) for r in ranges):
        out.append("driftguards")
    if any(repo.trailers("Pure-Move", r) for r in ranges):
        out.append("puremove")
    return out


def unclaimed_paths(repo: Repo, ranges: Sequence[str]) -> Optional[List[str]]:
    """The paths of the commits not saying `Pure-Move: verified`; None when every commit says it... or none does."""
    paths: Set[str] = set()
    claimed = False
    for r in ranges:
        for sha in repo.lines("rev-list", r):
            if repo.lines("log", "-1", "--format=%(trailers:key=Pure-Move,valueonly,unfold)", sha) == ["verified"]:
                claimed = True
                continue
            paths |= set(repo.lines("diff-tree", "--no-commit-id", "--name-only", "-r", "-m", "--root", sha))
    return sorted(paths) if claimed else None


def unclaimed_areas(plan: Plan, unclaimed: List[str]) -> List[str]:
    return [c for c in (plan(unclaimed) or []) if c in PURE_MOVE_AREAS] if unclaimed else []


def without_moved_areas(repo: Repo, say: Say, plan: Plan, ranges: Sequence[str], checks: List[str]) -> List[str]:
    """A VERIFIED PURE MOVE OWES NO IDENTITY AREA: the areas only the unclaimed commits' paths select."""
    if not any(c in PURE_MOVE_AREAS for c in checks):
        return checks
    unclaimed = unclaimed_paths(repo, ranges)
    if unclaimed is None:
        return checks
    areas = unclaimed_areas(plan, unclaimed)
    kept: List[str] = []
    for c in checks:
        if c in PURE_MOVE_AREAS and c not in areas:
            say.log(f"{c}: left out -- only commits saying 'Pure-Move: verified' select it (the puremove check holds "
                    "them)", True)
            continue
        kept.append(c)
    return kept


def split(say: Say, env: Mapping[str, str], checks: List[str]) -> List[str]:
    """THE SPLIT CHECK: PREPUSH_ONLY runs only those, PREPUSH_ELSEWHERE all but those."""
    only, elsewhere = env.get("PREPUSH_ONLY", ""), env.get("PREPUSH_ELSEWHERE", "")
    if not only and not elsewhere:
        return checks
    def out(c: str) -> bool:
        return c not in only.split() if only else c in elsewhere.split()
    others = [c for c in checks if out(c)]
    if others:
        say.log("left to the landing check's other job(s): " + " ".join(others), True)
    return [c for c in checks if not out(c)]


DRIFT_PATHS = ".github/identity-drift-paths.txt"
DRIFT_GATE_PATHS = ".github/identity-drift-gate-paths.txt"
CLAIMS = ("--format=%(trailers:key=Red-Unrelated,valueonly,unfold,separator=%x2C)%x09"
          "%(trailers:key=Fixes-Red,valueonly,unfold,separator=%x2C)")


def touches(repo: Repo, say: Say, paths: List[str], listing: str) -> Optional[bool]:
    """Whether a path matches the listing's globs; None (blocked) when the matcher fails."""
    code, out = repo.output([".github/actions/changed-paths/matches.sh", listing], "".join(p + "\n" for p in paths))
    if code != 0 or not out.strip():
        say.alarm(f"cannot match the pushed paths against {listing} -- blocked")
        return None
    return out.strip() == "true"


def claimed_reds(repo: Repo, ranges: Sequence[str]) -> str:
    """The runs the pushed commits claim to fix (`Fixes-Red:`), but a commit saying `Red-Unrelated:`'s."""
    runs: Set[int] = set()
    for r in ranges:
        for line in repo.git_out("log", CLAIMS, r).split("\n"):
            unrelated, _, fixes = line.partition("\t")
            if unrelated == "":
                runs |= {int(w.lstrip("#")) for w in re.split(r"[,; \n]", fixes) if re.fullmatch(r"#?[0-9]+", w)}
    return " ".join(str(n) for n in sorted(runs))


def drift_claims(repo: Repo, say: Say, is_file: Callable[[str], bool], push: Push,
                 paths: List[str]) -> Optional[Tuple[str, str]]:
    """THE IDENTITY DRIFT CHECK's (reds, birth) for a push to main touching a drift path; None: blocked."""
    if not push.to_main or push.fast_only or not is_file(DRIFT_PATHS):
        return "", ""
    touched = touches(repo, say, paths, DRIFT_PATHS)
    if touched is None:
        return None
    birth = drift_birth(repo, say, is_file, push, paths, touched)
    if birth is None:
        return None
    return claimed_reds(repo, push.ranges) if touched else "", birth


def drift_birth(repo: Repo, say: Say, is_file: Callable[[str], bool], push: Push, paths: List[str],
                touched: bool) -> Optional[str]:
    """A CHECK'S BIRTH OR TIGHTENING: the gate's own files touched, or `New-Check:` on a push touching a drift path."""
    if is_file(DRIFT_GATE_PATHS):
        gate = touches(repo, say, paths, DRIFT_GATE_PATHS)
        if gate is None:
            return None
        if gate:
            return "the gate's own files"
    return "New-Check:" if touched and any(repo.trailers("New-Check", r) for r in push.ranges) else ""
