#!/usr/bin/env python3
"""How many of a country's split order-independence rows a convergence run runs
HOW MANY OF A COUNTRY'S SPLIT ORDER-INDEPENDENCE ROWS A CONVERGENCE RUN RUNS: `both` (p01 and p12: passes 0,1 and
1,2, holding all three passes to one corpus) or `one` (p01 alone: passes 0 and 1).

BOTH on main, always. Off main (an agent's branch; a measure/ branch runs no order row at all) ONE, but only when the
ref's change from main is KNOWN and touches nothing .github/order-independence-paths.txt lists -- what can make the
films depend on arrival order. Anything else is `both`: a read that fails or hangs, an answer that is not a file
list, a list the forge may have cut short, a path list that cannot be read.

THE TRADE-OFF (also in identity-model-convergence.yml's leg comment): `one` compares two arrival orders where `both`
compares three, so an order dependence that shows only between passes 1 and 2 goes unseen on that branch -- until
its change reaches main, where both rows run on every push. A change that can move the order (any module's main code,
the convergence specs, the build, this wiring) runs both wherever it is. Saves one ~8-10 minute row per split country
(DE, UK, US) per convergence run off main; Poland and Spain run their passes in one row either way.

usage: order-phases.sh <ref> <sha>    env: GH_REPO (owner/repo), GH_TOKEN; ORDER_PATHS (default the paths file)
Prints `both` or `one`, and why on stderr; exits 0 either way. Tested by scripts/ci/order_phases_test.py."""
from __future__ import annotations

import os
import sys
from typing import Callable, List, Mapping, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from seams import tool_files  # noqa: E402
from vcs import tool_github  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# A read that fails is never "nothing changed": the filter errors (gh exits 5) unless `files` is a list.
FILES_FILTER = '.files | if type == "array" then .[].filename else error("no file list") end'
Answer = Tuple[str, str]  # `both` or `one`, and why
Changed = Callable[[], Optional[List[str]]]


def both(why: str) -> Answer:
    return "both", why


def path_prefixes(text: str) -> List[str]:
    """The order paths of the paths file: every line that is neither blank nor a comment, as written."""
    return [line for line in text.splitlines() if line.strip() and not line.lstrip().startswith("#")]


def settled_before_reading(ref: str, sha: str, repo: str, prefixes: List[str], paths_file: str) -> Optional[Answer]:
    """The `both` that needs no read of what changed, None when that must be read."""
    if ref == "refs/heads/main":
        return both("main runs every order row")
    if not sha or not repo:
        return both("no ref, commit or repository to compare")
    if not prefixes:
        return both(f"no order paths readable at {paths_file}")
    return None


def order_code(files: List[str], prefixes: List[str]) -> Optional[str]:
    """Why the first file that is order code is: `<file> is order code (<prefix>)`; None when none is."""
    for file in files:
        for prefix in prefixes:
            if file.startswith(prefix):
                return f"{file} is order code ({prefix})"
    return None


def verdict(ref: str, sha: str, repo: str, prefixes: List[str], paths_file: str, changed: Changed,
            max_files: int) -> Answer:
    early = settled_before_reading(ref, sha, repo, prefixes, paths_file)
    if early is not None:
        return early
    files = changed()
    if files is None:
        return both("could not read the change from main")
    if len(files) >= max_files:
        return both(f"{len(files)} files changed: a list that may be cut short")
    code = order_code(files, prefixes)
    if code is not None:
        return both(code)
    return "one", f"{len(files)} file(s) changed from main, none order code"


def file_names(answer: Optional[str]) -> Optional[List[str]]:
    return None if answer is None else [line for line in answer.splitlines() if line]


def main(argv: List[str], env: Mapping[str, str], files: tool_files.Files, github: tool_github.GitHub) -> int:
    ref, sha = (argv + ["", ""])[:2]
    repo = env.get("GH_REPO", "")
    paths_file = env.get("ORDER_PATHS") or os.path.join(REPO_ROOT, ".github/order-independence-paths.txt")
    prefixes = path_prefixes(files.read_text_or(paths_file, ""))
    timeout = float(env.get("ORDER_PHASES_TIMEOUT", "60"))

    def changed() -> Optional[List[str]]:
        return file_names(github.read(["api", f"repos/{repo}/compare/main...{sha}", "--jq", FILES_FILTER], timeout,
                                      quiet=True))

    answer, why = verdict(ref, sha, repo, prefixes, paths_file, changed, int(env.get("ORDER_PHASES_MAX_FILES", "100")))
    print(f"order-phases: {answer} -- {why}", file=sys.stderr)
    print(answer)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment(), tool_files.Files.here(), tool_github.GitHub.here()))
