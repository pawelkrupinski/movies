#!/usr/bin/env python3
# Purpose: The tools' e2e layer's mutation proof: each scenario fails when the behaviour it holds is broken
"""THE MUTATION PROOF of the tools' end-to-end layer (scripts/e2e/, the owner, 2026-11-27): each scenario FAILS when the
behaviour it holds is broken.

  scripts/e2e/mutations.py [SCENARIO...]       # every mutation below, or those of the named scenario modules

For each entry, the scenario module runs with $E2E_MUTATION naming ONE exact text replacement in one tool file (it must
match once, or the entry is stale and says so); scripts/e2e/e2e_world.py then runs every tool from a copy of the tool
trees with it applied. The proof holds when the scenario fails. The unmutated layer passing is run-script-tests' run.
Exit 0 when every mutation was caught. A run of each scenario apiece: run it when a scenario or the code a mutation
names changes, never as a push step."""
from __future__ import annotations

import json
import os
import sys
from typing import Dict, List, Tuple

HERE = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import cli_harness  # noqa: E402
import machine  # noqa: E402

# (scenario module, the tool file, its text, the mutation, what it breaks)
MUTATIONS: List[Tuple[str, str, str, str, str]] = [
    ("fail_before_e2e_test", "scripts/fail_before.py",
     'DEFAULT_PATTERNS = ("*_test.py", "*-test.sh", "test/resources/*", ".claude/*")',
     'DEFAULT_PATTERNS = ("*",)',
     "BEFORE takes the commit's production files too, so a fix's test passes there"),
    ("ci_wait_e2e_test", "scripts/ci_wait.py",
     '["git", "merge-base", "--is-ancestor", base, head]',
     '["git", "merge-base", "--is-ancestor", head, base]',
     "the ancestry asked the wrong way round: a cancelled run is never followed to its descendant's"),
    ("ci_wait_e2e_test", "scripts/ci_wait.py",
     'if any(c not in (None, "success", "cancelled") for c in conclusions):',
     'if any(c not in (None, "success", "cancelled", "failure") for c in conclusions):',
     "a red run read as no failure"),
    ("deploy_e2e_test", "infra/bin/deploy", "    if behind > 0:", "    if behind > 1000:",
     "a checkout behind origin main deployed: the host rolled back"),
    ("deploy_e2e_test", "infra/bin/deploy", "    if END not in out:", "    if not out:",
     "a forced command's answer read as the host's state"),
    ("pre_push_e2e_test", "scripts/run-script-tests",
     "return 1 if too_old_python(chosen.tests) else run_here(o, s, env, scratch, scope, chosen)",
     "return 1 if too_old_python(chosen.tests) else (run_here(o, s, env, scratch, scope, chosen) and 0)",
     "a failing self-test's run exits 0: the push it should block lands"),
    ("pre_push_e2e_test", "scripts/pure_move_check.py",
     "pure = not verdict.hunks and not verdict.risks", "pure = not verdict.risks",
     "a move that also edits code passes as a pure move"),
    ("pre_push_e2e_test", "scripts/install-hooks.sh",
     "git config core.hooksPath scripts/hooks\n", "git config core.hooksPath scripts/hooks-gone\n",
     "the hooks installed where git never runs them: every push lands unchecked"),
]


def run(module: str, change: Dict[str, str]) -> cli_harness.Done:
    env = dict(machine.environment(), E2E_MUTATION=json.dumps(change), PYTHONDONTWRITEBYTECODE="1")
    return cli_harness.run_bounded([sys.executable, "-B", os.path.join(HERE, module + ".py")], env=env, cwd=HERE,
                                   stdin=None, bound=900)


def caught(module: str, rel: str, old: str, new: str, why: str) -> bool:
    """The scenario run with this mutation: whether it failed (a stale entry is no catch)."""
    done = run(module, {"file": rel, "old": old, "new": new})
    output = done.output
    stale = "is stale" in output
    ok = done.code != 0 and not stale
    print(f"{'CAUGHT' if ok else 'STALE ' if stale else 'MISSED'} {module}: {why} ({rel})", flush=True)
    if not ok:
        print(output[-3000:])
    return ok


def main(argv: List[str]) -> int:
    missed = [m[0] for m in MUTATIONS if (not argv or m[0] in argv) and not caught(*m)]
    print("every mutation caught" if not missed else f"NOT CAUGHT: {', '.join(missed)}")
    return 1 if missed else 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
