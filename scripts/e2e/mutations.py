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
    ("push_k8s_secrets_e2e_test", "infra/bin/push-k8s-secrets", 'if [[ "$have" == "$want" ]]; then',
     'if [[ "$have" == "$want-never" ]]; then', "an unchanged key patched again on every run"),
    ("push_k8s_secrets_e2e_test", "infra/bin/push-k8s-secrets", "  if (( dry )); then echo",
     "  if (( dry > 5 )); then echo", "--dry-run patching the cluster anyway"),
    ("push_k8s_secrets_e2e_test", "infra/bin/push-k8s-secrets", '--patch-file /dev/stdin" >/dev/null',
     '--patch $patch" >/dev/null', "the Secret's values in kubectl's argv"),
    ("fleet_ssh_e2e_test", "infra/bin/fleet-ssh", "    if marked.search(existing):", "    if False:",
     "the generated block appended again on every run"),
    ("fleet_ssh_e2e_test", "infra/bin/fleet-ssh", "        updated = existing + sep + block", "        updated = block",
     "the person's own ssh config overwritten"),
    ("sbt_warm_e2e_test", "scripts/lib/sbt_warm_servers.py", "        if not self.procs.signal(pid, signum):",
     "        if not self.procs.signal(pid, 0):", "--stop reporting a stop and leaving the server running"),
    ("sbt_warm_e2e_test", "scripts/ci/sbt_guard.py", "    if status == 0 or STALE_READ not in log or PLUGIN_RETRIED in log:",
     "    if True:", "a stale-TASTy failure not retried: the call fails"),
    ("land_precheck_e2e_test", "scripts/land_precheck.py", "        if replay.code == 0:", "        if True:",
     "a change that conflicts with main read as rebasing cleanly"),
    ("land_precheck_e2e_test", "scripts/land_precheck.py", '"PASS" if code == 0 else "UNCHECKED"',
     '"PASS" if code in (0, 1) else "UNCHECKED"', "a commit-rule refusal read as a pass"),
    ("land_precheck_e2e_test", "scripts/land_precheck.py", "elif code in BOUND_ENDED or sbt_never_ran(text):",
     "elif code in BOUND_ENDED:", "an sbt that never ran read as a lint failure, refusing a sound change"),
    ("identity_stamp_carry_e2e_test", "scripts/identity_stamp_carry.py", "    if done.returncode == 0:\n        return []",
     "    if True:\n        return []", "a change that is no pure move carried as one: the stamp it moved is not re-decided"),
    ("identity_stamp_carry_e2e_test", "scripts/identity_stamp_carry.py", "    if r.digest != stamps[head][r.stamp]:",
     "    if False:", "a row's digest never compared with the build's: a hand-edited row proven"),
    ("identity_stamp_carry_e2e_test", "scripts/identity_stamp_carry.py", "    if r.equals != stamps[base][r.stamp]:",
     "    if False:", "a row's `equals` never compared with the build's: a stamp carried over the wrong predecessor"),
    ("identity_stamp_carry_e2e_test", "scripts/identity_stamp_carry.py", "    found = cited_range(r.base, r.head, old, new)",
     "    found = (r.base, r.head)", "a row proven at the range it cites, not the pushed one: a rebase over identity code passes"),
    ("identity_stamp_carry_e2e_test", "scripts/identity_stamp_carry.py",
     "        why = published_refusals(verdicts, ran_head, ran_base, patch_id or diff_id(base, head))", "        why = []",
     "a what-if verdict its author wrote carried: no published asset asked for"),
    ("identity_tools_e2e_test", "scripts/identity_slice_cli_modes.py", '"--labels", labels, *o.passed]', '"--labels", labels]',
     "the selector never passed to the JVM: every listing explained"),
    ("identity_tools_e2e_test", "scripts/identity_slice_cli_modes.py", "        return fa.read() == fb.read()",
     "        return True", "two refs' different traces called identical"),
    ("identity_tools_e2e_test", "scripts/identity_what_if_cli_verdict.py", 'record(ctx, sha, base, cc, "pure-move", "-")',
     'record(ctx, base, base, cc, "pure-move", "-")', "a pure move's verdict recorded under the wrong head: the carry finds none"),
    ("identity_tools_e2e_test", "scripts/identity_what_if_cli_verdict.py", "    for cc in o.ccs:\n        record(ctx, sha, base",
     "    for cc in o.ccs[:1]:\n        record(ctx, sha, base", "only the first country's pure-move verdict recorded"),
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
