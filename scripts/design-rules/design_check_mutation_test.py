#!/usr/bin/env python3
"""THE GENERAL RULE CORE FAILS CLOSED, proved by mutation (takes_skip_gate_mutation_test.py's model): each mutant below
is the core (scripts/design-rules/) with ONE rule taken out, and design_check_test.py must FAIL against every one -- a
rule whose removal no test notices could silently stop holding a language. A mutant is applied by exact text; a mutant
whose text is not in its file exactly once fails this test too (the core changed: re-aim the mutant). The mutated copy
runs from a scratch directory beside links to the real scripts/ siblings it imports (lib, ci, py-quality, js-quality)."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from typing import Dict, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
SCRIPTS = os.path.dirname(HERE)
TEST = os.path.join(HERE, "design_check_test.py")
TABLE = "test/resources/design-rules.tsv"
# name -> (file, exact text in it, what replaces it)
MUTANTS: Dict[str, Tuple[str, str, str]] = {
    "no suffix rule": ("naming.py", 'out.append(("suffix", key, 1))', "pass"),
    "no abbreviation rule": ("naming.py", 'out.append(("abbreviation", key, 1))', "pass"),
    "idioms refused too": ("../lib/rules_table.py", "return set(self.abbreviations) - set(self.idioms)", "return set(self.abbreviations)"),
    "one file's names clash": ("naming.py", "and len({p for _, p in orders.values()}) > 1", ""),
    "effects make no collaborator": ("collaborators.py", "return bool(any(m.effects.values()) or ", "return bool("),
    "no data guard": ("collaborators.py", " or carries_logic(m, rules))", ")"),
    "no closure over users": ("collaborators.py", "                found.add(user)\n", "                pass\n"),
    "fan-out counts data": ("collaborators.py", "return len(m.imports & collaborating)", "return len(m.imports)"),
    "no databag rule": ("collaborators.py", 'out += [("databag",', 'out += [] and [("databag",'),
    "abstractness over every module (V1)": ("packages.py", "members = inside & collaborating", "members = inside"),
    "no layer rule": ("packages.py", "if there is not None and there.rank > here.rank:", "if False:"),
    "no unplaced rule": ("packages.py", 'out.append(("unplaced", source, 1))\n            continue', "continue"),
    "adapters ignored": ("design_lint.py", "kind not in allowed", "True"),
    "no state rule": ("design_lint.py", 'out.append(("state", m.path, len(m.state)))', "pass"),
    "a reference is an effect": ("python_facts.py", 'kind and (id(node) in called or kind == "env")', "kind"),
    "the main guard runs at import": ("python_facts.py", "if isinstance(stmt, DEFINITIONS) or is_main_guard(stmt):", "if isinstance(stmt, DEFINITIONS):"),
    "a local shadows nothing": ("python_facts.py", "visible = module - (local_names(fn) - declared)", "visible = module"),
    "no reason lint": ("exemptions.py", 'out += [f"{kind} {key}: {p}" for p in coupling_reasons.reason_problems(name, why)]', "pass"),
    "js state not read": ("../js-quality/facts.js", "if (name) written.add(name);", ";"),
    "js clock not read": ("../js-quality/facts.js", "['clock', ['Date.now', 'performance.now']],", ""),
}


def scratch_core(root: str) -> str:
    """A copy of scripts/design-rules, scripts/lib and scripts/js-quality (the mutable ones) beside links to the rest."""
    scripts = os.path.join(root, "scripts")
    os.makedirs(scripts)
    for name in ("design-rules", "lib", "js-quality"):
        shutil.copytree(os.path.join(SCRIPTS, name), os.path.join(scripts, name), ignore=shutil.ignore_patterns("node_modules", "__pycache__"))
    for name in ("ci", "py-quality", "js-quality-report", "js_quality_report.py"):
        os.symlink(os.path.join(SCRIPTS, name), os.path.join(scripts, name))
    os.makedirs(os.path.join(root, "test", "resources"))  # the rules table report.js reads beside its checkout
    shutil.copy(os.path.join(os.path.dirname(SCRIPTS), TABLE), os.path.join(root, TABLE))
    return os.path.join(scripts, "design-rules")


def mutate(core: str, path: str, old: str, new: str) -> None:
    full = os.path.normpath(os.path.join(core, path))
    with open(full, encoding="utf-8") as f:
        text = f.read()
    if text.count(old) != 1:
        raise AssertionError(f"mutant text not in {path} exactly once: {old!r}")
    with open(full, "w", encoding="utf-8") as f:
        f.write(text.replace(old, new))


def run_test(core: str) -> Tuple[int, str]:
    env = {**os.environ, "DESIGN_RULES_UNDER_TEST": core}
    done = subprocess.run([sys.executable, TEST, "-f"], env=env, capture_output=True, text=True, timeout=600)
    return done.returncode, done.stdout + done.stderr


class DesignCheckMutationTest(unittest.TestCase):
    def test_the_core_test_fails_against_every_mutant(self) -> None:
        survivors = []
        for name, (path, old, new) in MUTANTS.items():
            with tempfile.TemporaryDirectory() as root:
                core = scratch_core(root)
                mutate(core, path, old, new)
                rc, _ = run_test(core)
                if rc == 0:
                    survivors.append(name)
        self.assertEqual([], survivors, "mutants design_check_test.py does not catch")

    def test_the_core_test_passes_against_the_unmutated_copy(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            rc, out = run_test(scratch_core(root))
        self.assertEqual(0, rc, out)


if __name__ == "__main__":
    unittest.main()
