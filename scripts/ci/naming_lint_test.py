#!/usr/bin/env python3
"""naming_lint.py: two top-level types of one package named with the same words in another order fail (unless allowed
while renamed, a row no clash matches said as stale); a NEW type named for an action (…Apart, …Plans, …Reads) with at
most one public def warns, an old one or one with two public defs does not.
NAMING_LINT_UNDER_TEST names another copy of the tool."""

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("NAMING_LINT_UNDER_TEST") or os.path.join(HERE, "naming_lint.py")

DIR = "worker/src/main/scala/services/sweeps"


def scala(name: str, defs=("run",), kind="final class", package="services.sweeps") -> str:
    body = "".join(f"  def {d}(x: Int): Int = x\n" for d in defs)
    return f"package {package}\n\n{kind} {name} {{\n{body}  private def helper(y: Int): Int = y\n}}\n"


class NamingLintTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.git("scratch_repo", "-q")
        self.base = self.commit({"README": "x\n"}, "base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def git(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.git("scratch_git", "add", "-A")
        self.git("scratch_git", "commit", "-qm", message)
        return self.git("scratch_git", "rev-parse", "HEAD")

    def lint(self, *ranges, tool=TOOL):
        args = [sys.executable, "-I", tool, "check", "--root", self.repo]
        for r in ranges:
            args += ["--range", r]
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run(args, cwd=self.repo, env=env, capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    # RULE 1
    def test_two_types_of_one_package_with_the_same_words_reordered_fail(self):
        self.commit({f"{DIR}/CacheSweep.scala": scala("CacheSweep"), f"{DIR}/SweepCache.scala": scala("SweepCache")}, "both")
        rc, out = self.lint()
        self.assertEqual(1, rc, out)
        self.assertIn("NAMING REFUSED (rule 1)", out)
        self.assertIn("services.sweeps: CacheSweep", out)
        self.assertIn("SweepCache", out)

    def test_the_same_words_in_two_packages_pass(self):
        self.commit({f"{DIR}/CacheSweep.scala": scala("CacheSweep"),
                     "common/src/main/scala/tools/SweepCache.scala": scala("SweepCache", package="tools")}, "apart")
        rc, out = self.lint()
        self.assertEqual(0, rc, out)

    def test_a_class_and_its_companion_are_one_name(self):
        self.commit({f"{DIR}/CacheSweep.scala": scala("CacheSweep") + "object CacheSweep {\n  val x = 1\n}\n"}, "pair")
        self.assertEqual(0, self.lint()[0])

    def test_a_nested_type_is_no_top_level_name(self):
        nested = scala("CacheSweep").replace("  private def helper", "  final class SweepCache\n  private def helper")
        self.commit({f"{DIR}/CacheSweep.scala": nested}, "nested")
        self.assertEqual(0, self.lint()[0])

    def test_different_words_pass(self):
        self.commit({f"{DIR}/CacheSweep.scala": scala("CacheSweep"), f"{DIR}/SweepCaches.scala": scala("SweepCaches")}, "x")
        self.assertEqual(0, self.lint()[0])

    def test_the_package_clause_decides_not_the_directory(self):
        self.commit({f"{DIR}/CacheSweep.scala": scala("CacheSweep"),
                     f"{DIR}/other/SweepCache.scala": scala("SweepCache")}, "one package, two directories")
        self.assertEqual(1, self.lint()[0])

    def test_an_allowed_pair_passes_and_a_stale_allow_row_is_said(self):
        # The tool with an allow row of its own: the real table holds only renames under way, and may be empty.
        with open(TOOL) as f:
            source = f.read()
        tool = os.path.join(self.dir.name, "naming_lint.py")
        with open(tool, "w") as f:
            f.write(source.replace("ALLOWED_PERMUTATIONS = {\n",
                                   'ALLOWED_PERMUTATIONS = {\n    ("services.sweeps", frozenset({"CacheSweep", "SweepCache"})): "a rename under way",\n', 1))
        self.commit({f"{DIR}/CacheSweep.scala": scala("CacheSweep"), f"{DIR}/SweepCache.scala": scala("SweepCache")}, "being renamed")
        rc, out = self.lint(tool=tool)
        self.assertEqual(0, rc, out)
        self.assertIn("allowed: a rename under way", out)
        self.assertNotIn("no clash matches any more (services.sweeps", out)
        os.remove(os.path.join(self.repo, f"{DIR}/SweepCache.scala"))
        rc, out = self.lint(tool=tool)
        self.assertEqual(0, rc, out)
        self.assertIn("an ALLOWED_PERMUTATIONS row no clash matches any more (services.sweeps: CacheSweep, SweepCache)", out)

    def test_test_sources_are_not_read(self):
        self.commit({"worker/src/test/scala/services/sweeps/CacheSweep.scala": scala("CacheSweep"),
                     "worker/src/test/scala/services/sweeps/SweepCache.scala": scala("SweepCache")}, "specs")
        self.assertEqual(0, self.lint()[0])

    # RULE 2
    def test_a_new_action_named_type_with_one_public_def_warns(self):
        head = self.commit({f"{DIR}/ListingsApart.scala": scala("ListingsApart"),
                            f"{DIR}/TitlePlans.scala": scala("TitlePlans", defs=()),
                            f"{DIR}/SlotRead.scala": scala("SlotRead", kind="object")}, "new")
        rc, out = self.lint(f"{self.base}..{head}")
        self.assertEqual(0, rc, out)
        for name in ("ListingsApart", "TitlePlans", "SlotRead"):
            self.assertIn(f"naming WARNING (rule 2): a new type named for an action with at most one public def: {name}", out)

    def test_two_public_defs_or_an_old_file_or_another_suffix_do_not_warn(self):
        old = self.commit({f"{DIR}/ListingsApart.scala": scala("ListingsApart")}, "old")
        head = self.commit({f"{DIR}/TitlePlans.scala": scala("TitlePlans", defs=("plan", "explain")),
                            f"{DIR}/SweepPass.scala": scala("SweepPass"),
                            f"{DIR}/Thread.scala": scala("Thread")}, "new")
        rc, out = self.lint(f"{old}..{head}")
        self.assertEqual(0, rc, out)
        self.assertNotIn("WARNING", out)

    def test_a_class_and_its_companion_are_counted_together_once(self):
        text = scala("TitlePlans") + "object TitlePlans {\n  def apply(x: Int): TitlePlans = new TitlePlans\n}\n"
        head = self.commit({f"{DIR}/TitlePlans.scala": text, f"{DIR}/SlotReads.scala": scala("SlotReads", defs=()) +
                            "object SlotReads {\n  val x = 1\n}\n"}, "new")
        rc, out = self.lint(f"{self.base}..{head}")
        self.assertNotIn("TitlePlans", out)
        self.assertEqual(1, out.count("SlotReads ("), out)

    def test_private_defs_are_not_public(self):
        text = scala("TitlePlans").replace("  private def helper(y: Int): Int = y\n",
                                           "  private def a(y: Int): Int = y\n  protected def b(y: Int): Int = y\n")
        head = self.commit({f"{DIR}/TitlePlans.scala": text}, "new")
        self.assertIn("TitlePlans", self.lint(f"{self.base}..{head}")[1])


if __name__ == "__main__":
    unittest.main()
