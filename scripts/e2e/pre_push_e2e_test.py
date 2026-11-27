#!/usr/bin/env python3
# script-test-reads: scripts/hooks/pre-push scripts/pre_push*.py scripts/lib/pre_push_*.py scripts/run-script-tests scripts/lib/script_tests_*.py scripts/pure-move-check scripts/pure_move_*.py scripts/install-hooks.sh scripts/hooks-path.sh scripts/e2e/*.py
"""END TO END: the landing chain on the laptop -- scripts/install-hooks.sh installs the hooks, a REAL `git push` runs
the installed scripts/hooks/pre-push (shell or Python: it is driven as a black box, by git), which selects its checks
from what the push changes and runs the real tools they name: scripts/run-script-tests over the push's self-tests,
scripts/pure-move-check over a `Pure-Move: verified` commit. In a clone of this checkout (the tools as the working tree
has them; scripts/e2e/e2e_world.py) pushing to a bare origin, with the stub GitHub answering the red gate green.

  - a script change whose self-test fails: the push is REFUSED, naming the test and `scripttests FAILED`, and origin's
    main does not move; fixed, the same push lands;
  - a commit claiming `Pure-Move: verified` that also changes a line: REFUSED by `puremove`; a real move lands.
Each push runs only the check under test (PREPUSH_ONLY, the landing check's own split): the rest of the hook's checks
-- sbt, the ratchets -- are theirs to test, and minutes each.
Mutations (scripts/e2e/mutations): run-script-tests exiting 0 over a failed self-test; pure-move-check passing a move
that also edits code; install-hooks.sh pointing git at a directory with no hooks."""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402

GATES = ("Main", "Identity model convergence", "Identity takes")
PROBE = "scripts/probe_adder.py"
TOOL = "#!/usr/bin/env python3\n# Purpose: The e2e layer's probe tool: adds two numbers\nimport sys\n" \
       "print(int(sys.argv[1]) + int(sys.argv[2]))\n"
TEST = ("import os, subprocess, sys, unittest\n\n\nclass ProbeTest(unittest.TestCase):\n"
        "    def test_adds(self):\n        tool = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'probe_adder.py')\n"
        "        out = subprocess.run([sys.executable, tool, '2', '3'], capture_output=True, text=True).stdout\n"
        "        self.assertEqual('{expected}', out.strip())\n\n\nunittest.main()\n")
SCALA = "package {package}\n\nobject E2eProbe {{\n  def answer: Int = {answer}\n}}\n"


class PrePushE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.movies_clone()
        self.clone = self.repo.clone
        gh = self.w.github()
        for workflow in GATES:
            gh.add_run(workflow, self.repo.main())
        self.assertRan(self.w.run(["scripts/install-hooks.sh"], self.clone), 0, "core.hooksPath = scripts/hooks")

    def push(self, check: str) -> e2e_world.cli_harness.Done:
        """`git push` to main, running only `check` of what the hook selects. The floor bash and the --linux
        container are left out (PREPUSH_SKIP): which of them a machine has is not this flow's, and the hook's own
        tests hold them."""
        return self.w.git_run(self.clone, "push", "origin", "HEAD:main",
                              env={"PREPUSH_ONLY": check, "PREPUSH_NO_REUSE": "1", "PREPUSH_SKIP": "floor linux"})

    def test_a_failing_self_test_blocks_the_push_until_it_passes(self) -> None:
        before = self.repo.main()
        self.w.write(self.clone, PROBE, TOOL, executable=True)
        self.w.write(self.clone, PROBE.replace(".py", "_test.py"), TEST.format(expected=6))
        self.w.git(self.clone, "add", "scripts")  # the catalogue lists what git tracks
        self.assertRan(self.w.run(["python3", "-B", "scripts/tool_catalogue.py"], self.clone), 0)  # docs/tools.md
        self.w.commit(self.clone, "a probe tool, its test wrong")
        done = self.push("scripttests")
        self.assertNotEqual(0, done.code, done.output[-6000:])
        self.assertRegex(done.output, r"FAIL +[0-9]+s +scripts/probe_adder_test\.py")
        self.assertIn("scripttests FAILED", done.output)
        self.assertIn("blocked by: scripttests", done.output)
        self.assertEqual([], [line for line in done.output.splitlines() if line.startswith("FAIL ")
                              and "probe_adder_test" not in line], "another self-test failed: " + done.output[-4000:])
        self.assertEqual(before, self.repo.main(), "a push whose self-test failed moved main")
        self.w.write(self.clone, PROBE.replace(".py", "_test.py"), TEST.format(expected=5))
        head = self.w.commit(self.clone, "the probe's test, right")
        done = self.push("scripttests")
        self.assertRan(done, 0, "scripttests ok")
        self.assertRegex(done.output, r"PASS +[0-9]+s +scripts/probe_adder_test\.py|[0-9]+ passed")
        self.assertEqual(head, self.repo.main())
        self.assertTrue(self.w.calls("agentctl"), "the hook never asked agentctl's prepush-check")
        self.assertNotIn("UNREAD", done.output, "the red gate could not read the stub GitHub")
        self.assertTrue([c for c in self.w.calls("gh") if c["argv"][:2] == ["run", "list"]], "the red gate never asked")

    def test_a_pure_move_claim_is_checked(self) -> None:
        self.w.write(self.clone, "web/src/main/scala/e2eprobe/E2eProbe.scala", SCALA.format(package="e2eprobe", answer=1))
        self.w.commit(self.clone, "a probe class")
        self.w.git(self.clone, "push", "-q", "--no-verify", "origin", "HEAD:main")  # the base the moves start from
        base = self.w.git(self.clone, "rev-parse", "HEAD")
        self.w.git(self.clone, "mv", "web/src/main/scala/e2eprobe", "web/src/main/scala/e2emoved")
        self.w.write(self.clone, "web/src/main/scala/e2emoved/E2eProbe.scala", SCALA.format(package="e2emoved", answer=2))
        self.w.commit(self.clone, "move the probe\n\nPure-Move: verified")
        done = self.push("puremove")
        self.assertNotEqual(0, done.code, done.output[-6000:])
        self.assertIn("puremove FAILED", done.output, done.output[-6000:])
        self.assertIn("NOT PURE", done.output, done.output[-6000:])
        self.assertEqual(base, self.repo.main(), "a move that is no move landed as one")
        self.w.write(self.clone, "web/src/main/scala/e2emoved/E2eProbe.scala", SCALA.format(package="e2emoved", answer=1))
        self.w.git(self.clone, "add", "-A")
        self.w.git(self.clone, "commit", "-q", "--no-verify", "--amend", "-m", "move the probe\n\nPure-Move: verified")
        head = self.w.git(self.clone, "rev-parse", "HEAD")
        self.assertRan(self.push("puremove"), 0, "puremove ok")
        self.assertEqual(head, self.repo.main())


if __name__ == "__main__":
    unittest.main()
