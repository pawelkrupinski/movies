#!/usr/bin/env python3
"""END TO END: scripts/ci-wait, the one way a tool or an agent waits on CI, against the stub GitHub (scripts/e2e/
gh_stub.py through the gh shim) and a REAL git clone -- the ancestry it follows a cancelled run by is real git's.
scripts/e2e/e2e_world.py.

  - a run seen queued, then running, then green: exit 0 once it completes, each state polled through;
  - a red run: exit 1, naming the run;
  - main's runs cancel each other: the run for commit A cancelled, the run for its descendant B green -- the wait on A
    follows to B (git says B descends from A) and exits 0, naming B as superseding A.
Mutations (scripts/e2e/mutations): a cancelled run never followed; a failure read as success."""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402

GREEN = (("queued", ""), ("in_progress", ""), ("completed", "success"))


class CiWaitE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.toy_repo("app", {"a.txt": "a\n"})
        self.gh = self.w.github()

    def wait(self, *args: str) -> e2e_world.cli_harness.Done:
        return self.w.run([self.w.tool("scripts/ci-wait"), *args, "--interval", "0.05", "--timeout", "1m"],
                          self.repo.clone, env={"CI_WAIT_GH_TIMEOUT": "20"})

    def test_a_run_is_followed_to_its_green_verdict(self) -> None:
        run = self.gh.add_run("Main", self.repo.main(), steps=GREEN)
        done = self.wait("--run", str(run))
        self.assertRan(done, 0, str(run))
        views = [c for c in self.w.calls("gh") if c["argv"][:2] == ["run", "view"]]
        self.assertGreaterEqual(len(views), 3, "every state was not polled through")

    def test_a_red_run_fails_the_wait(self) -> None:
        run = self.gh.add_run("Main", self.repo.main(), steps=(("in_progress", ""), ("completed", "failure")))
        self.assertRan(self.wait("--run", str(run)), 1, str(run))

    def test_a_cancelled_run_is_answered_by_the_run_that_superseded_it(self) -> None:
        first = self.repo.main()
        self.w.write(self.repo.clone, "b.txt", "b\n")
        second = self.w.commit(self.repo.clone, "b")
        self.w.git(self.repo.clone, "push", "-q", "origin", "HEAD:main")
        self.gh.add_run("Main", first, steps=(("in_progress", ""), ("completed", "cancelled")))
        later = self.gh.add_run("Main", second, steps=GREEN)
        done = self.wait("--workflow", "Main", "--sha", first)
        self.assertRan(done, 0, str(later), "superseded run")
        self.assertFalse([c for c in self.w.calls("gh") if "compare" in " ".join(c["argv"])],
                         "the ancestry was asked of GitHub, not of the clone's git")


if __name__ == "__main__":
    unittest.main()
