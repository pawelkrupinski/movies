#!/usr/bin/env python3
""".github/scripts/kick-convergence.sh (dispatch each convergence suite only for a push that can change its verdict),
black-box, against a scratch repository and a fake gh: its read of the suite's last run works where GNU timeout(1) does
not exist (stock macOS) and a hung read is bounded; an unreadable base, a base the checkout lacks and a diff that fails
each dispatch; a push outside the paths does not; a run of the commit already queued or running is not dispatched
again; the reuse hint goes out as reuse-run for the identity model's suite alone. ConvergenceDispatchGateSpec holds the
same gate from the JVM side. Ported from kick-convergence-test.sh; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path(".github/scripts/kick_convergence.py", ".github/scripts/kick-convergence.sh")
# gh: `run list` answers $STUB/last (or, while $STUB/hang exists, records its pid there and never answers) -- the
# in-flight read $STUB/inflight; `workflow run` (logged, as every call) refused with $REFUSE
GH = '''case "$1 $2" in
  "run list") case " $* " in *" databaseId,headSha,status "*) cat "$STUB/inflight" 2>/dev/null; exit 0 ;; esac
              [ -e "$STUB/hang" ] && { echo $$ >> "$STUB/hang"; exec sleep 86400; }
              cat "$STUB/last" 2>/dev/null ;;
  "workflow run") [ -z "${REFUSE:-}" ] || { echo "HTTP 403: Resource not accessible" >&2; exit 1; } ;;
esac'''
IDENTITY = "Identity model convergence"


class KickConvergenceTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH)
        # timeout(1) as stock macOS has it: not at all
        self.fakes.script("timeout", 'echo "timeout: command not found" >&2\nexit 127')
        self.write("paths", "worker/src/main/**\n")
        self.checkout = self.repo("repo")
        self.base = self.commit("worker/src/main/Pipeline.scala", "v1\n", "base")
        self.docs = self.commit("docs/notes.md", "notes\n", "docs")
        self.pipeline = self.commit("worker/src/main/Pipeline.scala", "v2\n", "pipeline")
        self.last(self.base)
        self.out = ""

    def commit(self, path: str, text: str, message: str) -> str:
        self.write(f"repo/{path}", text)
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", message)
        return self.git(self.checkout, "rev-parse", "HEAD")

    def last(self, sha: str) -> None:
        """The head of the suite's newest run, as `run list` answers it ("" for a suite that never ran)."""
        self.write("last", sha + "\n" if sha else "")

    def kick(self, head: str, suite: str = "Country convergence", **env: str) -> int:
        """The gate over `head`: its exit; the suites dispatched in self.dispatched(), its output in self.out."""
        self.fakes.clear()
        done = self.run_tool(head, "main", suite, cwd=self.checkout, env={
            "STUB": self.work, "CONVERGENCE_PATHS_FILE": self.path("paths"), "DISPATCH_RETRY_SECONDS": "0",
            "KICK_CONVERGENCE_GH_TIMEOUT": "30", **env})
        self.out = done.output
        return done.code

    def dispatched(self) -> List[str]:
        return [c[2] for c in self.fakes.calls("gh") if c[:2] == ["workflow", "run"]]

    def dispatch_args(self) -> List[List[str]]:
        return [c for c in self.fakes.calls("gh") if c[:2] == ["workflow", "run"]]

    def test_a_push_outside_the_paths_is_not_dispatched_and_a_pipeline_change_is(self) -> None:
        self.assertEqual((0, []), (self.kick(self.docs), self.dispatched()), self.out)
        # the base is the suite's newest run ON MAIN, never a measurement's or another branch's
        listed = [c for c in self.fakes.calls("gh") if c[:2] == ["run", "list"] and "headSha" in c]
        self.assertTrue(listed and all("--branch" in c and c[c.index("--branch") + 1] == "main" for c in listed), listed)
        self.assertEqual((0, ["Country convergence"]), (self.kick(self.pipeline), self.dispatched()), self.out)

    def test_a_commit_whose_run_is_already_queued_or_running_is_not_dispatched_again(self) -> None:
        self.write("inflight", "3653\n")
        self.assertEqual((0, []), (self.kick(self.pipeline), self.dispatched()), self.out)
        self.assertRegex(self.out, "run 3653 for .* is already queued or running")

    def test_no_base_or_one_the_checkout_lacks_dispatches(self) -> None:
        self.last("")
        self.assertEqual(["Country convergence"], (self.kick(self.docs), self.dispatched())[1], "a suite that never ran")
        self.last("0123456789abcdef0123456789abcdef01234567")
        self.assertEqual(["Country convergence"], (self.kick(self.docs), self.dispatched())[1], "a base not held")

    def test_a_hung_read_of_the_last_run_is_no_base_cut_off_by_its_bound(self) -> None:
        # the fake never answers, so the gate gets past it only by the read's bound cutting it off: its own exit and no
        # gh left running prove that, never elapsed time
        self.write("hang", "")
        self.assertEqual((0, ["Country convergence"]), (self.kick(self.docs, KICK_CONVERGENCE_GH_TIMEOUT="1"),
                                                        self.dispatched()), self.out)
        self.assertEqual(0, cli_harness.still_running(self.path("hang")))

    def reuse_args(self, found: str, suite: str = IDENTITY) -> List[str]:
        finder = self.fakes.script("finder", f'echo "{found}"')
        self.kick(self.pipeline, suite, CONVERGENCE_REUSE_FINDER=finder)
        return self.dispatch_args()[0]

    def test_the_reuse_hint_goes_out_as_reuse_run_for_the_identity_suite_alone(self) -> None:
        plain: Dict[str, List[str]] = {s: ["workflow", "run", s, "--ref", "main"] for s in (IDENTITY, "Country convergence")}
        self.assertEqual(plain[IDENTITY] + ["-f", "reuse-run=2821"], self.reuse_args("2821"))
        self.assertEqual(plain[IDENTITY], self.reuse_args(""), "no run found: the plain dispatch")
        self.assertEqual(plain[IDENTITY], self.reuse_args("2821 -f takes-only=true"), "an answer that is no run id")
        self.assertEqual(plain["Country convergence"], self.reuse_args("2821", "Country convergence"))

    def test_a_diff_that_cannot_be_matched_is_not_nothing_changed(self) -> None:
        matcher = self.fakes.script("broken-matcher", "exit 1")
        self.assertEqual(["Country convergence"], (self.kick(self.docs, CHANGED_PATHS_MATCHER=matcher),
                                                   self.dispatched())[1], self.out)

    def test_a_refused_dispatch_fails_the_job(self) -> None:
        self.assertEqual(1, self.kick(self.pipeline, REFUSE="1"), self.out)
        self.assertIn("::error::could not dispatch Country convergence", self.out)


if __name__ == "__main__":
    unittest.main()
