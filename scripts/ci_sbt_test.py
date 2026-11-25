#!/usr/bin/env python3
"""scripts/ci-sbt, black-box, against a scratch repository, a fake identity-ci-run and a fake gh: the verdict is the
"Run the command" step's (pass 0, fail 1), never the run's -- an earlier step's failure, a cancelled run, no command
step, a run past its deadline are NO VERDICT (2), so scripts/fail-before --ci cannot read an infrastructure red as
"fails before" -- and the working-tree snapshot carries tracked changes and new sources, never another untracked file.
Ported from ci-sbt-test.sh case for case, plus the lines it says, the dispatch's exact argv, the usage errors and the
failures before a run; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci_sbt.py", "scripts/ci-sbt")
# identity-ci-run: prints a line, then the run id ($RUN); fails on $DISPATCH_FAILS
DISPATCHER = '''
err("identity-ci-run: pushed")
if env.get("DISPATCH_FAILS"):
    exit(1)
out(env.get("RUN", "101"))
'''
# gh: `run view` answers the run's JSON ($VIEW), `run download` lays an sbt.log (fails on $DOWNLOAD_FAILS)
GH = '''
if argv[:2] == ["run", "view"]:
    with open(env["VIEW"]) as f:
        out(f.read(), end="")
elif argv[:2] == ["run", "download"] and not env.get("DOWNLOAD_FAILS"):
    directory = argv[argv.index("--dir") + 1]
    os.makedirs(directory + "/identity-run-all", exist_ok=True)
    with open(directory + "/identity-run-all/sbt.log", "w") as f:
        f.write("[info] sbt said this\\n")
else:
    err("could not download")
    exit(1)
'''


def view(run: str, command: str, setup: str = "success", status: str = "completed") -> str:
    return json.dumps({"status": status, "conclusion": run, "jobs": [
        {"name": "plan", "steps": []},
        {"name": "run (all)", "steps": [{"name": "Set up sbt", "conclusion": setup},
                                        {"name": "Run the command", "conclusion": command}]}]})


class CiSbtTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        self.write("repo/worker/src/main/A.scala", "old\n")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "base")
        self.head = self.git(self.checkout, "rev-parse", "HEAD")
        self.dispatcher = self.fakes.program("identity-ci-run", DISPATCHER)
        self.gh = self.fakes.program("gh", GH)
        self.answer(view("success", "success"))

    def answer(self, text: str) -> None:
        self.write("view.json", text)

    def ci_sbt(self, *args: str, **extra: str) -> cli_harness.Done:
        env: Dict[str, Optional[str]] = {
            "KINOWO_IDENTITY_CI_RUN": self.dispatcher, "KINOWO_GH": self.gh, "KINOWO_CI_SBT_POLL": "0",
            "GH_DOWNLOAD_PUBLIC": "0", "GH_DOWNLOAD_ATTEMPTS": "1", "VIEW": self.path("view.json"), **extra}
        return self.run_tool(*args, env=env, cwd=self.checkout)

    def dispatched(self) -> List[List[str]]:
        return self.fakes.calls("identity-ci-run")

    def test_a_passed_command_step_exits_0_and_prints_the_sbt_log(self) -> None:
        done = self.ci_sbt("--", "worker/Test/test")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("[info] sbt said this\n", done.stdout)
        self.assertEqual([["--head", self.head, "--no-wait", "--countries", "pl", "--command", "worker/Test/test"]],
                         self.dispatched(), "a clean checkout runs HEAD itself, the command whole")
        self.assertEqual("identity-ci-run: pushed\n"
                         f"ci-sbt: run 101: `sbt worker/Test/test` at {self.head[:9]} on ci-runner's measure lane\n"
                         "ci-sbt: run 101: PASSED `worker/Test/test`\n", done.stderr)

    def test_the_words_after_the_separator_are_one_command(self) -> None:
        self.ci_sbt("--quiet", "--", "worker/testOnly", "X;", "web/compile")
        self.assertEqual("worker/testOnly X; web/compile", self.dispatched()[0][-1])

    def test_a_failed_command_step_exits_1(self) -> None:
        self.answer(view("failure", "failure"))
        done = self.ci_sbt("--quiet", "--", "worker/Test/test")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("", done.stdout, "--quiet prints no log")
        self.assertTrue(done.stderr.endswith("ci-sbt: run 101: FAILED `worker/Test/test`\n"), done.stderr)

    def test_a_step_before_the_command_failed_is_no_verdict(self) -> None:
        self.answer(view("failure", "skipped", setup="failure"))
        done = self.ci_sbt("--quiet", "--", "worker/Test/test")
        self.assertEqual(2, done.code, done.output)
        self.assertTrue(done.stderr.endswith(
            "ci-sbt: run 101: NO VERDICT: the command step is skipped (run failure)\n"), done.stderr)

    def test_a_cancelled_command_is_no_verdict(self) -> None:
        self.answer(view("cancelled", "cancelled"))
        self.assertEqual(2, self.ci_sbt("--quiet", "--", "worker/Test/test").code)

    def test_a_run_that_never_reached_the_command_is_no_verdict(self) -> None:
        self.answer(json.dumps({"status": "completed", "conclusion": "failure", "jobs": [
            {"name": "plan", "steps": [{"name": "plan", "conclusion": "failure"}]}]}))
        done = self.ci_sbt("--quiet", "--", "worker/Test/test")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("NO VERDICT: the command step is no command step (run failure)\n", done.stderr)

    def test_a_run_past_the_deadline_is_no_verdict(self) -> None:
        self.answer(json.dumps({"status": "in_progress", "jobs": []}))
        done = self.ci_sbt("--quiet", "--", "worker/Test/test", KINOWO_CI_SBT_TIMEOUT="0")
        self.assertEqual(2, done.code, done.output)
        self.assertTrue(done.stderr.endswith("ci-sbt: run 101 did not complete within 0s\n"), done.stderr)

    def test_an_unreadable_view_is_read_again_until_the_deadline(self) -> None:
        self.answer("not json")
        done = self.ci_sbt("--quiet", "--", "worker/Test/test", KINOWO_CI_SBT_TIMEOUT="0")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("did not complete within 0s", done.stderr)

    def test_a_dirty_checkout_runs_a_snapshot_commit_on_head(self) -> None:
        self.write("repo/worker/src/main/A.scala", "new\n")
        self.write("repo/worker/src/test/ASpec.scala", "spec\n")
        self.write("repo/.env", "SECRET=1\n")
        done = self.ci_sbt("--log", self.path("out.log"), "--", "worker/Test/test")
        self.assertEqual(0, done.code, done.output)
        snapshot = self.dispatched()[0][1]
        self.assertNotEqual(self.head, snapshot)
        self.assertEqual(self.head, self.git(self.checkout, "rev-parse", f"{snapshot}^"))
        self.assertEqual("new", self.git(self.checkout, "show", f"{snapshot}:worker/src/main/A.scala"))
        self.assertEqual("spec", self.git(self.checkout, "show", f"{snapshot}:worker/src/test/ASpec.scala"))
        self.assertEqual("worker/src/main/A.scala\nworker/src/test/ASpec.scala",
                         self.git(self.checkout, "ls-tree", "-r", "--name-only", snapshot), "never another file")
        self.assertEqual(f"ci-sbt: the working tree of {self.head[:7]}",
                         self.git(self.checkout, "log", "-1", "--format=%s", snapshot))
        self.assertIn("ci-sbt: untracked files outside */src/ are not run: .env\n", done.stderr)
        self.assertEqual("", self.git(self.checkout, "diff", "--cached", "--name-only"), "nothing staged")
        self.assertEqual("", done.stdout, "--log takes the sbt output")
        self.assertEqual("[info] sbt said this\n", self.read("out.log"))

    def test_a_change_in_the_tick_the_index_was_written_is_still_snapshotted(self) -> None:
        # Tool self-tests #533: setUp's commit and the rewrite fell in one second (macOS git compares whole seconds),
        # same size, so only git's racy-entry check -- the entry's mtime against the INDEX FILE's -- sees the change.
        # Pinned here: the file and the index share one mtime and ctime is not compared.
        source = self.path("repo/worker/src/main/A.scala")
        written = os.stat(source)
        self.git(self.checkout, "config", "core.trustctime", "false")
        self.write("repo/worker/src/main/A.scala", "new\n")
        for path in (source, os.path.join(self.checkout, ".git", "index")):
            os.utime(path, ns=(written.st_atime_ns, written.st_mtime_ns))
        done = self.ci_sbt("--quiet", "--", "worker/Test/test")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("new", self.git(self.checkout, "show", f"{self.dispatched()[0][1]}:worker/src/main/A.scala"))

    def test_commit_runs_that_commit(self) -> None:
        self.write("repo/worker/src/main/A.scala", "new\n")
        done = self.ci_sbt("--commit", "HEAD", "--quiet", "--", "worker/Test/test")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(self.head, self.dispatched()[0][1])

    def test_an_unknown_commit_is_no_verdict(self) -> None:
        done = self.ci_sbt("--commit", "nope", "--", "worker/Test/test")
        self.assertEqual((2, "ci-sbt: no commit nope\n"), (done.code, done.stderr))
        self.assertEqual([], self.dispatched())

    def test_a_failed_dispatch_is_no_verdict(self) -> None:
        done = self.ci_sbt("--", "worker/Test/test", DISPATCH_FAILS="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual("identity-ci-run: pushed\n"
                         f"ci-sbt: could not dispatch `worker/Test/test` at {self.head[:9]} (identity-ci-run)\n",
                         done.stderr)

    def test_a_dispatch_without_a_run_id_is_no_verdict(self) -> None:
        done = self.ci_sbt("--", "worker/Test/test", RUN="queued")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("ci-sbt: identity-ci-run gave no run id (got: queued)\n", done.stderr)

    def test_a_log_that_cannot_be_downloaded_is_said_and_the_verdict_kept(self) -> None:
        done = self.ci_sbt("--", "worker/Test/test", DOWNLOAD_FAILS="1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("", done.stdout)
        self.assertIn("ci-sbt: could not download run 101's sbt log\n", done.stderr)

    def test_usage_errors_exit_2(self) -> None:
        for args, why in ((("worker/Test/test",), "unknown argument: worker/Test/test (the sbt command goes after --)"),
                          ((), "no command: put the sbt command after --"),
                          (("--",), "no command: put the sbt command after --"),
                          (("--log",), "--log needs a value"),
                          (("--commit",), "--commit needs a value")):
            done = self.ci_sbt(*args)
            self.assertEqual((2, f"ci-sbt: {why}\nci-sbt: see ci-sbt --help\n"), (done.code, done.stderr), args)
        self.assertEqual([], self.dispatched())

    def test_help_prints_the_header(self) -> None:
        done = self.ci_sbt("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("ONE sbt COMMAND ON ci-runner's MEASURE LANE", done.stdout)
        self.assertIn("\n  scripts/ci-sbt [--commit <rev> | --tree] [--log <file>] [--quiet] -- '<sbt command>'\n",
                      done.stdout)


if __name__ == "__main__":
    unittest.main()
