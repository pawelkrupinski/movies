#!/usr/bin/env python3
"""scripts/ci/wait-for-run-artifact.sh, black-box, against a fake gh whose answers a scratch directory scripts call by
call: the artifacts listing answers with the names in artifacts-<n> (n = how many artifact listings came before, the
last file repeating), the jobs listing with `jobs` -- the `<status> <conclusion>` lines the tool's filter makes of the
jobs matching the producer -- and fails once when `jobs-fail` exists, as a GitHub API 5xx does. POLL_SECONDS=0: no
case waits. Ported from wait-for-run-artifact-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/wait_for_run_artifact.py", "scripts/ci/wait-for-run-artifact.sh")
GH = r"""
dir="$STUB"; [ -n "${STUB_DELAY:-}" ] && sleep "$STUB_DELAY"  # a slow runner's gh
case "$2" in
  *artifacts*)
    n=$(cat "$dir/calls" 2>/dev/null || echo 0); echo $((n + 1)) > "$dir/calls"
    f="$dir/artifacts-$n"; [ -f "$f" ] || f=$(ls "$dir"/artifacts-* | sort -t- -k2 -n | tail -1)
    cat "$f" ;;
  *jobs*)
    if [ -f "$dir/jobs-fail" ]; then rm -f "$dir/jobs-fail"; echo "HTTP 502" >&2; exit 1; fi
    echo "$PRODUCER" > "$dir/producer"
    cat "$dir/jobs" ;;
esac
"""


class WaitForRunArtifactTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.stub = self.path("stub")
        os.makedirs(self.stub)
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.jobs("")

    def artifacts(self, *listings: Sequence[str]) -> None:
        for n, names in enumerate(listings):
            self.write(f"stub/artifacts-{n}", "".join(f"{name}\n" for name in names))

    def jobs(self, *lines: str) -> None:
        self.write("stub/jobs", "".join(f"{line}\n" for line in lines))

    def wait(self, timeout: str, env: Optional[Dict[str, str]] = None) -> cli_harness.Done:
        return self.run_tool("stage-worker", "e2e (corpus)", timeout, env={
            "GITHUB_REPOSITORY": "owner/repo", "GITHUB_RUN_ID": "42", "POLL_SECONDS": "0", "STUB": self.stub,
            **(env or {})})

    def calls(self) -> int:
        return int(self.read("stub/calls"))

    def test_ready_at_once_when_the_artifact_is_already_listed(self) -> None:
        self.artifacts(["stage-worker"])
        done = self.wait("60")
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"^artifact stage-worker is ready after \d+s\n$")
        self.assertEqual([["api", "repos/owner/repo/actions/runs/42/artifacts?per_page=100", "--paginate", "--jq",
                           ".artifacts[].name"]], self.fakes.calls("gh"))

    def test_waits_through_listings_without_it_until_it_appears_asking_three_times(self) -> None:
        self.artifacts([], [], ["stage-web", "stage-worker"])
        self.assertEqual(0, self.wait("60").code)
        self.assertEqual(3, self.calls())

    def test_the_jobs_are_asked_of_this_run_filtered_by_the_fragment_from_the_environment(self) -> None:
        self.artifacts([], ["stage-worker"])
        self.assertEqual(0, self.wait("60").code)
        self.assertEqual(["api", "repos/owner/repo/actions/runs/42/jobs?per_page=100", "--paginate", "--jq",
                          '.jobs[] | select(.name | contains(env.PRODUCER)) | "\\(.status) \\(.conclusion)"'],
                         self.fakes.calls("gh")[1])
        self.assertEqual("e2e (corpus)\n", self.read("stub/producer"))

    def test_gives_up_the_moment_the_producer_finished_without_it(self) -> None:
        self.artifacts(["stage-web"])
        self.jobs("completed failure")
        done = self.wait("60")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("::error::'e2e (corpus)' finished (failure) without uploading stage-worker", done.stdout)

    def test_names_every_producing_jobs_conclusion(self) -> None:
        self.artifacts([])
        self.jobs("completed failure", "completed cancelled")
        self.assertIn("finished (failure,cancelled) without", self.wait("60").stdout)

    def test_takes_an_upload_that_landed_as_the_producer_finished(self) -> None:
        self.artifacts([], ["stage-worker"])
        self.jobs("completed success")
        self.assertEqual(0, self.wait("60").code)

    def test_keeps_waiting_while_another_job_the_fragment_names_still_runs(self) -> None:
        self.artifacts([], [], ["stage-worker"])
        self.jobs("completed failure", "in_progress ")
        self.assertEqual(0, self.wait("60").code)

    def test_times_out_while_the_producer_is_still_running(self) -> None:
        self.artifacts([])
        done = self.wait("0")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("::error::no artifact stage-worker after 0s of 'e2e (corpus)' running", done.stdout)

    def test_rides_out_a_failed_jobs_listing_and_keeps_polling(self) -> None:
        self.artifacts([], ["stage-worker"])
        self.write("stub/jobs-fail", "")
        done = self.wait("60")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("::warning::could not list this run's jobs; polling again", done.stdout)

    def test_waits_while_the_producer_is_still_queued_its_timeout_not_begun(self) -> None:
        self.artifacts([], [], ["stage-worker"])
        self.jobs("queued ")
        # the wait it says is the clock's, never the poll count's: each gh call a second, as on a loaded runner
        # (Tool self-tests #629, macOS: "after 1s" where the case expected "after 0s")
        done = self.wait("0", env={"STUB_DELAY": "1"})
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"'e2e \(corpus\)' is still queued for a runner after \d+s: waiting for it to start")

    def test_but_not_past_the_queued_bound_naming_it(self) -> None:
        self.artifacts([])
        self.jobs("queued ", "waiting ")
        done = self.wait("60", env={"WAIT_QUEUED_SECONDS": "0", "STUB_DELAY": "1"})
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.stdout, r"::error::'e2e \(corpus\)' still waited for a runner after \d+s, so stage-worker "
                         r"never came")

    def test_a_matrix_with_one_producing_job_running_counts_its_timeout(self) -> None:
        self.artifacts([])
        self.jobs("queued ", "in_progress ")
        self.assertEqual(1, self.wait("0").code)

    def test_finds_the_artifact_first_in_a_listing_larger_than_the_pipe_buffer(self) -> None:
        self.artifacts(["stage-worker"] + [f"other-artifact-{i:05d}" for i in range(1, 20001)])
        self.jobs("completed success")
        self.assertEqual(0, self.wait("60").code)

    def test_a_name_is_matched_whole(self) -> None:
        self.artifacts(["stage-worker-2", "x-stage-worker"], ["stage-worker"])
        self.assertEqual(0, self.wait("60").code)
        self.assertEqual(2, self.calls())

    def test_each_argument_is_required(self) -> None:
        for args in ((), ("stage-worker",), ("stage-worker", "e2e (corpus)")):
            with self.subTest(args=args):
                done = self.run_tool(*args, env={"GITHUB_REPOSITORY": "owner/repo", "GITHUB_RUN_ID": "42"})
                self.assertNotEqual(0, done.code, done.output)
                self.assertEqual([], self.fakes.calls("gh"))


if __name__ == "__main__":
    unittest.main()
