#!/usr/bin/env python3
# script-test-reads: scripts/identity-slice scripts/identity_slice*.py scripts/identity-slice-tool-files.txt scripts/identity-tools.sh scripts/identity-explain scripts/tool-conventions.sh scripts/jvm-gate.sh scripts/lib/*.py scripts/ci/population_walk_diff.py
"""scripts/identity-slice's other modes, black-box: --faithfulness RUN reads the leg's artifact and log through a
bounded gh (a hung gh fails the call, a throttled read is asked again, a download cut short leaves nothing behind) and
--faithfulness capture slices the capture's agreement takes; --prod-mirror reads prod's state from the LOCAL mirror only
once its freshness is checked against the recording; --explain replays the capture it is handed (scripts/identity-explain
its alias); --emit-drift-slice tells a replay's verdict from a crash and files the population rows its arrival walk
finds; --drift replays the whole recording. Ported from identity-slice-test.sh; unchanged across the swap to Python."""
from __future__ import annotations

import glob
import os
import signal
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_slice_cli_fixture import BACKLOG, SliceCase, count  # noqa: E402
from harness import cli_harness  # noqa: E402  (scripts/lib, on the path the fixture put it on)

KEY = ("de", "--recording", "777", "--key", "Kino|Lalka")
# gh whose artifact read is throttled once (HTTP 502), then answers with the leg's takes
THROTTLED_GH = '''#!/usr/bin/env bash
[ "$1 $2" = "run download" ] || exit 1
echo download >> "$STUB/gh-downloads"
[ "$(wc -l < "$STUB/gh-downloads")" -gt 1 ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
mkdir -p "$dir/de-takes" && echo takes > "$dir/de-takes/de.json.gz"
'''
# gh that has written part of the download when it is cut short
SLOW_GH = '''#!/usr/bin/env bash
while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
mkdir -p "$dir/de-takes" && echo partial > "$dir/de-takes/de.json.gz"
touch "$STUB/gh-started"; sleep 20
'''
WALK = ("# drift-slices/de/kino-lalka-1: 1 flip(s) in 1.0s\\ndrift-slices/de/kino-lalka-1\\tKino\\tLalka\\t"
        "film-film-film: model tmdb:1@1 -> tmdb:2@2 -> tmdb:1@3 [chain 3]\\n")


class FaithfulnessTest(SliceCase):
    def gh(self, text: str) -> None:
        os.chmod(self.write("bin/gh", text), 0o755)

    def test_a_gh_that_hangs_fails_the_call_cut_off_by_its_bound(self) -> None:
        hung = self.write("gh-hung", "")
        self.gh(f'#!/usr/bin/env bash\necho $$ >> "{hung}"\nexec sleep 86400\n')
        done = self.slice("de", "--faithfulness", "123456789",
                          env={"GH_DOWNLOAD_ATTEMPTS": "1", "KINOWO_IDENTITY_DOWNLOAD_TIMEOUT": "1"})
        self.assertEqual((1, 1), (done.code, self.err.count("could not download run 123456789")), done.output)
        self.assertEqual((1, 0), (self.err.count("failed after 1 attempt(s) (exit 124)"), cli_harness.still_running(hung)))

    def test_a_throttled_artifact_read_is_asked_again_and_its_takes_kept(self) -> None:
        self.gh(THROTTLED_GH)
        self.slice("de", "--faithfulness", "223456789", env={"GH_DOWNLOAD_BACKOFF": "0"})
        takes = os.path.join(self.cache, "diagnostics/223456789/de-takes/de.json.gz")
        self.assertEqual((2, True), (len(self.read("gh-downloads").splitlines()), os.path.getsize(takes) > 0), self.err)

    def test_a_download_cut_short_leaves_no_download_directory_and_its_lock_free(self) -> None:
        self.gh(SLOW_GH)
        tool = self.start("de", "--faithfulness", "323456789")
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("gh-started"))))
        os.killpg(tool.pid, signal.SIGTERM)  # a Ctrl-C: TERM to the group
        tool.wait()
        diagnostics = os.path.join(self.cache, "diagnostics")
        locks = glob.glob(os.path.join(self.cache, "locks", "diagnostics-323456789-*.flock"))

        def downloads() -> int:
            return len(glob.glob(os.path.join(diagnostics, "323456789.dl.*")))

        def held() -> int:
            return sum(1 for lock in locks if self.lock_state(lock) == "held")
        # the download's subshell cleans up once bounded has ended gh's group, after the tool itself has gone
        self.assertTrue(cli_harness.wait_until(lambda: downloads() == 0 and held() == 0), (downloads(), held()))
        self.assertEqual(1, len(locks))

    def test_capture_slices_its_agreement_takes_on_the_recording_it_names(self) -> None:
        families = self.write("capture/de.json.gz", "families\n")
        self.write("capture/de.inputs", "recording\t777\n")
        done = self.slice("de", "--faithfulness", "capture", "--families", families, "--sample", "7")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.calls().count(f"--faithfulness {families} --sample 7 --seed 1"), self.calls())
        self.assertIn("faithfulness of the agreement takes", self.err)


class ProdMirrorTest(SliceCase):
    def test_the_mirrors_freshness_is_checked_against_the_recordings_moment_then_the_local_mirror_read(self) -> None:
        self.slice(*KEY, "--prod-mirror")
        calls = self.calls()
        self.assertEqual(1, count("^freshness de --quiet .*--not-before 2026-10-14T05:00:00Z", calls), calls)
        self.assertEqual(1, calls.count("--prod-mirror mongodb://127.0.0.1:28017/"), calls)
        self.assertEqual(0, calls.count("--results"), "no result of prod's moment is kept for another call")

    def test_the_deployed_worker_build_whose_code_wrote_the_mirrors_state_is_named(self) -> None:
        self.slice(*KEY, "--prod-mirror", env={"KINOWO_DEPLOYED_WORKER": "0123abcd"})
        self.assertEqual((1, 1), (self.calls().count("--deployed 0123abcd"),
                                  self.err.count("written by the deployed worker 0123abcd")), self.err)

    def test_the_mirror_reader_is_dropped_into_a_refs_tree(self) -> None:
        self.slice(*KEY, "--ref", "HEAD", "--prod-mirror")
        self.assertIn("ProdMirror", self.content(self.ref_tree(self.head, "worker/src/fixtures/scala/tools/ProdMirror.scala")))

    def test_a_stale_mirror_is_warned_loudly_and_read(self) -> None:
        self.slice(*KEY, "--prod-mirror", "de", env={"FRESHNESS_EXIT": "3"})
        self.assertEqual((1, 1), (self.err.count("THE PROD MIRROR IS NOT FRESH"), self.calls().count("--prod-mirror")))

    def test_no_mirror_at_all_refuses_never_falling_back_to_prod(self) -> None:
        done = self.slice(*KEY, "--prod-mirror", env={"FRESHNESS_EXIT": "2"})
        self.assertEqual((2, 0), (done.code, count("^java", self.calls())), done.output)

    def test_another_countrys_mirror_is_refused(self) -> None:
        self.assertEqual(2, self.slice(*KEY, "--prod-mirror", "uk").code)

    def test_a_slice_without_it_reads_no_mirror(self) -> None:
        self.slice(*KEY)
        self.assertEqual(0, count("freshness|--prod-mirror", self.calls()))


class ExplainTest(SliceCase):
    def setUp(self) -> None:
        super().setUp()
        self.write("capture/de.json.gz", "c\n")
        self.write("capture/identity-judgements.tsv", "country\n")
        self.trace = (f"scripts.IdentityExplain de --fixture {self.work}/capture/de.json.gz --labels "
                      f"{self.work}/capture/identity-judgements.tsv --venue Kino")

    def test_explain_replays_the_capture_it_is_handed_under_the_refs_code(self) -> None:
        self.slice("de", "--explain", "--fixture", self.path("capture"), "--venue", "Kino", "--title", "Lalka",
                   "--ref", self.head)
        calls = self.calls()
        self.assertEqual(1, calls.count(self.trace + " --title Lalka"), calls)
        explainer = "worker/src/test/scala/scripts/IdentityExplain.scala"
        self.assertEqual((1, f"// {explainer}"), (calls.count(f"java {self.cache}/refs/{self.head} "),
                                                   self.content(self.ref_tree(self.head, explainer))))
        self.assertEqual(0, calls.count("--recording"), "no recording is fetched for it")

    def test_identity_explain_is_identity_slice_explain(self) -> None:
        self.slice("de", "--fixture", self.path("capture"), "--venue", "Kino",
                   tool=os.path.join(self.repo_dir, "scripts/identity-explain"))
        self.assertEqual(1, self.calls().count(self.trace), self.err)

    def test_two_refs_print_their_outcomes_side_by_side_then_the_traces_diff(self) -> None:
        done = self.slice("de", "--explain", "--fixture", self.path("capture"), "--ref", "HEAD~1", "--ref", "HEAD",
                          env={"COLUMNS": "40"})
        self.assertEqual((0, f"{'HEAD~1':<18} | HEAD\n\nthe two refs' traces are identical\n"),
                         (done.code, done.stdout), done.output)


class DriftSliceTest(SliceCase):
    """--emit-drift-slice: a replay's verdict is told apart from a crash (an OOM once printed REPLAYS RED)."""

    def drift(self, kind: str, walk: str = "") -> cli_harness.Done:
        env = {"STUB_EMIT_SLICE": self.path("slice-dir"), "STUB_DRIFT": kind, **({"STUB_WALK": walk} if walk else {})}
        return self.slice(*KEY, "--emit-drift-slice", env=env)

    def population_rows(self, prefix: str = "") -> int:
        return count("^population\tde\t" + prefix, self.read("repo/" + BACKLOG))

    def test_a_replay_ending_red_is_replays_red_exit_0(self) -> None:
        self.assertEqual((0, 1), (self.drift("red").code, self.err.count("REPLAYS RED")), self.err)

    def test_a_replay_ending_clean_replays_clean(self) -> None:
        self.assertEqual((0, 1), (self.drift("clean").code, self.err.count("replays clean")), self.err)

    def test_the_arrival_walk_files_each_flip_as_a_population_row_once(self) -> None:
        # Main #2515: the slice lands with every population row its arrival walk finds, filed in the same step
        done = self.drift("clean", WALK)
        self.assertEqual((0, 1, 1), (done.code, self.err.count("population row(s) filed"),
                                     count("^population\tde\t", done.stdout)), done.output)
        self.assertEqual(0, self.drift("clean", WALK).code)
        self.assertEqual(1, self.population_rows("\tdrift-slices/de/kino-lalka-1\tKino\tLalka\t"))
        # ...a clean walk files none, said
        self.assertEqual((0, 1), (self.drift("clean").code, self.err.count("finds no flip no population row names")))
        self.assertEqual(2, count(".", self.read("repo/" + BACKLOG)))

    def test_a_walk_that_fails_files_nothing_exit_6(self) -> None:
        done = self.drift("clean", "# drift-slices/de/kino-lalka-1: FAILED boom\\n")
        self.assertEqual((6, 1, 0), (done.code, self.err.count("population walk failed"), self.population_rows()),
                         done.output)

    def test_a_replay_without_a_verdict_is_no_verdict_exit_5_never_red_or_clean(self) -> None:
        for kind, cause in (("oom", "the replay ran out of memory"), ("exit1", "the replay exited 1 with no verdict line"),
                            ("killed", "the replay was killed or timed out")):
            done = self.drift(kind)
            self.assertEqual((5, 1, 0, 0), (done.code, self.err.count("NO VERDICT: " + cause), self.err.count("REPLAYS RED"),
                                            self.err.count("replays clean")), kind + done.output)

    def test_drift_replays_the_whole_recording_through_the_incremental_sequence(self) -> None:
        done = self.slice("de", "--recording", "777", "--drift")
        self.assertEqual(0, done.code, done.output)
        allow = f"--allow {self.repo_dir}/test/resources/fixtures/identity-drift-allow.tsv --out"
        self.assertEqual(1, count("scripts.IdentityDrift de --recording .* " + allow, self.calls()), self.calls())


if __name__ == "__main__":
    unittest.main()
