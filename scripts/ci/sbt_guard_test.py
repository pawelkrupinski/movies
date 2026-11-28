#!/usr/bin/env python3
"""scripts/ci/sbt-guard/sbt against a stand-in sbt, black-box: it passes arguments, output and status through, runs a
batch once more (after `staleTastyDrop`) only when it failed on a stale TASTy read that the build's own retry did not
take, and runs a command again after each backoff while it fails only on downloading a dependency from a remote
repository (coursier's negative records forgotten first). The stand-ins are `sbt` (each call logged; the script names,
per call number, what it prints and how it exits), `curl` (the Retry-After HEAD request, answered from a file) and a
sleep that logs how long the guard waited instead of waiting. Ported from sbt-guard-test.sh case for case; the same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/sbt_guard.py", "scripts/ci/sbt-guard/sbt")
GUARD_DIR = os.path.join(cli_harness.ROOT, "scripts", "ci", "sbt-guard")
POM = "https://repo1.maven.org/maven2/io/sentry/sentry-logback/8.65.0/sentry-logback-8.65.0.pom"
SBT = r'''
n=$(( $(cat "$FAKE_SBT_DIR/calls" 2>/dev/null || echo 0) + 1 ))
echo "$n" > "$FAKE_SBT_DIR/calls"
case "$(sed -n "${n}p" "$FAKE_SBT_DIR/script")" in
  ok)      echo "out $*"; echo "err $*" >&2; exit 0 ;;
  fail)    echo "[error] Compilation failed"; exit 1 ;;
  stale)   echo "[warn] Ignoring analyses of \$anonfun due to error in reading TASTy."; exit 1 ;;
  retried) echo "[warn] Ignoring analyses of x due to error in reading TASTy."
           echo "[warn] [stale TASTy] running the batch again: compile"; exit 1 ;;
  # lm-coursier's failures, as Main #2678 (a 404 from Central) and PR #275's run (a 429) printed them
  dl404)   echo "[error] sbt.librarymanagement.ResolveException: Error downloading io.sentry:sentry-logback:8.65.0"
           echo "[error]   Not found"
           echo "[error]   not found: /home/runner/.ivy2/localio.sentry/sentry-logback/8.65.0/ivys/ivy.xml"
           echo "[error]   not found: https://repo1.maven.org/maven2/io/sentry/sentry-logback/8.65.0/sentry-logback-8.65.0.pom"
           exit 1 ;;
  dl429)   echo "[error] sbt.librarymanagement.ResolveException: Error downloading io.sentry:sentry-logback:8.65.0"
           echo "[error]   download error: Caught java.io.IOException (Server returned HTTP response code: 429 for URL: https://repo1.maven.org/maven2/io/sentry/sentry-logback/8.65.0/sentry-logback-8.65.0.pom) while downloading"
           exit 1 ;;
  local)   echo "[error] sbt.librarymanagement.ResolveException: Error downloading com.example:missing:1.0"
           echo "[error]   not found: /home/runner/.ivy2/localcom.example/missing/1.0/ivys/ivy.xml"
           exit 1 ;;
  *)       exit 9 ;;
esac
'''


class SbtGuardTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("sbt", SBT, shell="/bin/bash")
        self.fakes.script("curl", 'cat "$FAKE_SBT_DIR/headers" 2>/dev/null; true', shell="/bin/bash")
        self.fakes.script("fake-sleep", "true")
        self.write("headers", "")
        self.coursier = self.path("coursier")

    def guard(self, *script: str, args: Optional[List[str]] = None, backoff: str = "0 0", retry_after_max: Optional[str] = None,
              path: Optional[str] = None) -> cli_harness.Done:
        self.fakes.clear()
        if os.path.exists(self.path("calls")):
            os.remove(self.path("calls"))
        self.write("script", "".join(line + "\n" for line in script))
        return self.run_tool(*(args or ["-batch", "set x := 1", "compile"]), env={
            "HOME": self.path("home"), "FAKE_SBT_DIR": self.work, "COURSIER_CACHE": self.coursier,
            "SBT_GUARD_DOWNLOAD_BACKOFF": backoff, "SBT_GUARD_SLEEP": os.path.join(self.fakes.bin, "fake-sleep"),
            "SBT_GUARD_RETRY_AFTER_MAX": retry_after_max,
            "PATH": path or f"{GUARD_DIR}{os.pathsep}{os.environ['PATH']}"})

    def sbt_args(self) -> List[str]:
        return [" ".join(c) for c in self.fakes.calls("sbt")]

    def slept(self) -> List[str]:
        return [c[0] for c in self.fakes.calls("fake-sleep")]

    def test_a_passing_run_is_one_call_with_its_arguments_and_streams_passed_through(self) -> None:
        done = self.guard("ok")
        self.assertEqual((0, ["-batch set x := 1 compile"]), (done.code, self.sbt_args()))
        self.assertEqual("out -batch set x := 1 compile\n", done.stdout)
        self.assertEqual("err -batch set x := 1 compile\n", done.stderr)

    def test_an_ordinary_failure_is_one_call_and_its_status(self) -> None:
        done = self.guard("fail")
        self.assertEqual((1, 1), (done.code, len(self.sbt_args())))

    def test_a_stale_read_the_build_did_not_retry_drops_then_runs_the_batch_again(self) -> None:
        done = self.guard("stale", "ok", "ok")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["-batch set x := 1 compile", "-batch staleTastyDrop", "-batch set x := 1 compile"],
                         self.sbt_args())

    def test_a_rerun_that_fails_again_keeps_its_failure(self) -> None:
        done = self.guard("stale", "ok", "fail")
        self.assertEqual((1, 3), (done.code, len(self.sbt_args())))

    def test_a_stale_read_the_build_already_retried_is_left_failed(self) -> None:
        done = self.guard("retried")
        self.assertEqual((1, 1), (done.code, len(self.sbt_args())))

    def stale_negative_record(self) -> None:
        self.write("coursier/v1/x/.a-1.0.jar.error", "")
        self.write("coursier/v1/x/a-1.0.pom", "")

    def test_a_dependency_centrals_404_runs_again_forgetting_coursiers_negative_records_first(self) -> None:
        self.stale_negative_record()
        done = self.guard("dl404", "ok")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["-batch set x := 1 compile"] * 2, self.sbt_args())
        self.assertEqual(["a-1.0.pom"], sorted(os.listdir(self.path("coursier/v1/x"))))
        self.assertEqual(1, done.stderr.count("sbt-guard: the run failed downloading a dependency"))

    def test_a_429_then_a_404_is_retried_through_both_backoffs(self) -> None:
        done = self.guard("dl429", "dl404", "ok")
        self.assertEqual((0, 3), (done.code, len(self.sbt_args())))

    def test_failing_on_every_attempt_is_one_more_run_than_the_backoffs_then_its_own_status(self) -> None:
        done = self.guard("dl404", "dl429", "dl404")
        self.assertEqual((1, 3), (done.code, len(self.sbt_args())))

    def test_a_dependency_no_remote_repository_was_asked_for_is_not_retried(self) -> None:
        done = self.guard("local")
        self.assertEqual((1, 1), (done.code, len(self.sbt_args())))

    def test_a_429_with_retry_after_45_waits_45_over_the_30s_step_then_the_60s_step_as_is(self) -> None:
        self.write("headers", "HTTP/2 429\r\nRetry-After: 45\r\n\r\n")
        done = self.guard("dl429", "dl429", "ok", backoff="30 60")
        self.assertEqual((0, 3, ["45", "60"]), (done.code, len(self.sbt_args()), self.slept()))
        self.assertEqual([POM], sorted({c[-1] for c in self.fakes.calls("curl")}))
        self.assertEqual(2, done.stderr.count("sbt-guard: repo1.maven.org rate-limited the download (HTTP 429, Retry-After: 45"))

    def test_a_retry_after_past_the_bound_waits_the_bound_not_an_hour(self) -> None:
        self.write("headers", "HTTP/2 429\r\nretry-after: 3600\r\n\r\n")
        self.guard("dl429", "ok", backoff="30", retry_after_max="300")
        self.assertEqual(["300"], self.slept())

    def test_a_429_with_no_retry_after_waits_the_backoff_step(self) -> None:
        self.guard("dl429", "ok", backoff="30")
        self.assertEqual(["30"], self.slept())

    def test_a_retry_after_shorter_than_the_step_waits_the_step(self) -> None:
        self.write("headers", "HTTP/2 429\r\nRetry-After: 5\r\n\r\n")
        self.guard("dl429", "ok", backoff="30")
        self.assertEqual(["30"], self.slept())

    def test_a_404_waits_the_step_and_asks_no_retry_after(self) -> None:
        self.guard("dl404", "ok", backoff="30")
        self.assertEqual((["30"], []), (self.slept(), self.fakes.calls("curl")))

    def test_a_retry_that_fails_otherwise_keeps_that_failure_with_no_further_retry(self) -> None:
        done = self.guard("dl404", "fail")
        self.assertEqual((1, 2), (done.code, len(self.sbt_args())))

    def test_an_empty_backoff_means_no_retry(self) -> None:
        done = self.guard("dl404", "ok", backoff="")
        self.assertEqual((1, 1), (done.code, len(self.sbt_args())))

    def test_a_client_run_drops_through_its_server_then_runs_again(self) -> None:
        self.guard("stale", "ok", "ok", args=["--client", "worker/testOnly X"])
        self.assertEqual(["--client worker/testOnly X", "--client staleTastyDrop", "--client worker/testOnly X"],
                         self.sbt_args())

    def test_no_real_sbt_behind_it_is_127(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "sbt"))
        done = self.guard("ok", path=f"{GUARD_DIR}{os.pathsep}/usr/bin{os.pathsep}/bin")
        self.assertEqual(127, done.code, done.output)


if __name__ == "__main__":
    unittest.main()
