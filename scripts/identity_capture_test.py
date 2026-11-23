#!/usr/bin/env python3
"""scripts/identity-capture.sh, black-box: its vault and tunnel handling, the classpath it hands the capture JVM and
the arguments it passes on. No vault, no ssh, no sbt, no JVM, no Mongo: `secrets`, `ssh`, `nc`, `mongosh`, `sbt`,
`java` and `gh` are fakes, and `java` notes the environment the capture JVMs would inherit. Ported from
identity-capture-test.sh case for case, with the dry run, the arguments, a route the caller holds, the compile's
failures and a tunnel that never answers besides; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_capture.py", "scripts/identity-capture.sh")
FAKE_URI = "mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin"
TUNNEL_URI = "mongodb://u:p@127.0.0.1:27017/kinowo?authSource=admin&directConnection=true"
# `secrets run movies SPEC... -- CMD`: CMD with each SPEC's variable set to a fake in-cluster URI, never a real secret
SECRETS = f'''shift 2
while [ "$1" != "--" ]; do export "${{1%%=*}}={FAKE_URI}"; shift; done
shift
exec "$@"'''
JAVA = ('echo "MONGODB_URI=${MONGODB_URI:-<unset>} FAMILY=${KINOWO_IDENTITY_FAMILY_URI:-<unset>} '
        'NEWEST=${KINOWO_IDENTITY_NEWEST_RECORDING:-<unset>} TMDB=${KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY:-<unset>}" '
        '>> "$JAVA_ENV"')
# gh lists the runs out of creation order, as GitHub does
GH = "printf '41\\t2026-10-01T00:00:00Z\\n43\\t2026-10-03T00:00:00Z\\n42\\t2026-10-02T00:00:00Z\\n'"
UNSET = ("MONGODB_URI", "KINOWO_IDENTITY_FAMILY_URI", "KINOWO_IDENTITY_FAMILY_SEED", "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY",
         "KINOWO_PROD_MONGODB_URI", "KINOWO_MONGO_SSH")


class IdentityCaptureTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        classes = self.path("worker", "target", "scala-3", "test-classes")
        self.write("worker/target/scala-3/test-classes/scripts/IdentityCapture.class", "")
        self.fakes.script("secrets", SECRETS, shell="/usr/bin/env bash")
        self.fakes.script("ssh", 'touch "$TUNNEL_UP"; exec sleep 30')
        self.fakes.script("nc", '[ -e "$TUNNEL_UP" ]')
        self.fakes.script("mongosh", '[ -e "$TUNNEL_UP" ]')
        self.fakes.script("sbt", f'echo "{classes}"')
        self.fakes.script("java", JAVA)
        self.fakes.script("gh", GH)

    def capture(self, *args: str, env: Optional[Dict[str, str]] = None) -> cli_harness.Done:
        base: Dict[str, Optional[str]] = {name: None for name in UNSET}
        base.update({"PROD_TUNNEL_PORT": "27017", "TUNNEL_UP": self.path("tunnel-up"), "JAVA_ENV": self.path("java-env"),
                     "KINOWO_IDENTITY_CAPTURE_WORK": self.path("work"), "KINOWO_IDENTITY_CACHE": self.path("cache")})
        base.update(env or {})
        return self.run_tool(*args, env=base)

    def java_env(self) -> List[Dict[str, str]]:
        """Each capture JVM's environment: MONGODB_URI, FAMILY, NEWEST, TMDB."""
        if not os.path.exists(self.path("java-env")):
            return []
        return [dict(word.split("=", 1) for word in line.split(" ")) for line in self.read("java-env").splitlines()]

    def java_args(self) -> List[List[str]]:
        return [call[call.index("scripts.IdentityCapture") + 1:] for call in self.fakes.calls("java")]

    def test_a_capture_reading_prod_reaches_it_only_through_the_tunnel(self) -> None:
        done = self.capture("pl")
        self.assertEqual(0, done.code, done.output)
        [java] = self.java_env()
        # the capture JVMs inherit NO MONGODB_URI, so each seeds its local throwaway database, never prod
        self.assertEqual("<unset>", java["MONGODB_URI"])
        self.assertEqual(TUNNEL_URI, java["FAMILY"])
        # the newest recording (by creation time), found by the shared helper
        self.assertEqual("43", java["NEWEST"])
        self.assertEqual(FAKE_URI, java["TMDB"])
        # both secrets in ONE `secrets run`, under the names the capture reads them by
        self.assertEqual([["run", "movies", "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=TMDB_API_KEY",
                           "KINOWO_PROD_MONGODB_URI=MONGODB_URI"]],
                         [call[:call.index("--")] for call in self.fakes.calls("secrets")])

    def test_no_arguments_every_country_exits_clean_and_reaches_the_jvm(self) -> None:
        # the vault re-exec hands on an EMPTY argument list, which bash 3.2's `set -u` refused to expand
        done = self.capture()
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("unbound variable", done.output)
        self.assertEqual(1, len(self.java_env()))
        self.assertEqual([[]], self.java_args())

    def test_the_arguments_are_passed_on_as_given(self) -> None:
        done = self.capture("--parallel", "1", "--heap", "11g", "PL", "--capture")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["--parallel", "1", "--heap", "11g", "PL", "--capture"]], self.java_args())

    def test_a_route_the_caller_holds_reads_no_prod_and_asks_the_vault_only_for_the_key(self) -> None:
        done = self.capture("de", env={"KINOWO_IDENTITY_FAMILY_URI": "mongodb://127.0.0.1:27018/"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["run", "movies", "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=TMDB_API_KEY"]],
                         [call[:call.index("--")] for call in self.fakes.calls("secrets")])
        self.assertEqual([], self.fakes.calls("ssh"))
        self.assertEqual("mongodb://127.0.0.1:27018/", self.java_env()[0]["FAMILY"])

    def test_every_secret_already_set_asks_no_vault(self) -> None:
        done = self.capture("pl", "--fill", env={"KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY": "k"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.fakes.calls("secrets"))
        self.assertEqual([], self.fakes.calls("ssh"))   # a fill reads no family answers from prod
        self.assertEqual(("<unset>", "k"), (self.java_env()[0]["FAMILY"], self.java_env()[0]["TMDB"]))

    def test_a_dry_run_touches_neither_the_vault_nor_prod_nor_github(self) -> None:
        done = self.capture("--dry-run", "uk")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(([], [], []), (self.fakes.calls("secrets"), self.fakes.calls("ssh"), self.fakes.calls("gh")))
        self.assertEqual([["--dry-run", "uk"]], self.java_args())
        self.assertEqual("<unset>", self.java_env()[0]["NEWEST"])

    def test_the_capture_jvms_status_is_the_tools(self) -> None:
        self.fakes.script("java", JAVA + "\nexit 3")
        self.assertEqual(3, self.capture("pl", env={"KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY": "k",
                                                    "KINOWO_IDENTITY_FAMILY_URI": "mongodb://x/"}).code)

    def test_a_compile_that_fails_says_so_with_sbts_log(self) -> None:
        self.fakes.script("sbt", "echo '[error] it broke'; exit 1")
        done = self.capture("pl", "--dry-run")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("[error] it broke", done.stderr)
        self.assertIn("sbt failed to compile the integration classpath", done.stderr)
        self.assertEqual([], self.fakes.calls("java"))

    def test_sbt_printing_no_classpath_with_the_capture_fails_naming_it(self) -> None:
        self.fakes.script("sbt", "echo '[success] Total time: 3 s'")
        done = self.capture("pl", "--dry-run")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("no classpath holding scripts.IdentityCapture", done.stderr)
        self.assertEqual([], self.fakes.calls("java"))

    def test_a_tunnel_that_never_answers_fails_before_the_capture(self) -> None:
        self.fakes.script("ssh", "exit 255")
        done = self.capture("pl")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("prod Mongo unreachable", done.stderr)
        self.assertEqual([], self.fakes.calls("java"))

    def test_help_prints_the_header(self) -> None:
        done = self.capture("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("--parallel", done.stdout)
        self.assertIn("CAPTURE OR FILL", done.stdout)
        self.assertEqual([], self.fakes.calls())


if __name__ == "__main__":
    unittest.main()
