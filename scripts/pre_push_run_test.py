#!/usr/bin/env python3
"""THE PRE-PUSH HOOK'S RUN, black-box (scripts/hooks/pre-push over a range or git's stdin): what a push of each shape
checks (PREPUSH_LIST=1 prints it after every range rule: Fixes-Red: brings driftguards, Pure-Move: brings puremove and
drops the identity area, --fast-only / --verify-only narrow it, PREPUSH_ONLY / PREPUSH_ELSEWHERE split it), the red
gate and the stranded-trailer refusal, PREPUSH_SKIP per check, how a check's run reads (`▸ pre-push <check> ok (Ns)`,
`! pre-push <check> FAILED (Ns)`, `blocked by:`, `all clear`, `pre-push STALLED:` and exit 75 -- the lines agentctl
land parses), the known-red baseline, the reuse, a missing tool, git's stdin (measure/ and tag pushes, agentctl's
prepush-check) and how it calls scripts/run-script-tests, sbt and scripts/sbt-warm (the basic and identity sets of
local-tests.txt). Hermetic (pre_push_fixture.py); passes unchanged on the shell hook and on the Python one."""
from __future__ import annotations

import os
import re
import shutil
import sys
import unittest
from typing import List, Tuple

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
from pre_push_fixture import GREEN, RED, PushCase, lines, plain  # noqa: E402
from cli_harness import Done  # noqa: E402

# every check a path selects, with a path selecting it (the plan test holds the selection itself)
SELECTING = {
    "judgements": "test/resources/fixtures/identity-judgements.tsv", "allowlists": ".github/allow-lists.txt",
    "newchecks": ".github/gating-checks.tsv", "schedules": "infra/nix/files/scheduled-workflows.json",
    "eslint": "package.json", "jsquality": "infra/version-dashboard/src/a.ts", "pyquality": "pyrightconfig.json",
    "drift": "android/app/src/main/java/pl/kinowo/model/City.kt", "snapshot": "test/resources/fixtures/08-06-2026/a",
    "dashboards": "infra/test/test_dashboards.py", "metricalerts": "test/resources/metrics-without-alerts.tsv",
    "scripttests": "data/a.tsv", "actionlint": ".github/workflows/x.yml", "shellcheck": "scripts/x.sh",
    "sbt": "Dockerfile", "flipbacklog": "test/resources/fixtures/drift-gate-seeds.tsv",
    "stampcarry": "test/resources/identity-stamp-carry.tsv", "identity": "identity/src/main/scala/A.scala",
    "driftguards": "worker/src/main/scala/A.scala", "ios": "ios/A.swift"}
SECONDS = r"\(\d+s\)"
# what the hook needs on a PATH that lacks one tool (the missing-tool case): every other one it runs, linked in
BASE_TOOLS = ("git bash sh env python3 sed awk grep cat tr cut sort head tail wc mktemp dirname rm mkdir mv date find "
              "ps comm uname sleep kill ls tee readlink touch basename jq perl printf pwd")
FAKE_SBT_WARM = '#!/usr/bin/env bash\nprintf "%s\\n" "$@" >> "$STUB_WARM_ARGS"\n' \
    'case "$1" in identityImpact*|identityWalkDigest*) echo "sbt identityImpact: no"; exit 1 ;; esac\n' \
    'echo "Tests: succeeded 3"\n'


class ListTest(PushCase):
    """PREPUSH_LIST=1: the checks a range runs, after every rule the range brings."""

    def test_a_range_runs_what_its_paths_select(self):
        head = self.change("docs and ios", {"docs/a.md": "x", "ios/A.swift": "x"})
        self.assertEqual(["ios"], self.listed("--range", f"{self.base}..{head}"))

    def test_nothing_selected_nothing_listed(self):
        head = self.change("docs", {"docs/a.md": "x"})
        done = self.hook("--range", f"{self.base}..{head}")
        self.assertEqual((0, ""), (done.code, done.stdout), done.output)

    def test_a_fixes_red_trailer_brings_the_drift_guards(self):
        head = self.change("a fix\n\nFixes-Red: 123", {"docs/a.md": "x"})
        self.assertEqual(["driftguards"], self.listed("--checks-only", f"{self.base}..{head}"))

    def test_a_pure_move_claim_drops_the_identity_area(self):
        head = self.change("a move\n\nPure-Move: verified", {"identity/src/main/scala/A.scala": "x"})
        done = self.hook("--checks-only", f"{self.base}..{head}", env={"PREPUSH_LIST": "1"})
        self.assertEqual(["driftguards", "sbt", "puremove"], lines(done.stdout), done.output)
        self.assertIn("identity: left out -- only commits saying 'Pure-Move: verified' select it", done.stderr)

    def test_an_unclaimed_commit_keeps_the_area(self):
        self.change("a move\n\nPure-Move: verified", {"identity/src/main/scala/A.scala": "x"})
        head = self.change("an edit", {"identity/src/main/scala/B.scala": "x"})
        self.assertEqual(["driftguards", "sbt", "identity", "puremove"],
                         self.listed("--checks-only", f"{self.base}..{head}"))

    def test_fast_only_keeps_the_fast_checks(self):
        head = self.change("tools", {"scripts/x.sh": "#!/bin/sh\n", "ios/A.swift": "x"})
        done = self.hook("--fast-only", f"{self.base}..{head}", env={"PREPUSH_LIST": "1"})
        self.assertEqual(["shellcheck", "sbt"], lines(done.stdout), done.output)
        self.assertIn("--fast-only: the red gate is not judged (it is when the batch is pushed)", done.stderr)

    def test_verify_only_adds_the_unit_tests_and_the_readers(self):
        head = self.change("a resource", {"web/src/main/resources/a.conf": "x"})
        self.assertEqual(["sbt"], self.listed("--verify-only", f"{self.base}..{head}"))
        head = self.change("a read file", {"docs/read-by-a-test.md": "x"})
        self.assertEqual(["scripttests"], self.listed("--verify-only", f"{head}~1..{head}"))

    def test_the_landing_checks_split(self):
        head = self.change("tools", {"scripts/x.sh": "#!/bin/sh\n", "ios/A.swift": "x"})
        done = self.hook("--checks-only", f"{self.base}..{head}", env={"PREPUSH_LIST": "1", "PREPUSH_ONLY": "sbt ios"})
        self.assertEqual(["sbt", "ios"], lines(done.stdout), done.output)
        self.assertIn("left to the landing check's other job(s): scripttests shellcheck", done.stderr)
        done = self.hook("--checks-only", f"{self.base}..{head}", env={"PREPUSH_LIST": "1", "PREPUSH_ELSEWHERE": "sbt"})
        self.assertEqual(["scripttests", "shellcheck", "ios"], lines(done.stdout), done.output)


class GateTest(PushCase):
    """The red gate (--range is judged as a push to main) and the stranded-trailer refusal."""

    def push(self, message: str, path: str = "web/src/main/scala/A.scala", **env: str) -> Done:
        parent = self.git(self.checkout, "rev-parse", "HEAD")
        head = self.change(message, {path: message})
        return self.hook("--range", f"{parent}..{head}", env={"PREPUSH_LIST": "1", **env})

    def test_green_lets_a_production_commit_through(self):
        done = self.push("a change", GH_MAIN=f"[{GREEN}]", GH_IMC=f"[{GREEN}]")
        self.assertEqual((0, ["sbt"]), (done.code, lines(done.stdout)), done.output)

    def test_red_refuses_an_undeclared_production_commit(self):
        done = self.push("a change", GH_MAIN=f"[{RED}]")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("Main is RED on main: run 38520439037 (failure) https://x/38520439037", done.stderr)
        self.assertIn("carries no Fixes-Red: trailer naming a run of the red episode (red: 38520439037)", done.stderr)
        self.assertIn("main is red: only a fix for it lands.", done.stderr)

    def test_red_lets_a_declared_fix_through(self):
        done = self.push("a fix\n\nFixes-Red: 38520439037", GH_MAIN=f"[{RED}]")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("every pushed production commit is declared a fix for a red run", done.output)

    def test_red_lets_a_tools_only_commit_through(self):
        done = self.push("a tool", "docs/a.md", GH_MAIN=f"[{RED}]")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("is tools-only (touch no production path): lands while red", done.output)

    def test_an_unread_gate_refuses_production_and_passes_tools(self):
        done = self.push("a change", GH_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("red-build gate UNREAD (Main's verdict unread; Identity model convergence's verdict unread; "
                      "Identity takes's verdict unread): the push is REFUSED", done.stderr)
        self.assertIn("could not read Main's verdict from gh (HTTP 502: Bad Gateway) in 3 attempts", done.stderr)
        done = self.push("a tool", "docs/a.md", GH_FAILS="1")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("every pushed commit is tools-only, which lands while red", done.output)

    def test_the_gate_skipped_or_not_judged(self):
        done = self.push("a change", GH_MAIN=f"[{RED}]", PREPUSH_SKIP="redgate")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("redgate SKIPPED (PREPUSH_SKIP)", done.stderr)
        head = self.change("another", {"web/src/main/scala/B.scala": "x"})
        done = self.hook("--checks-only", f"{self.base}..{head}", env={"PREPUSH_LIST": "1", "GH_MAIN": f"[{RED}]"})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("--checks-only: the red gate is not judged (it is when these commits are pushed)", done.stderr)

    def test_a_stranded_trailer_refuses_every_push(self):
        done = self.push("a fix\n\nFixes-Red: 38520439037\n\nmore prose after it", "docs/a.md")
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.stderr, r"[0-9a-f]{9}: a Fixes-Red: line outside the message's closing trailer block")
        self.assertIn("Put it in the message's LAST paragraph", done.stderr)
        done = self.push("again\n\nPure-Move: verified\n\nprose", "docs/b.md", PREPUSH_SKIP="trailers")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("trailers SKIPPED (PREPUSH_SKIP)", done.stderr)


class RunTest(PushCase):
    """A check's run: its lines, the verdict and the exit status."""

    def range_of(self, path: str = "test/resources/fixtures/08-06-2026/a") -> str:
        head = self.change(f"change {path}", {path: "x"})
        return f"{self.base}..{head}"

    def test_a_passing_check(self):
        ran = self.path("snapshot.ran")
        done = self.hook("--range", self.range_of(), env={"STUB_SNAPSHOT_RAN": ran, "PREPUSH_SKIP": "redgate"})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("▸ pre-push snapshot — for 1 changed path(s)", plain(done.stdout))
        self.assertRegex(plain(done.stdout), r"▸ pre-push snapshot ok " + SECONDS)
        self.assertRegex(lines(plain(done.stdout))[-1], r"^▸ pre-push all clear " + SECONDS + "$")
        head = self.git(self.checkout, "rev-parse", "HEAD")
        self.assertEqual(f"check {head}\n", self.read("snapshot.ran"))

    def test_a_failing_check_blocks(self):
        done = self.hook("--range", self.range_of(), env={"STUB_SNAPSHOT_EXIT": "1", "PREPUSH_SKIP": "redgate"})
        self.assertEqual(1, done.code, done.output)
        # as agentctl's classify reads it: `^! pre-push\S* \S+ FAILED\b`, the colour codes in place
        self.assertRegex(done.stderr, r"(?m)^\x1b\[33m! pre-push\x1b\[0m snapshot FAILED " + SECONDS + "$")
        self.assertIn("FAIL: the snapshot inputs moved at c0ffee", done.stderr)
        self.assertRegex(done.stderr, r'blocked by: snapshot \(\d+s\)\. Fix, or bypass with PREPUSH_SKIP="snapshot" '
                                      r"/ --no-verify\.")
        self.assertNotIn("all clear", done.output)

    def test_every_check_skipped_by_name(self):
        files = {path: "#!/bin/sh\n" if path.endswith(".sh") else "x" for path in SELECTING.values()}
        head = self.change("everything\n\nPure-Move: verified", files)
        skip = " ".join(list(SELECTING) + ["puremove", "redgate"])
        done = self.hook("--range", f"{self.base}..{head}", env={"PREPUSH_SKIP": skip})
        self.assertEqual(0, done.code, done.output)
        for check in [c for c in SELECTING if c != "identity"] + ["puremove"]:
            self.assertIn(f"{check} SKIPPED (PREPUSH_SKIP)", done.stderr, check)
        self.assertIn("all clear", done.stdout)

    def test_a_stall_ends_the_check_with_the_stall_signal(self):
        done = self.hook("--range", self.range_of(), env={
            "STUB_SNAPSHOT_SLEEP": "120", "PREPUSH_STALL_SECONDS": "2", "KINOWO_STALL_POLL_SECONDS": "1",
            "PREPUSH_SKIP": "redgate"}, bound=90)
        self.assertEqual(75, done.code, done.output)
        self.assertRegex(done.stderr, r"pre-push STALLED: snapshot \(no progress for 2s: .*; ended after \d+s\)")
        self.assertIn("blocked by: snapshot", done.stderr)

    def test_a_known_red_fails_as_on_the_base(self):
        baselines = self.path("baselines")
        os.makedirs(baselines)
        with open(os.path.join(baselines, self.base), "w") as f:
            f.write(f"pre-push baseline {self.base}\n▸ pre-push snapshot — for 1 changed path(s)\n"
                    "FAIL: the snapshot inputs moved at c0ffee\n! pre-push full log: /tmp/tmp.x/snapshot.log\n"
                    "! pre-push snapshot FAILED (4s)\n")
        env = {"STUB_SNAPSHOT_EXIT": "1", "PREPUSH_SKIP": "redgate", "PREPUSH_BASELINE_DIR": baselines}
        done = self.hook("--range", self.range_of(), env=env)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("KNOWN RED: snapshot fails the same way on the base", done.stderr)
        self.assertRegex(done.stdout, r"all clear but main's own known red: snapshot " + SECONDS)
        done = self.hook("--range", f"{self.base}..HEAD", env={**env, "STUB_SNAPSHOT_LINE": "another failure"})
        self.assertEqual(1, done.code, done.output)

    def test_a_baseline_for_another_base_holds_nothing(self):
        baselines = self.path("baselines")
        os.makedirs(baselines)
        with open(os.path.join(baselines, self.base), "w") as f:
            f.write("not a baseline\n")
        done = self.hook("--range", self.range_of(), env={
            "STUB_SNAPSHOT_EXIT": "1", "PREPUSH_SKIP": "redgate", "PREPUSH_BASELINE_DIR": baselines})
        self.assertEqual(1, done.code, done.output)
        self.assertIn(f"does not start 'pre-push baseline {self.base}': every failing check blocks", done.stderr)

    def test_a_passed_push_is_reused(self):
        ran = self.path("snapshot.ran")
        env = {"PREPUSH_NO_REUSE": None, "PREPUSH_REUSE_DIR": self.path("reuse"), "STUB_SNAPSHOT_RAN": ran,
               "PREPUSH_SKIP": "redgate"}
        rng = self.range_of()
        self.assertEqual(0, self.hook("--range", rng, env=env).code)
        done = self.hook("--range", rng, env=env)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("reused, not run again", done.output)
        self.assertEqual(1, len(lines(self.read("snapshot.ran"))))

    def test_a_missing_tool_blocks(self):
        tools = self.path("tools")
        os.makedirs(tools)
        for tool in BASE_TOOLS.split():
            found = shutil.which(tool)
            if found:
                os.symlink(found, os.path.join(tools, tool))
        done = self.hook("--range", self.range_of(".github/workflows/x.yml"), env={
            "PATH": tools, "PREPUSH_SKIP": "redgate newchecks schedules sbt"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("actionlint not found: the actionlint check cannot run, so the push is BLOCKED (never skipped). "
                      "Fix: brew install actionlint -- then push again (or PREPUSH_SKIP=actionlint, named in your "
                      "report).", done.stderr)


class ScriptTestsTest(PushCase):
    """The scripttests check: how it calls scripts/run-script-tests, natively, under the floor bash and on Linux."""

    runs: List[str] = []

    def setUp(self) -> None:
        super().setUp()
        self.runs = []

    def run_check(self, **env: str) -> Tuple[Done, List[str], str]:
        floor = self.fakes.script("floor-bash-3", "echo 3")
        head = self.change("a tool's notes", {"infra/bin/notes.txt": str(len(self.runs))})
        self.runs.append(head)
        args = self.path("runner.args")
        if os.path.exists(args):
            os.remove(args)
        done = self.hook("--range", f"{self.base}..{head}", env={
            "STUB_RUNNER_ARGS": args, "PREPUSH_FLOOR_BASH": floor, "PREPUSH_SKIP": "redgate",
            "PREPUSH_UNAME": "Darwin", **env})
        called = sorted(lines(self.read("runner.args"))) if os.path.exists(args) else []
        return done, called, floor

    def test_native_with_the_floor_and_linux_beside(self):
        done, called, floor = self.run_check()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([f"--changed {self.base} --floor {floor}", f"--linux --changed {self.base}"], called)
        self.assertIn("run-script-tests: 1 passed", done.stdout)
        self.assertIn("on Linux: run-script-tests: 1 passed", done.stdout)
        self.assertIn(f"under {floor}: the floor's scope, in the same run (0 test(s))", done.stdout)

    def test_skips_and_docker_off(self):
        done, called, _ = self.run_check(PREPUSH_SKIP="redgate floor linux")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([f"--changed {self.base}"], called)
        self.assertIn("scripttests under the floor bash 3.2 SKIPPED (PREPUSH_SKIP=floor)", done.stderr)
        done, _, _ = self.run_check(STUB_NO_DOCKER="1")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("on Linux: SKIPPED (Docker is not running", done.stdout)
        done, called, _ = self.run_check(PREPUSH_UNAME="Linux", PREPUSH_SKIP="redgate floor")
        self.assertEqual([f"--changed {self.base}"], called[-1:])
        self.assertIn("on Linux: this run is Linux's", done.stdout)

    def test_a_failing_run_blocks(self):
        done, _, _ = self.run_check(STUB_RUNNER_EXIT="1", STUB_RUNNER_SAYS="FAIL scripts/x-test.sh")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("run-script-tests: FAIL scripts/x-test.sh", done.stderr)
        self.assertIn("blocked by: scripttests", done.stderr)


class SbtTest(PushCase):
    """The sbt check's one sbt run (the lint plan, local-tests.txt's basic set), and the identity area through the
    checkout's scripts/sbt-warm."""

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("sbt", "exit(int(env.get('STUB_SBT_EXIT', '0')))")
        self.fakes.script("java", "exit 0")

    def test_one_sbt_run(self):
        head = self.change("a Dockerfile", {"Dockerfile": "FROM x"})
        done = self.hook("--range", f"{self.base}..{head}", env={"PREPUSH_SKIP": "redgate"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["-batch", "-Dsbt.server.forcestart=false", "worker/Test/testOnly lint.DockerSpec",
                           "web/Test/testOnly lint.StandingSpec", "Test/compile",
                           "e2e/testOnly services.movies.FilmScheduleEndToEndSpec",
                           "repochecks/testOnly deploy.* -deploy.GitopsSpec"]], self.fakes.calls("sbt"))
        self.assertRegex(done.stdout, r"sbt ok " + SECONDS)

    def test_a_test_config_compiled_first(self):
        head = self.change("a web spec", {"web/src/it/scala/ASpec.scala": "x"})
        self.assertEqual(0, self.hook("--range", f"{self.base}..{head}", env={"PREPUSH_SKIP": "redgate"}).code)
        self.assertEqual(["-batch", "-Dsbt.server.forcestart=false", "web/It/compile"], self.fakes.calls("sbt")[0][:3])

    def test_a_failing_sbt_shows_its_summary(self):
        self.fakes.program("sbt", "out('[error] Failed tests: ASpec'); exit(1)")
        head = self.change("a Dockerfile", {"Dockerfile": "FROM x"})
        done = self.hook("--range", f"{self.base}..{head}", env={"PREPUSH_SKIP": "redgate"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("[error] Failed tests: ASpec", done.stderr)
        self.assertRegex(done.stderr, r"full log: .*sbt\.log")

    def test_the_identity_area_through_sbt_warm(self):
        warm = self.path("warm.args")
        head = self.change("an identity change", {"identity/src/main/scala/A.scala": "x",
                                                  "scripts/sbt-warm": FAKE_SBT_WARM})
        done = self.hook("--range", f"{self.base}..{head}", env={
            "STUB_WARM_ARGS": warm, "PREPUSH_SKIP": "redgate driftguards scripttests shellcheck pyquality"})
        self.assertEqual(0, done.code, done.output)
        called = lines(self.read("warm.args"))
        self.assertIn("Test/compile; e2e/testOnly services.movies.FilmScheduleEndToEndSpec; "
                      "repochecks/testOnly deploy.* -deploy.GitopsSpec", called[0])
        self.assertEqual([], self.fakes.calls("sbt"))
        self.assertTrue(any(c.startswith("identityImpact ") for c in called), called)
        self.assertTrue(any(re.match(r"worker/testOnly services\.identity\.\* -- "
                                     r"-Didentity\.population\.candidates=\S+population-walk$", c) for c in called),
                        called)
        self.assertIn("identity: no test-impact selection (sbt identityImpact failed) -- every identity test",
                      done.stderr)
        self.assertIn("Tests: succeeded 3", done.stdout)
        self.assertRegex(done.stdout, r"identity ok " + SECONDS)

    def test_the_identity_plan_alone(self):
        plan = self.path("identity.plan")
        head = self.change("an identity change", {"identity/src/main/scala/A.scala": "x",
                                                  "scripts/sbt-warm": FAKE_SBT_WARM})
        done = self.hook("--range", f"{self.base}..{head}", env={
            "STUB_WARM_ARGS": self.path("warm.args"), "PREPUSH_IDENTITY_PLAN": plan,
            "PREPUSH_SKIP": "redgate driftguards scripttests shellcheck pyquality sbt"})
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"identity: the plan alone, to {plan} (PREPUSH_IDENTITY_PLAN): nothing run", done.stdout)
        self.assertEqual("# no test-impact selection (sbt identityImpact failed) -- every identity test\n"
                         "worker/testOnly services.identity.*\n", self.read("identity.plan"))


class StdinTest(PushCase):
    """git's own call: a line per pushed ref on stdin, the remote's name and URL as arguments."""

    def test_refs_that_check_nothing(self):
        head = self.change("ios", {"ios/A.swift": "x"})
        zero = "0" * 40
        done = self.hook("origin", "url", stdin=f"refs/heads/x {head} refs/heads/measure/a {zero}\n")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("measure/ or check/ branch: a CI run's ref, never landed -- nothing to check", done.stdout)
        done = self.hook("origin", "url", stdin=f"refs/tags/t {head} refs/tags/t {zero}\n")
        self.assertIn("tag push: a tag changes nothing main builds -- nothing to check", done.stdout)
        done = self.hook("origin", "url", stdin=f"(delete) {zero} refs/heads/old {head}\n")
        self.assertEqual((0, ""), (done.code, done.stdout), done.output)

    def test_a_push_to_main_asks_agentctl_first(self):
        head = self.change("ios", {"ios/A.swift": "x"})
        agentctl = os.path.join(self.path("home"), "bin", "agentctl")
        os.makedirs(os.path.dirname(agentctl))
        with open(agentctl, "w") as f:
            f.write('#!/bin/sh\necho "$*" > "$AGENTCTL_SAW"; cat >> "$AGENTCTL_SAW"; exit 1\n')
        os.chmod(agentctl, 0o755)
        line = f"refs/heads/main {head} refs/heads/main {self.base}\n"
        done = self.hook("origin", "https://x/repo", stdin=line, env={"AGENTCTL_SAW": self.path("saw")})
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(f"prepush-check --hook origin https://x/repo\n{line}", self.read("saw"))

    def test_a_push_to_main_runs_its_checks(self):
        head = self.change("ios", {"ios/A.swift": "x"})
        done = self.hook("origin", "url", stdin=f"refs/heads/main {head} refs/heads/main {self.base}\n",
                         env={"PREPUSH_LIST": "1", "PREPUSH_SKIP": "redgate"})
        self.assertEqual((0, ["ios"]), (done.code, lines(done.stdout)), done.output)


if __name__ == "__main__":
    unittest.main()
