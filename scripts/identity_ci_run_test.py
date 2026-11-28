#!/usr/bin/env python3
"""scripts/identity-ci-run, black-box, against scratch repositories, a git that watches its measure/ pushes and a fake
gh: several runs of ONE commit at once (identity-landing-check's groups, beside its identity-measure) push their
measure/ branches one at a time, never together; a push the remote rejects names the remote's reason; every --command
is a job of its own; a finished run's shard artifacts are laid into one --out (exit the run's own), --no-wait prints
the run. Ported from identity-ci-run-test.sh (its identity_run_collect cases through the command line); the same cases
hold the shell tool and its Python rewrite (cli_harness.tool_path).

Why one at a time (2026-10-23): the remote took two of four concurrent pushes of the same new commit as "unable to
migrate objects to permanent storage" -- receive-packs of the same objects (100+, so kept as a pack) write the same
pack-<hash>.keep with different contents (each its own pid), and the second one's quarantine cannot move in. A push
after the first finds the commit on the remote already and sends no objects at all."""
from __future__ import annotations

import json
import os
import re
import shutil
import sys
import unittest
from concurrent.futures import ThreadPoolExecutor
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_ci_run.py", "scripts/identity-ci-run")
# gh: a dispatch noted (its argv, a JSON line) and refused unless $DISPATCH_OK; the dispatched run (901, on $TIP)
# listed (through the caller's --jq, as gh applies it), read as ci-wait reads it ($CONCLUSION), and its artifacts
# ($ARTIFACT) downloaded
GH = r'''
import shutil, subprocess
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else ""
verb = " ".join(argv[:2])
if verb == "workflow run":
    with open(env["DISPATCHES"], "a") as f:
        f.write(json.dumps(argv) + "\n")
    exit(0 if env.get("DISPATCH_OK") else 1)
if verb == "run list":
    runs = json.dumps([{"databaseId": 901, "headSha": env["TIP"], "createdAt": "2999-01-01T00:00:00Z",
                        "displayTitle": "Identity run"}])
    if not flag("--jq"):
        out(runs); exit(0)
    done = subprocess.run(["jq", "-r", flag("--jq")], input=runs, capture_output=True, text=True)
    sys.stdout.write(done.stdout); sys.stderr.write(done.stderr); exit(done.returncode)
if verb == "run view" and flag("--json") == "url":
    out("https://example.test/runs/901"); exit(0)
if verb == "run view":
    out(json.dumps({"databaseId": 901, "headSha": env["TIP"], "headBranch": "measure/x", "workflowName": "Identity run",
                    "status": "completed", "conclusion": env.get("CONCLUSION", "success"), "event": "workflow_dispatch",
                    "url": "https://example.test/runs/901", "number": 9, "createdAt": "2999-01-01T00:00:00Z"}))
    exit(0)
if verb == "run download":
    if os.path.isdir(env.get("ARTIFACT", "")):
        shutil.copytree(env["ARTIFACT"], flag("--dir"), dirs_exist_ok=True)
        exit(0)
    err("no valid artifacts found to download"); exit(1)
exit(1)
'''
# git: the real one, but a measure/ push says when another push was under way meanwhile (an overlap), or -- with
# $REJECT -- is rejected the way the remote rejected the racing ones
GIT = '''#!/usr/bin/env bash
if [ "$1" = push ] && [[ " $* " == *refs/heads/measure/* ]] && [[ " $* " != *--delete* ]]; then
  if [ -n "${REJECT:-}" ]; then
    echo "error: files './objects/tmp_objdir-incoming-x/pack/pack-1.keep' and './objects/pack/pack-1.keep' differ in contents" >&2
    echo " ! [remote rejected] $(printf '%%s' "$*" | sed 's/.* //') (unable to migrate objects to permanent storage)" >&2
    echo "error: failed to push some refs to 'origin'" >&2
    exit 1
  fi
  touch "%(pushes)s/active/$$"
  sleep 1
  n=$(ls "%(pushes)s/active" | wc -l | tr -d ' ')
  [ "$n" -le 1 ] || echo overlap >> "%(pushes)s/log"
  "%(git)s" "$@"; rc=$?
  rm -f "%(pushes)s/active/$$"
  echo pushed >> "%(pushes)s/log"
  exit $rc
fi
exec "%(git)s" "$@"
'''


class IdentityCiRunTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.origin, self.checkout = self.origin_and_checkout()
        self.write("repo/.github/workflows/identity-run.yml", "on: workflow_dispatch\n")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "main")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.write("repo/change.txt", "change\n")
        self.git(self.checkout, "add", "change.txt")
        self.git(self.checkout, "commit", "-qm", "a change")
        self.tip = self.git(self.checkout, "rev-parse", "HEAD")
        os.makedirs(self.path("pushes", "active"))
        self.gh = self.fakes.program("gh", GH)
        with open(os.path.join(self.fakes.bin, "git"), "w") as f:
            f.write(GIT % {"pushes": self.path("pushes"), "git": shutil.which("git") or "git"})
        os.chmod(os.path.join(self.fakes.bin, "git"), 0o755)

    def ci_run(self, *args: str, **env: str) -> cli_harness.Done:
        """identity-ci-run of the tip (of Test/compile when no -- is given), from the checkout."""
        command = () if "--" in args else ("--", "Test/compile")
        return self.run_tool("--head", self.tip, *args, *command, cwd=self.checkout, env={
            "KINOWO_GH": self.gh, "KINOWO_IDENTITY_CACHE": self.path("cache"), "IDENTITY_MEASURE_REMOTE": "origin",
            "HOME": self.work, "TIP": self.tip, "DISPATCHES": self.path("dispatches"), "IDENTITY_CI_RUN_POLL": "1",
            **env})

    def pushes(self, what: str) -> int:
        with open(self.path("pushes", "log")) as f:
            return sum(1 for line in f if line.strip() == what)

    def measure_branches(self) -> str:
        return self.git(self.origin, "for-each-ref", "refs/heads/measure")

    def dispatched_args(self) -> List[str]:
        with open(self.path("dispatches")) as f:
            return [arg for line in f for arg in json.loads(line)]

    def test_runs_of_one_commit_at_once_push_one_at_a_time(self) -> None:
        with ThreadPoolExecutor(3) as pool:
            done = list(pool.map(lambda _: self.ci_run(), range(3)))
        self.assertEqual((3, 0), (self.pushes("pushed"), self.pushes("overlap")))
        # each reaches the dispatch (refused here), its branch deleted as before
        self.assertEqual([2, 2, 2], [d.code for d in done], [d.stderr for d in done])
        self.assertEqual("", self.measure_branches())

    def test_a_push_the_remote_rejects_is_no_run_saying_why(self) -> None:
        done = self.ci_run(REJECT="1")
        self.assertEqual(2, done.code, done.stderr)
        said = [re.sub(r"measure/[^ :]+", "measure/B", line) for line in done.stderr.split("\n")
                if line.startswith("identity-ci-run: ")]
        self.assertEqual(["identity-ci-run: could not push measure/B: remote rejected (unable to migrate objects to "
                          "permanent storage)"], said)

    def test_each_command_is_a_job_of_its_own(self) -> None:
        self.ci_run("--command", "runMain Bench de --older a", "--command", "runMain Bench uk", "--", "worker/run x")
        self.assertIn("command=runMain Bench de --older a\nrunMain Bench uk\nworker/run x", self.dispatched_args())

    def shards(self) -> None:
        """identity-run.yml's jobs each upload an identity-run-<shard> artifact (gh run download --pattern puts each
        in a directory of its own), beside an unrelated one."""
        self.write("artifact/identity-run-de/de/takes.tsv", "de\n")
        self.write("artifact/identity-run-de/sbt-de.log", "log-de\n")
        self.write("artifact/identity-run-uk/uk/sub/takes.tsv", "uk\n")
        self.write("artifact/identity-run-uk/sbt-uk.log", "log-uk\n")
        self.write("artifact/unrelated/x", "x\n")

    def laid(self, out: str) -> List[str]:
        return sorted(os.path.relpath(os.path.join(here, f), out) for here, _, files in os.walk(out) for f in files)

    def test_a_finished_runs_shards_are_laid_into_one_out(self) -> None:
        self.shards()
        done = self.ci_run("--out", self.path("collected"), DISPATCH_OK="1", ARTIFACT=self.path("artifact"))
        self.assertEqual(0, done.code, done.stderr)
        self.assertEqual(["de/takes.tsv", "sbt-de.log", "sbt-uk.log", "uk/sub/takes.tsv"], self.laid(self.path("collected")))
        self.assertIn(f"identity-ci-run: output in {self.path('collected')}", done.stderr)
        self.assertIn("-f", self.dispatched_args())
        self.assertIn("recording=", self.dispatched_args())

    def test_a_failed_run_is_exit_1_its_output_laid_all_the_same(self) -> None:
        self.shards()
        done = self.ci_run("--out", self.path("collected"), DISPATCH_OK="1", ARTIFACT=self.path("artifact"),
                           CONCLUSION="failure")
        self.assertEqual(1, done.code, done.stderr)
        self.assertIn("identity-ci-run: run 901: failure", done.stderr)
        self.assertIn("sbt-de.log", self.laid(self.path("collected")))

    def test_a_run_with_no_identity_run_artifact_is_no_output(self) -> None:
        self.write("artifact/unrelated/x", "x\n")
        done = self.ci_run("--out", self.path("collected"), DISPATCH_OK="1", ARTIFACT=self.path("artifact"))
        self.assertEqual(2, done.code, done.stderr)
        self.assertIn("run 901 left no identity-run artifact", done.stderr)

    def test_no_wait_prints_the_run(self) -> None:
        done = self.ci_run("--no-wait", DISPATCH_OK="1")
        self.assertEqual((0, "901\n"), (done.code, done.stdout), done.stderr)
        self.assertIn("(it deletes measure/", done.stderr)

    def test_usage(self) -> None:
        self.assertEqual(2, self.run_tool("--head", self.tip, cwd=self.checkout).code, "no command")
        self.assertEqual(2, self.ci_run("--recording", "latest").code, "a recording is a run id")
        self.assertEqual(2, self.ci_run("--bogus").code, "an unknown argument")


if __name__ == "__main__":
    unittest.main()
