#!/usr/bin/env python3
"""scripts/ci/takes-skip-gate.sh, black-box, against a scratch repository and a stub proof: SKIP for a title-rule change
the proof clears, and RUN for every other case -- each condition failing on its own, every failure mode of the proof,
every input missing. The gate must NEVER answer SKIP when the takes should run. Every RUN case names ITS reason, so a
check taken out of the gate shows even where a later check still answers RUN: the mutation test
(takes_skip_gate_mutation_test.py) runs this file against mutants of the gate, each with one check removed, and
requires it to fail on every one. Ported from takes-skip-gate-test.sh; the same cases hold the shell gate and its
Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import gzip
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import unittest
from typing import Dict, List, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TEMPLATE_ENV = "TAKES_SKIP_GATE_TEMPLATE"  # a Shared's directory to clone (set by the mutation test, which builds it once)
TOOL = cli_harness.tool_path("scripts/ci/takes_skip_gate.py", "scripts/ci/takes-skip-gate.sh")
RULES = "common/src/main/resources/title-rules.tsv"
FILES = (RULES, "common/src/main/scala/services/titles/rules/Rules.scala",
         "common/src/main/scala/services/titles/rules/TitleRulePlaceholders.scala",
         "common/src/main/scala/services/titles/TitleNormalizer.scala",
         "identity/src/main/scala/services/identity/Resolver.scala", "common/src/main/scala/services/movies/Matcher.scala",
         "worker/src/main/scala/services/Projection.scala", "docs/a.md", "README.md", "scripts/ci/takes-skip-gate.sh",
         "scripts/ci/takes_skip_gate.py", "scripts/ci/identity-takes-check.sh",
         "testkit/src/main/scala/services/movies/NormalizerTrace.scala",
         "testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala",
         "test/resources/fixtures/identity-judgements.tsv", "worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala",
         ".github/workflows/x.yml")
# one commit per case, each on the base: its name -> the files it changes
CASES: Dict[str, Tuple[str, ...]] = {
    "rules": (RULES,),
    "rules-docs": (RULES, "common/src/main/scala/services/titles/rules/Rules.scala", "docs/a.md", "README.md"),
    "normalizer": ("common/src/main/scala/services/titles/TitleNormalizer.scala",),
    "placeholders": ("common/src/main/scala/services/titles/rules/TitleRulePlaceholders.scala",),
    "resolver": (RULES, "identity/src/main/scala/services/identity/Resolver.scala"),
    "matcher": (RULES, "common/src/main/scala/services/movies/Matcher.scala"),
    "projection": (RULES, "worker/src/main/scala/services/Projection.scala"),
    "judgements": (RULES, "test/resources/fixtures/identity-judgements.tsv"),
    "ratchet": (RULES, "worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala"),
    "workflow": (RULES, ".github/workflows/x.yml"),
    "unknown": (RULES, "some/new/file.txt"),
    "docs-only": ("docs/a.md", "README.md"),
    "empty": (),
    "tool-trace": (RULES, "testkit/src/main/scala/services/movies/NormalizerTrace.scala"),
    "tool-check": (RULES, "testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala"),
    # the gate, whichever language it is written in
    "tool-gate": (RULES, "scripts/ci/takes-skip-gate.sh", "scripts/ci/takes_skip_gate.py"),
    "tool-takes": (RULES, "scripts/ci/identity-takes-check.sh"),
    "identity-rule": ("common/src/main/scala/services/titles/rules/IdentityRules.scala",),
}
# THE PROOF: STUB_PROOF says how it ends -- pass (0, all alike), differ (1), unreplayable (3), crash (2), hang (never
# answers), liar (exits 0 but says some differ), silent (exits 0, says nothing), zero (exits 0 having read nothing),
# listed (exits 0, all alike, but writes a differ row). It logs its arguments.
PROOF = r"""#!/usr/bin/env bash
echo "$*" >> "$PROOF_LOG"
out=$2
: > "$out"
case "${STUB_PROOF:-pass}" in
  pass)   echo "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 0 could not be made again, 0 decision-trace rule name(s) otherwise (no take reads them)" ;;
  differ) echo "[normalizer-trace] 2 reading(s) made again: 1 read otherwise, 0 could not be made again, 0 decision-trace"; exit 1 ;;
  unreplayable) echo "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 1 could not be made again, 0 decision-trace"; exit 3 ;;
  crash)  echo "[normalizer-trace] could not read the trace: boom" >&2; exit 2 ;;
  hang)   echo $$ >> "$PROOF_HUNG"; exec sleep 86400 ;;
  liar)   echo "[normalizer-trace] 2 reading(s) made again: 1 read otherwise, 0 could not be made again, 0 decision-trace" ;;
  silent) ;;
  zero)   echo "[normalizer-trace] 0 reading(s) made again: 0 read otherwise, 0 could not be made again, 0 decision-trace" ;;
  listed) echo "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 0 could not be made again, 0 decision-trace"
          printf 'pl\tclean\tx\tx\ty\n' > "$out" ;;
esac
exit 0
"""


class Shared:
    """The scratch repository, traces and proof every case reads, made once for the class."""

    def __init__(self, directory: str, template: Optional[str] = None) -> None:
        """Built from scratch, or cloned from `template` (a Shared's directory: APFS copy-on-write where there is one)."""
        self.dir = directory
        self.repo = os.path.join(directory, "repo")
        if template is not None:
            self.clone(template)
            return
        os.makedirs(self.repo)
        self.git("init", "-q", "-b", "main")
        for f in FILES:
            self.put(f, "base")
        self.git("add", "-A")
        self.git("commit", "-q", "-m", "base")
        self.base = self.git("rev-parse", "HEAD")
        self.commits = {name: self.commit(name, files) for name, files in CASES.items()}
        self.trace = self.gz("trace.jsonl.gz", b'{"c":"pl","m":"clean","a":["x"],"o":"x"}\n'
                                               b'{"c":"pl","m":"clean","a":["y"],"o":"y"}\n')
        self.empty_trace = self.gz("empty-trace.jsonl.gz", b"")
        self.broken_trace = os.path.join(directory, "broken-trace.jsonl.gz")
        with open(self.broken_trace, "w") as f:
            f.write("not a gzip")
        self.proof = os.path.join(directory, "proof")
        with open(self.proof, "w") as f:
            f.write(PROOF)
        os.chmod(self.proof, 0o755)

    def clone(self, template: str) -> None:
        """The template's repository, traces and proof copied here, its commits read back (the mutation test builds the
        template once and every mutant's run clones it instead of replaying the git history)."""
        if subprocess.run(["cp", "-cR", template + "/.", self.dir], capture_output=True).returncode != 0:
            shutil.copytree(template, self.dir, dirs_exist_ok=True)
        self.base = self.git("rev-parse", "main")
        self.commits = {name: self.git("rev-parse", name) for name in CASES}
        self.trace, self.empty_trace = (os.path.join(self.dir, n) for n in ("trace.jsonl.gz", "empty-trace.jsonl.gz"))
        self.broken_trace, self.proof = os.path.join(self.dir, "broken-trace.jsonl.gz"), os.path.join(self.dir, "proof")

    def put(self, relative: str, text: str) -> None:
        full = os.path.join(self.repo, relative)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text + "\n")

    def git(self, *args: str) -> str:
        return cli_harness.scratch_git(self.repo, *args)

    def commit(self, name: str, files: Tuple[str, ...]) -> str:
        self.git("checkout", "-q", "-B", name, self.base)
        for f in files:
            self.put(f, f"changed {f}")
        self.git("add", "-A")
        self.git("commit", "-q", "-m", name, "--allow-empty")
        return self.git("rev-parse", "HEAD")

    def gz(self, name: str, data: bytes) -> str:
        path = os.path.join(self.dir, name)
        with gzip.open(path, "wb") as f:
            f.write(data)
        return path


# THE GATE ENDED AS IT STARTS ITS PROOF: runs the gate (argv[1], its arguments after) with subprocess.Popen wrapped, so
# that spawning the proof (argv[0] == $ENDED_AT) records the proof's pid in $ENDED_PIDS and TERMs the gate at once --
# before the gate has the process in hand. The window a loaded runner hit (Tool self-tests #632), made certain.
ENDED_AS_IT_STARTS = r"""
import os, runpy, signal, subprocess, sys
spawn = subprocess.Popen
def spawn_then_ended(args, *rest, **kwargs):
    proc = spawn(args, *rest, **kwargs)
    if list(args)[:1] == [os.environ["ENDED_AT"]]:
        with open(os.environ["ENDED_PIDS"], "a") as f:
            f.write(f"{proc.pid}\n")
        os.kill(os.getpid(), signal.SIGTERM)
    return proc
subprocess.Popen = spawn_then_ended
sys.argv = sys.argv[1:]
runpy.run_path(sys.argv[0], run_name="__main__")
"""


class TakesSkipGateTest(cli_harness.ToolCase):
    TOOL = TOOL
    shared: Shared
    _shared_dir: str

    @classmethod
    def setUpClass(cls) -> None:
        cls._shared_dir = os.path.realpath(tempfile.mkdtemp(prefix="takes-skip-gate-"))
        cls.shared = Shared(cls._shared_dir, os.environ.get(TEMPLATE_ENV))

    @classmethod
    def tearDownClass(cls) -> None:
        shutil.rmtree(cls._shared_dir, ignore_errors=True)

    def setUp(self) -> None:
        super().setUp()
        self.proof_log = self.path("proof.log")
        self.hung = self.path("hung")
        open(self.hung, "w").close()
        self.err = ""

    def gate(self, *args: str, cwd: Optional[str] = None, mode: Optional[str] = "shadow",
             env: Optional[Dict[str, str]] = None) -> str:
        """The gate called as given -> "<stdout first line> <exit>"; its reasons kept for `because`."""
        open(self.proof_log, "w").close()
        done = self.run_tool(*args, cwd=cwd or self.shared.repo, env={
            "TAKES_SKIP_MODE": mode, "TAKES_SKIP_PROOF_TIMEOUT": "2", "PROOF_LOG": self.proof_log,
            "PROOF_HUNG": self.hung, **(env or {})})
        self.err = done.stderr
        self.stdout = done.stdout
        return f"{(done.stdout.split(chr(10)) + [''])[0]} {done.code}"

    def decide(self, case: str, trace: Optional[str] = None, mode: Optional[str] = "shadow",
               proof: Optional[str] = None) -> str:
        s = self.shared
        return self.gate(s.base, s.commits.get(case, case), trace or s.trace, "--", s.proof, mode=mode,
                         env={"STUB_PROOF": proof} if proof else None)

    def because(self, text: str) -> str:
        return "1" if text in self.err else "0"

    def proof_calls(self) -> List[str]:
        with open(self.proof_log) as f:
            return [line for line in f.read().split("\n") if line]

    # SKIP: the one case that holds
    def test_a_title_rule_change_the_proof_clears_is_skip_handing_the_proof_the_base_trace(self) -> None:
        self.assertEqual("SKIP 0", self.decide("rules"), self.err)
        self.assertEqual([f"--out {self.proof_calls()[0].split()[1]} {self.shared.trace}"], self.proof_calls())
        self.assertTrue(self.proof_calls()[0].split()[1].endswith("/differ.tsv"))
        self.assertEqual(["SKIP", "measure: footprint, 0 readings differ (2 readings)", ""], self.stdout.split("\n"))

    def test_rules_and_their_docs_the_normaliser_and_enforce_mode_are_skip(self) -> None:
        self.assertEqual("SKIP 0", self.decide("rules-docs"), self.err)
        self.assertEqual("SKIP 0", self.decide("normalizer"), self.err)
        self.assertEqual("SKIP 0", self.decide("rules", mode="enforce"), self.err)

    # RUN: the mode
    def test_a_mode_off_unset_or_anything_else_is_run_without_running_the_proof(self) -> None:
        for mode in ("off", "", None, "yes"):
            with self.subTest(mode=mode):
                self.assertEqual("RUN 1 1", f"{self.decide('rules', mode=mode)} {self.because('TAKES_SKIP_MODE is')}")
                self.assertEqual([], self.proof_calls())

    # RUN: the commits
    def test_an_abbreviated_base_is_run(self) -> None:
        s = self.shared
        self.assertEqual("RUN 1 1", f"{self.gate(s.base[:9], s.commits['rules'], s.trace, '--', s.proof)} "
                                    f"{self.because('base and head must be full commit names')}")

    def test_an_unknown_head_is_run(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide('1' * 40)} {self.because('git could not diff')}")

    def test_base_and_head_one_commit_is_run(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide(self.shared.base)} {self.because('base and head are one commit')}")

    def test_no_change_at_all_is_run(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide('empty')} {self.because('no file changed')}")

    def test_a_gate_called_without_its_proof_command_is_run(self) -> None:
        s = self.shared
        self.assertEqual("RUN 1 1", f"{self.gate(s.base, s.commits['rules'], s.trace)} {self.because('usage:')}")
        self.assertEqual("RUN 1 1", f"{self.gate(s.base, s.commits['rules'], s.trace, 'x', s.proof)} "
                                    f"{self.because('usage:')}")

    def test_outside_a_repository_is_run(self) -> None:
        s = self.shared
        self.assertEqual("RUN 1 1", f"{self.gate(s.base, s.commits['rules'], s.trace, '--', s.proof, cwd=self.work)} "
                                    f"{self.because('git could not diff')}")

    # RUN: the files
    def test_a_change_beside_the_rules_that_is_neither_a_rule_nor_inert_is_run(self) -> None:
        for case in ("placeholders", "judgements", "ratchet", "workflow", "unknown"):
            with self.subTest(case=case):
                self.assertEqual("RUN 1 1", f"{self.decide(case)} {self.because('is neither a title-rule file nor inert')}")

    def test_identity_matcher_resolver_or_projection_code_is_run_named(self) -> None:
        for case, named in (("resolver", "identity/src/main/scala/services/identity/Resolver.scala"),
                            ("matcher", "common/src/main/scala/services/movies/Matcher.scala"),
                            ("projection", "worker/src/main/scala/services/Projection.scala"),
                            ("identity-rule", "common/src/main/scala/services/titles/rules/IdentityRules.scala")):
            with self.subTest(case=case):
                self.assertEqual("RUN 1 1", f"{self.decide(case)} {self.because(named)}")

    def test_an_identity_named_rule_file_is_denied_whatever_its_place(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide('identity-rule')} "
                                    f"{self.because('identity, matcher, resolver or projection code changed')}")

    def test_the_title_rule_placeholders_are_no_title_rule(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide('placeholders')} "
                                    f"{self.because('TitleRulePlaceholders.scala is neither')}")

    def test_docs_alone_is_run_no_rule_to_prove(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide('docs-only')} {self.because('no title-rule file changed')}")

    def test_the_trace_tooling_changed_is_run(self) -> None:
        for case in ("tool-trace", "tool-check", "tool-gate", "tool-takes"):
            with self.subTest(case=case):
                self.assertEqual("RUN 1 1", f"{self.decide(case)} {self.because('the trace tooling changed')}")

    # RUN: the trace
    def test_no_empty_or_broken_base_trace_is_run(self) -> None:
        s = self.shared
        for trace, why in ((os.path.join(s.dir, "no-such-trace.jsonl.gz"), "no base trace at"),
                           (s.empty_trace, "holds no reading"), (s.broken_trace, "is not a whole gzip")):
            with self.subTest(why=why):
                self.assertEqual("RUN 1 1", f"{self.decide('rules', trace=trace)} {self.because(why)}")

    # RUN: the proof
    def test_every_failure_of_the_proof_is_run_with_its_reason(self) -> None:
        for proof, why in (("differ", "the proof exited 1"), ("unreplayable", "the proof exited 3"),
                           ("crash", "the proof exited 2"), ("liar", "the proof said no"),
                           ("silent", "the proof said no"), ("zero", "the proof read nothing again"),
                           ("listed", "though it exited 0")):
            with self.subTest(proof=proof):
                self.assertEqual("RUN 1 1", f"{self.decide('rules', proof=proof)} {self.because(why)}")

    def test_the_proofs_output_is_passed_on_as_reasons(self) -> None:
        self.decide("rules", proof="crash")
        self.assertIn("[normalizer-trace] could not read the trace: boom", self.err)

    def test_zz_slow_a_proof_that_hangs_is_cut_off_leaving_nothing_running(self) -> None:
        self.assertEqual("RUN 1 1", f"{self.decide('rules', proof='hang')} {self.because('the proof exited 124')}")
        self.assertEqual(0, cli_harness.still_running(self.hung))

    def test_a_gate_ended_mid_proof_ends_its_proof_too(self) -> None:
        s = self.shared
        gate = subprocess.Popen([self.TOOL, s.base, s.commits["rules"], s.trace, "--", s.proof], cwd=s.repo,
                                env=self.env({"TAKES_SKIP_MODE": "shadow", "STUB_PROOF": "hang", "PROOF_LOG": self.proof_log,
                                              "PROOF_HUNG": self.hung}),
                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            self.assertTrue(cli_harness.wait_until(lambda: os.path.getsize(self.hung) > 0))
            gate.send_signal(signal.SIGTERM)
            self.assertNotEqual(0, gate.wait(timeout=60))
        finally:
            gate.kill()
            gate.wait()
        self.assertEqual(0, cli_harness.still_running(self.hung))

    def test_zz_slow_a_gate_ended_as_it_starts_its_proof_ends_its_proof_too(self) -> None:
        s, pids = self.shared, self.path("ended-pids")
        gate = subprocess.run([sys.executable, "-c", ENDED_AS_IT_STARTS, self.TOOL, s.base, s.commits["rules"], s.trace,
                               "--", s.proof], cwd=s.repo, timeout=60, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                              env=self.env({"TAKES_SKIP_MODE": "shadow", "STUB_PROOF": "hang", "PROOF_LOG": self.proof_log,
                                            "PROOF_HUNG": self.hung, "ENDED_AT": s.proof, "ENDED_PIDS": pids}))
        self.assertNotEqual(0, gate.returncode)
        self.assertTrue(os.path.getsize(pids) > 0)
        self.assertEqual(0, cli_harness.still_running(pids))

    def test_a_proof_command_that_does_not_exist_is_run(self) -> None:
        s = self.shared
        self.assertEqual("RUN 1 1", f"{self.gate(s.base, s.commits['rules'], s.trace, '--', self.path('no-such-proof'))} "
                                    f"{self.because('the proof exited 127')}")


if __name__ == "__main__":
    unittest.main()
