#!/usr/bin/env python3
"""scripts/identity-measure-local, black-box: the whole-corpus base-vs-head identity measure with no GitHub -- each ref
resolved once in a worktree of its own through scripts/it-local.sh (every country's dump, then the ratchet's final
takes), a resolved ref kept and shared, each country's model and final takes diffed, the classification's verdict and
its exit; a ref another run resolves waited for, the JVM slots and the memory gate honoured, a crashed resolve kept out.
A scratch repository holds a copy of the tool (the extensionless name, whichever it execs) with it-local.sh, the takes
diff and the classification stubbed. Ported from identity-measure-local-test.sh case for case; the same cases hold the
shell tool and its Python rewrite."""
from __future__ import annotations

import fcntl
import glob
import os
import re
import shutil
import subprocess
import sys
import unittest
from typing import Dict, List, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL_SCRIPTS = ("identity-measure-local", "identity_measure_local.py", "jvm-gate.sh", "tool-conventions.sh")
# it-local.sh: logs each sbt command, and writes what each would ("--out DIR --full CC", "--measure --out DIR")
IT_LOCAL = r'''#!/usr/bin/env bash
for command in "$@"; do
  echo "$(basename "$PWD") $command" >> "$STUB/calls"
  [ -n "${KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY:-}" ] && echo "live $KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" >> "$STUB/live"
  set -- $command
  case "$command" in
    *IdentityResolveDump*) while [ $# -gt 0 ]; do case "$1" in --out) out=$2 ;; --full) cc=$2 ;; --corpus-dir) corpus=$2 ;; esac; shift; done
                           rec=${corpus%/pair/*}; rec=${rec##*/}
                           if [ -f "$STUB/crash-dump" ]; then
                             printf '{"recording": "%s", "par' "$rec" > "$out/decisions-$cc.jsonl"; exit 137
                           fi
                           echo "{\"recording\": \"$rec\"}" > "$out/decisions-$cc.jsonl" ;;
    *UnmatchedClustersRatchet*) while [ $# -gt 0 ]; do [ "$1" = --out ] && out=$2; shift; done
                                if [ -f "$STUB/crash-ratchet" ]; then
                                  printf '%s\n' 'Exception in thread "main" java.lang.NoClassDefFoundError: modules/wiring/IdentityCutoverWiring$' \
                                    '	at tools.UnmatchedClusters$.familiesOf(UnmatchedClusters.scala:94)' '[success] Total time: 38 s'
                                else mkdir -p "$out" && echo takes > "$out/ratchet-takes.tsv"; fi ;;
  esac
done
'''
# the takes diff: its words logged; a diff held open (STUB_PAUSE names a FIFO) says so and blocks reading it until the
# case writes it -- the window another run's work falls into; then it reads its two sides
TAKES_DIFF = '''import os, sys
stub = os.environ["STUB"]
open(os.path.join(stub, "diffs"), "a").write(" ".join(a for a in sys.argv[1:] if not a.startswith("/")) + "\\n")
pause = os.environ.get("STUB_PAUSE")
if pause and os.path.exists(pause):
    open(pause + ".reached", "w").close()
    open(pause).read()
if "--final" not in sys.argv:
    open(os.path.join(stub, "read"), "a").write(" ".join(open(f).read().strip() for f in sys.argv[2:4]) + "\\n")
sys.exit(1 if os.path.exists(os.path.join(stub, "moved")) else 0)
'''
# the classification: its directory's layout logged, its verdict the stub's
CLASSIFY = '''import os, sys
root = sys.argv[1]
open(os.path.join(os.environ["STUB"], "classified"), "a").write(
    " ".join(sorted(os.path.relpath(os.path.join(d, f), root) for d, _, fs in os.walk(root) for f in fs)) + "\\n")
sys.exit(1 if os.path.exists(os.path.join(os.environ["STUB"], "wrong")) else 0)
'''
CLASSIFIED = ("identity-takes-pl/base/decisions-pl.jsonl identity-takes-pl/base/ratchet-takes.tsv "
              "identity-takes-pl/head/decisions-pl.jsonl identity-takes-pl/head/ratchet-takes.tsv "
              "identity-takes-uk/base/decisions-uk.jsonl identity-takes-uk/base/ratchet-takes.tsv "
              "identity-takes-uk/head/decisions-uk.jsonl identity-takes-uk/head/ratchet-takes.tsv")


def lock_free(lock: str) -> bool:
    """No process holds `lock`'s kernel lock (shell-spec.sh's spec_lock_state `free`)."""
    with open(lock, "a") as f:
        try:
            fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            return False
        fcntl.flock(f, fcntl.LOCK_UN)
    return True


class MeasureLocalTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo")
        for name in TOOL_SCRIPTS:
            source = os.path.join(cli_harness.ROOT, "scripts", name)
            if os.path.exists(source):
                os.makedirs(self.path("repo", "scripts"), exist_ok=True)
                shutil.copy2(source, self.path("repo", "scripts", name))
        os.makedirs(self.path("repo", "scripts", "lib"), exist_ok=True)
        for module in glob.glob(os.path.join(cli_harness.ROOT, "scripts", "lib", "*.py")):
            shutil.copy2(module, self.path("repo", "scripts", "lib"))
        os.chmod(self.write("repo/scripts/it-local.sh", IT_LOCAL), 0o755)
        self.write("repo/scripts/ci/identity_takes_diff.py", TAKES_DIFF)
        self.write("repo/scripts/identity_measure.py", CLASSIFY)
        self.write("repo/test/resources/fixtures/identity-judgements.tsv", "c\n")
        self.write("repo/.gitignore", "target/\n")
        self.base = self.commit_tree(self.checkout, "base")[:12]
        self.write("repo/change", "head\n")
        self.head = self.commit_tree(self.checkout, "head")[:12]
        # a laid recording in the cache: its corpus and two countries' trees; 7 and 39 older ones
        self.cache = self.path("cache")
        self.laid = self.path("cache", "recordings", "41", "pair", "test", "resources", "fixtures")
        for tree in (f"{self.laid}/corpus", f"{self.laid}/enrichment-pl", f"{self.laid}/enrichment-uk",
                     self.path("cache/recordings/7/pair/test/resources/fixtures/corpus"),
                     self.path("cache/recordings/39/pair/test/resources/fixtures/corpus"),
                     self.path("cache/recordings/39/pair/test/resources/fixtures/enrichment-pl")):
            os.makedirs(tree, exist_ok=True)
        self.store = self.path("cache", "measure-local", "41")

    def tool(self) -> str:
        return os.path.join(self.checkout, "scripts", "identity-measure-local")

    def environment(self, **extra: Optional[str]) -> Dict[str, Optional[str]]:
        return {"STUB": self.work, "KINOWO_IDENTITY_CACHE": self.cache, "TMDB_API_KEY": None,
                "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY": None, **extra}

    def measure(self, *args: str, cwd: Optional[str] = None, **extra: Optional[str]) -> cli_harness.Done:
        return self.run_tool(*args, tool=self.tool(), env=self.environment(**extra), cwd=cwd, bound=120)

    def start(self, name: str, *args: str, **extra: Optional[str]) -> "subprocess.Popen[bytes]":
        """The tool in the background, its output in <name>.out."""
        with open(self.path(f"{name}.out"), "w") as out:
            proc = subprocess.Popen([self.tool(), *args], env=self.env(self.environment(**extra)), cwd=self.work,
                                    stdout=out, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL,
                                    start_new_session=True)
        self.addCleanup(lambda: (proc.kill(), proc.wait()))
        return proc

    def text(self, name: str) -> str:
        return self.read(name) if os.path.exists(self.path(name)) else ""

    def calls(self) -> List[str]:
        return self.text("calls").splitlines()

    def forget(self, *names: str) -> None:
        for name in names:
            if os.path.exists(self.path(name)):
                os.remove(self.path(name))

    def commit(self, name: str) -> str:
        self.write(f"repo/{name}", f"{name}\n")
        return self.commit_tree(self.checkout, name)[:12]

    def test_usage_errors_and_help(self) -> None:
        self.assertEqual(2, self.measure("--head", self.head).code)
        done = self.measure("--head", self.head, "--base")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("--base needs a value", done.output)
        done = self.measure("--base", self.base, "--head", self.head, "--countries", "pl,xx")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("unknown country xx", done.output)
        done = self.measure("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"(?m)^THE IDENTITY MEASURE, LOCALLY")

    def test_nothing_moved_is_landable_each_ref_resolved_once_in_its_own_worktree(self) -> None:
        done = self.measure("--base", self.base, "--head", self.head, "--out", self.path("m"))
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.output, r"(?m)^LANDABLE \(local\)")
        self.assertIn("recording 41, countries pl uk", done.output)
        self.assertEqual([self.base] * 3 + [self.head] * 3, [c.split(" ")[0] for c in self.calls()])
        self.assertIn(f"--corpus-dir {self.laid}/corpus --fixtures {self.laid}", self.text("calls"))
        self.assertEqual([f"pl --base-name {self.base} --head-name {self.head}",
                          f"pl --final --base-name {self.base} --head-name {self.head}",
                          f"uk --base-name {self.base} --head-name {self.head}",
                          f"uk --final --base-name {self.base} --head-name {self.head}"],
                         [d.replace("--judgements ", "") for d in self.text("diffs").splitlines()])
        self.assertEqual(CLASSIFIED + "\n", self.text("classified"))
        self.assertIn("not seen, not refilled", done.output)

    def test_the_verdicts_exits_and_a_resolved_ref_kept(self) -> None:
        self.assertEqual(0, self.measure("--base", self.base, "--head", self.head, "--out", self.path("m")).code)
        self.forget("calls", "diffs")
        self.write("wrong", "")
        done = self.measure("--base", self.base, "--head", self.head, "--out", self.path("m"), "--countries", "pl")
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.output, r"(?m)^NOT LANDABLE \(local\)")
        self.forget("wrong", "calls", "diffs")
        self.write("moved", "")
        done = self.measure("--base", self.base, "--head", self.head, "--out", self.path("m"), "--countries", "pl")
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.output, r"(?m)^LANDABLE ONCE JUDGED \(local\)")
        self.assertEqual("", self.text("calls"))
        self.forget("moved")
        done = self.measure("--base", self.base, "--head", self.head, "--out", self.path("other-checkout"))
        self.assertEqual((0, ""), (done.code, self.text("calls")), done.output)

    def test_a_relative_out_is_the_callers(self) -> None:
        done = self.measure("--base", self.base, "--head", self.head, "--out", "rel", "--countries", "pl", cwd=self.work)
        self.assertEqual(0, done.code, done.output)
        for sha in (self.base, self.head):
            self.assertGreater(os.path.getsize(self.path("rel", sha, "decisions-pl.jsonl")), 0)

    def test_live_gaps_need_a_key_and_ask_live_in_an_out_of_their_own(self) -> None:
        self.assertEqual(2, self.measure("--base", self.base, "--head", self.head, "--live-gaps", TMDB_API_KEY="").code)
        done = self.measure("--base", self.base, "--head", self.head, "--live-gaps", TMDB_API_KEY="k1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["live k1"] * 6, self.text("live").splitlines())
        first = next(line for line in done.output.splitlines() if "out " in line)
        self.assertTrue(first.endswith("-live"), first)

    def test_a_ref_resolved_for_some_countries_resolves_only_what_is_missing(self) -> None:
        newer = self.commit("more")
        self.assertEqual(0, self.measure("--base", self.base, "--head", newer, "--countries", "pl", "--out",
                                         self.path("sub")).code)
        self.forget("calls")
        self.assertEqual(0, self.measure("--base", self.base, "--head", newer, "--countries", "pl,uk", "--out",
                                         self.path("sub2")).code)
        self.assertEqual([f"{newer} worker/IntegrationTest/runMain integration.IdentityResolveDump --full uk",
                          f"{newer} worker/IntegrationTest/runMain scripts.UnmatchedClustersRatchet --measure"],
                         [re.sub(r" --countries .*", "", re.sub(r" --(out|corpus-dir|fixtures) [^ ]+", "", c))
                          for c in self.calls() if c.startswith(newer)])

    def test_a_ref_another_run_is_resolving_is_waited_for_then_resolved(self) -> None:
        third = self.commit("third")
        holder = self.hold_lock(os.path.join(self.store, f"{third}.flock"))
        runner = self.start("locked", "--base", self.base, "--head", third, "--countries", "pl", "--out",
                            self.path("locked"), KINOWO_MEASURE_LOCAL_POLL="0.2")
        self.assertTrue(cli_harness.wait_until(lambda: "waiting for it" in self.text("locked.out")), self.text("locked.out"))
        self.assertIn(f"{third} is being resolved by pid {holder.pid}", self.text("locked.out"))
        self.assertEqual([], [c for c in self.calls() if c.startswith(third)])
        holder.kill()
        holder.wait()
        self.assertEqual(0, runner.wait(120), self.text("locked.out"))
        self.assertEqual(2, len([c for c in self.calls() if c.startswith(third)]))

    def test_a_ref_held_past_the_lock_deadline_is_given_up_on_naming_its_holder(self) -> None:
        fifth = self.commit("fifth")
        holder = self.hold_lock(os.path.join(self.store, f"{fifth}.flock"))
        runner = self.start("stuck", "--base", self.base, "--head", fifth, "--countries", "pl", "--out",
                            self.path("stuck"), KINOWO_MEASURE_LOCAL_POLL="0.2", KINOWO_MEASURE_LOCAL_LOCK_WAIT="1")
        code = runner.wait(60)
        holder.kill()
        holder.wait()
        self.assertNotEqual(0, code, self.text("stuck.out"))
        self.assertIn(f"is still being resolved by pid {holder.pid} after 1s", self.text("stuck.out"))

    def test_every_jvm_slot_busy_the_resolve_waits_for_one(self) -> None:
        fourth = self.commit("fourth")
        slot = self.path("cache", "jvm-slots", "1.flock")
        holder = self.hold_lock(slot)
        runner = self.start("slots", "--base", self.base, "--head", fourth, "--countries", "pl", "--out",
                            self.path("slots"), KINOWO_JVM_SLOTS="1", KINOWO_JVM_SLOT_POLL="0.2")
        self.assertTrue(cli_harness.wait_until(lambda: "JVM slots busy" in self.text("slots.out")), self.text("slots.out"))
        self.assertIn("[identity-measure-local] all 1 JVM slots busy", self.text("slots.out"))
        self.assertEqual([], [c for c in self.calls() if c.startswith(fourth)])
        holder.kill()
        holder.wait()
        self.assertEqual(0, runner.wait(120), self.text("slots.out"))
        self.assertTrue([c for c in self.calls() if c.startswith(fourth)])
        self.assertTrue(lock_free(slot))

    def test_a_resolve_waits_on_the_memory_gate(self) -> None:
        done = self.measure("--base", self.base, "--head", self.head, "--countries", "pl", "--out", self.path("gated"),
                            KINOWO_MEMORY_GATE="1", KINOWO_MEMORY_GATE_PROBE="echo 99",
                            KINOWO_MEMORY_GATE_FREE_PROBE="echo 0", KINOWO_MEMORY_GATE_MAX_WAIT="0")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("[identity-measure-local] memory gate: still 99 GB", done.output)

    def test_a_ratchet_dying_with_no_takes_is_a_setup_error_naming_it(self) -> None:
        # 2026-10-19: the ratchet died on a NoClassDefFoundError, sbt said [success], and the measure went on to NOT
        # LANDABLE with no reason given
        self.assertEqual(0, self.measure("--base", self.base, "--head", self.head, "--countries", "pl").code)
        sixth = self.commit("sixth")
        self.write("crash-ratchet", "")
        done = self.measure("--base", self.base, "--head", sixth, "--countries", "pl", "--out", self.path("crashed"))
        self.forget("crash-ratchet")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("the head did not resolve", done.output)
        self.assertIn("NoClassDefFoundError: modules/wiring/IdentityCutoverWiring$", done.output)
        self.assertNotIn("LANDABLE", done.output)
        done = self.measure("--base", self.base, "--head", sixth, "--countries", "pl", "--out", self.path("again"))
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.output, r"(?m)^LANDABLE \(local\)")

    def race(self, *args: str) -> Tuple[int, int]:
        """A run on recording 41 held in its first diff while one on 39 runs whole: both exits."""
        self.forget("read", "pause.reached")
        pause = self.path("pause")
        os.mkfifo(pause)
        first = self.start("race41", "--base", self.base, "--head", self.head, "--recording", "41", "--countries", "pl",
                           *args, STUB_PAUSE=pause)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(pause + ".reached")), self.text("race41.out"))
        second = self.measure("--base", self.base, "--head", self.head, "--recording", "39", "--countries", "pl", *args)
        fd = os.open(pause, os.O_WRONLY)
        os.unlink(pause)
        os.close(fd)
        return first.wait(120), second.code

    def test_one_base_head_on_two_recordings_at_once_each_reads_its_own(self) -> None:
        # 2026-10-21: runs sharing an out dir read each other's recording through its re-pointed links
        for args in ((), ("--out", self.path("one-out"))):
            self.assertEqual((0, 0), self.race(*args), self.text("race41.out"))
            read = self.text("read").splitlines()
            self.assertIn('{"recording": "41"} {"recording": "41"}', read)
            self.assertIn('{"recording": "39"} {"recording": "39"}', read)

    def test_a_half_dump_is_never_taken_for_a_finished_one(self) -> None:
        seventh = self.commit("seventh")
        self.write("crash-dump", "")
        done = self.measure("--base", self.base, "--head", seventh, "--countries", "pl", "--out", self.path("half"))
        self.forget("crash-dump", "calls")
        self.assertEqual(2, done.code, done.output)
        self.assertNotIn("LANDABLE", done.output)
        done = self.measure("--base", self.base, "--head", seventh, "--countries", "pl", "--out", self.path("half2"))
        self.assertEqual(0, done.code, done.output)
        self.assertTrue([c for c in self.calls()
                         if c.startswith(f"{seventh} worker/IntegrationTest/runMain integration.IdentityResolveDump")])

    def test_a_recording_with_no_laid_pair_is_a_setup_error(self) -> None:
        self.assertEqual(2, self.measure("--base", self.base, "--head", self.head, "--recording", "99").code)


if __name__ == "__main__":
    unittest.main()
