#!/usr/bin/env python3
# script-test-reads: scripts/identity-slice scripts/identity_slice*.py scripts/identity-slice-tool-files.txt scripts/identity-tools.sh scripts/tool-conventions.sh scripts/jvm-gate.sh scripts/lib/*.py
# script-test-stress: served JVMs and layouts shared between concurrent calls (Order independence #94)
"""scripts/identity-slice --serve, black-box: the batch goes to the ref's kept JVM -- started once, then reused, which
writes the takes the diff reads -- and --emit-case through it; --stop-served stops only what is still a kept JVM; an
edit's kept JVM retires the one of the checkout's older state; one that never listens is stopped at its deadline. ONE
JVM, ONE SLOT: a kept JVM holds one of the machine's JVM slots (scripts/jvm-gate.sh) for its whole life through a keeper
beside it, which waits out the JVM's start and ends with it. Ported from identity-slice-test.sh; unchanged across the
swap to Python."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import unittest
from typing import Set

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_slice_cli_fixture import SliceCase, children, count, keeper_of  # noqa: E402
import cli_harness  # noqa: E402  (scripts/lib, on the path the fixture put it on)

KEY = ("de", "--recording", "777", "--key", "Kino|Lalka")


class ServeTest(SliceCase):
    def test_serve_starts_one_kept_jvm_per_ref_and_recording_and_sends_it_each_batch(self) -> None:
        self.write("b.tsv", "Kino\tLalka\n")
        first = self.slice(*KEY, "--ref", "HEAD", "--serve")
        self.slice("de", "--recording", "777", "--batch", self.path("b.tsv"), "--ref", "HEAD", "--serve", "--out",
                   self.path("served.tsv"))
        calls = self.calls()
        self.assertEqual((1, 2), (calls.count(f"--serve {self.cache}/serve/"), count("^request ", calls)), calls)
        # ...each request carrying the batch's own arguments and where to write its takes, its output streamed back
        self.assertEqual(1, count(f"^request --labels .*--batch {self.work}/b.tsv", calls), calls)
        self.assertEqual(1, self.read("served.tsv").count("Kino|Lalka"))
        self.assertEqual("decided by the kept JVM\n", first.stdout, first.output)

    def test_stop_served_stops_every_kept_jvm_and_the_next_serve_starts_another(self) -> None:
        self.slice(*KEY, "--ref", "HEAD", "--serve")
        self.stop_served()
        self.assertEqual([], self.pid_files())
        self.clear_calls()
        self.slice(*KEY, "--ref", "HEAD", "--serve")
        self.assertEqual(1, self.calls().count(f"--serve {self.cache}/serve/"), self.err)

    def test_stop_served_kills_only_a_pid_still_a_kept_jvm_and_forgets_the_file(self) -> None:
        # a pid file outlives its JVM, and the pid comes back as someone else's process
        stranger = subprocess.Popen(["sleep", "30"])
        self.addCleanup(lambda: (stranger.kill(), stranger.wait()))
        self.write(os.path.join(self.cache, "serve/stale.pid"), f"{stranger.pid}\n")
        self.stop_served()
        self.assertEqual((None, False), (stranger.poll(), os.path.exists(os.path.join(self.cache, "serve/stale.pid"))))

    def test_emit_case_starts_a_kept_jvm_and_asks_it_for_the_case(self) -> None:
        self.slice(*KEY, "--ref", self.head, "--emit-case")
        calls = self.calls()
        cases = f"{self.repo_dir}/test/resources/fixtures/identity-slices"
        self.assertEqual((1, 1), (calls.count(f"--serve {self.cache}/serve/"), count(f"^request .*--emit-case {cases}",
                                                                                         calls)), calls)

    def test_an_edits_kept_jvm_stops_the_one_of_the_checkouts_older_state(self) -> None:
        # an edit of this checkout's code is a new ref key: the older state's kept JVM is never left holding 6 GB
        self.slice(*KEY, "--serve")
        self.write("repo/identity/src/main/Resolver.scala", "// edited\n")
        self.slice(*KEY, "--serve")
        self.assertEqual((1, 1), (self.err.count("stopped the kept JVM of an older state"), len(self.pid_files())),
                         self.err)

    def test_a_kept_jvm_that_never_listens_is_stopped_at_the_deadline_and_the_call_fails(self) -> None:
        # the stub JVM never listens and never ends on its own: the call fails only by its start deadline, and stops it
        hung = self.write("serve-hung", "")
        done = self.slice(*KEY, "--ref", "HEAD~1", "--serve", env={"STUB_SERVE_HANG": hung,
                                                                    "KINOWO_IDENTITY_SLICE_START_SECONDS": "2"})
        self.assertEqual((1, 1, 0, 0), (done.code, self.err.count("did not listen within 2s"), len(self.pid_files()),
                                        cli_harness.still_running(hung)), done.output)


class SlotTest(SliceCase):
    """ONE JVM, ONE SLOT, as sbt-warm's kept server: uncounted, kept JVMs were up to 6 GB each the slots never saw."""

    def setUp(self) -> None:
        super().setUp()
        self.slot = os.path.join(self.cache, "jvm-slots/1.flock")

    def slotted(self, *args: str, **env: str) -> cli_harness.Done:
        return self.slice(*KEY, *args, env={"KINOWO_JVM_SLOTS": "1", **env})

    def keeper(self) -> int:
        keepers = keeper_of(self.served_pid())
        self.assertEqual(1, len(keepers), keepers)
        return keepers[0]

    def assert_gone(self, pid: int) -> None:
        self.assertTrue(cli_harness.wait_until(lambda: not cli_harness.alive(pid)), f"pid {pid} is still running")

    def test_keeper_of_names_the_keeper_alone_not_its_polls_subshell(self) -> None:
        # a keeper mid-poll: its subshell carries its command line too. It "watches" this run's own pid, which no other
        # run's fake keeper can share (92 in 100 runs saw each other's with a fixed number, 2026-11-16)
        script = self.write("fake-keeper.sh", "( sleep 30; : ) &\nwait\n")
        fake = subprocess.Popen(["bash", script, "--keep-served", str(os.getpid())], start_new_session=True)
        self.addCleanup(lambda: (os.killpg(fake.pid, signal.SIGKILL), fake.wait()))
        self.assertTrue(cli_harness.wait_until(lambda: bool(children(fake.pid, "bash"))))
        self.assertEqual([fake.pid], keeper_of(os.getpid()))

    def test_a_kept_jvm_holds_a_slot_through_its_keeper_until_it_is_gone(self) -> None:
        self.slotted("--ref", "HEAD", "--serve")
        keeper = self.keeper()
        self.assertEqual(("held", str(keeper), True), (self.lock_state(self.slot), self.content(self.slot),
                                                       cli_harness.alive(keeper)))
        # ...and stops after 10 idle minutes by default, as sbt-warm's kept server
        self.assertEqual(1, count("--idle-minutes 10$", self.calls()))
        # a call reaching the live kept JVM takes no second slot: with the only slot held, it never waits
        done = self.slotted("--ref", "HEAD", "--serve", KINOWO_JVM_SLOT_MAX_WAIT="0")
        self.assertEqual(("decided by the kept JVM\n", 0), (done.stdout, self.err.count("JVM slot")), done.output)
        self.stop_served()
        self.assert_gone(keeper)
        self.assertEqual("free", self.lock_state(self.slot))

    def test_a_keeper_started_without_a_slot_takes_the_first_that_frees(self) -> None:
        holder = self.hold_lock(self.slot)
        self.slotted("--ref", "HEAD", "--serve", KINOWO_JVM_SLOT_MAX_WAIT="0", KINOWO_IDENTITY_SLICE_IDLE_MINUTES="3")
        keeper = self.keeper()
        holder.kill()
        holder.wait()
        self.assertTrue(cli_harness.wait_until(lambda: self.content(self.slot) == str(keeper)), self.content(self.slot))
        self.assertEqual("held", self.lock_state(self.slot))
        self.assertEqual(1, count("--idle-minutes 3$", self.calls()))
        self.stop_served()
        self.assert_gone(keeper)

    def test_a_keeper_waits_out_its_jvms_start_then_holds_on_until_it_is_gone(self) -> None:
        # The keeper starts the moment the JVM's process does, often before that process has exec'd java: a pid not
        # YET an IdentitySlice is the JVM still starting, not one gone (under load the keeper ended at once)
        go = self.path("exec-now")
        jvm = subprocess.Popen(["bash", "-c", 'until [ -e "$0" ]; do sleep 0.05; done; '
                                'exec perl -e "sleep 60" scripts.IdentitySlice', go])
        self.addCleanup(lambda: (jvm.kill(), jvm.wait()))
        keeper = subprocess.Popen([self.tool, "--keep-served", str(jvm.pid)], env=self.tool_env(),
                                  stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.addCleanup(lambda: (keeper.kill(), keeper.wait()))
        # a poll of the JVM not yet exec'd has passed (the keeper sleeps between polls), and the keeper lives on
        polled = self.polls_passed(keeper.pid, set(), 1)
        open(go, "w").close()
        self.assertTrue(cli_harness.wait_until(lambda: b"scripts.IdentitySlice" in self.args_of(jvm.pid)))
        # two polls begun after the JVM was seen exec'd: the second's look at it came after the exec
        self.polls_passed(keeper.pid, polled | children(keeper.pid, "sleep"), 2)
        self.assertIsNone(keeper.poll(), "the keeper ended while its JVM runs")
        jvm.kill()
        jvm.wait()
        self.assertTrue(cli_harness.wait_until(lambda: keeper.poll() is not None), "the keeper outlived its JVM")

    def polls_passed(self, keeper: int, before: Set[int], polls: int) -> Set[int]:
        """Awaits `polls` polls of the keeper's besides those of `before`, each a `sleep` between two looks at the JVM."""
        seen: Set[int] = set()

        def new_polls() -> bool:
            seen.update(children(keeper, "sleep"))
            return len(seen - before) >= polls
        self.assertTrue(cli_harness.wait_until(new_polls), "the keeper stopped polling")
        return before | seen

    @staticmethod
    def args_of(pid: int) -> bytes:
        return subprocess.run(["ps", "-o", "args=", "-p", str(pid)], capture_output=True).stdout


if __name__ == "__main__":
    unittest.main()
