"""scripts/lib/jvm_gate.py, jvm_memory.py and cpu_meter.py: a Python lock holder and a shell one (scripts/jvm-gate.sh)
exclude each other on the same files, a slot is held on descriptor 7 and handed off, the governor's admit is asked as
the shell gate asks it, the memory gate waits while memory is short, and the CPU meter sums a tree's samples.
Run: python3 scripts/lib/jvm_gate_test.py"""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest
from typing import Dict, List, Optional, Sequence

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cli_harness  # noqa: E402
from cpu_meter import CpuMeter, centis, sample  # noqa: E402
from jvm_gate import SLOT_FD, FileLock, Governor, Host, JvmGate, gov_job, lock_busy, whole  # noqa: E402
from jvm_memory import Clock, MemoryGate, SlotWait  # noqa: E402

GATE = os.path.join(cli_harness.ROOT, "scripts/jvm-gate.sh")


class FakeCommands:
    def __init__(self, admit: int = 0, probe: str = "agentctl governor: admit cores route width\n") -> None:
        self.calls: List[List[str]] = []
        self.admit, self.probe = admit, probe

    def status(self, argv: Sequence[str], pass_fds: Sequence[int] = (), quiet: bool = False,
               out_to_err: bool = False) -> int:
        self.calls.append(list(argv))
        return self.admit if "admit" in argv else 0

    def output(self, argv: Sequence[str]) -> Optional[str]:
        self.calls.append(list(argv))
        return self.probe


class Case(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory(prefix="jvm-gate-")
        self.work = os.path.realpath(self.dir.name)
        self.addCleanup(self.dir.cleanup)
        self.said: List[str] = []
        self.env: Dict[str, str] = {"KINOWO_GOVERNOR": "0", "KINOWO_JVM_SLOTS": "2", "KINOWO_MEMORY_GATE": "0"}
        self.addCleanup(self.close_slot)

    @staticmethod
    def close_slot() -> None:
        try:
            os.close(SLOT_FD)
        except OSError:
            pass

    def gate(self, commands: Optional[FakeCommands] = None, system: str = "Darwin") -> JvmGate:
        host = Host(self.env, os.getpid(), system, self.work, lambda: self.work, self.said.append)
        governor = Governor(host, commands or FakeCommands())
        memory = MemoryGate(self.env, Clock(lambda _s: None), self.said.append, lambda: False, system)
        slots = os.path.join(self.work, "slots")
        return JvmGate(host, governor, memory, SlotWait(memory, slots), slots)

    def shell(self, script: str, *args: str) -> "subprocess.Popen[str]":
        proc = subprocess.Popen(["bash", "-c", '. "$1"; shift; ' + script, "_", GATE, *args],
                                env=cli_harness.isolated_env(os.environ), stdout=subprocess.PIPE, text=True)
        self.addCleanup(lambda: (proc.kill(), proc.wait()))
        return proc


class LocksTest(Case):
    def test_a_python_holder_and_a_shell_one_exclude_each_other(self) -> None:
        path = os.path.join(self.work, "x.flock")
        mine = FileLock(path)
        self.assertTrue(mine.take(os.getpid()))
        self.assertEqual(f"{os.getpid()}", open(path).read().strip())
        probe = subprocess.run(["bash", "-c", '. "$1"; lock_busy "$2" && echo held || echo free', "_", GATE, path],
                               capture_output=True, text=True, env=cli_harness.isolated_env(os.environ))
        self.assertEqual("held", probe.stdout.strip())
        mine.give()
        self.assertEqual("", open(path).read())
        holder = self.shell('jvm_lock "$1" && echo took && exec sleep 600', path)
        self.assertEqual("took", holder.stdout.readline().strip() if holder.stdout else "")
        self.assertTrue(lock_busy(path))
        self.assertFalse(FileLock(path).take(os.getpid()))
        holder.kill()
        holder.wait()
        self.assertFalse(lock_busy(path))
        self.assertTrue(FileLock(path).take(os.getpid()))


class SlotsTest(Case):
    def test_slots_are_taken_on_descriptor_7_handed_off_and_freed(self) -> None:
        gate = self.gate()
        self.assertTrue(gate.slot_try())
        self.assertEqual(os.path.join(self.work, "slots/1.flock"), gate.slot)
        self.assertTrue(gate.riding())
        self.assertEqual(gate.slot, self.env["KINOWO_JVM_SLOT_HELD"])
        riding = subprocess.run(["bash", "-c", '. "$1"; jvm_slot_riding && echo riding', "_", GATE], pass_fds=(SLOT_FD,),
                                env={**cli_harness.isolated_env(os.environ), **self.env}, capture_output=True, text=True)
        self.assertEqual("riding", riding.stdout.strip(), "a shell child rides the Python holder's slot")
        other = self.gate()
        gate.hand_off(4242)
        self.assertEqual("4242", open(os.path.join(self.work, "slots/1.flock")).read().strip())
        self.assertNotIn("KINOWO_JVM_SLOT_HELD", self.env)
        self.assertFalse(lock_busy(os.path.join(self.work, "slots/1.flock")), "handed off to no live holder: freed")
        self.assertTrue(other.slot_try())
        other.release()
        self.assertFalse(lock_busy(other.slot or os.path.join(self.work, "slots/1.flock")))

    def test_a_slot_wait_goes_ahead_past_its_deadline_while_memory_is_not_short(self) -> None:
        self.env.update({"KINOWO_JVM_SLOTS": "1", "KINOWO_JVM_SLOT_MAX_WAIT": "0"})
        env = {**cli_harness.isolated_env(os.environ), "KINOWO_IDENTITY_CACHE": self.work, "KINOWO_JVM_SLOTS": "1"}
        holder = subprocess.Popen(["bash", "-c", '. "$1"; jvm_slot_try && echo took && exec sleep 600', "_", GATE],
                                  env=env, stdout=subprocess.PIPE, text=True)
        self.addCleanup(lambda: (holder.kill(), holder.wait()))
        self.assertEqual("took", holder.stdout.readline().strip() if holder.stdout else "")
        gate = self.gate()
        gate.slot_dir = gate.slot_wait.slot_dir = os.path.join(self.work, "jvm-slots")
        gate.slot_take("tool")
        self.assertEqual("", gate.slot)
        self.assertEqual(["[tool] every JVM slot still busy after 0s: starting anyway"], self.said)


class GovernorTest(Case):
    def test_a_ticket_is_admitted_as_the_shell_gate_asks_for_it(self) -> None:
        agentctl = os.path.join(self.work, "agentctl")
        with open(agentctl, "w") as f:
            f.write("#!/bin/sh\n")
        os.chmod(agentctl, 0o755)
        self.env.update({"KINOWO_GOVERNOR": "1", "KINOWO_GOVERNOR_BIN": agentctl, "AGENTCTL_GOV_DIR": self.work,
                         "KINOWO_GOV_CLASS": "check", "PWD": "/here"})
        commands = FakeCommands()
        gate = self.gate(commands)
        self.env["KINOWO_GOV_JOB"] = "keeper"
        gate.slot_take("sbt-warm")
        probe, admit = commands.calls
        self.assertEqual([agentctl, "gov", "probe"], probe)
        self.assertEqual(["gov", "admit", "--fd", "7", "--ticket", gate.slot, "--class", "check", "--mem", "5", "--label",
                          "sbt-warm", "--pid", str(os.getpid()), "--cwd", "/here", "--job", "keeper"], admit[1:])
        self.assertEqual(f"{agentctl}|admit cores route width", self.env["KINOWO_GOVERNOR_FEATURES"])
        self.assertTrue(gate.riding())
        gate.cpu_take("sbt-warm testOnly", "testOnly")
        self.assertEqual(2, len(commands.calls), "a process holding a ticket takes no second one")
        gate.release()
        self.env["AGENTCTL_GOV_DIR"] = os.path.join(self.work, "refused")
        refused = self.gate(FakeCommands(admit=1))
        refused.slot_take("tool")
        self.assertEqual(f"[tool] the governor ({agentctl} gov admit) failed: starting without a ticket", self.said[-1])
        self.assertEqual([], os.listdir(self.env["AGENTCTL_GOV_DIR"]), "a refused ticket's file removed")

    def test_jobs_cores_and_cpu_options(self) -> None:
        self.assertEqual(["keeper", "keeper", "what-if", "slice", "measure", "compile", "job"],
                         [gov_job({}, label) for label in ("sbt-warm", "keeper", "identity-what-if x", "identity-slice",
                                                           "identity-measure-local", "identity-tools", "other")])
        self.assertEqual("x", gov_job({"KINOWO_GOV_JOB": "x"}, "sbt-warm"))
        self.assertEqual([1, 1, 4], [whole("0.5"), whole("1.5"), whole("4")])
        self.assertEqual("-XX:ActiveProcessorCount=4", self.gate().cpu_options())
        self.assertEqual("", self.gate(system="Linux").cpu_options())
        self.env["KINOWO_JVM_CPUS"] = "0"
        self.assertEqual("", self.gate().cpu_options())


class MemoryTest(Case):
    def test_the_gate_waits_while_memory_is_short_and_says_so(self) -> None:
        marks = os.path.join(self.work, "polls")
        self.env.update({"KINOWO_MEMORY_GATE": "1", "KINOWO_MEMORY_GATE_FREE_PROBE": "echo 0", "KINOWO_MEMORY_GATE_POLL": "1",
                         "KINOWO_MEMORY_GATE_PROBE": f"echo x >> {marks}; [ $(wc -l < {marks}) -ge 3 ] && echo 1 || echo 30"})
        gate = self.gate()
        gate.memory.wait("t")
        self.assertTrue(self.said[0].startswith("[t] memory gate: 30 GB paged out, 0 GB free (over KINOWO_MEMORY_GATE_GB=20"))
        self.assertEqual("[t] memory gate: open after 2s", self.said[-1])


class MeterTest(unittest.TestCase):
    def test_a_trees_cpu_is_its_samples_growth(self) -> None:
        self.assertEqual([6012, 366012, 9006012], [centis("01:00.12"), centis("01:01:00.12"), centis("1-01:01:00.12")])
        listing = " 10  1 0:01.00\n 11 10 0:02.00\n 12 11 1:00.00\n 99  1 9:00.00\n"
        self.assertEqual({10: 100, 11: 200, 12: 6000}, sample([10], lambda: listing))
        samples = iter([{10: 100, 11: 200}, {10: 300, 11: 250, 12: 500}])
        meter = CpuMeter(lambda: [10], 60, lambda _p: True, lambda _roots: next(samples))
        meter.start()
        self.assertEqual(7, meter.stop())
        self.assertEqual(0, CpuMeter(lambda: [], 1, lambda _p: True).stop())


if __name__ == "__main__":
    unittest.main()
