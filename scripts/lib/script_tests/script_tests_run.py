"""A RUN OF THE SELECTED SELF-TESTS (scripts/run-script-tests, after its selection): its width under THE GOVERNOR (one
ticket charged min(--jobs, tests) cores plus each wide test's extra, or the narrower grant; none under a ticket
already), STRESS's copies and CPU burners, the pool, THE TREE IS LEFT AS FOUND, the report and the times kept."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
from dataclasses import dataclass
from typing import Callable, Dict, List, MutableMapping, Optional, Protocol, Sequence, Tuple, cast

from governor import gov_queue
from processes import machine
from script_tests.select.script_tests_found import declared_requirement, declared_width
from script_tests.script_tests_pool import Plan, Pool, Tools, Widening
from script_tests.script_tests_report import Report, floor_block, heavy_hint, keep_times, longest_first
from script_tests.select.script_tests_tree import Attribution, Leftovers, Window, tree_state

from script_tests.select.script_tests_changed import Git
def provision(root: str, tests: Sequence[str], venv_tool: str,
              run: Callable[[Sequence[str]], Tuple[int, str]]) -> Tuple[Dict[str, str], List[str]]:
    """THE PYTHON PACKAGES A SELF-TEST NEEDS: a test saying `# script-test-requires: <requirements.txt> <module>` runs
    under a python3 that imports <module> -- CI's own when it does already, else the pinned venv scripts/python-venv
    builds once. Returns the directory to put first on each such test's PATH (`run` is the venv tool's `argv ->
    (status, stdout)`) and one refusal for each package that cannot be had, naming the module and the install command:
    a test that skipped itself for want of numpy passed on the laptop and failed in CI (2026-11-28)."""
    bins: Dict[str, str] = {}
    refusals: List[str] = []
    for path in tests:
        need = declared_requirement(os.path.join(root, path))
        if need is None:
            continue
        code, out = run([venv_tool, os.path.join(root, need.requirements), need.module])
        if code == 0 and out.strip():
            bins[path] = out.strip().splitlines()[-1]
        else:
            refusals.append(f"run-script-tests: {path} needs the python module {need.module}, which python3 lacks: "
                            f"install it with `{venv_tool} {need.requirements} {need.module}` "
                            f"(a pinned venv, built once) and run again")
    return bins, refusals


BURNER = 'while kill -0 "$1" 2> /dev/null; do i=0; while [ "$i" -lt 20000 ]; do i=$((i + 1)); done; done'


class Gate(Protocol):
    """THE GOVERNOR as a run sees it (jvm_gate.JvmGate): whether one governs here, the slot this run took, and the
    governed width of a fan-out."""
    slot: str

    def governs(self) -> bool: ...

    def width(self, label: str, job: str, want: int, mem: str = "1", fixed: bool = False) -> int: ...

    def governor_output(self, args: Sequence[str]) -> Optional[str]: ...

    def gov_class(self, label: str) -> str: ...

    def release(self) -> None: ...

    def core_try(self, label: str, job: str) -> Optional[Tuple[int, str]]: ...

    def core_give(self, held: Tuple[int, str]) -> None: ...


@dataclass(frozen=True)
class Settings:
    jobs: int
    jobs_given: bool
    stress: int
    burn: str
    timeout: int
    stall: int
    log_dir: str
    durations: str
    tree_check: bool
    floor_bash: str  # the floor's findings' bash, for the line saying how many


@dataclass(frozen=True)
class Width:
    jobs: int
    test_width: Optional[int] = None
    wide_tests: bool = False
    budget: int = 0  # the cores the grant holds when wide tests share it (0: the pool is just --jobs wide)
    more: int = 0  # the cores the run asked for beyond its grant: taken later, as the governor frees them


def cores() -> int:
    code, out = machine.output(["getconf", "_NPROCESSORS_ONLN"])
    return int(out.strip()) if code == 0 and out.strip().isdigit() else 0


def queue_seconds(env: MutableMapping[str, str]) -> float:
    """How often a waiting run says where it stands in the governor's queue (KINOWO_GOV_QUEUE_SECONDS, 30)."""
    try:
        return max(float(env.get("KINOWO_GOV_QUEUE_SECONDS") or 30), 0.05)
    except ValueError:
        return 30.0


def govern(gate: Optional[Gate], env: MutableMapping[str, str], root: str, jobs: int, tests: Sequence[str]) -> Width:
    """THE GOVERNED FAN-OUT's width for min(--jobs, tests): each test then runs one core of the ticket (its declared
    width when the run's own ticket counts it), and sees no ticket of the run's."""
    want = min(jobs, len(tests))
    if want < 1 or gate is None or not gate.governs():
        return Width(jobs)
    widths = [declared_width(os.path.join(root, t)) for t in tests]
    extra = sum(w - 1 for w in widths)
    if want + extra > cores():
        extra = 0
    with gov_queue.reporting("run-script-tests", lambda args: gate.governor_output(args), os.getpid(),
                             gate.gov_class("run-script-tests"), lambda line: print(line, file=sys.stderr, flush=True),
                             queue_seconds(env)):
        granted = gate.width("run-script-tests", "script-tests", want + extra, "2")
    env.pop("KINOWO_JVM_SLOT_HELD", None)
    return within(jobs, want, extra, granted, bool(gate.slot), max(widths, default=1))


def within(jobs: int, want: int, extra: int, granted: int, own_ticket: bool, widest: int = 1) -> Width:
    """THE GRANTED WIDTH: a grant that holds the widest wide test is a BUDGET of cores the pool spends on what runs at
    once (a wide test its declared width, any other one core, never more than --jobs tests): the old rule took every
    wide test's extra off the pool's width up front, so 6 cores of a 7-core grant left the pool ONE wide while the
    wide tests ran a minute each (2026-11-27: 8 tests, 13 minutes, one at a time). A grant narrower than the widest
    test runs every test one core wide."""
    if own_ticket and extra > 0 and widest <= granted:
        return Width(min(jobs, want), 1, True, granted, max(0, want + extra - granted))
    return Width(granted if granted < want else jobs, 1, False, 0, max(0, want - granted) if own_ticket else 0)


def widening(gate: Gate, plan: Plan) -> Optional[Widening]:
    """The run's wish for more cores than it was granted (THE GRANTED WIDTH), asked of the governor as they free."""
    if plan.more <= 0:
        return None

    def take() -> Optional[object]:
        return gate.core_try("run-script-tests", "script-tests")

    def give(held: object) -> None:
        gate.core_give(cast(Tuple[int, str], held))
    return Widening(take, give, plan.more, machine.monotonic)


def expanded(paths: Sequence[str], stress: int) -> Tuple[List[str], List[str]]:
    """Each test once, or STRESS's N copies of each, with each run's label."""
    if not stress:
        return list(paths), [""] * len(paths)
    runs = [(p, f" (run {r}/{stress})") for r in range(1, stress + 1) for p in paths]
    return [p for p, _ in runs], [label for _, label in runs]


def pool_shape(plan: Plan) -> str:
    """How wide the pool runs, said at the start: `8 at a time`, with a budget `3 at a time, within 5 cores`."""
    return f"{plan.jobs} at a time" + (f", within {plan.budget} cores" if plan.budget else "")


def burners(count: int) -> List["subprocess.Popen[bytes]"]:
    """STRESS's load: busy loops, each ending once this run is gone (a killed run strands none)."""
    return [machine.spawn(["bash", "-c", BURNER, "run-script-tests-burner", str(os.getpid())],
                          machine.Ran(out=subprocess.DEVNULL, err=subprocess.DEVNULL)) for _ in range(count)]


class Run:
    """One run of the selected tests, from start to its summary line: its exit status."""

    def __init__(self, s: Settings, pool: Pool, git: Git, sleep: Callable[[float], None]) -> None:
        self.s, self.pool, self.git, self.sleep = s, pool, git, sleep
        plan = pool.plan
        self.report = Report(plan.paths, plan.runs, plan.floor_tests, s.log_dir, s.stress,
                             lambda: machine.local_text("%Y%m%d-%H%M%S", machine.wall()))

    def tree_check_on(self) -> bool:
        root = self.pool.plan.root
        return (self.s.tree_check and not self.s.stress and os.access(root, os.W_OK)
                and self.git(["rev-parse", "--is-inside-work-tree"])[1].strip() == "true")

    def run(self, tests: int, changed: bool, heavy: Sequence[str], findings: Sequence[str]) -> int:
        plan, started = self.pool.plan, machine.monotonic()
        print(f"\033[36m▸\033[0m {len(plan.paths)} script test(s), {pool_shape(plan)}", flush=True)
        load = []
        if self.s.stress:
            burn = int(self.s.burn) if self.s.burn else (cores() or 2)
            print(f"  stress: each of {tests} test(s) {self.s.stress} times at once, beside {burn} CPU burner(s)",
                  flush=True)
            load = burners(burn)
        checked = self.tree_check_on()
        before = tree_state(self.git) if checked else []
        self.pool.run_all(lambda i, result: self.report.add(i, result, self.pool.file(i, "log")))
        for burner in load:
            machine.send(burner.pid, signal.SIGTERM)
            burner.wait()
        if checked:
            self.attribute(before)
        keep_times(self.s.durations, self.report.seconds)
        return self.summary(tests, changed, heavy, findings, started)

    def attribute(self, before: Sequence[str]) -> None:
        results, plan = self.pool.results, self.pool.plan
        windows = {i: Window(r.start, r.end, r.seconds) for i, r in results.items()}
        check = Attribution(plan.root, plan.scratch, self.pool.run_alone, self.sleep, machine.wall)
        left = check.attribute(before, tree_state(self.git), windows)
        for index in sorted(left.polluted):
            self.report.pollute(plan.paths[index], left.polluted[index])
        say_unclaimed(left, plan.paths)

    def summary(self, tests: int, changed: bool, heavy: Sequence[str], findings: Sequence[str], started: float) -> int:
        lines = self.report.table() + self.report.failures + [""]
        if self.s.stress:
            lines += self.report.stress_verdicts(tests)
        elif changed and heavy:
            lines.append(heavy_hint(heavy))
        lines += floor_block(findings, self.s.floor_bash)
        took = int(machine.monotonic() - started)
        total = len(self.pool.plan.paths)
        if self.report.failed:
            lines.append(f"{self.report.failed} of {total} failed ({took}s)")
        else:
            lines.append(f"all {total} passed ({took}s)")
        print("\n".join(lines), flush=True)
        return 1 if self.report.failed or findings else 0


def say_unclaimed(left: Leftovers, paths: Sequence[str]) -> None:
    if not left.unclaimed:
        return
    print("\nrun-script-tests: left in the checkout during the run, and made again by no test run alone (an edit "
          "beside the run?), not failing it:")
    for line, candidates in left.unclaimed:
        print(f"    {line}   (running then: {''.join(paths[c] + ' ' for c in candidates)})")


def plan_run(root: str, scratch: str, selected: Sequence[str], s: Settings, width: Width) -> Tuple[Plan, int]:
    """The plan of a run (the longest first, STRESS's copies) and how many tests it runs."""
    ordered = longest_first(root, selected, s.durations)
    paths, runs = expanded(ordered, s.stress)
    jobs = width.jobs if not s.stress or s.jobs_given else len(paths)
    return Plan(root, scratch, paths, runs, jobs, s.timeout, s.stall, test_width=width.test_width,
                wide_tests=width.wide_tests, budget=width.budget, more=width.more), len(ordered)


def tools(lib: str, scripts: str) -> Tools:
    return Tools(os.path.join(lib, "processes", "tool_bounded.py"), os.path.join(scripts, "stall_watch.py"),
                 os.path.join(scripts, "stall-report.sh"))
