"""Our runner instances on ci-runner (Host): <state>/<slot>.env says which class, slice and labels a slot was started
for, the movies-runner@<slot> unit whether it lives, its container whether it runs a job and what it uses -- every
probe through the Machine it is given, what a probe's answer means here (runner_scheduler.py's docstring, THE
RUNNERS)."""
from __future__ import annotations

import os
import sys
from typing import Dict, List, Optional, Set, Tuple

from .machine import Machine
from .model import (CONSUMER_MARKER, EXPLORE_MARKER, LABELS, LANES_MARKER, PIN_PREFIX, STAGE_MARKER, Config,
                    Instance)
from .readings import RunnerUsage

LIVE_STATES = ("active", "activating", "reloading")


def parse_env(text: str) -> Dict[str, str]:
    """A <slot>.env file's KEY=value lines."""
    return dict(line.split("=", 1) for line in text.splitlines() if "=" in line)


def slice_class_of(written: Dict[str, str], cls: str) -> str:
    """The class whose slice the runner was started in (a borrowed check's is the measure slice), from RUNNER_SLICE
    (written as ci-movies-<class>.slice); empty for its own."""
    slice_class = written.get("RUNNER_SLICE", "")[len("ci-movies-"):-len(".slice")] or cls
    return "" if slice_class == cls else slice_class


def registered(written: Dict[str, str], cls: str) -> Tuple[str, str]:
    """(label, pin) the runner registered: the first label (a measure lane's for a measurement; a light runner's markers
    come after), empty when it is its class's own, and the run's pin label beside it (empty: none)."""
    labels = written.get("RUNNER_LABELS", "").split(",")
    label = "" if labels[0] == LABELS.get(cls) else labels[0]
    return label, next((l for l in labels if l.startswith(PIN_PREFIX)), "")


def labels_of(cls: str, label: str, pin: str) -> str:
    """The RUNNER_LABELS a start registers: its lane's label (its class's when empty), its run's pin, and on a light
    runner the markers saying this scheduler serves the lanes (LANES_MARKER and the explorer, stage and consumer
    lanes')."""
    labels = (label or LABELS[cls]) + (f",{pin}" if pin else "")
    return labels + (f",{LANES_MARKER},{EXPLORE_MARKER},{STAGE_MARKER},{CONSUMER_MARKER}" if cls == "light" else "")


def runs_worker(ps_output: str) -> bool:
    """A Runner.Worker among `docker top -eo pid,comm`'s rows."""
    return any(line.split()[-1:] == ["Runner.Worker"] for line in ps_output.splitlines())


class Host:
    """Our runner instances: <state>/<slot>.env says which class a slot was started for; the unit says if it lives."""

    def __init__(self, machine: Machine, cfg: Config):
        self.machine = machine
        self.cfg = cfg
        self.idle_since: Dict[int, float] = {}
        self.usage = RunnerUsage(machine, cfg.cpu_window_seconds)

    def unit(self, slot: int) -> str:
        return f"{self.cfg.unit}@{slot}.service"

    def instances(self, now: float) -> List[Instance]:
        out: List[Instance] = []
        seen: Set[Optional[str]] = set()
        for env in sorted(self.machine.glob(os.path.join(self.cfg.state_dir, "*.env"))):
            out += self.instance(env, now, seen)
        self.usage.keep_only(seen, now)
        return out

    def instance(self, env: str, now: float, seen: Set[Optional[str]]) -> List[Instance]:
        """The slot's instance (its container's cgroup added to `seen`); none when its unit has ended, whose .env then
        goes: its one job is done (or it failed to start), the slot is free."""
        slot = int(os.path.basename(env)[:-4])
        state = self.machine.run(["systemctl", "is-active", self.unit(slot)]).stdout.strip()
        if state not in LIVE_STATES:
            self.machine.remove(env)
            self.idle_since.pop(slot, None)
            return []
        written = parse_env(self.machine.read(env) or "")
        cls = written.get("RUNNER_CLASS", "light")
        slice_class = slice_class_of(written, cls)
        # Still starting (ExecStartPre mints its registration token; no container to probe yet): no job yet, so it
        # waits for one -- read busy, the next tick started a second runner for the same queued job.
        busy = state != "activating" and self.runs_job(slot)
        started = self.machine.modified(env)
        since = self.idle_mark(slot, busy, started, now)
        cgroup = None if state == "activating" else self.container_cgroup(slot, slice_class or cls)
        seen.add(cgroup)
        label, pin = registered(written, cls)
        return [Instance(slot, cls, busy, since, started, *self.container_usage(cgroup, now),
                         slice_class=slice_class, label=label, pin=pin)]

    def idle_mark(self, slot: int, busy: bool, started: float, now: float) -> float:
        """When the slot was last seen not busy: now while it is busy, else since it was first seen idle (its start)."""
        if busy:
            self.idle_since.pop(slot, None)
            return now
        return self.idle_since.setdefault(slot, started)

    def container_usage(self, cgroup: Optional[str], now: float) -> Tuple[Optional[float], Optional[float], Optional[float]]:
        return self.usage.read(cgroup, now) if cgroup else (None, None, None)

    def container_cgroup(self, slot: int, slice_class: str) -> Optional[str]:
        """The slot's container's own cgroup (docker's systemd driver: <the slice it runs in>/docker-<id>.scope); None
        while it has no container."""
        out = self.machine.run(["docker", "inspect", "-f", "{{.Id}}", f"{self.cfg.unit}-{slot}"])
        cid = out.stdout.strip()
        if out.returncode != 0 or not cid:
            return None
        return os.path.join(self.cfg.class_cgroup.format(cls=slice_class), f"docker-{cid}.scope")

    def start(self, slot: int, cls: str, slice_class: str = "", label: str = "", pin: str = "") -> None:
        """Start the slot for one job of `cls` (`label`, one of its lanes'; empty: its class's; `pin`, a run's pin label
        it registers beside it), in `slice_class`'s slice (its own when empty). A light runner also says it serves the
        measure lanes (LANES_MARKER)."""
        self.machine.write(os.path.join(self.cfg.state_dir, f"{slot}.env"),
                           f"RUNNER_CLASS={cls}\nRUNNER_LABELS={labels_of(cls, label, pin)}\n"
                           f"RUNNER_SLICE=ci-movies-{slice_class or cls}.slice\n")
        self.machine.run(["systemctl", "start", "--no-block", self.unit(slot)])

    def runs_job(self, slot: int) -> bool:
        """A Runner.Worker in the slot's container -- and True when the probe cannot tell: a runner is never stopped on
        doubt. `docker top` keeps the container's rows by ps's PID column, so the options must name `pid` (`-eo comm`
        alone always failed, every runner read idle, and #1005's eight convergence rows were stopped mid-job)."""
        top = self.machine.run(["docker", "top", f"{self.cfg.unit}-{slot}", "-eo", "pid,comm"])
        if top.returncode != 0:
            print(f"slot {slot}: docker top failed ({top.returncode}): {top.stderr.strip()} -- read as busy",
                  file=sys.stderr, flush=True)
            return True
        return runs_worker(top.stdout)

    def stop(self, slot: int) -> None:
        # Never one that took a job since the tick looked: check again right before.
        if self.runs_job(slot):
            return
        self.machine.run(["systemctl", "stop", "--no-block", self.unit(slot)])
