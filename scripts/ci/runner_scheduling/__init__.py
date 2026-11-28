"""ci-runner's movies runner scheduler, as a package (runner_scheduler.py, beside it, is the entry and says the rule):

  model       the job classes, lanes and labels, the Config, and the data a tick reads and decides
  charges     what admission charges: bitcashier's reserve, the memory and cores free, what our runners still owe
  jobs        the queue: demand per class, the lanes' queued jobs and their pins, the pace, the starved classes
  stops       the idle runners a tick stops
  admission   the decision (decide) over one tick's admission ledger
  machine     the one adapter: ci-runner's files, processes, clock and GitHub's API
  readings    the host's live readings, through the machine
  host        our runner instances: their state files, units and containers, through the machine
  github      GitHub's API: the queued jobs, through the machine
  loop        a tick, its log line, and the loop

Every module but machine is stdlib and its siblings alone: ci-runner fetches the scheduler's files and nothing else of
movies (bitcashier infra, nix/hosts/ci-runner/movies-runner-scheduler-pin.nix). These are the names its tests and the
lanes' shell tools read."""
from .admission import decide
from .charges import (builder_reserve_gib, cpu_charge, cpu_spare, memory_free_gib, reserved_builders, saturated,
                      share_cpus, whole_jobs)
from .github import GitHub
from .host import Host
from .jobs import demand_of, iso_seconds, lane_demand_of, oldest_waits, pace_seconds, queue_of, starved
from .machine import Machine
from .model import (CLASSES, CONSUMER_MARKER, EXPLORE_LANE, EXPLORE_MARKER, LABELS, LANES, LANES_MARKER, MEASURE_LANES,
                    PIN_PREFIX, STAGE_MARKER, Bitcashier, ClassCpu, Config, Instance, Queued, Readings)
from .readings import CpuMeter, CpuMeters, RunnerUsage, cpu_times, parse_cpu_list, read_readings

__all__ = ["decide", "builder_reserve_gib", "cpu_charge", "cpu_spare", "memory_free_gib", "reserved_builders", "saturated",
           "share_cpus", "whole_jobs", "GitHub", "Host", "demand_of", "iso_seconds", "lane_demand_of", "oldest_waits",
           "pace_seconds", "queue_of", "starved", "Machine", "CLASSES", "CONSUMER_MARKER", "EXPLORE_LANE",
           "EXPLORE_MARKER", "LABELS", "LANES", "LANES_MARKER", "MEASURE_LANES", "PIN_PREFIX", "STAGE_MARKER",
           "Bitcashier", "ClassCpu", "Config", "Instance", "Queued", "Readings", "CpuMeter", "CpuMeters", "RunnerUsage",
           "cpu_times", "parse_cpu_list", "read_readings"]
