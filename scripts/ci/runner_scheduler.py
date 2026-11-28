#!/usr/bin/env python3
"""CI-RUNNER'S MOVIES RUNNER SCHEDULER (the owner, 2026-11-06: "make movies runs take all the ci-runner runners when
bitcashier is not using them"). ci-runner is bitcashier's CI host; movies borrows what bitcashier leaves idle.

WHAT IT DECIDES, every tick (30 s): how many movies runners may exist now, and which job class each new one serves.

  * BITCASHIER ALWAYS WINS. Its 12 builders and 6 deployers are persistent runners on the host (default.nix
    `ghRunners`); a bitcashier job waits only when every runner of its kind is busy. So while every builder or every
    deployer is busy (bitcashier has, or may have, queued work beyond its free runners) NO check or measurement runner
    is started, and the idle ones are stopped. Movies jobs already running finish, under ci-movies.slice's lower CPU
    weight. Only the light lane (below) still admits: a 1 GiB, 1-core job holds no bitcashier job back.
  * ADMISSION IS WHAT IS FREE NOW, memory AND CPU (the owner, 2026-11-07: "fix the schedule so runners don't stay
    idle" ... "don't let cpu be underutilized"). The fixed worst-case budgets this replaced (8 GiB per busy bitcashier
    job, 16 GiB per movies job in its slice) held movies at 3 runners with 58 GiB free. Every tick reads, live:
      - MEMORY: the tighter of the host's MemAvailable and ci.slice's headroom (memory.max less its working set:
        memory.current without inactive page cache, which the kernel takes back first), less a reserve for the
        bitcashier jobs that may start before the next reading counts them, less `memory_margin_gib`, must hold the
        new job's class's `job_expected_gib` (a measurement 11 GiB, a landing check 10, the container's --memory=16g
        bounding the worst one; a light job 1).
        The builder reserve is `builder_burst` idle builders (or fewer, when fewer are idle), each at what a BUSY
        builder holds now: the median working set of the builder units running a job, at least
        `per_idle_builder_gib` (none busy: that floor) and at most `per_busy_builder_cap_gib`. Not every idle builder
        at full size: on 2026-11-07 13:30Z one busy builder "held" the whole ci-check.slice (the idle runners' own
        processes and caches) and twelve times that left room 0 with 87 GiB free and 10 of 32 cores busy -- the
        quieter bitcashier was, the less movies ran. A bitcashier job queues only once every runner of its kind is
        busy (saturated, below), so bitcashier's queue needs no reserve of its own; a burst beyond `builder_burst`
        is caught by the next tick (idle movies runners stop) and, under real pressure, by the kernel: movies'
        containers run at oom_score_adj 1000 (bitcashier infra, movies-runners.nix), so an OOM kill takes movies
        work, never bitcashier's, and bitcashier's tier slices keep their memory.low. An idle deployer reserves
        `per_idle_deployer_gib`.
      - THE CLASS'S SLICE likewise: its memory.max less its working set must hold the margin and the new job. Verify
        scrape recording #14 ran nine measurements in a 48 GiB slice, and on 2026-11-07 07:40-08:22Z six-then-nine did:
        its OOM killer took 18 processes (exit 137).
      - THE SLICE'S CAPS (the owner, 2026-11-08: "cap = charge"): a limited slice never holds more of its class's
        runners than its memory.max fits containers at their --memory (`container_gib`, 16 GiB: every movies job's peak
        fits it -- the United States' convergence rows 13-14 GiB, every other country's rows at most 11, a landing check
        9, over ci-runner's journal 2026-11-07..08 -- and so does the largest heap on the label, identity-live-drift's
        11g). The working set alone let settled measurements at 7 GiB admit more than the slice holds at their peaks;
        with what a job may take charged, the slice's OOM killer cannot fire on movies' own growth.
      - CPU: the busy cores over `cpu_window_seconds` (/proc/stat) OF THE CPUS THE CLASS'S SLICE RUNS ON (its
        cpuset.cpus.effective; unread, the host's), up to `cpu_start_below` of them (`light_cpu_start_below` for a
        light job), must hold a job of the class at the cores it CAN BURN, `job_cpus` (the JVM's
        ActiveProcessorCount=4 of a check or measurement container; 1 for a light job) -- and every SETTLING runner
        of ours on those CPUs is charged the same cap, less what the busy reading already holds of it (cpu.stat: its
        CPU time over the window's span). Charging new jobs what jobs USED (their median, 2026-11-07..09) fed back
        on itself: on 11-09 20:32-20:34Z a burst of drift rows was charged the 1.3 cores contended measurements had
        shown, six started beside ~11 cores of other load on the measure cpuset's 20, the CPUs read 20/20 within six
        minutes, and a UK drift row took 900 s (527 s on a quiet ci-runner, 375 s hosted). At the cap that burst
        starts one. A SETTLED runner is in the reading (its window's cores); of the settled ones only the largest
        rest of a cap is charged on top, for one job ramping up between readings (cpu_owed). Charging every settled
        job its whole cap (2026-11-10..16) held checks at 2 and measurements at 3-4 while their cpusets read half
        idle (2026-11-15/16: checks 2.8 cores at the median, measurements 2.2, the measure cpuset never past 15 of
        20) -- the owner, 2026-11-16: "allow even more in parallel as long as mem and cpu are not maxed out". CPU,
        unlike memory, is not cap = charge: an overcommitted core slows movies' jobs (bitcashier's slices outweigh
        ours), it kills none. On a saturated host another job only slows every job for no throughput, so nothing
        starts past cpu_start_below of the live reading. Per cpuset, not
        per host (the owner, 2026-11-08): measurements run on CCD1 and two CCD0 cores, checks on the rest of CCD0
        (bitcashier infra), and the host-wide reading both packed nine and ten measurements onto CCD1's 16 threads
        beside an idle CCD0 (2026-11-08 04:36-06:00Z, ~1.8 cores each: Germany's drift rows 36-51 min, 14-15 with
        three) and held them back for bitcashier's load on CPUs they cannot use. Classes on disjoint CPUs never
        spend each other's cores, so the check lane's cores are kept from measurements only where they share CPUs.
    A runner of ours that has not fully shown its memory and CPU yet -- one waiting for its job, or started within
    `settle_seconds` -- is charged what it has NOT shown yet: its class's expected size less its container's working
    set (and its cores' cap less what it showed). So a queue never starts a burst into memory
    that only looked free, and what a young runner already holds is not counted twice (15:46Z: three runners 3-4 min
    old were charged 11 GiB and 4 cores each beside what the readings already had of them: room 0 on 13 of 32
    cores). The memory margin is each reading's once, never per job. The log line names each term (`held by`).
    A job starts only when all three hold it, never past its slots; when memory runs short of what our waiting runners
    would take (bitcashier started work), the idle ones go, lowest class first.
  * PRIORITY between movies' own jobs: landing checks (`movies-check`), then measurements (`movies-measure`), in the
    `max_slots` slots. A runner is started for a QUEUED job of a class only (ephemeral, one job, the label of that
    class alone), the highest class first, so a free slot goes to a waiting landing check before a measurement.
  * THE CHECK LANE (2026-11-08 05:50Z: a landing check queued behind nine measurements, 'memory': 0; agentctl's remote
    check gives up at 80 min, and land entry ws3b bounced checking -> queued for ~2.5 h). Priority alone reserved
    nothing: measurements took every GiB the moment no check was queued, so the next check waited for one to END.
    Measurement admission now leaves `check_lane_jobs` checks' slots, memory and cores free, less the checks running:
    a queued check starts on the next tick whenever bitcashier is not saturated. Checks past the lane compete as before.
  * BORROWED CHECKS (the owner, 2026-11-16: "let landing checks borrow the idle measurement CPUs"): a check its own
    cpuset cannot hold may start in the measure slice (RUNNER_SLICE; its label stays movies-check) when the live
    reading of the measure CPUs, less what our runners there owe it (cpu_owed), holds the check's cores -- measured
    CPU, as every other start; a ledger of every job there at its whole cap never let a check borrow beside any
    measurement on 14 threads (2026-11-19) -- only while every queued red-fix or landing measurement has its runner,
    at most borrow_max_checks at once, and always leaving one measurement's memory, slice cap and slot, and its cores
    while a landing-path measurement is in flight (borrow_check). Optional measurements (movies-measure) wait behind a
    borrow (the owner, 2026-11-17, "A3").
  * CHECK CAPACITY (the owner, 2026-11-17, "A1-3": checks waited ~20 min for a slot and ran ~10): a check is charged 3
    cores, not its JVM's 4 (a settled one used 2.6 at the median), its cpuset is 18 threads (bitcashier infra), and
    its own cpuset never holds more checks than its threads fit at their p90, check_peak_cpus (the 2026-11-03 lesson).
  * THE MEASURE LANES (2026-11-16: Main red, and its fix's pre-push drift check -- Identity takes 43816825023, ten rows
    on movies-measure -- ran 1-3 rows at a time for 80+ min while 14-27 optional measurement jobs shared the label;
    GitHub hands a runner to any job its labels match). A measurement runner registers ONE lane's label
    (MEASURE_LANES): movies-red-fix, movies-landing, movies-stage (deploy staging, the owner 2026-11-19), movies-measure,
    movies-consumer (a row waiting on another row's upload, the owner 2026-11-19), movies-explore (below). Each free measure start
    goes to the highest lane with a job no runner of that lane waits for, so a landing check's rows take the next measure slot ahead of every
    optional measurement, a gate-red fix's ahead of those. In everything else a lane is a measurement: size, slice,
    admission, caps. Light runners also register LANES_MARKER, which says to a plan job (scripts/ci/measure-lane.sh)
    that this scheduler serves the lanes.
  * THE EXPLORER LANE (the owner, 2026-11-18, "7"): the nightly drift and family explorers' rows ask for movies-explore,
    a fourth measure lane below movies-measure, and none of it starts while a landing check waits for a runner; an idle
    explorer runner then goes. On 2026-11-17 21:15-21:40Z the drift explorer's rows refilled the measure CPUs each
    time one ended, and a landing check could not borrow them ("held by cpu", borrowed []). A running row is never
    stopped: the rows yield the measure CPUs as they end. measure-lane.sh asks for the lane only where a light runner
    says this scheduler serves it (EXPLORE_MARKER).
  * THE BUSY TICK: while a check job is queued, a tick every busy_tick_seconds, not tick_seconds, so it waits less
    for this to see it. (THE WARM CHECK RUNNER, 2026-11-18..21, registered a pinned check runner ahead for a running
    precommit-check plan's later checks; retired 2026-11-21, the owner: none of a landing check's jobs that wait on the
    plan runs here any more -- the old refs went to Main, identity's shards to GitHub's hosted runners -- so it held an
    idle check runner ~60 s each landing, beside bitcashier's.)
  * THE CHECK LANES (2026-11-17: a red fix still checking 41 min after it was queued, its jobs queued for
    `movies-check` runners with every other landing's): the same for the check class, CHECK_LANES
    movies-check-red-fix then movies-check; a red fix's precommit-check jobs ask for the first.
  * OLDEST FIRST (2026-11-17: precommit-check run 44061032302's `heavy rows (e2e)`, created 10:09Z, queued until 10:45Z
    while later landings' check jobs started). A runner registered with a lane's label alone is handed WHICHEVER queued
    job of the lane GitHub picks, not the oldest, so under a backlog one change's jobs were overtaken again and again.
    A job may also ask for its run's PIN label (PIN_PREFIX + the run id: precommit-check.yml's every check job), and a
    lane runner then registers the lane's label plus the pin of the OLDEST queued job of the lane no runner already
    waits for (uncovered): GitHub hands a runner only a job whose labels it has all, so that runner can take that run's
    jobs alone. A runner waiting with a pin covers its run's jobs only; a job with no pin is served as before.
  * THE LIGHT LANE (the owner, 2026-11-07: "why was it waiting so long, can you fix that?"): the light jobs (`movies`:
    timings reports, prunes, dispatches -- seconds, under 1 GiB) have `light_slots` slots of their own past
    `max_slots`, admitted at their own size whatever the heavy queue or bitcashier's saturation. Charged a
    measurement's 11 GiB and 4 cores and ranked last, identity run 42370669779's 10-second timings job waited 26 min
    (12:32-12:58Z) behind 20 queued measurements.
  * THE WARM LIGHT RUNNER (the owner, 2026-11-18, "10"): warm_light_runners light runners (ci-runner: 1) are kept
    registered and idle past the queued light jobs, so a landing check's plan job -- light, pinned to nothing, and
    every check job after it waiting for it -- is taken the moment it queues, not a tick and a registration later.
  * MAIN NEVER RUNS HERE (the owner, 2026-11-06, the same day's "main's convergence takes ci-runner's free slots"
    reversed: "run main branch convergence and Main workflow always on gh, measurements always on ci"). Main's jobs ask
    for GitHub's hosted runners alone, so nothing here serves them and nothing is published for them.

THE RUNNERS are instances of the template unit movies-runner@<slot>.service (bitcashier infra,
nix/hosts/ci-runner/movies-runners.nix): one container, GitHub's runner registered --ephemeral with the labels this
writes to <state>/<slot>.env, one job, then the unit ends and the slot is free. An instance is never stopped while it
runs a job (a Runner.Worker in its container).

  runner_scheduler.py run --config FILE             the host loop (ci-runner's movies-runner-scheduler unit)
  runner_scheduler.py tick --config FILE [--dry-run] one pass, printing what it saw and did

THE PACKAGE: the rule lives in runner_scheduling/ (its __init__ lists the modules), this file is its entry. ci-runner
fetches this file and each of the package's files at a pinned movies commit, each with its own hash (bitcashier infra,
nix/hosts/ci-runner/movies-runner-scheduler-pin.nix, which bin/movies-scheduler-pin writes), and runs this file.

Tested by runner_scheduler_test.py.
"""
from __future__ import annotations

import sys
from typing import List

from runner_scheduling import Config, CpuMeters, GitHub, Host, Machine
from runner_scheduling.loop import loop


def main(argv: List[str]) -> int:
    """The composition root: the machine, the config it reads, the host, the reader and the CPU meters, and the loop."""
    flags = {a for a in argv[1:] if a == "--dry-run"}
    rest = [a for a in argv[1:] if a not in flags]
    args = dict(zip(rest[0::2], rest[1::2]))
    if argv[:1] in (["run"], ["tick"]) and "--config" in args:
        machine = Machine()
        cfg = Config.load(machine, args["--config"])
        return loop(machine, cfg, Host(machine, cfg), GitHub(machine, cfg, cfg.reader_app_id, cfg.reader_key),
                    CpuMeters(cfg.cpu_window_seconds), "--dry-run" in flags, argv[0] == "tick")
    print(__doc__, file=sys.stderr)
    return 64


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
