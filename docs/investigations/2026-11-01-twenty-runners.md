# Main and convergence on 20 runners (2026-11-01)

The owner's request: "optimize Main and convergence runs for 20 runners", and then "as well as optimize run length".
GitHub Free gives the account 20 concurrent jobs. A push to main starts Main, Main kicks Identity model convergence,
and landing checks (`precommit-check.yml`) and the other workflows share the same 20 slots. Goal: the shortest time
from a push to both verdicts, with landing checks still able to start, and no check's coverage changed.

All durations below come from the jobs API. **Measurement trap:** since about 2026-11-01 08:00 UTC, GitHub's run and job
`created_at`/`completed_at` read one hour ahead (DST end). Steps' timestamps are right. Runs after 08:00Z are timed from
their steps only. Main run 41287457930 (09:06Z) is left out of the baseline for that reason.

## 1. Baseline

### Sample

- **Main**, 8 green push runs, 2026-10-31 16:10Z to 2026-11-01 04:01Z: 41218395620, 41220302927, 41221900193,
  41243778179, 41250750971, 41252843639, 41266164948, 41271467462.
- **Convergence** on main, 11 completed runs, 2026-10-31 16:43Z to 2026-11-01 04:05Z: 41221039905, 41222427076,
  41225863205, 41230763706, 41232758655, 41238985192, 41242893057, 41253023172, 41255733986, 41266358776, 41271681408.
  Red runs are included because every row of them ran to the end. A red row costs the same runner time.
- **The whole account**, every job of 105 runs created after 2026-10-31 22:00Z, to measure concurrency.

### Push to both verdicts

| | median | p75 |
|---|---|---|
| Main wall (push to last job) | 15.5 min | 22 min |
| Push to convergence's rows created | 3.0 min (13.2 including runs that waited for the lane) | |
| Convergence wall | 30.5 min | 41 min |
| **Push to both verdicts** (19 pairs) | **35 min** | 46 min |

### The critical path, step by step (convergence's last row, 11 runs, medians)

| segment | median | what it is |
|---|---|---|
| push to rows created | 13.2 min | Gates, then `kick-convergence` queued 1.8 min behind ci's second wave, then the preflight queued again. In 7 of 11 runs the run also waited behind the previous run in its one lane, because a convergence run (30-55 min) is longer than the gap between pushes. |
| the last row's queue | **15.0 min** | The row that ends last is a 13-15 min drift or order row, queued behind 20 busy runners. |
| its setup (checkout, convergence-setup) | 1.7 min | Mongo start, corpus, enrichment tree, JDK, sbt caches. Most of it already runs in the background. |
| its step | 12.0 min | For a drift row this includes **~2.8 min of compiling** scrapers/Test and worker/Test in every row of the last 5 runs (below). |
| post-steps + verdict job | 1.1 min | |
| **push to verdict** | **41.8 min** | (run-level median over the 11 runs) |

**Queueing is about two thirds of the path, not the work.** The biggest single item is a 13-15 min row that starts last.

### Per-job durations (medians, from step timestamps; queue = started - created)

Convergence (per row, the 26 rows of a main run; 252 runner-min per run):

| row | n | queue | duration | max |
|---|---|---|---|---|
| takes-drift-2 (germany) | 11 | 6.9 | 14.9 | 17.2 |
| takes-drift-2 (united-states) | 11 | 5.0 | 14.2 | 15.7 |
| takes-drift-2 (united-kingdom) | 11 | 6.8 | 13.8 | 17.3 |
| takes-drift-1 (germany) | 11 | 4.9 | 13.6 | 15.9 |
| takes-drift-1 (united-states) | 11 | 5.5 | 13.3 | 16.3 |
| order-independence-p12 (germany) | 11 | 7.3 | 13.2 | 15.4 |
| takes-drift-1 (united-kingdom) | 11 | 0.2 | 13.2 | 18.5 |
| order-independence-p01 (germany) | 11 | 2.5 | 13.0 | 15.9 |
| order-independence-p12 (united-states) | 11 | 9.1 | 12.8 | 15.0 |
| order-independence-p01 (united-states) | 11 | 6.7 | 12.5 | 15.4 |
| convergence (poland) | 11 | 6.1 | 12.5 | 14.3 |
| convergence (germany) | 11 | 1.3 | 12.2 | 13.3 |
| convergence (united-states) | 11 | 5.7 | 11.4 | 13.1 |
| takes-drift-1 (poland) | 11 | 8.2 | 11.2 | 14.4 |
| next-day (united-states) | 11 | 8.8 | 10.5 | 11.9 |
| order-independence-p12 (united-kingdom) | 11 | 3.0 | 9.2 | 12.4 |
| takes-drift-2 (poland) | 11 | 1.5 | 9.0 | 12.0 |
| order-independence-p01 (united-kingdom) | 11 | 0.2 | 8.8 | 11.3 |
| convergence (united-kingdom) | 11 | 6.9 | 6.9 | 10.3 |
| takes-drift-2 (spain) | 11 | 3.6 | 5.4 | 6.6 |
| takes-drift-1 (spain) | 11 | 7.3 | 5.4 | 5.8 |
| convergence (spain) | 11 | 3.1 | 4.4 | 5.5 |
| takes (germany) | 11 | 1.6 | 2.0 | 8.7 |
| takes (poland) | 11 | 1.1 | 1.9 | 8.6 |
| takes (united-states) | 11 | 7.2 | 1.7 | 13.2 |
| takes (united-kingdom) | 11 | 6.5 | 1.6 | 10.4 |
| takes (spain) | 11 | 7.9 | 1.0 | 3.6 |
| report (timings) | 11 | 0.1 | 0.4 | 0.7 |
| preflight | 11 | 0.1 | 0.2 | 0.3 |
| verdict complete | 11 | 0.1 | 0.1 | 0.1 |
| request-bisect | 1 | 0.1 | 0.1 | 0.1 |

Main (the 22 jobs that run on every push; 79 runner-min per run):

| job | n | queue | duration | max |
|---|---|---|---|---|
| page tests (safari / pw phones 2/2) | 8 | 3.0 | 9.4 | 10.3 |
| test (worker-identity) | 8 | 1.3 | 8.0 | 8.6 |
| page tests (safari / pw phones 1/2) | 8 | 2.4 | 7.4 | 10.8 |
| page tests (safari / pw behaviour 2/2) | 8 | 2.6 | 6.9 | 7.7 |
| page tests (safari / pw behaviour 1/2) | 8 | 2.1 | 5.2 | 5.5 |
| test (rest) | 8 | 0.8 | 3.9 | 5.7 |
| page tests (chrome / stable) | 8 | 2.9 | 3.7 | 3.9 |
| integration-test (rest) | 8 | 0.2 | 3.6 | 4.2 |
| test (worker) | 8 | 1.0 | 3.4 | 3.7 |
| mobile-local-server | 8 | 0.9 | 3.2 | 3.7 |
| integration-test (identity) | 8 | 0.0 | 3.0 | 3.4 |
| page tests (chrome / pw phones) | 8 | 2.1 | 2.9 | 3.6 |
| e2e (corpus) | 8 | 2.0 | 2.6 | 3.1 |
| e2e (rest) | 8 | 1.1 | 2.6 | 3.0 |
| page tests (chrome / pw behaviour + desktop) | 8 | 2.9 | 2.5 | 3.1 |
| test (heavy2) | 8 | 0.4 | 2.5 | 3.8 |
| test (heavy1) | 8 | 1.1 | 2.1 | 2.6 |
| image-worker | 2 | 1.0 | 1.1 | 1.3 |
| kick-convergence | 8 | 1.7 | 0.4 | 0.5 |
| report (timings) | 8 | 0.5 | 0.3 | 0.4 |
| gates | 8 | 0.2 | 0.2 | 0.3 |
| publish-web | 2 | 1.9 | 0.2 | 0.2 |
| publish-worker | 2 | 0.9 | 0.1 | 0.1 |
| scope | 8 | 0.3 | 0.1 | 0.2 |
| commit rules | 8 | 2.1 | 0.1 | 0.1 |

### Concurrency over the account (30 s samples, 2026-10-31 22:00Z to 2026-11-01 08:00Z)

- Peak running: 20 (22 in one sample, which is timestamp skew). 13% of the time all 20 were busy, and in 92% of
  the samples where something was queued, 18-20 were running. The limit is real and it is the bottleneck when it binds.
- Runner-minutes by workflow: convergence 1847, **landing checks 1028**, Main 497, Nightly stress 460, Record scrape
  fixtures 184. The landing checks use twice what Main does.

### The landing check (`precommit-check.yml`), 13 green runs 2026-11-01 03:40-09:12Z

| job | median / max | where the minutes go |
|---|---|---|
| check (rest) | 8.7 / 15.9 min | The hook step is 7.7 min (median). In the slowest run (41276112019), the hook ran the `sbt` check for 292 s and then `identity` for 585 s, one after the other. Setup is about 1.5 min. |
| heavy rows | 11.8 / 12.5 min | The hook step is 10.4 min. The rows run serially: itrest 275 s, itidentity 137 s, e2ecorpus 118 s, e2e 136 s. |
| check (oldrefs) | 1.4 / 5.0 min | |
| check (page) | 1.2 / 1.4 min | After the others (`needs`). |
| plan | 0.7 min | |

A landing check takes about 12-16 min because both of its long jobs run their checks serially. Its queue wait is the
other cost: under FIFO it waits for every row already queued ahead of it.

### Why the drift rows compiled every time

Every drift row (`takes-drift-1/2`) restores the `sbt-target-takes-` family first. Only the takes row saved that family,
and only when its check compiled something. After a run of pushes that touched no identity code, the last entry was
evicted (the repo sits near the 10 GB cache cap), and nothing saved a new one. Every drift row then fell back to Main's
`sbt-target-e2e-` entry, which has no worker test classes, and compiled 290 + 594 sources:

| runs | drift rows that compiled from scratch | sbt total (avg) | of it the replay |
|---|---|---|---|
| 41221039905 .. 41238985192 (6 runs) | 0 of 60 | 385-637 s | 356-446 s |
| 41242893057 .. 41271681408 (5 runs) | **50 of 50** | 501-582 s | 375-444 s |

That is about 140-170 s per row, on 10 rows of every run, and on the critical path.

## 2. What packs into 20, and what doesn't

A small discrete-event model (`scratchpad/sim.py` in the session; FIFO queue, 20 runners, measured medians, matrix
`max-parallel` modelled as "a capped row is queued only when a sibling ends") gave:

| layout | Main ends | convergence ends | push to both |
|---|---|---|---|
| today (YAML order, drift compile misses) | 10.3 | 25.1 | 25.1 |
| + drift compile cache | 10.3 | 21.6 | 21.6 |
| + longest-first (US, DE, PL, UK, ES; drift, convergence, order, next-day, takes), *not achievable: below* | 10.3 | 18.3 | 18.3 |
| + per-leg caps that leave headroom (C16-C18) | 10.3-15 | 24-27 | 24-27 |
| + Main matrix caps (test 3, chrome 1-2, e2e 1) | 13-24 | 17.4-18.4 | 18.4-24 |

The model is optimistic: it has no other workflows, no runner pick-up latency and no lane waits. Its ordering is the
point, not its absolute numbers.

**Longest-first cannot be configured on github.com.** It was implemented (legs and phases listed longest-first) and
measured on the branch (run 41290215479). The 27 rows' job ids came out interleaved across legs and phases, in the same
scrambled order as before: Poland's drift rows first, Germany's takes 6th, and the UK's drift rows last. Runners went
out in roughly that id order. List order in a matrix does not order the queue, so the change was reverted (commit
"Revert ..."). The only real lever left is to make each row shorter and to start the run earlier. The work floor is ~300 runner-min over 20 runners, about 15 min, and the longest row is
about 13 min after the compile fix.

**Why no hard headroom for landing checks.** GitHub hands a free runner to the oldest queued job. `max-parallel` does not
reserve a slot. It keeps rows unqueued, and a capped row that is released later goes to the back of the queue, behind
any landing check that arrived in the meantime. Capping convergence so that the two together stay at 18 or below costs
6-9 min on every convergence run, about 25 runner-min of work spread over fewer slots. It saves a landing check that
arrives mid-push about 2-4 min of waiting. Capping Main only moves Main's own jobs behind convergence and does not help a
landing check either. Neither trade is worth making, so neither cap is in this change. Landing checks get their time
back the other way: there is less convergence work ahead of them in the queue (about 50 runner-min less per run,
section 4), and their own longest job is shorter (section 3).

## 3. What changed

1. ~~Longest first~~: tried, measured as having no effect, and reverted (section 2).
2. **The drift rows keep their build** (`country-convergence-leg.yml`, `convergence-setup`, `sbt-target-cache`). On
   main, `takes-drift-1` saves the takes family when what it restored was not a takes entry. `sbt-target-cache` now
   reports the matched key. The takes row's own entry, when it compiled, still owns the key.
3. **Kick convergence at t=0** (`main.yml`). `kick-convergence` no longer `needs: gates`. That dependency only placed it
   behind ci's second wave, where it queued 1.8 min. CiRunnerBudgetSpec, KickConvergenceWiringSpec and
   MainAfterRedCiSpec are updated to match.
4. **Landing check: `check (identity)`** (`precommit-check.yml`). The identity module's tests ran after the sbt check
   inside `check (rest)`: 292 s and then 585 s, a 15.9 min job in run 41276112019. They now run in their own job at t=0
   (`PREPUSH_ONLY=identity`). That job sets nothing up when the hook selects no identity check, but its hook step still
   runs, so a missed selection fails rather than skipping. Fast refs are unchanged.

Not changed (and why):

- **Folding the takes row into another row.** It is ~1.7 min a row, mostly setup, and 8 runner-min a run. It would
  rename gated jobs, reuse inputs and red keys. Also, when identity code changes the check runs for up to 8.7 min, and
  that time would land on a 13 min row.
- **Splitting `heavy rows` into a matrix.** The rows are itrest 275 s, itidentity 137 s, e2ecorpus 118 s and e2e 136 s,
  run serially in a 11.8 min job. Splitting would bring the job to about 6 min but would take 3 runners instead of 1.
  LandingCheckSplitSpec pins it to one job deliberately. This is the next step if the landing check is still the
  complaint. It needs the owner's call because it trades runners for landing latency.
- **The sbt disk cache in `setup-sbt-build`** is never found (`...-sbt-diskcache-...` exists nowhere in the cache list).
  This is worth a follow-up: 0.5-2 min per sbt job, unmeasured.
- **Hermetic drift rows call wikidata.** "catalogue ids: unexpected body from https://query.wikidata.org" appears in a
  hermetic drift replay, and each replay then runs 60 settle rounds. This is identity code, so it is out of scope here.
  It is reported for the identity owner.

## 4. Measured after

### Branch run: convergence on `ci/twenty-runners` (run 41290215479, 2026-11-01 08:46-09:10Z)

The drift rows restored a takes entry, so the compile was a hit, as it will be on main once the new save has run. The
account was busy during the run (peak 19 concurrent jobs account-wide). Main was not dispatched on the branch:
`main.yml`'s `publish-web` and `publish-worker` tag `registry.kinowo.net` images on any ref when `CI_ROLE` is primary,
and Flux image automation deploys from those tags. A Main run on a branch would deploy the branch.

| | baseline (median of 11) | branch run |
|---|---|---|
| convergence wall (rows created to last row end) | 30.5 min | **23.5 min** |
| runner-min, all rows | 250 | **200** |
| last row's queue | 15.0 min | 10.6 min |
| last row's setup / step | 1.7 / 12.0 min | 1.0 / 11.7 min |
| peak rows of this run at once | 20 | 15 |

Per row, in minutes (baseline median, then this run; setup is checkout plus convergence-setup):

| row | before | after | after: setup |
|---|---|---|---|
| takes-drift-2 (germany) | 14.9 | 11.8 | 1.2 |
| takes-drift-2 (united-states) | 14.2 | 10.9 | 1.3 |
| takes-drift-2 (united-kingdom) | 13.8 | 13.5 | 0.9 |
| takes-drift-1 (germany) | 13.6 | 7.1 | 1.3 |
| takes-drift-1 (united-states) | 13.3 | 7.7 | 1.2 |
| order-independence-p12 (germany) | 13.2 | 12.7 | 1.2 |
| takes-drift-1 (united-kingdom) | 13.2 | 12.7 | 1.0 |
| order-independence-p01 (germany) | 13.0 | 12.8 | 1.2 |
| order-independence-p12 (united-states) | 12.8 | 10.0 | 1.2 |
| convergence (poland) | 12.5 | 11.7 | 1.1 |
| order-independence-p01 (united-states) | 12.5 | 7.1 | 1.1 |
| convergence (germany) | 12.2 | 12.4 | 1.0 |
| convergence (united-states) | 11.4 | 11.0 | 0.9 |
| takes-drift-1 (poland) | 11.2 | 6.5 | 1.2 |
| next-day (united-states) | 10.5 | 8.4 | 1.1 |
| order-independence-p12 (united-kingdom) | 9.2 | 5.6 | 1.1 |
| takes-drift-2 (poland) | 9.0 | 6.7 | 1.2 |
| order-independence-p01 (united-kingdom) | 8.8 | 9.8 | 0.8 |
| convergence (united-kingdom) | 6.9 | 6.3 | 1.1 |
| takes-drift-2 (spain) | 5.4 | 2.5 | 0.8 |
| takes-drift-1 (spain) | 5.4 | 2.2 | 0.9 |
| convergence (spain) | 4.4 | 4.5 | 0.8 |
| takes (germany) | 2.0 | 1.4 | 1.3 |
| takes (poland) | 1.9 | 1.2 | 1.2 |
| takes (united-states) | 1.7 | 1.2 | 1.1 |
| takes (united-kingdom) | 1.6 | 1.4 | 1.3 |
| takes (spain) | 1.0 | 0.9 | 0.8 |

The drift rows lost 2.5-6 min each: the compile is gone, and seeds differ per run. The order, convergence and takes rows
stayed within run-to-run noise, as expected, since they were never compiling. Two drift rows were red on seeds 21
(UK) and 22 (Poland) with 1-2 flips. That is the identity model on the branch's rotating seeds, not this change, which
touches no identity code. It was reported to the reds owner.

### Critical path, before and after

| segment | before (median) | after (expected on main) |
|---|---|---|
| push to rows created | 3.0 min (+ lane waits) | ~1.2 min: the kick at t=0, then the preflight |
| last row's queue | 15.0 min | 10.6 min on the branch; less again with ~50 runner-min less work in the queue |
| last row: setup + step | 1.7 + 12.0 min | 1.0 + 11.7 min |
| verdict | 0.9 min | 0.3 min |
| **push to convergence verdict** | **~33 min without lane waits (41.8 with them)** | **~25 min** |

Shorter runs also mean fewer lane waits: a run that ends sooner lets the next one start sooner.

### On main after landing

The first Main + convergence pair on main after this change lands is measured the same way (step timestamps). The result
is reported with the landing.
