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

### On main after landing (67345c04: Main 41293603926, convergence 41293639713, 09:33-10:02Z)

| | baseline (median) | first run after |
|---|---|---|
| `kick-convergence` starts | +2.1 min (queued 1.8) | **+0.0 min** (ended +0.4) |
| convergence's first job | +3.0 min | +4.5 min (its preflight waited for the lane: run 41291836903 was still in flight) |
| Main wall | 15.5 min | 18.2 min |
| convergence wall | 30.5 min | **24.3 min** |
| push to both verdicts | 35 min | **28.8 min** |
| convergence runner-min | 250 | 244 |
| peak Main + convergence jobs at once | 20 | 18 |

In that run the drift rows still compiled everything, because no takes entry existed yet. Their save worked
(`sbt-target-takes-...67ff671e`, 111 MB, 09:46Z), so the next run restores it. The run also exposed a bug: on main the
saving `actions/cache` reports no matched key, so every `takes-drift-1` row read "restored nothing" and would have saved
on every run. The follow-up commit reads the restored tree instead (worker test classes present right after the restore,
before anything compiles), and drops the `matched-key` output. With the cache hitting, the branch run's numbers apply:
about 50 runner-min less per run, and drift rows 2.5-6 min shorter.

## 5. Under 10 minutes? (2026-11-01, afternoon)

The owner: "i want convergence run to be under 10m", clarified as "the whole workflow": from the run's first job start
to its last job's end, including `verdict complete`, `timings`, `request-bisect` and any queueing between them. All times
below come from step timestamps (the run and job timestamps read an hour ahead today).

### Where a row's time goes (main runs 41296844565 and 41299606411; branch runs 41302871174, 41302893834)

| part | per row | notes |
|---|---|---|
| checkout | 13 s | |
| `convergence-setup` (JDK, sbt caches, Mongo, pair, tree, corpus) | 37-83 s | Most of it already runs in the background. The fill step adds 29 s to a convergence row. |
| sbt load + compile | 25-60 s | Each row compiles 1 source; the drift rows restore the takes build (section 3). |
| **the replay itself** | **5-11 min** | DE boot `bootCutover` 293 s; US order pass pair `replayCutover` 629 s; UK drift seed 628 s. |

Setup is 1.0-1.5 min of a 10-13 min row. The rest is the replay, and it is CPU-bound: the runner's `[cpu]` sampler reads
3.3-3.9 of 4 cores busy for most of a convergence row (java plus mongod), with no iowait. So compiling once, restoring
fixtures once, or a faster Mongo would each save seconds per row, not minutes.

### Whole-workflow wall today

| runs | wall (first step to last step) |
|---|---|
| last 10 main runs (41253023172 .. 41299606411) | median 26 min (19.2-52.0) |
| the 4 since 67345c04 landed | 19.2, 22.8, 24.3, 25.7: median 23.6 min |

### The floor, measured: an idle account

Branch run 41302871174 (`runner=ubuntu-22.04-arm`) happened to start while the account was almost idle (0-1 x64 jobs
running at 11:38-11:40Z). Every row got a runner within 1.8 min of the preflight. **Its rows still ended 14.3 min after
the first job started**, because the longest rows are that long by themselves: drift UK 11.7, order DE p01 12.7,
convergence DE 12.0, US convergence/order 10.6. That is the floor for today's rows with no queueing at all. Off main it
also ran no p12 rows; on main those are 10-13 min as well.

arm64 runners are no faster than x64 for this work (same rows, same minutes, within noise), and they share the same 20
concurrent jobs (sampled: x64 + arm never above 20). There is no second pool to use.

### What each lever is worth

| lever | what it saves | gets under 10? |
|---|---|---|
| compile once / bake an image | 25-60 s per row (sbt load stays) | no: the longest row is still 10-12 min |
| restore fixtures once per leg, faster Mongo | part of 37-83 s per row | no |
| merge the tiny rows (takes, Spain) into one wave | ~6 jobs, less queueing | no: the 14.3 min floor stays |
| skip unaffected pushes | already there: Main's kick dispatches nothing for a push outside `.github/convergence-paths.txt` (27 of the last 60 commits). Of the 33 that do dispatch, 15 change CI files the rows run (provably not identical inputs), 18 change identity/worker code | no, for the pushes that do run |
| arm64 runners | 0 | no |
| JFR profile (below) | about 10% of a US order row, a few % elsewhere | no, and it is identity code |
| one replay per order row instead of two passes sharing 4 cores | order rows 12 -> ~7 min, but one more job per country, and a cross-row comparison the spec does not have | no: the drift and convergence rows are 10-12 min on their own |

### JFR profile (branch run 41302893834, `profile=true`)

- **US order-independence-p01**: 8.0% of samples in `LiveProjectionIndex.wasPlaced` (identity/src/main), plus 3.6% in
  `ListingKey$Native.equals` and 3.1% in `DecidedBy.accepted`, all under `FamilyScope.score` ->
  `LiveProjectionIndex.written` (12-13% inclusive): an `exists` over a collection doing a HashMap lookup per element.
  That reads like a linear scan per listing, worth about 10% of the row if it became a lookup.
- **DE convergence**: parsing recorded answers dominates the own-code frames: `MetacriticClient.parseSearchResults` 6.5%,
  `RottenTomatoesClient.parseSearchResults` 2.5%, `JsonBodies.parse` 2.9%, `TitleRule.apply` 2.3%.

Both are identity/enrichment code, outside this CI work, and reported to the identity owner. Neither is worth the 1.7x a
10-minute run needs.

### Verdict

With identical coverage on GitHub's free runners, the whole workflow cannot get under 10 minutes: an uncontended run
takes 14.3 min, and a run on main shares the 20 slots with Main's ~22 jobs, which adds 5-10 min of queueing. No CI change
was landed for this target, because none of the levers moves the floor. Under 10 needs one of these, for the owner to
decide:

1. **Faster replays (identity code)**: every 10-13 min row must reach about 7 min of replay, so about 1.7x faster.
   The profile above is the starting point, but it shows no single 40% hotspot.
2. **Bigger machines**: GitHub larger runners (8-16 cores, a paid plan) or a self-hosted runner. The rows are
   CPU-bound at 4 cores, so 8 cores would roughly halve the two-pass order rows and speed up the parallel parts of the
   others.
3. **Less work per push (a coverage change)**:
   - a shorter replay per row, e.g. drift over a slice of each corpus per push with the full corpus nightly;
   - plus one wave: one drift seed per push instead of two, and one order phase per push alternating p01/p12, with the
     daily run still running all of them. That is 8 fewer rows and about 80 runner-min less, which removes most of the
     queueing, but the floor stays at about 14 min unless the rows themselves get shorter.

## 6. How to measure (2026-11-01)

The owner: "add measurements that will make these adjustments / investigations straightforward in the future", then
"per time range and trend", "gc / mem pressure / processor and memory", "identification of machine per each run",
"json in VL", "publish timings per leg" and "as much stats as possible". Everything in sections 1-5 was measured with
ad-hoc API scripts. This section replaces them with one tool, one per-job telemetry step and one store.

### The pieces

| piece | where | what it does |
|---|---|---|
| `scripts/ci/ci-timings` (`ci_trends.py`) | laptop, and the collector | `report`, `trend`, `compare`, `legs`, `export` (below) |
| `.github/actions/job-telemetry` | every job of Main (`ci.yml`, `main.yml`), the landing check (`precommit-check.yml`) and convergence (`identity-model-convergence.yml`, `country-convergence-leg.yml`) | `start` right after checkout, `finish` last (`!cancelled()`; before a convergence row's leg verdict). Uploads the job's `ci-metrics-<job>-<runner>` artifact (7 days). Every step continues on error. |
| `scripts/ci/machine-id.sh` | `start` | one `machine: {json}` log line and metrics line |
| `scripts/ci/cgroup-sampler.sh start 5` | `start` | the resource series, every 5 s (overhead below) |
| `scripts/ci/job_metrics.py` | `start`/`finish` | sets `JDK_JAVA_OPTIONS=-Xlog:gc:file=$RUNNER_TEMP/ci-gc/gc-%p.log` for every later JVM; at the end parses the GC logs, the JUnit reports, the series; reads the API rate limit at both ends |
| `scripts/ci/cache-marker.sh` | `sbt-target-cache`, `setup-jdk` | `ci-cache <name> <hit\|partial\|miss> <key>` plus restore time and size |
| `ci_timings.py` (each workflow's `timings` job) | in-run | the run summary now ends with **one table per leg** (convergence's countries; Main's job groups): each row's queue, setup by category, work, total, machine, GC and pressure; the leg's wall clock and its critical row |
| collector (`ci-metrics-collector` on ci-runner, infra branch `ci-metrics-collector`) | ci-runner, every 5 min | runs `ci-timings export --since 2d`, POSTs the events to `victoria-logs-ci` (`/insert/jsonline`, stream fields `source,repo,workflow`); a run is recorded in its state file only once accepted, so it is idempotent and resumable. A one-off backfill pass runs `--since 90d`. |
| `victoria-logs-ci` | monitoring host, port 9429, `vlogs-ci.jebaka.com` | its own VictoriaLogs (1.52.1 has one retention per instance): **400 days**, capped at 10 GiB |
| Grafana "CI" dashboard | grafana.jebaka.com | wall-clock trend per workflow, per-job p50/p90, setup vs work, queue wait and slot saturation, GC share and memory pressure per job, by CPU model / image, cache hit rate; variables for workflow and job |

The runners never talk to VictoriaLogs: they only upload artifacts. Runs older than the artifacts' 7 days still have
every timing (from the API's steps), just no resource, GC, test or cache figures.

### Commands

```
# The last 10 green Main runs on main: per job/step, setup vs work, bound, critical paths
scripts/ci/ci-timings report --workflow main.yml --last 10 --critical
# ...one report per CPU model, image, leg, job group or label
scripts/ci/ci-timings report --workflow main.yml --last 20 --by cpu-model
# A change's effect in one command: the newest 10 on main against the newest 3 on a branch, or two commit ranges
scripts/ci/ci-timings compare --workflow main.yml --before main:10 --after my-branch:3
scripts/ci/ci-timings compare --workflow main.yml --before abc123..def456 --after def456..0a1b2c3
# Per day for 14 days, with sparklines, slopes, changes, and regressions against the 14 days before
scripts/ci/ci-timings trend --workflow main.yml --since 14d --by day [--threshold 20] [--match 'job: .*safari']
# Convergence per leg, per day
scripts/ci/ci-timings trend --workflow identity-model-convergence.yml --since 7d --by leg
# Machine-readable
scripts/ci/ci-timings --format json|csv trend ...
# One run's per-leg tables
scripts/ci/ci-timings legs 41298328467
# The landing check: identity changes apart from the rest, one row per run with its queue beside its execution
scripts/ci/ci-timings report --workflow precommit-check.yml --branch '' --last 40 --by change --per-run --critical
```

`--slots N` (or `CI_TIMINGS_SLOTS`; default 20, movies' runner count) sets the slot count behind `sat_mean`/`sat_peak`/`sat_full`
and the "account slots" report line; bitcashier's collector runs `--repo bitcashier/core --slots 12`.

`--vl-url https://vlogs-ci.jebaka.com` (or `CI_TIMINGS_VL_URL`) reads VictoriaLogs first (LogsQL), and the API only for
runs not yet ingested, so a range older than the API's 90 days needs no refetching. Without it every command reads the
API through `gh` and keeps completed runs in `~/.cache/ci-timings`. Selections: `<branch>:<N>`, `<sha>..<sha>`,
`runs:<id>,<id>`, `<date|7d>..<date|now>`; green runs only unless `--include-red`.

**Regressions.** `trend` compares the range with the one before it. A row is flagged when its p50 rose by more than
`--threshold` % (default 20) and by at least 10 s (time rows; shares and rates have no minimum), or a cache's hit rate
fell by more than the threshold. Each flag names the push window where it started: the change point (the split of the
chronological per-run values with the largest gap between the medians either side, ties to the most uniform sides) as
"between run A (sha, time) and run B (sha, time)", and says when the commonest image version or CPU model changed at the
same point ("lines up with a machine change: cpu_model EPYC 7763 → Xeon 8370C").

**The bound** on each job row: `memory/GC` when GC pauses are ≥ 5% of the job's wall, `memory` when PSI memory full ≥ 1%,
some ≥ 10%, swap ≥ 256 MB or an OOM kill, `io` when PSI io full ≥ 5% or iowait ≥ 10%, else `cpu` at ≥ 85% busy,
`waiting` under 35%, `mixed` between; plus `r(dur,gc|psi_mem|cpu|queue)` when a job's duration correlates with one of
them at |r| ≥ 0.6 over 5+ runs. So "is it CPU, memory or waiting?" is one column.

**Clock skew.** Only step timestamps are trusted. A job whose `started_at` is ≥ 10 min from its first step is skewed by
that much (rounded to the quarter hour), and a `created_at` after its first step started is impossible and pulled back;
the skew comes off the job's `created_at` before its queue wait is computed. `report` prints how many jobs it corrected
(58 of 10 Main runs on 2026-11-01).

### Overhead

- The sampler: one sample is two `cat`s over `/proc/<pid>/stat` and the cgroup files, four awks and a `df`, about 10 ms
  of CPU every 5 s: ~0.2% of one core of the runner's 4 (0.05% of the machine). It used to fork a `grep` per process per
  sample for the java/mongod RSS; that is now part of the same awk.
- GC logging: `-Xlog:gc` at info, one line per pause. The `Picked up JDK_JAVA_OPTIONS` banner is stripped for the
  hermetic child JVMs (itkit `ChildJvm`), as it already was for `JAVA_TOOL_OPTIONS`.
- Per job: `start` ~1 s, `finish` ~1 s plus the artifact upload (~2-4 s, 10-200 KB).
- The collector: ~2.2 API calls per run (jobs, artifacts, the commit), measured over 150 runs of a 3 h window
  (337 calls); about 11 MB of events per 150 runs before compression.

### The event schema (VictoriaLogs, `source:ci`)

Every event: `_time` (ISO-8601 UTC), `_msg` (one human line), `source` = `ci`, `repo`, `workflow` (name), `workflow_file`,
`run_id`, `run_number`, `run_attempt`, `sha`, `branch`, `event`, `run_conclusion`, `kind`. Job-level events add `job`,
`job_id`, `group` (the job without caller prefix and matrix suffix: `page tests`, `takes-drift-1`), `leg` (a convergence
row's country, else the telemetry label), `label`.

| kind | `_time` | fields |
|---|---|---|
| `run` | first job start | `wall_s`, `runner_min`, `peak_jobs`, `path_queue_s`, `path_work_s`, `critical_path` (job names, →), `sat_mean`, `sat_peak`, `sat_full` (share of the window at 20), `jobs`, `skewed_jobs`, `skipped_jobs`, `skipped` (the jobs a scope gate skipped), `start`, `end` (epoch s), `change_files`, `change_additions`, `change_deletions`, `change_parents` |
| `job` | first step start | `conclusion`, `duration_s`, `queue_s`, `skew_s`, `runner_name`, `setup_s`, `work_s`, `bound`, `start`, `end`, `runnable` (epoch s), `rate_start`, `rate_end` (API rate limit remaining); `machine_*`: `runner_name`, `runner_os`, `hosted`, `arch`, `image`, `image_version`, `cpu_model`, `cores`, `cpu_flags_hash`, `mem_mb`, `disk_gb`, `disk_free_gb`, `kernel`, `cgroup_cpu_max`, `cgroup_mem_max`, `virt`, `product`, `vendor`, `region`; `res_*` over the job: `cpu`, `user`, `sys`, `iowait`, `steal` (shares of all cores), `load_mean`, `load_max`, `runq_mean`, `runq_max`, `mem_used_max_mb`, `mem_avail_min_mb`, `swap_used_max_mb`, `psi_cpu_some`, `psi_mem_some`, `psi_mem_full`, `psi_io_some`, `psi_io_full` (shares of wall time), `disk_read_mb`, `disk_write_mb`, `disk_free_min_mb`, `net_rx_mb`, `net_tx_mb`, `open_files_max`, `oom_kills`; `gc_*`: `jvms`, `collections`, `pause_s`, `max_pause_ms`, `heap_after_max_mb`, `of_wall`, `worst_jvm_overhead`; `tests_*`: `files`, `suites`, `tests`, `failed`, `skipped`, `time_s` |
| `step` | step start | `step`, `step_index`, `category` (runner, checkout, toolchain/cache, compile, services, fixtures, tests, publish, other; the first six are "setup"), `offset_s`, `duration_s`, and the `res_*` above over the step when it ran ≥ 15 s |
| `jvm` | job start | `pid`, `log`, `main` (main class or jar), `flags` (-X/-XX), `gc_count`, `gc_full`, `gc_pause_ms`, `gc_max_pause_ms`, `heap_after_max_mb`, `heap_committed_max_mb`, `uptime_s`, `gc_overhead` |
| `cache` | job start | `cache` (sbt-target, sbt-deps), `family` (the key without its hash), `status` (hit/partial/miss), `key`, `size_mb`, `restore_s` |
| `suite`, `test` | job start | the 15 slowest suites and tests of the job (and every failed test): `suite`, `test`, `tests`, `failed`, `skipped`, `time_s`, `status` |
| `process` | job start | the 5 commands with the largest RSS: `comm`, `rss_mb` (peak, all its processes), `cpu_s`, `threads` (peak), `procs` (peak) |

The per-job artifact's own lines (`machine`, `job_start`, `series`, `procs`, `jvm`, `cache`, `tests`, `suite`, `test`,
`rate_limit`, `job_end`) are what `export` turns into these events; `series` columns are cgroup-sampler.sh's
`SERIES_HEADER`.

### Not collected yet (and what it would take)

- sbt task timings (update/compile/test per module, incremental vs full): sbt prints none by default; needs a
  `-Dsbt.task.timings` run or a small sbt plugin writing them to a file the finish step reads.
- Mongo op counters (mongo-top.sh) and Playwright browser launch time: need a hook in the Mongo start script and the
  page-test action; Playwright's per-spec times already arrive as `suite`/`test` events from its JUnit report.
- Convergence replay internals (listings replayed, resolves, flips, per-phase split, corpus size): the `PhaseTimer`
  phases are already pushed to Prometheus by the in-run reporter (`kinowo_ci_phase_seconds`); the counters need the
  replay code to write a small JSON file the finish step picks up (identity code, not done here).
- Coursier and fixture download bytes: covered only as the machine's `net_rx_mb` per job and step.

## 7. Main under 8 minutes? (2026-11-01, afternoon)

The owner: "getting Main under 8m": the whole Main run, first job start to last job end, queueing included. Measured
with section 6's `ci-timings` (step timestamps only).

### Before: where Main's 16.9 minutes went (10 green runs on main, 41252843639 .. 41298328467)

`ci-timings report --workflow main.yml --last 10 --critical`:

| | p50 |
|---|---|
| wall | 16:52 (p90 20:33) |
| critical path | queue 4:21 + work 11:30 |
| setup vs work, runner-min per run | setup 12.3 (checkout 4.3, JDK/sbt/caches 5.2, browsers/Mongo/swift 2.3, compile 0.4) against work 64.6 (tests 63.4) |
| the critical path | `scope` → `page tests (safari / pw phones 1 or 2/2)` (9.6-9.8 min) → `image-worker` (queue 2.0 + 1.1) → `publish-worker` (queue 1.4 + 0.1) → `timings` (queue 0.5 + 0.3) |

Setup is not where the time goes: a sbt row restores its JDK, coursier and target caches in ~22 s (hit), checks out in
~13 s, and compiles in seconds. Two things are: **the WebKit phone rows' tests** (9.4 min of a 9.7 min job, one
Playwright worker at ~4.5 s a test), and **three queue hops after ci** (`image-worker`, `publish-worker`, `timings`),
each a fresh job behind every convergence row already queued.

### What changed (branch `measure/main-speed`, then main)

1. **Two, then three WebKit workers** (`run-page-test`'s `PW_WORKERS`). One was sized in 2817f38e7 for Forgejo's shared
   host (~16 job containers on one CCD, about a core each). A github.com runner is 4 dedicated vCPUs. Two workers:
   the longest row 6.9 min at 66-71% CPU; three: 5.1-5.5 min at 80-87%. No test failed on any run.
2. **The worker image is built in ci's `e2e (corpus)` row**, beside the web image, from its own Docker context, under
   its SHA tag only; `image-worker` is gone and `publish-worker` needs only `ci` and `gates`. One queue hop after ci
   instead of two. Nothing ships before ci is green, as before (`publish-*` still `needs: [ci, gates]`).
   The `stage-worker` artifact had no other consumer and is no longer uploaded.
3. **A dispatch on a branch ships nothing**: `gates` outputs `ship` (main only), both publishes need it, and
   `kick-convergence` kicks only from main. That is what made these measurements possible without deploying.
4. Tried and reverted: the drift slices on every core instead of half. The suite went from 278 s to 446-452 s at 92%
   CPU: it competes with the row's other suites. Tried on the branch and not landed: the drift slices halved by
   position into a second suite on `heavy1` (run 41319004595): 578 s and 288 s, see below.

Coverage is unchanged: every test, spec, slice and check still runs before a deploy.

### After

Branch dispatches of the final shape (three WebKit workers, the worker image in ci): runs 41311300585, 41312711652,
41316846216 (the first also carried the reverted every-core drift-slice experiment, which only touched
`test (worker-identity)`). Before: the 10 green runs on main above. `ci-timings compare --before runs:... --after
runs:...`, medians:

| | before (10 runs on main) | after (3 branch runs) | change |
|---|---|---|---|
| **Main wall** | **16:52** (p90 20:33) | **13:27** (p90 19:45) | -20% |
| critical path: queue | 4:21 | 10:46 | the account was saturated during all three |
| critical path: work | 11:30 | 5:32 | -52% |
| runner-min per run | 75.3 | 69.5 | -8% (no `image-worker`) |
| page tests (safari / pw phones 1/2) | 9:40 | 5:25 | -44% |
| page tests (safari / pw phones 2/2) | 9:50 | 5:06 | -48% |
| page tests (safari / pw behaviour 2/2) | 6:20 | 5:00 | -21% |
| page tests (safari / pw behaviour 1/2) | 4:58 | 3:39 | -26% |
| e2e (corpus), now building both images | 2:39 | 3:18 | +25% (off the critical path) |
| test (worker-identity) | 7:48 | 8:43 | +12%: `IdentityDriftSlicesSpec` grew (below) |
| queue hops after ci | 3 (`image-worker` 2:00, `publish-*` 1:26-2:31, `timings` 0:28) | 2 (`publish-*`, `timings`) | -1 hop |

The first run of the shape, 41308264642 (two WebKit workers, a quiet account, 23 s of queueing in all), ran **8:00**
first job to last, with no publish (a branch ships nothing; on main `publish-*` adds one hop of ~10 s plus its queue).

Two numbers explain why the wall is still above 8 minutes:

- **Queueing.** The first run on main after the telemetry landed (41309546859) took 35:29, of which 27:15 was jobs
  waiting for a runner; the branch runs waited 5-23 min. In those windows the account carried Main, the convergence run
  Main kicks (26 rows of 10-13 min), other agents' convergence measurement runs on `measure/*` branches (26 rows each)
  and landing checks. GitHub hands a free runner to the oldest queued job, so every Main job created after another
  workflow's rows waits for them; the post-ci hops always are.
- **`IdentityDriftSlicesSpec` is the new long pole.** It replays every committed drift slice (a drift or flip red is
  not closed until its slice is committed), computing them in one lazy pass two at a time. It took 278 s at 12:55Z,
  420-450 s by 14:30Z as slices landed, and grows with every closed drift red. Halving it by position (not landed) gave
  578 s and 288 s: a few slices dominate.

### Verdict, and what under 8 minutes would take

Main's own work now fits in about 7.5 min (the WebKit rows ~5.5, `test (worker-identity)` ~8 at today's slice count,
then one publish hop and `timings`), down from 11.5. **Under 8 minutes wall is not reachable on the shared 20 free
runners with identical coverage**, because queueing alone was 5-27 min in every busy window today. It would take, for
the owner to choose:

1. **Main never queues behind other work.** Either (a) move what is not Main off the shared 20: the landing checks and
   `measure/*` convergence runs to the self-hosted ci-runner (it already runs agentctl's checks), or (b) a paid plan with
   more concurrent jobs or larger runners. With no queueing, the branch runs show Main at ~8 min today and ~7 once (2)
   lands.
2. **Split `IdentityDriftSlicesSpec` by measured slice cost** across `worker-identity` and `heavy1`/`heavy2`, so no row
   carries more than ~3 min of it (identity test code; per-slice times need one instrumented run). Same slices, same
   seeds. Worth ~2-3 min of Main today and more as slices accumulate.
3. **Fold the two post-ci hops into one** (a coverage-neutral but gate-semantics change): either `timings` runs beside
   `publish-*` instead of after them (its report then misses the two ~10 s publish jobs, which VictoriaLogs still has),
   or one job held from t=0 polls ci and publishes the moment it is green (no hop at all, one runner held for the run,
   and a hand-written equivalent of `needs` in front of the deploy). Worth one queue wait, 0.5-2.5 min under load.
4. Not worth it: more WebKit workers (three already run the rows at 80-89% CPU), fewer setup seconds (setup is ~45 s
   of a 5-8 min sbt row, caches hit).

## 8. The whole convergence run under 10 minutes? (2026-11-02)

The owner: "continue optimizing to get the whole run under 10 mins": first job start to last job end. Since 2026-11-01
22:20Z, measurement runs (`measure/*`) run on ci-runner's 4 `movies-measure` slots and convergence on main on the hosted
20 (section 7's option 1a, commit c04dff246c8).

### Why some rows got slower after the replay hot-spot fixes

Runs `measure/replay-hotspots-{before,after}-{1,2}` (41388709004, 41388736754 / 41388723663, 41388751097) ran at the
same time on the same 4 slots. Heads: before 9e4b4b05, after 00b4c67e (LiveProjectionIndex, DecidedBy, ParsedByContent,
RelayBanner; not yet c4c17de0bf4). Per row, against the number of other ci-runner jobs running beside it:

| row | before (min) | after (min) | what decides it |
|---|---|---|---|
| order p01 (germany) | 8.0, 8.4 (2.8 others) | 8.2 (2.8 others), **13.6 (7.6 others)** | same pin and fills: matched inputs give 400 s vs 393 s of sbt; only the after run with 7.6 jobs beside it was slow |
| order p01 (united-kingdom) | 6.7, 7.0 (~3 others) | 6.9 (4.1 others), **9.8 (7.4 others)** | identical inputs; the same after code ran 324 s and 483 s |
| takes-drift-1 (united-states) | 10.4 (seed 23), 26.2 (seed 19, 8.5 others) | 12.5 (seed 25), 20.3 (seed 21, 8.2 others) | the seed follows the run number, so every run replays a different one; duration tracks the jobs beside it |

**No code regression.** Each slower figure is one run under ~2.5x the contention of its pair, or another drift seed.
ci-runner was 99% busy when sampled (11:00Z: check tier 14 cores, bitcashier builders 11.5, measure tier 2.3).

### Where the wall goes (newest runs)

| | convergence on main, hosted (5 runs to 41404627085) | measurement on ci-runner (6 runs to 41411465401) |
|---|---|---|
| wall p50 | 27:09 | 211:08 (p90 356:40) |
| critical path: queue | 11:41 | **209:40** |
| critical path: work | 15:25 | 8:04 (12-row runs; 26-row runs 10:44-12:38) |
| setup per row (checkout + convergence-setup) | 0.9-1.3 min | 1.0-1.4 min |

Setup and compile are not the lever (about a minute a row, sbt-deps hit 98-100%). Two things are:

1. **Capacity for measurements.** Since midnight the measure slots ran 3.7-3.8 of 4 every hour and the queue held 5-6
   runs; the hosted 20 averaged **4.8 busy** (hourly 0.3-11.6). ci-runner itself is CPU-saturated, so more slots there buy
   nothing: a 26-row run is ~850 vCPU-min, about a whole 16-core host for 50 minutes.
2. **The row floor.** Uncontended hosted rows (p50): drift 13.6-15.3 min (CPU 41-50% of 4 cores: the incremental replay is
   mostly serial), takes 6-13 min (the head's resolve plus the population walk b686b6cad1b added, up to 300 s, report
   only; the base is re-resolved when the rotating fills change its key), order DE p01 12.5. With unlimited runners a run
   still ends ~16 min after it starts.

### What changed

`MOVIES_MEASURE_RUNNERS=hosted` (repository variable, default unset = no change): a measurement's legs and bisects go to
hosted runners while landing checks stay on ci-runner. SelfHostedRunnersSpec holds both sides.
**Retired 2026-11-06** (section 10, "Reversed"): measurements always run on ci-runner, with no way back to hosted.

Measured with the same 12-row measurements before and after the move to ci-runner (`ci-timings compare`, 6 runs each:
hosted 41355106349 .. 41360768671, ci-runner 41374801177 .. 41411465401):

| | on ci-runner | on hosted | change |
|---|---|---|---|
| wall p50 | 165:52 | 40:50 | -75% |
| critical path queue p50 | 158:52 | 27:22 | -83% |
| critical path work p50 | 7:37 | 15:36 | rows run uncontended on ci-runner once they start |

### Verdict

Under 10 minutes is not reachable with identical coverage on this hardware: ci-runner is saturated and the longest rows
alone take 13-15 min. It needs, in order:

1. **Owner: switch measurements back to hosted** (`gh variable set MOVIES_MEASURE_RUNNERS -R pawelkrupinski/movies --body hosted`):
   measurement walls from ~3-6 h to ~40 min today. To keep Main from queueing behind them, **GitHub Pro** ($4/month)
   doubles the hosted concurrency to 40.
2. **Rows of at most ~8 min**: the drift replay (serial, 2 of 4 cores busy) and the takes row's population walk
   (report only; it could be a row of its own) are the long poles, both identity-owned code.
3. Then, with ~27 free runners at a run's start, wall ≈ preflight + the longest row + verdict, about 10 min.

## 9. Every row under 8 minutes? (2026-11-02)

The owner: every row of the convergence run at most 8 minutes of its own time, start to end, queue excluded — the
step toward the whole run under 10. Before (7 green runs on main on hosted runners, 41368107199 .. 41436200288, p50):
drift rows 11.6-14.3, takes 6-11.9, order 10-12.5, convergence 4.3-10.8, next-day 9.8.

### Where a row's time went

- **Drift rows compiled.** A takes or drift row restored the convergence key's exact entry or Main's e2e entry for the
  commit (a suite row's, the e2e job's: no worker test classes) ahead of the takes family's prefix, and compiled
  scrapers/Test and worker/Test, 891 sources: ~150-190 s of every drift row (sbt `Total time` 674-807 s against a
  replay of 518-632 s, run 41404627085).
- **Drift replay on one core.** JFR (drift-only run 41442178510, profiled): the replay's thread is the row; of it,
  `IdentityResolver.resolveRegion` 45-52%, `AgreementStage.apply` 20-40%, the whole resolve 9-12%. The incremental
  model resolved an update's packed regions one after another.
- **Takes rows in turn.** One sbt session ran the head's ratchet (87-253 s), then its resolve (142-388 s, the
  population walk up to 279 s of it, one family after another), then the base was resolved after it (98-252 s; the
  rotating fills change its key every run).
- **Order and convergence rows are the production pipeline**, CPU-bound on 3.3-3.9 of 4 cores (order) or bound on
  the model's first projection (convergence: `identityTick` 195-255 s of a 330-400 s boot). Hosted runners' CPUs
  differ ~2x: the same US boot took 174 s on an EPYC 9V45 and 332 s on a Xeon 8370C.
- `FamilyScope.denialByNode`: 5.5-9% of order and convergence CPU, asked again per candidate by every pooled cluster.

### What changed (landed f43b1fcd5e)

| commit | what | for |
|---|---|---|
| FamilyScope: denialByNode memoised | per (node, film), a pure function within one scope | order, convergence CPU |
| PopulationMonotonicity on every core | families walked side by side, findings gathered in family order (DaemonExecutors' bounded pool) | takes |
| Takes check: final takes beside the resolve | the head's ratchet in a checkout of its own, its own sbt, while the session resolves (session heap at most 8g beside it) | takes |
| takes-base row on main | the base's resolve and final takes on a runner of their own, as a takes-only measurement already did; the takes row waits for them (a base row still queued leaves the base to it) | takes |
| setup: takes family key for takes and drift rows | their own key is `sbt-target-takes-…` and they skip Main's e2e entry, so the restore carries worker's test classes | drift (and takes) |
| IncrementalResolver `regionThreads` (opt-in) | a round's regions resolved side by side, folded in batch order; LiveCorpus's read views under its lock; production and every caller but IdentityDrift keep 1 | drift |
| Poland's order passes in their own rows | `convergencePolandWithoutOrder` + `order-independence-p01/p12 (poland)`, as DE/UK/US | PL convergence |
| profiles | `profile=true` records JFR in takes and drift rows too, 1024-frame stacks | measuring |

agentctl needed no change: its readers key on `takes`/`takes-drift-N` rows and the takes rows' verdict.txt (a `takes-base`
row is one more `takes…` row, green when it stores the base); `convergence_reuse.py` mirrors the new matrix (34 rows) and
the gate's verdict rows are unchanged. The new rows are registered unproven in `.github/gating-checks.tsv`.

### Proof: nothing moved

- **Takes**: branch runs 41444461247 and 41444484357 (takes base = main ca95b8aa31, both sides resolved), every country
  `lost 0, changed 0, gained 0` for the model and the final takes; `scripts/identity-measure --run` LANDABLE for both.
- **Drift**: drift-only runs on seeds 37 and 38, main ca95b8aa31 (41442986849) against the branch (41443007331), side by
  side: all 10 rows' `drift-<cc>.tsv`, `flips-<cc>.tsv`, `pending-<cc>.tsv`, verdict and report (seconds aside) byte
  identical. Replay seconds per seed: PL 409→184 / 331→315, DE 629→348 / 628→338, UK 518→543 / 683→410, US 622→263 /
  606→345, ES 47→33 / 45→56 (different machines; see the CPU note below).
- **PopulationMonotonicity**: the `kind=identity.population_flips` line, `seconds=` aside, identical to main's run
  41436200288 in every country (DE: 9 families, 23 listings flipped; UK 5; PL 1; ES 1; US 0). Walk seconds DE 267→108,
  UK 121→38-65, US 104→41-71, PL 84→43.

### Row times (`ci-timings compare`, own time, queue excluded, hosted runners)

Before: 7 green main runs 41368107199 .. 41436200288. After: branch runs 41444438573, 41444461247, 41444484357
(runner=ubuntu-22.04, 12:24-14:00Z). Load: the account was saturated while they ran (my three runs, drift-only and
experiment runs, Main's own convergence): critical-path queue 76 min p50 against 12, so only the per-row times compare.

| row | before p50 | after p50 | after p90 |
|---|---|---|---|
| takes-drift-1 / -2 (US) | 11.9 / 14.3 | **6.4 / 8.1** | 8.4 / 9.5 |
| takes-drift-1 / -2 (UK) | 13.4 / 13.7 | 11.1 / 11.3 | 11.4 / 11.4 |
| takes-drift-1 / -2 (DE) | 11.7 / 12.4 | 9.7 / 9.4 | 9.9 / 10.1 |
| takes-drift-1 / -2 (PL) | 8.3 / 7.6 | **7.4 / 6.7** | 7.6 / 7.5 |
| takes-drift-1 / -2 (ES) | 4.8 / 4.9 | **2.7 / 2.4** | 2.9 / 2.7 |
| takes (US / UK / DE / PL / ES) | 11.9 / 11.6 / 10.0 / 7.3 / 6.1 | **8.6** / **5.6** / **8.0** / **5.5** / **4.5** | 9.4 / 7.6 / 8.5 / 5.6 / 5.2 |
| takes-base (new) | - | 0.9-1.3 | 1.0-4.0 |
| convergence (PL) | 10.6 | **5.7** | 6.6 |
| order-independence-p01 / -p12 (PL, new) | (in the PL convergence row) | **6.4 / 6.5** | 6.8 / 6.6 |
| convergence (US / UK / DE / ES) | 10.8 / 9.6 / 10.6 / 4.3 | 9.2 / 8.3 / 12.6 / 4.9 | 10.1 / 9.8 / 12.7 / 5.0 |
| next-day (US) | 9.8 | 9.5 | 10.2 |
| order p01 / p12 (DE) | 12.5 / 10.4 | 12.2 / 12.6 | 12.9 / 12.9 |
| order p01 / p12 (UK) | 10.0 / 10.0 | 10.2 / 8.8 (3 runs: 6.6-10.5) | |
| order p01 / p12 (US) | 11.5 / 11.3 | 7.9 / 11.3 (3 runs: 7.8-12.4) | |

**The CPU lottery decides as much as the code.** Hosted 4-core runners are AMD EPYC 7763, 9V45, 9V74 or Xeon
8370C/8573C, and the same work differs ~2x between them: UK drift-1 replayed seed 37 in 518 s on a 9V74 (main) and 543 s
on a 7763 (branch); US convergence's boot took 174 s on a 9V45 and 332 s on an 8370C. Three runs a side cannot separate
less than ~20% on the CPU-bound rows; the drift-only pair on fixed seeds is the cleaner comparison.

### What one order pass per row would give (experiment, not landed)

`measure/rows8-onepass` (run 41441424195) ran ONE pass per order row (passes 0 and 2, nothing compared): UK 7.7 / 8.0,
US 8.7 / 8.6, DE 9.3 min, against 10-12.5 for two concurrent passes. A pass alone uses ~2 of 4 cores (UK: 312 s,
636 cpu-s). Landing it needs the comparison moved out of the row: each row writes its `ReplayCorpus` (serialised), and a
light `order-independence (<country>)` job after the country's rows deserialises them and runs `CorpusComparison`
(the same `==` and the same divergence report) — about a day's work with its wiring specs, and one more hop on the
run's critical path.

### Verdict: what is still over 8 minutes, and what it would take

Under 8 now (p50): all takes rows but US (8.6), every PL and ES row, US drift-1, the new PL order rows. Still over:

1. **UK drift (11)**, **DE drift (9.5)**: the replay's thread is `AgreementStage.apply` (20-40%) and regions too large
   to split (UK's opera/ballet families exceed one region). Next: the whole resolve beside the sequence, and a look at
   `AgreementStage.apply`'s per-projection cost — identity-owned code.
2. **Order rows (DE 12-13, UK/US 8-12)**: the production pipeline on all four cores. One pass per row (above) brings UK
   to ~8, US ~8.6, DE ~9.3; the rest is CPU.
3. **Convergence DE (12.6) / US (9.2) / UK (8.3), next-day US (9.5)**: the boot's first projection (the model's restore,
   195-255 s) runs its regions one after another. `regionThreads` would cut it ~2-2.5 min, but in the production wiring
   the model's lookups track which question read which document (ObservationReads), and two regions asking one question
   at once race there; making that safe is a production change for the identity owner, not done here.
4. **Owner action, hardware**: these rows are CPU-bound on 4 vCPU. GitHub's larger runners (Team plan, 8-core Linux
   $0.032/min) would take the order and convergence rows to roughly 60% of today (they keep 3.3-3.9 cores busy),
   and remove the 2x CPU-model spread; ci-runner is saturated, so more self-hosted slots there buy nothing.

## 10. The landing check's wall time, before and after the speed-up (2026-11-03)

**What changed** (all on main): `fbfbe220f4b`, `326a136e39e`, `b84fa13bdae`, `be5a6b1812f` -- blobless checkout, set-up only
for the selected shares, identity in 3 shards, heavy rows in 2 jobs, optional jobs gated by the plan -- and `5fc05de190d`
(identity shards only when the plan gives an identity check).

**Method.** Existing `precommit-check.yml` runs only, green only, no new dispatches. Wall time = first step start to last
step end over the run's jobs (step timestamps only; the `timings / report` job is left out). Change type: identity =
the run has identity-check jobs; other = scripts/docs-only (no identity job). Before = runs created before `fbfbe220f4b`
(2026-11-02 06:09Z; the API listing reaches back to 2026-11-01 18:53Z); transition = between the two commits (the
changes landing one by one); after = from `5fc05de190d` (08:10Z). Queue wait = first job's first step minus its creation.

| change type | window | n | wall p50 | wall p90 | queue wait p50 (first job) | critical-path queue p50 / p90 | every job's queue summed p50 / p90 |
|---|---|---|---|---|---|---|---|
| identity | before | 18 | 17.1 min | 32.6 min | 0.1 min | 0.6 / 18.1 min | 4.7 / 38.9 min |
| identity | transition | 13 | 26.0 min | 42.6 min | 0.6 min | 21.7 / 37.1 min | 52.7 / 105.7 min |
| identity | after | 12 | 16.4 min | 21.4 min | 0.1 min | 0.7 / 8.1 min | 1.3 / 15.4 min |
| other | before | 1 | 3.5 min | 3.5 min | 2.0 min | 2.0 / 2.0 min | 2.0 / 2.0 min |
| other | after | 10 | 6.5 min | 9.4 min | 0.1 min | 0.6 / 7.5 min | 1.0 / 9.0 min |

The two queue columns (added 2026-11-03) come from `ci-timings report --by change --per-run` over the same windows
(`--since/--until`; its window is the first job's start, so its n differs by one or two from the table's: 20/13/13
identity, 1/11 other). Critical-path queue = the waits of the jobs on the run's critical path (from the job that ended
last back through the job that released it); every job's queue = all of the run's jobs' waits added up, a measure of
slot contention rather than of wall time.

**Reading it.** For identity changes the median barely moves (17.1 to 16.4 min) but the tail shrinks by a third (p90
32.6 to 21.4 min), and the baseline's own 17:08 p50 is reproduced. The transition window is slower (26 min p50) because
it ran under heavy slot contention (the last job of a run waited 18 min p50 for a slot), not because of the change; the
first job's queue wait stays near zero throughout, so wait shows up as later jobs starting late, not as a late start.
"Other" has only one before-run, so no before/after claim for it: after, scripts/docs-only checks take 6.5 min p50.
`ci-timings compare` was not usable for this split then (it keyed on branch/range, not on change type, and took minutes
per cold selection); the numbers came from the Actions jobs API directly. Now `report --by change` splits identity from
other runs, `--per-run` prints one row per run (wall, created-to-end, critical-path queue and execution, every job's
queue summed), and a cold 40-run selection with metrics takes ~35 s (runs fetched 8 at a time) instead of minutes.

**What limits it now.** The 6 check slots on ci-runner. Sharding adds parallelism only while slots are free: under load
the identity suites run about 2x slower and the extra jobs queue for slots, so the p90 follows the number of checks in
flight, not the shard count.

**Next lever.** Cut the work rather than spread it: skip identity shards whose inputs a change cannot touch (plan-level
selection by path, as 5fc05de190d did for the whole group), or raise the slot count on ci-runner if memory allows.

## 11. The landing check under 10 minutes: where the minutes go, and a build cache of its own (2026-11-03)

**Where they go** (`ci-timings report --workflow precommit-check.yml --by change --per-run`, identity changes after
`5fc05de190d`, 13 runs): wall p50 17.0 / p90 22.0 min; critical path p50 queue 0.7 + execution 16.2 min. At the median
it is execution, not waiting. One identity shard (run 41604183761, shard 2/3, 16:20): set-up 1:25; `sbt identityImpact`
(compile + selection) 3:02 at ~600 CPU-seconds; its third of the identity specs 11:48 at ~2,000 CPU-seconds. ci-runner
is one 32-thread Ryzen 9 5950X with 6 check slots: during an identity check it is 85-95% busy with 30-50 runnable
threads, and the same shard's tests took 6:02 when it started after the others (≈2× from contention). An identity check
burns ~10,300 CPU-seconds. The shards are even (suite time 2,739 / 2,832 / 2,685 s); single "real family" tests of
TitleRelationSpec, ReadSetClosureSpec and PopulationMonotonicitySpec take 400-570 s each under that load.

**Why every job compiled ~700 sources.** The check's sbt jobs restored the shared `sbt-target-<os>-` family, which no
Main job writes since each Main row keyed a family of its own; only occasional identity/discovery workflows wrote it.
On 2026-11-03 every check restored `sbt-target-Linux-abc71c15…`, saved 2026-11-02 16:55 (108 MB, no web, web tests or
e2e tests), so each of a check's 5-6 sbt jobs recompiled common's tests (234 sources), web (237), web's tests (220) and
e2e's tests: ~3 min and ~600 CPU-seconds a job, ~30% of the check's CPU, all at once on the saturated host.

**What changed.** `precheck-build-cache.yml` runs after every Main run on main, on a hosted runner: it compiles what a
check compiles (identityImpact's every module's main, fixtures and tests, plus web's and worker's integration tests) and
saves the `sbt-target-precheck-<os>-` family, which only it writes. Every sbt job of `precommit-check.yml` restores that
family first (an exact hit when the change touches no Scala source), the old shared family only until its first entry.

**Before/after** (CI; before = run 41604183761, a 25-path identity change; after = measurement run 41611692664, a
comment-only change to IdentityMeasures.scala on main c5d450409c3, the only landing check running; seeded by
precheck-build-cache run 41611023906, 3:46 on a hosted runner, entry 67 MB):

| | before | after |
|---|---|---|
| build cache restored | `sbt-target-Linux-abc71c15…` (2026-11-02 16:55, partial) | `sbt-target-precheck-Linux-82867c69…` (minutes old, complete) |
| identity shards: `identityImpact` (compile + selection) | 3:02 / 2:30 / 2:54 | 1:31 / 1:34 / 1:30 |
| identity shards: tests | 11:48 / 6:02 / 11:30 | 16:00 / 9:44 / 14:46 |
| check (rest): pre-push sbt | 445 s | 445 s |
| wall | 19:41 | 20:51 |
| ci-runner during the run | busy 86-95%, run queue ~40 | busy 94-97%, run queue ~60 |

The compile step halved in every shard (−1.5 min a job on the critical path, about −1,000 CPU-seconds a check), but
the wall did not move: the test phase ran ~40% slower because the host was busier. The after run was the ONLY landing
check, yet three convergence measure legs (`ci-runner-movies-measure-1/2/4`, run 41608446529's takes-drift rows) ran
on the same 32 threads. The landing check's wall now follows what else runs on ci-runner, not its own work: the next
lever is CPU priority for the check slots over the measure slots (infra: cgroup `cpu.weight` per runner pool, or fewer
measure slots while checks run), then splitting the single 400-570 s identity tests so no shard waits on one.

## 12. A title's credited-work words, once per title (2026-11-03, owner-approved)

**Why.** The shard-CPU profiles (§11's follow-up; PopMono, ReadSetClosure, drift replay) put ~20-25% of each resolver
spec's CPU in `TitleRelationMeasures.containment`, nearly all of it the author-credit regex `creditedWork(form.text)`,
re-run on every own form of the listing for every candidate film of every resolve.

**Change.** `TitleForm.creditedWords`, a lazy val of the form's credit-stripped words (`creditedWork(text)`'s words, or
`words` when the title closes on no credit); `containment` reads it. A pure function of the form's own text: TitleForm
is a content-keyed case class, nothing is cached across listings, and a lazy val's first read is synchronised, so the
parallel resolves read one value. It moves the identity-rules digest (the next deploy re-resolves stored families once);
the owner approved that on 2026-11-03.

**Proof: nothing moved.** IdentityMeasuresSpec checks the memo against the per-pair expression over 4,000 seeded random
titles (credits in three languages, one-word names, brackets, accompaniments), twice per form and from 8 threads at
once. Whole-corpus measure (identity-measure, all countries, run 41674758328, 185,197 listings over pl/de/uk/us/es): 0 takes moved in every country, model and final stage (LANDABLE). Drift CI
(identity-drift-ci --against main, run 41674804919): CLEAN.

**Gain.** One concurrent precheck pair (`ci-timings report --by change --per-run --critical`): before 41675753879
(main + comments in the same files), after 41675756995. The identity shards' pre-push step: 12:15 / 11:48 / 9:53
before vs 11:52 / 13:43 / 5:47 after (sum 33:56 vs 31:22, -8%); exec critical path 12:36 vs 8:08. The after run's jobs
queued 26 min to the before's 15, on a busier host, so the pair is noisy. Local ReadSetClosurePart1Spec user-s (188/188
before, 172/194 after) was taken at load average 58 on 14 cores: no signal.

## 13. Convergence in 20 runners, longest first under per-leg caps (2026-11-06)

The owner: "reduce convergence run to 20 runners at most and balance the times (minimize)".

**Before** (12 complete main runs, 42112839746 .. 42225884509; 53 jobs each): wall median ~37 min (27.8-50.5), peak
16-20 jobs at once, 285-369 runner-min. Rows were queued all at once and GitHub picked which started (section 2): the
longest row, `takes-drift-1 (germany)` (median 20.4 min, p90 36), started up to 10 min late, and each split country's
`order-compare` waits for every row of its country, so that drift row's start decided the run.

**Change** (`scripts/ci/convergence_slots.py`): section 2 measured that list order does not order GitHub's queue when
every row is queued at once. A `max-parallel` cap changes what is queued: only the first `max-parallel` rows of a matrix
are, the next when one ends. So the preflight now hands each leg its rows longest first (`phases`, from each row's
median job minutes in `scripts/ci/convergence-row-minutes.tsv`) and a cap, its share of the 20. Every country starts
with one slot, and each next slot goes to the country that ends last (its `order-compare` included) until the 20 are
given out. `takes-base` starts just before `takes`, because the takes row waits for the base row only while that row
runs. A takes-only or drift-only dispatch keeps the leg's own rows, uncapped. The rows, their checks and their names are
unchanged, so the gating registry is unchanged.

**Predicted** (`convergence_slots.py plan`): de 5 slots, us 6, uk 4, pl 4, es 1 (20 in all). The rows end ~21.8 min
after they start (de's drift-1 plus its compare). The wall is about 24 min with preflight, verdict and timings, against
~37. Runner-minutes are unchanged (~300-330), since every row is still its own job. Merging the 1-3 min `takes-base`
rows into others would save ~1 min of setup each but would rename gated jobs. Not done. The remaining floor is the
German drift row itself (p90 36 min). Splitting its seeds is the drift harness's business.

**Measured** (first main run with it, 42239410325, 200f38091b): green. Each leg started its rows in the listed order
(Germany's drift-1 1.9 min in, Spain's five one after another), peak 19 jobs at once, 286 runner-min. Every country
except Germany ended by 21.3 min, as predicted. The wall was 41.1 min because `takes-drift-1 (germany)` itself ran 35.9
min (its p90). Before, runs whose German drift row ran that long took 44-50 min (42116325192, 42225884509). The German
drift row is now the whole remaining gap. A median run should land near the predicted ~24 min.

### No drift row on the critical path (2026-11-06, later)

`takes-drift-1 (germany)` had held two seeds since the explorer added 581 to it that morning. Seed 8 alone is a 15-21 min
row, and 8 plus 581 took 19-36 min. Seed 581 now has a row of its own, `takes-drift-3 (germany)`. A country may name
further rows in `drift-gate-seeds.tsv` (`drift_seeds.py rows_of`), and the preflight plans every row a country has, for
main's suite and for drift-only dispatches alike. The seed set and the per-seed verdicts are unchanged. A pin reaches rows
1 and 2 only. `identity-drift-ci --for-red` on an extra row's red replays every row of that country on its fixed seeds.
The plan: de 5 slots, and the rows end ~20.4 min after they start (seed 8's row 19.0 is the longest), within 20 runners.

## 10. Main's rows on ci-runner's free slots (2026-11-06)

**Reversed the same evening (see "Reversed" at the end of this section).** Main is back on GitHub's hosted runners
only, and measurements are on ci-runner only.

The owner: "make movies runs take all the ci-runner runners when bitcashier is not using them", and then approved
three things. Main and convergence on main may use movies' self-hosted slots, which **reverses 2026-11-01's "Main's own
runs never leave GitHub's hosted runners"**. Movies gets extra slots when bitcashier is idle, up to the host's memory, and
bitcashier always wins. Among movies' own jobs, landing checks go first, then main's rows, then measurements.

### Before (measured 2026-11-06, jobs API)

- **bitcashier** (org runners, 12 builders and 6 deployers, persistent): 2026-11-01..06, no builder was busy for 71% of
  the minutes, the mean was 1.1 busy, and 6 or more were busy 7% of the time. That is ~1,700 job-min a day.
- **movies' 10 slots** (6 `movies-check`, 4 `movies-measure`, last 48 h): 5.0 busy on average, all 10 busy 15% of the
  time, none busy 21%.
- **hosted**: all 20 busy 29% of the time. Convergence rows queued p50 3.7 min, p75 9.4, p90 13.0, 14,086 job-min in
  48 h. While hosted jobs queued, ~4,100 slot-min stood idle on ci-runner.
- **memory**, not runner count, is the host's limit: a movies job holds 7-13 GiB (cgroup peaks), so the 125 GiB host (ci.slice
  115 GiB) fits ~10 jobs. bitcashier's builders have peaked at 93 GiB together.

### The design

- **The scheduler** (`scripts/ci/runner_scheduler.py`, pinned onto ci-runner by bitcashier's infra) runs every 30 s. It
  reads bitcashier's runners from their cgroups. While every builder or every deployer is busy, it starts no movies
  runner and stops idle ones. Its ceiling is (115 GiB − 8 − 8 per busy bitcashier job) / 10 GiB, at most 10. It reads
  movies' queued jobs from the API and starts one ephemeral runner (`movies-runner@<slot>`) per queued job, using the
  label of that job's class only: check, then main, then measure, then light (`movies`). A runner that is running a job
  is never stopped.
- **Main's share**: the scheduler publishes `free_for_main` to `capacity.json` on the release `ci-runner-capacity`. That
  number is the ceiling, less what is running or about to be started, less one slot kept for the next landing check. On
  main, the convergence preflight's `capacity` step reads it, and a file older than 3 minutes reads as 0.
  `convergence_slots.plan(self_hosted=N)` then hands the N longest rows (never the takes pair) `movies-main` through
  each leg's `row-runners`. Those rows lead their leg's list, on top of the leg's hosted share. Nothing waits on
  ci-runner: with ci-runner busy or down, every row is hosted, as before.
- **Gating**: `movies-main` is a `label` row in `.github/gating-checks.tsv`, `unproven`. A row that fails on it reports
  and holds no push until a green run proves it (`gating_checks.py prove`).
- **CPU**: checks keep CCD0 to themselves (2026-11-03). Main's rows share CCD1 with measurements, at weight 200 against
  their 100.

### Measure after

Once the owner applies it, compare 10 main convergence runs on each side with `ci-timings compare --workflow
identity-model-convergence.yml`: the convergence rows' queue, the run's wall time, and the rows' durations on
ci-runner against hosted. Ten movies jobs on 32 threads oversubscribe the host, and section 8 measured rows up to 2.5x
slower under contention.

### Reversed (2026-11-06, evening)

The owner: **"run main branch convergence and Main workflow always on gh, measurements always on ci"**. This replaces
the design above before any run used it. The `movies-main` label was never proven in `gating-checks.tsv`, and
ci-runner's scheduler had not been re-pinned onto it. What changed:

- **Main and main's convergence are on GitHub's hosted runners only.** The preflight's `capacity` step is gone, and
  so are the legs' `row-runners` input, `convergence_slots.plan(self_hosted=)`, `convergence_reuse` `self_hosted`
  plumbing and the `movies-main` label row. With it went `gating_checks.py`'s `label` kind, which only that row used,
  and RunnerLabel's `matrix.`/`fromJson` support, which only `row-runners` used. Every row of a leg runs on the leg's
  one `runner`.
- **Measurements are on ci-runner only.** This covers every convergence run off main (measure/*: identity-measure,
  identity-drift-ci, convergence-bisect and the landing check's suites). The `MOVIES_MEASURE_RUNNERS=hosted` escape
  from section 8 is gone (the variable was unset). `MOVIES_CI_RUNNERS=self-hosted` stays the one switch: unset, while
  ci-runner is down, everything is hosted. SelfHostedRunnersSpec holds both sides.
- **The scheduler** (`runner_scheduler.py`) keeps bitcashier-first and the memory ceiling, with classes check, then
  measure, then light. It has no `main` class and no longer publishes `capacity.json`, so its `check_headroom` and
  `publisher_*` config keys are refused as out of date. ci-runner runs the pinned code (bitcashier infra
  `movies-runner-scheduler-pin.nix`, 171528495f) until the owner re-pins it. Until then, the old code does no harm. No
  job asks for `movies-main`, so its `main` class never starts a runner, and nothing reads the `capacity.json` it
  still publishes every minute on the release `ci-runner-capacity`. The file just goes stale. The re-pin also drops
  the publisher keys, the lander-key credential and the `ci-movies-main` slice. After the re-pin, the
  `ci-runner-capacity` release can be deleted.

## 14. Every other workflow on ci-runner (2026-11-06, night)

The owner extended the decision above: "distribute other workflows as well". There are three classes:

- **(a) hosted.** THE GATE's workflows on main and anything a push to main waits on.
- **(b) ci-runner.** Measurements and every other Linux workflow off main's critical path. They use the scheduler's
  labels, `movies` for light jobs and `movies-measure` for heavy ones (sbt, mongod, nix builds, browsers), with
  bitcashier first.
- **(c) hosted.** What the movies-runner container cannot give, and since 2026-11-08 (the owner) Nightly stress:
  browser timing stress needs steady CPU, which the shared host's containers do not guarantee (Nightly stress #29).

A (b) job's `runs-on` is `vars.MOVIES_CI_RUNNERS == 'self-hosted' && '<label>' || '<its old hosted label>'`. So the one
repository switch still sends everything hosted while ci-runner is down (`ubuntu-22.04` stays the fallback of a mongod
job, MongoServerVersionParitySpec). No (b) workflow is time-critical enough to earn a hosted fallback of its own: none
gates a push, and the Stage NixOS closures deploy waits at most one queue. No new label or job reaches THE GATE, so
`gating-checks.tsv` gains no row. SelfHostedRunnersSpec holds it: every job of every workflow outside the gate side
must ask for a ci-runner label unless it is listed as needing a hosted machine. Each of those workflows also sets
`LANG`. `report-timings.yml` now runs where its caller runs: hosted only for Main and Identity model convergence on
main (`github.workflow`, the caller's name).

### The classification

The minutes are hosted runner-minutes over the 14 days to 2026-11-06, from the jobs API (job start to end).

| workflow | trigger | class | ci-runner label | 14-day hosted min | why |
|---|---|---|---|---|---|
| main.yml, ci.yml | push main | a | -- | (gate) | THE GATE |
| identity-model-convergence.yml, country-convergence-leg.yml on main | Main's kick, convergence-daily | a | -- | (gate) | THE GATE (off main it is a measurement: `movies-measure`, section 10) |
| report-timings.yml | called | a for the gate's callers, b otherwise | movies | -- | runs beside its caller |
| branch-ci.yml | bot PRs | a | -- | 260 | it is ci.yml, Main's own suite and runner rules |
| precommit-check.yml | check/** | b (already) | movies-check | 0 | landing check |
| convergence-bisect.yml | dispatch | b (already) | movies-measure, movies | 16 | measurement |
| record-scrape-fixtures.yml | nightly | b | movies-measure (legs), movies | 1,235 | non-gating capture |
| verify-scrape-recording.yml | after the recording | b | movies-measure (legs), movies | 1,381 | non-gating replay |
| identity-family-explorer.yml | weekly | b | movies-measure, movies | 1,038 | nightly explorer |
| order-independence.yml: seed, scala, android, report | nightly | b | movies-measure, movies | 1,283 | nightly |
| order-independence.yml: swift, ios (macOS), android-instrumented (emulator) | nightly | c | -- | 1,167 | macOS; KVM not proven in the container |
| nightly-stress.yml | nightly | c (2026-11-08) | -- | 805 | browser timing stress needs steady CPU: on movies-measure a co-tenant starved every Playwright worker at once (Nightly stress #29) |
| precheck-build-cache.yml | after Main | b | movies-measure | 647 | warms the landing checks' cache, gates nothing |
| tool-self-tests.yml | push main | b | movies, movies-measure | 580 | not read by THE GATE |
| nix-stage-closures.yml | push main | b | movies-measure | 174 | stages closures, activates nothing; hosts' port 22 is public |
| identity-lean-whole.yml, identity-live-drift.yml | after runs | b | movies-measure, movies | 172, 113 | report-only identity checks |
| identity-drift-explorer.yml | nightly | b | movies-measure, movies | 90 | nightly explorer |
| identity-run.yml, identity-refit.yml, identity-capture.yml, identity-decorations.yml, identity-rule-ablation.yml | dispatch, weekly | b | movies-measure, movies | 79, 37, 26, 0, 0 | identity bots and tools |
| prune-actions-caches.yml, identity-ci-staleness.yml, decodo-probe.yml, identity-findings-report.yml, security-headers.yml | cron | b | movies | 94, 39, 23, 13, 13 | light scripts |
| android.yml: preflight, build | push main | b | movies, movies-measure | 48 | Gradle on the hosted toolset copy |
| android.yml: instrumented | push main | c | -- | 33 | emulator: KVM not proven in the container |
| ios.yml: unit-integration | push main | b | movies | 21 | Linux checks |
| ios.yml: ui-tests | push main | c | -- | 418 | macOS |
| decoration-discovery, discover-cinemas, retire-venues, filmweb-diff, roster-audit, country-fixture-artifact | weekly bots | b | movies-measure, movies | 36, 1, 0, 22, 4, 10 | bots |
| nix-update, renovate, scala-steward, convergence-daily, convergence-fill, archive-recording, page-test-repeat | cron, dispatch | b | movies-measure or movies | 9, 2, 18, 4, 0, 0, 0 | bots and tools (convergence-daily's dispatched run is the gate's: a) |

Totals: **8,017 hosted min in 14 days move to ci-runner, ~570 a day**, of which ~515 a day are heavy
(`movies-measure`). Hosted keeps THE GATE plus (c): macOS (~106 min a day) and the two emulator jobs (~10 a day).

### Load on ci-runner

- **Free capacity.** Section 10 measured movies' 10 slots at 5.0 busy on average, or ~7,200 free slot-min a day, so
  570 more fits on count. Memory, not count, is the real limit. The heavy part goes to `movies-measure`, whose slice
  (`ci-movies-measure`, MemoryMax 48G) fits ~4 jobs of ~10 GiB. On 2026-11-02 measurements alone kept it at 3.7-3.8
  of 4 every hour.
- **When it lands.** Most of it is the nightly window, 23:30-03:00 UTC: the recording and its verify (~190 min a
  night), nightly-stress (~58), order-independence (~92), the drift explorer, and the family explorer's ~260 min every
  Sunday. Measurements are fewest then, but not absent.
- **Priority.** The scheduler serves checks, then measurements, then light. The nightly heavy jobs share the
  measurement class: they queue with the measurements, first come first served, still behind every landing check and
  every bitcashier job.
- **If it saturates** (watch `ci-timings compare` on measurements, queue p50 before and after), move these back to
  hosted, in this order. Each is one `runs-on` back to its literal label, plus its row in SelfHostedRunnersSpec's
  `NeedsHosted`:
  1. identity-family-explorer `walk` (~74 min a day as an average, all on Sunday).
  2. verify-scrape-recording's legs (~99 a day).
  3. record-scrape-fixtures' legs (~88 a day).

  Together they are 3,654 of the 8,017. The alternative is a scheduler class of their own below measurements, which
  would put them on the light label.
- **Egress.** The recording, the discovery bots, decodo-probe, filmweb-diff and security-headers now reach cinema
  sites, Filmweb and the proxy from ci-runner's address, bitcashier's host, instead of GitHub's pool. Watch for blocks
  that point at that address.

## 14. Faster without a fast/slow split (2026-11-06, owner's option C)

The owner asked to "speed up or split up convergence test workflow into slow and fast". Over 18 main runs
(42134462077 .. 42290433380) and the 44 red runs since 10-23, the commonest red row kind is also the slowest: the drift
rows (red in 15 runs, alone in 13; `takes-drift-1 (germany)`, seed 8, a 17-18 min replay). Takes rows were red in 14
runs, order-independence in 4 (one Polish episode), convergence in 3, next-day in none. A fast tier gating without drift
would have let the 13 drift-only reds through, and one with drift is no faster. So no split. The two runs after seed 581
got its own row took 21.9 and 23.8 min wall and 269-296 runner-min.

**Done:**
- **The drift harness's A/A in two rows.** `takes-drift-aa` replayed the row-1 seeds twice in turn: 35-69 min for
  Germany, the critical path of every harness push (3 of the 18 runs, walls of 40-72 min, 420-441 runner-min). Each
  replay is now a row (`takes-drift-aa-pass0|1`), and `order-compare` diffs their TSVs as `takes-drift-aa (<country>)`
  (scripts/ci/drift-replays-compare.sh). Predicted: a harness push ends with the ordinary run plus the ~1.5 min
  comparison.
- **Timings off the verdict.** The run's end is THE GATE's verdict, and its last job, the report, took 40-70 s. The
  `timings` job now dispatches report-timings.yml with the run's id (seconds), and that run waits for this one to end.

**Measured and not done:**
- **The drift rows in a matrix job of their own, so `order-compare` does not wait for them.** That saves the compare's
  ~1.5 min only with spare runners. Under the 20-runner cap two matrices fragment each country's share: the best split
  of 20 runners ends the rows at 23.1 min (us 26.8 on 4, 22.6 on 5), against 20.6 for today's single matrix. Revisit if
  the cap goes.
- **Compile once in the preflight.** The rows already restore the run's exact sbt-target key once the first row has
  saved it. In run 42290433380, 22 of 51 rows (the first ~20 to start, before any had saved) restored a partial key,
  then compiled one or two sources (about 10-30 s). A build ahead of the rows would save ~10 runner-min a run and add
  its own 2-3 min (sbt boot, compile, save) to the critical path.

**What is left:** the critical path is the Germany seed-8 drift replay itself, ~18 min of one JVM. Making it cheaper is
the drift harness's work, measured separately.

## 15. The takes rows in a workflow of their own, both in one pool of 20 (2026-11-08)

The owner: "split the TAKES rows of Identity model convergence into a separate workflow that gates on main", and "make
whatever kicks off convergence kick off both".

**The split.** `identity-takes.yml` (Identity takes) runs every leg's `takes`, `takes-base`, `takes-drift-<n>` and, on a
harness push, the A/A rows and their `takes-drift-aa` comparison, through the same `country-convergence-leg.yml` (so every
check keeps its name, `leg (<country>) / takes (<country>)`), with `verdict complete` (G7). `identity-model-convergence.yml`
keeps `convergence`, the order passes and their comparisons, and `next-day`. Convergence's preflight dispatches the takes
run first (`scripts/ci/dispatch-takes.sh`: same ref, its inputs, `parent-run`; the takes run is titled
`Identity takes (convergence run <id>)`), so Main's kick, the daily run, `identity-measure --full` and a hand dispatch all
start both. Takes-only and drift-only requests (identity-measure, identity-drift-ci, the drift explorer) dispatch Identity
takes alone: they ask for no convergence row. Each workflow holds a lane of its own (newest-commit batching, as before).

**The 20 runners.** GitHub's hosted concurrency is the account's, and the two runs overlap almost whole, so the caps of
both runs' legs must sum to 20. A fixed split of the 20 between the workflows ends later: each suite's slowest country then
waits on its own suite's slots (`convergence_slots.py`, from `convergence-row-minutes.tsv`):

| split (convergence / takes) | convergence rows end | takes rows end |
|---|---|---|
| 9 / 11 | 26.1 min | 19.0 min |
| 10 / 10 | 26.0 min | 19.7 min |
| 11 / 9 | 20.1 min | 27.8 min |
| one pool, 20 legs (`plan_suites`) | 26.0 min | 19.7 min |
| before: one workflow, 5 legs | 20.6 min (both) | |

So both preflights plan both suites' ten legs in ONE pool (`plan_suites`: each leg one slot, each next slot to the leg
that ends last) and keep their own part. With 20 legs instead of 5 the minimum of one slot a leg costs: under 20 runners
the best any split reaches is ~26 min for convergence's rows (Germany's on 2 slots), against ~20.6 when one workflow held
all of a country's rows. The takes verdict comes ~1 min sooner than before (19.7 min), convergence's ~5 min later.
Runner-minutes are unchanged (the same rows), plus one preflight, `verdict complete` and `timings` job (~2 min).

**Gating.** THE GATE reads Main, Identity model convergence and Identity takes. Identity takes' rows in
`.github/gating-checks.tsv` land `unproven` (its workflow row too: gating_checks.py lint now accepts an unproven gate
workflow with its owner), so until its first green run on main its red reports only; then
`gating_checks.py prove --workflow-file identity-takes.yml --run <id> --number <#n>`. Its reds belong to the
`convergence` red family (agentctl's red-families.json).

**Measured after** (the first green pair on main, 8ed9b703c9, jobs API, first step start to last step end, timings job
left out): convergence #1076 (42520182228) 25.5 min wall, 38 jobs, 184 runner-min; Identity takes #3 (42520192909) 35.9 min
wall, 28 jobs, 204 runner-min, its last takes row ending 35.7 min in -- the takes verdict. Together ~388 runner-min against
the old single run's ~351 (median), and the gate's wall is the later of the two, 35.9 min against the old median ~32. The
takes run started 9 s after convergence's (its preflight's dispatch). This pair ran right after the Scala 3.10 bump
(8f3d4ac7ac): the takes rows took 12.7-14.4 min against their 7.6-8.0 in the costs file, and the drift rows ~1.5x theirs
(cold build caches), so each leg's later rows queued behind its first two (`takes (united-kingdom)` 22.7 min queued under
its leg's cap of 2). The convergence-only pair before it (#1074, pre-bump: 25.6 min) matches. Refresh
`convergence-row-minutes.tsv` (`convergence_slots.py costs`) over ten warm pairs before judging the plan; the first Identity
takes runs on main (#1, #2) had no `leg` job (the rows handed to the leg as strings: fixed in 8ed9b703c9).

Heads-up: a takes run whose plan has no A/A row shows the leg's skipped `order-compare` job with its raw name
(`${{ matrix.phase }} (${{ inputs.country }})`), as Spain's convergence leg did before it split its order rows.

Resolved 2026-11-09 (the owner): `order-compare` left the shared leg. Identity model convergence and Verify scrape recording run
`order-independence-p01|p12 (<country>)` as jobs of their own (`.github/actions/order-compare`, the preflight's `order-matrix`), Identity
takes holds `takes-drift-aa` alone, and the leg holds the rows only.
