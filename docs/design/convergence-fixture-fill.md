# Convergence fixture fill

How a hermetic convergence leg's missing fixtures get recorded without waiting for the next
recording — and what that does to a verdict.

## The loop

1. **A hermetic leg names its gaps.** `HermeticHttpLeaf` refuses every request the pinned tree
   cannot answer and records it in `MissingFixtures`. The refusal is `NeverSent`, so the host's
   circuit breaker neither counts it nor opens on it: before that, the fourth refusal opened the
   breaker and every later request to the host was answered "circuit open" above the leaf, and a
   US leg named 12 of the ~2,300 Flicks and Drafthouse detail pages its tree lacked (run
   37597662228). At the end of the suite the leg writes every gap it could fetch without a secret
   — a plain GET of a URL holding no credential — to `enrichment-<code>.refetch.tsv` beside the
   tree, and its convergence row publishes it as `refetch-<code>-<corpus run>-<run id>.tsv`
   (main only). Gaps are reported, never fatal: the run's report says how many, by host.
2. **The next run's `fill` job fetches them.** Beside the legs, on a runner of its own,
   `.github/actions/convergence-fill` reads the newest refetch list of the pinned pair and runs
   `FillMissingFixtures`: through `HttpWiring.pacedWire` — the worker's own per-host pace, 429 gate
   and breaker (Flicks at 200 ms a request) — behind the recorder's own chain
   (`ArchiveReplayWiring.recordedChain`), so a page lands at the fixture key a replay looks up, and
   a 404 is remembered as a recording remembers one. It skips what earlier fills hold, takes the
   hosts in turn, and stops starting requests at minute 7 of the job.
3. **It publishes a fill.** What it fetched goes up as `fill-<code>-<corpus run>-<run id>.tar.zst`
   (main only).
4. **Every later hermetic leg replays the pinned tree with the pair's fills over it.**
   `convergence-setup` names them once ("Resolve the recorded pair") and `restore-enrichment-tree.sh`
   lays them over the tree, oldest first.

A new pin from `Record scrape fixtures` starts the loop over for its own pair; the pin step
prunes the fills and lists of the pairs it prunes.

## Why the gaps existed

`Record scrape fixtures` pins a pair only when its boot completes. Flicks' film pages and Alamo's
presentation endpoint became detail fetches on 2026-10-06, after the pinned recording (run
37407719592). The next recording (run 37562532213) hung in `identityDetails` at 0% CPU until its
step ceilings: the recording's pacers read the harness's frozen clock, so no slot ever came
round and the k-th request to a paced host waited k slots. The recording's pacers now read the
wall clock (`ArchiveReplayWiring.pacingClock`); a hermetic replay keeps the frozen one and skips
the waits.

## Determinism

- **Within a leg, nothing moves.** A leg names its fills once, at setup, and every row of it — the
  sample, convergence and order-independence rows — replays the same pinned tree and the same
  fills. The suite never touches the network; the fill runs on another runner.
- **Between legs, the inputs grow — on purpose.** A page that was a refusal in one leg can answer
  in the next. That can move the next leg's coverage and films, and that change is the point. It
  is a change of INPUT, so a red leg that follows a new fill is a data change, not a code change:
  the step summary of `fill (<code>)` in the run before says what was published.
- **A bisect replays the leg's own inputs.** `hermetic-pair` is `<corpus> <recorded> <tree>
  [fill...]`, so a bisect of a leg replays exactly the fills that leg replayed, whatever has been
  published since.
- **Publishing never opens a window.** Every fill and list is a new asset under a name nobody else
  writes (`<kind>-<code>-<corpus>-<run id>`); nothing is `--clobber`ed, so a leg restoring while
  another publishes never meets a deleted asset, and five countries' legs never write one name.

## What it does not fill

A request needing a credential (TMDB, OMDb), a header (TMDB's bearer) or a body (IMDb's GraphQL
POST) is never listed. Those stay the recorder's to record.
