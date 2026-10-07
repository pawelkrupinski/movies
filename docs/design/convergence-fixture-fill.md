# Convergence fixture fill

How a hermetic convergence leg's missing fixtures get recorded without waiting for the next
recording — and what that does to a verdict.

## The loop

1. **A hermetic leg names its gaps.** `HermeticHttpLeaf` refuses every request the pinned tree
   cannot answer and records it in `MissingFixtures`. The refusal is `NeverSent`, so the host's
   circuit breaker neither counts it nor opens on it: before that, the fourth refusal opened the
   breaker and every later request to the host was answered "circuit open" above the leaf, and a
   US leg named 12 of the ~2,300 Flicks and Drafthouse detail pages its tree lacked (run
   37597662228). At the end of the suite the leg writes every GET gap — its URL with any credential
   masked (`RedactedUrl`) — to `enrichment-<code>.refetch.tsv` beside the
   tree, and its convergence row publishes it as `refetch-<code>-<corpus run>-<run id>.tsv`
   (main only). Gaps are reported, never fatal: the run's report says how many, by host.
2. **The next run's rows fetch them before their suites.** Every hermetic row — convergence,
   order-independence and sample — restores the pinned tree with the pair's fills over it, then
   runs "Fill the gaps the last leg listed, before the suite" (`country-convergence-leg.yml`): it
   reads the newest refetch list of the pinned pair and runs `FillMissingFixtures` through
   `HttpWiring.pacedWire` — the worker's own per-host pace, 429 gate and breaker (Flicks at 200 ms
   a request) — behind the recorder's own chain (`ArchiveReplayWiring.recordedChain`), so a page
   lands at the fixture key a replay looks up, and a 404 is remembered as a recording remembers
   one. It skips what the restored tree already holds (the pinned tree and every fill), takes the
   hosts in turn, and stops starting requests 90 s after the step started — sbt's boot and the
   fixtures' compile, which the suite would pay anyway, come out of those seconds. What it fetched
   is laid over the row's tree, so **this run's** suite replays it.
3. **Each row publishes its fill.** After its suite the row publishes what it fetched as
   `fill-<code>-<corpus run>-<run id>-<phase>.tar.zst` (main only, best effort, never failing the
   verdict). Every later hermetic leg lays the pair's fills over the pinned tree at setup —
   `convergence-setup` names them once ("Resolve the recorded pair") and
   `restore-enrichment-tree.sh` lays them over the tree, oldest run first.
4. **A longer fill is a dispatch.** `Convergence fill` (`convergence-fill.yml`, `workflow_dispatch`
   only; inputs `countries`, default `pl,de,uk,es,us`, and `minutes`, default 7) runs
   `.github/actions/convergence-fill` per country on a runner of its own and publishes
   `fill-<code>-<corpus run>-<its run id>.tar.zst`. For a list longer than a row's 90 s — a new
   detail fetch that left thousands of pages unrecorded.

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

- **The suite stays hermetic.** The fill is a step of its own that finishes before the suite
  starts; the suite still touches no network and refuses every request its tree cannot answer.
  The live requests happen only in the fill step, under its deadline.
- **Rows of one leg may replay slightly different trees.** Each row fetches on its own runner, and
  a page one row got another may not have (a timeout, the deadline). That is a change of INPUT the
  row's own fill names, never of code.
- **Between runs, the inputs grow — on purpose.** A page that was a refusal in one run answers in
  the next, and in the run that fetched it. That can move coverage and films, and that change is
  the point. The fill step's `[fill]` line (and the step summary) says what each row fetched.
- **A bisect replays the row's own inputs.** `hermetic-pair` is `<corpus> <recorded> <tree>
  [fill...]`, and a row's bisect request appends the fill it fetched and published, laid over
  last — so a bisect replays exactly the tree that decided the verdict, whatever has been
  published since. (A fill that failed to publish is the one input a bisect cannot restore; it
  says so in a warning and replays without it.)
- **Publishing never opens a window.** Every fill and list is a new asset under a name nobody else
  writes (`<kind>-<code>-<corpus>-<run id>[-<phase>]`); nothing is `--clobber`ed, so a leg
  restoring while another publishes never meets a deleted asset, and no two rows write one name.

## What it does not fill

A TMDB request is listed with its `api_key` masked, and the fill signs it again with the lane's
`TMDB_API_KEY` — the parameter and the bearer header, as `TmdbClient` sends them
(`FillCredentials`); among the fill's steps the key reaches only the fetching one, and what it
records is keyed without it (`RecordingHttpFetch.fixtureKey`, `LookupQuery.of`), so no published
asset carries it. OMDb's `apikey` is masked in the list too but never signed: its free key allows
1,000 requests a day, which the workers spend. A POST is listed whole — content type and body,
the body base64 in the list — when neither its URL nor its body names a credential
(`RedactedUrl.carriesCredential`): IMDb's GraphQL, a query naming a title, which was most of a US
or UK leg's gaps (run 37656742608); the fill sends it again as it was sent. A request whose
credential is only in a header, or whose body names one, is never listed. Those stay the
recorder's to record.

A hermetic leg takes no fleet-wide pace slots (`ArchiveReplayWiring.pacingFleet`): on its frozen
clock the fleet pacer's slots never came round, and from Wikidata's fifth request on it answered
"circuit open" above the leaf, so those gaps were never named and never filled.
