# Convergence fixture fill

How a hermetic convergence leg's missing fixtures get recorded without waiting for the next
recording — and what that does to a verdict.

## The loop

1. **A hermetic leg names its gaps.** `HermeticHttpLeaf` refuses every request the pinned tree
   cannot answer and records it in `MissingFixtures`. The refusal is `NeverSent`, so the host's
   circuit breaker neither counts it nor opens on it: before that, the fourth refusal opened the
   breaker and every later request to the host was answered "circuit open" above the leaf, and a
   US leg named 12 of the ~2,300 Flicks and Drafthouse detail pages its tree lacked (run
   37597662228). At the end of the suite the leg writes every gap the fill could ask as it was
   asked (`MissingFixtures.listedAs`: GETs and byte reads, POSTs whole, header GETs whose URL carries
   the key the fill re-signs) — its URL with any credential masked (`RedactedUrl`) — to
   `enrichment-<code>.refetch.tsv` beside the tree, and its convergence row publishes it as
   `refetch-<code>-<corpus run>-<run id>-<run attempt>.tsv` (main only). Gaps are reported, never
   fatal: the run's report says how many, by host.
2. **The next run's convergence row fetches them before its suite.** Every hermetic row restores the
   pinned tree with the pair's published fills over it; the convergence row alone then runs "Fill
   the gaps the last leg listed, before the suite" (`country-convergence-leg.yml`) — the
   order-independence and sample rows replay the published fills and ask nothing, so one list costs
   one row's egress, not three. It reads the newest refetch list of the pinned pair, less every gap
   the newest four fills were refused (below), and runs `FillMissingFixtures` through
   `HttpWiring.pacedWire` — the worker's own per-host pace, 429 gate and breaker (Flicks at 200 ms
   a request) — behind the recorder's own chain (`ArchiveReplayWiring.recordedChain`), so a page
   lands at the fixture key a replay looks up, and a 404 is remembered as a recording remembers
   one. It skips what the restored tree already holds (the pinned tree and every fill), takes the
   hosts in turn, and stops starting requests 90 s after the step started — sbt's boot and the
   fixtures' compile, which the suite would pay anyway, come out of those seconds.
3. **The row publishes its fill, then replays it.** Straight after fetching, the row publishes what
   it got as `fill-<code>-<corpus run>-<run id>-<run attempt>-convergence.tar.zst` (main only, best
   effort, never failing the verdict), and only then lays it over its tree, so **this run's** suite
   replays it. On main a fill that failed to publish is not laid over: the suite replays the tree
   setup restored, which is what its bisect would replay. Every later hermetic leg lays the pair's
   fills over the pinned tree at setup — `convergence-setup` names them once ("Resolve the recorded
   pair") and `restore-enrichment-tree.sh` lays them over the tree, oldest run (then attempt) first.
4. **A refused gap rests.** What the fill asked and was refused — anything but a 404, which is an
   answer and lands in the fill — goes up as `refused-<code>-<corpus run>-<run id>-<run attempt>.tsv`,
   and the next fills skip every gap the newest four such lists name (`convergence-fill.sh gaps`).
   An origin refusing CI for good is asked again once four later fills have published lists without
   it, not on every run through the paid residential proxy.
5. **A longer fill is a dispatch.** `Convergence fill` (`convergence-fill.yml`, `workflow_dispatch`
   only; inputs `countries`, default `pl,de,uk,es,us`, and `minutes`, default 7) runs
   `.github/actions/convergence-fill` per country on a runner of its own and publishes
   `fill-<code>-<corpus run>-<its run id>-<attempt>.tar.zst` and its refused list. For a list longer
   than a row's 90 s — a new detail fetch that left thousands of pages unrecorded.

A new pin from `Record scrape fixtures` starts the loop over for its own pair; the pin step
prunes the fills and lists (refetch and refused) of the pairs it prunes.

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
- **Rows of one leg may replay slightly different trees.** The convergence row replays its own
  fill on top of the published ones; the order-independence and sample rows, set up at the same
  moment, replay only those published before. That is a change of INPUT each row's pair names,
  never of code.
- **Between runs, the inputs grow — on purpose.** A page that was a refusal in one run answers in
  the next, and in the run that fetched it. That can move coverage and films, and that change is
  the point. The fill step's `[fill]` line (and the step summary) says what each row fetched.
- **A bisect replays the row's own inputs.** `hermetic-pair` is `<corpus> <recorded> <tree>
  [fill...]`, and a red convergence row's bisect request appends the fill it published, laid over
  last — so a bisect replays exactly the tree that decided the verdict, whatever has been
  published since. Both halves hold by construction: on main a row replays its fill only once it
  is published, and a fill the pair names that setup cannot restore (asked three times) fails the
  leg's setup rather than being replayed without — as a release it cannot list does.
- **Publishing never opens a window.** Every fill and list is a new asset under a name nobody else
  writes (`<kind>-<code>-<corpus>-<run id>-<run attempt>[-<row>]`); nothing is `--clobber`ed, so a
  leg restoring while another publishes never meets a deleted asset, no two rows write one name,
  and "Re-run failed jobs" publishes under a new attempt instead of failing as "already exists".

## What it does not fill

A TMDB request is listed with its `api_key` masked, and the fill signs it again with the lane's
`TMDB_API_KEY` — the parameter and the bearer header, as `TmdbClient` sends them
(`FillCredentials`); among the fill's steps the key reaches only the fetching one, and what it
records is keyed without it (`RecordingHttpFetch.fixtureKey`, `LookupQuery.of`), so no published
asset carries it. OMDb's `apikey` is masked in the list too but never signed: its free key allows
1,000 requests a day, which the workers spend. A POST is listed whole — content type and body,
the body base64 in the list — when neither its URL nor its body names a credential
(`RedactedUrl.carriesCredential`): IMDb's GraphQL, a query naming a title, which was most of a US
or UK leg's gaps (run 37656742608); the fill sends it again as it was sent. A GET sent with headers
is listed only when its URL carries the credential the fill signs again with those headers (TMDB's
`api_key` beside its bearer); one whose credential is only in a header, a POST whose body names
one, and a URL carrying a credential a fixture's key hashes (`key=`, `sig=`… — anything but
`RecordingHttpFetch.CredentialParameters`, so its masked form names no real fixture) are never
listed. The same rule lists a failure the recording remembered. Those stay the recorder's to record.
A URL the fill signs is written to no log either: the fallback chain it asks through masks every
URL it logs or throws (`FallbackHttpFetch`).

A failure the recording remembered for a request is a gap too, unless it is an answer (a 404): a
circuit the recording opened on itself, a 503, a timeout — and a 403, an origin refusing the
recording's address. The fill asks every gap directly first, and only what the origin refuses
directly goes again through the residential proxy, as production reaches those origins
(`MissingFixtureFill.route`; Cineworld's detail API answers CI's own address 403). Its credentials,
like TMDB's key, reach only the fetching step; without them a refused request stays a gap.

A hermetic leg takes no fleet-wide pace slots (`ArchiveReplayWiring.pacingFleet`): on its frozen
clock the fleet pacer's slots never came round, and from Wikidata's fifth request on it answered
"circuit open" above the leaf, so those gaps were never named and never filled.
