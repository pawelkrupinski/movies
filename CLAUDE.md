# Project conventions for Claude

## Scripts must print what they did

When running backfill, migration, or investigation scripts (Scala via
`sbt "Test/runMain …"`, Python one-shots, `curl` probes, Mongo queries — any
script whose purpose is to *find out* or *change* state), the script MUST print
the human-readable data it touched so I can see the result without needing
another query.

Examples of what to print:

- **Investigation**: print the rows / records / API responses you inspected
  (key fields, not raw JSON dumps), so the conclusion is grounded in visible
  evidence. Don't paraphrase data without showing it.
- **Backfill / mutation**: print a `BEFORE → AFTER` line per row changed, or
  at minimum a sample of N rows touched + the total count. Never just print
  "done, updated 142 rows" — show what they changed to.
- **Lookups against external APIs** (TMDB, IMDb, Cinemeta, OMDb, Filmweb,
  Metacritic, RT): print the resolved id/title/year and the field of interest
  (rating, URL, …), not just "found a match".
- **Mongo queries**: print the documents (or the projected fields) the query
  matched, not just the count.

Output should be concise enough to read in the terminal — for very large
result sets, print the first N (10–20) plus a `(+ M more)` line.

If the data isn't human-readable (binary, opaque ids only, hashes), state
that explicitly: `"sha256:abc… (binary, not shown)"`.

This applies to ad-hoc scripts you write inside the conversation, not to the
production app's logging.

## Parallelize scripts, but don't get rate-limited

Long-running scripts that hit external services (TMDB, IMDb, Filmweb,
Metacritic, RT, OMDb, Cinemeta, Mongo, scraped cinema sites) should run their
per-row work in parallel — serial loops of hundreds of HTTP round-trips are
unacceptably slow when 90% of the time is spent waiting on the network.

Default to a fixed-thread pool of **5–10 concurrent workers** for scripts that
hit a single API. Stay well under each service's documented or empirically-
known rate limit:

- TMDB: ~50 req/s — 10 workers is fine.
- IMDb / Cinemeta / RT / Metacritic: undocumented; assume a few hundred per
  minute. 5–10 workers is fine; back off on any 429/503.
- Filmweb: undocumented; 5 workers comfortable, more risks soft-blocks.
- OMDb (free tier): 1000 req/day — sequential is fine; the limit is daily, not
  per-second.

If you see HTTP 429 / 503 / `Request limit reached!` style responses, halve
the concurrency and add a small sleep between retries. Don't push harder hoping
it'll work — the host is telling you to stop.

For Mongo, parallelism doesn't matter much (the driver pools connections); use
the default unless the script does CPU work between queries.

Always print throughput at the end (`done in 12.3s, ~8 req/s`) so the next
run can be tuned.

## Never persist Metacritic or Rotten Tomatoes search URLs

`MovieRecord.metacriticUrl` / `MovieRecord.rottenTomatoesUrl` may only hold a
**canonical** `/movie/...` or `/m/...` URL — the URL of the actual film page.
Search URLs (`/search/<query>` on Metacritic, `/search?search=<query>` on RT)
must never be written to the database or the in-memory cache.

Why: search URLs are unstable (the result set changes as titles get added /
renamed), they cache poorly on third-party sites for years, and most
importantly Mongo persistence makes them sticky — a bad slug stays bad until
someone re-enriches. The view layer (`MovieRecord.metacriticHref` /
`rottenTomatoesHref`) already synthesises a display-time search link when the
stored URL is `None`, so dropping search URLs from persistence loses nothing
user-facing.

Concretely:

- `MetacriticClient.urlFor` and `RottenTomatoesClient.urlFor` return
  `Option[String]`. They return `None` when no canonical slug 200s.
- Callers must NEVER reintroduce a `.getOrElse(searchUrl(...))` fallback. If
  you're tempted to add one for a "useful link", remember the view does it.
- If a future refactor surfaces a "search URL" anywhere in the persistence
  path (script, controller, service), treat it as a bug and remove the
  fallback. Don't filter the search URL out at the call site — make the
  upstream return None.

## Don't let classes register themselves as listeners

A class must not call `lifecycle.addStopHook`, `scheduler.scheduleAtFixedRate`,
`bus.subscribe`, `addListener`, or any similar self-registration in its own
constructor or init block. That kind of side-effect-on-construction makes the
class hard to instantiate in tests (the listener fires the moment you `new` it,
even when the test only wants to call one pure method), hides the wiring from
anyone reading the call graph, and ties the class's lifetime to its purpose.

Instead, expose the operation as a plain method (`def start()`, `def tick()`,
`def onSomething(…)`) and do the registration in the wiring code — the DI
module, `AppLoader`, or whichever composition root owns the lifecycle.

Tests can then construct the class freely and invoke the method directly when
they want the behaviour exercised.

When a bus carries multiple event types, the listener should be a
`PartialFunction[Event, Unit]` rather than a total `Event => Unit` with an
internal `match` and a no-op `case _`. The partial function lets the bus
filter on `isDefinedAt` so handlers only see events they care about, makes
each listener's surface obvious at a glance, and avoids the easy bug of an
accidentally-exhaustive match swallowing future event types silently.

```scala
// Yes:
def onShowtimeEvent: PartialFunction[CacheEvent, Unit] = {
  case CacheEvent.Refreshed(movies) => preload(movies)
}

// No:
def onShowtimeEvent(e: CacheEvent): Unit = e match {
  case CacheEvent.Refreshed(movies) => preload(movies)
  case _                            => ()  // silently eats every new event
}
```

## Never hardcode overrides keyed by movie identity

Do not add code that pins a film's data or behaviour by its title, IMDb id,
TMDB id, year, or any other per-film identifier. That means no
`Map("tt0241527" -> "harry-potter-and-the-sorcerers-stone")` slug overrides,
no `if (title == "Belle") ...` branches, no `imdbId match { case "tt..." => }`
special cases, no year-keyed exception lists, no per-film constants embedded
in clients or services. The existing `TitleOverrides` exists ONLY as the
narrow exception that proves the rule — it covers cases where TMDB's
Polish-locale search literally cannot surface the right film — and it is
maintained by me, not extended by you.

Why: per-film overrides paper over upstream data problems instead of fixing
them, they scale linearly with edge cases the codebase will never finish
collecting, they go stale silently when the upstream source corrects itself,
they hide the real bug from whoever reads the code next, and they make the
pipeline's behaviour for any given film depend on whether someone happened
to notice it and add an entry.

When you hit a film that resolves wrong (wrong MC slug, wrong IMDb id,
missing rating, etc.) the fix MUST be one of:

- A general data-driven path that handles this class of problem
  automatically — e.g. consulting TMDB's `/alternative_titles` to find the
  US release title rather than pinning HP1's slug; tightening the search-
  scrape's acceptance rule rather than blocking a specific bad slug.
- A change to how the data is parsed/normalised/cleaned at the source.
- Accepting that for this row the value will stay `None` (and letting the
  view layer fall back to a synthesised search link, a placeholder, etc.).

If after honestly looking you genuinely cannot find a non-identity-keyed
solution, STOP and ask me — describe the case, the upstream data, and what
the smallest-possible override would look like. I will almost always tell
you to drop it; the few times I won't, I'll be the one to add it. Do NOT
ship the override and ask forgiveness.

## Backfill stored data when ingestion or maintenance logic changes

Whenever you change how a field is ingested, parsed, normalised, scraped, or
otherwise maintained — in a way that would produce a different value for
records already persisted — you MUST also backfill the existing rows in
Mongo. The new logic only applies to records touched after the change;
without a backfill, the DB is left in a mixed state where old rows still
carry the pre-change value and look "correct" until something re-enriches
them, which may be never.

Examples that require a backfill:

- Changing how a URL is canonicalised, slugified, or validated (e.g. the
  Metacritic/RT search-URL rule above — every previously-stored bad URL stays
  bad until re-enriched).
- Changing the parsing of a scraped field (rating scale, date format, title
  normalisation) so the same source data now maps to a different stored
  value.
- Adding/removing/renaming a field, or changing which source wins when
  multiple are available.
- Tightening validation so values that were previously accepted should now
  be `None`/dropped.
- Fixing a bug in an enrichment client where the buggy output is already in
  the DB.

What "backfill" means concretely: write an ad-hoc script (under
`test/scala/scripts/` or similar) that re-runs the affected logic against
every stored row and updates Mongo in place. Follow the script conventions
above (print BEFORE → AFTER, parallelise within rate limits, print
throughput). Don't rely on natural re-enrichment cycles to "eventually"
heal the data — call it out and run the backfill as part of the same
change.

If a backfill is impractical (e.g. the source data is no longer available),
say so explicitly and propose an alternative (invalidate the field, mark
rows stale, schedule a re-enrichment) rather than silently leaving the
DB inconsistent.

## Always add tests for new or changed functionality

Every piece of new or modified behaviour MUST come with a test that exercises
it. This is non-negotiable for bug fixes (the test must fail before the fix
and pass after), new methods or branches, parsing/normalisation changes,
and any logic that decides what to persist or display. "I ran it once and
it looked right" is not a substitute — the test is what prevents the next
change from silently breaking this one.

**Default to writing the failing test first.** Whenever it is feasible —
bug fixes, new features, investigations into "why is this value wrong",
parser/normaliser changes, anything where you can express the desired
behaviour as an assertion — write the test before the production change
and watch it fail for the right reason. Then make it pass. This is the
strongest evidence that:

1. The test actually exercises the new code path (a green-from-the-start
   test often turns out to assert nothing useful).
2. The bug or missing feature is real and reproducible, not a
   misunderstanding.
3. The fix addresses the root cause, not a symptom that happened to
   disappear.

For investigations specifically: if you suspect a parser/enrichment/data
bug, the fastest way to confirm it is to write a test that feeds the
suspect input through the real code and asserts the expected output. If
it fails the way you predicted, you have both the diagnosis and the
regression test in one step.

Skip the test-first step only when it is genuinely impractical — e.g.
exploratory spikes you'll throw away, or behaviour that can only be
observed via a running server/browser. In those cases, still add a test
after the fact before considering the work done.

If the existing test for a neighbouring behaviour is the closest match,
extend it or copy its structure rather than inventing a new style.

For pure logic (parsers, formatters, normalisers, decision functions), unit
tests against in-memory inputs are enough. For services that compose other
services, prefer the existing spec patterns in `test/scala/services/...`
that wire fakes/in-memory implementations.

### Record fixtures for external-service clients

For clients that hit a real external API (TMDB, IMDb, Cinemeta, OMDb,
Filmweb, Metacritic, RT, scraped cinema sites), strongly consider capturing
a real response as a fixture on disk and writing a test that replays it
through the client. Live HTTP in tests is flaky and slow; hand-written
mock JSON drifts from reality and hides parser bugs the real payload
would catch.

When to record a fixture:

- You're adding a new client, or a new endpoint on an existing client.
- You're changing how a response is parsed (new field, changed shape,
  tightened validation).
- You hit a real-world payload that exposed a parser bug — capture that
  exact payload so the bug can't regress.

How to do it:

- Save the raw response under `test/resources/fixtures/<service>/<case>.<ext>`
  (json, html, xml — whatever the service returns). Trim noise (huge image
  arrays, tracking ids) only if it doesn't affect parsing.
- Write the test to load the fixture from disk and feed it to the client's
  parser/decoder directly, OR stub the HTTP layer to return the fixture
  bytes. The test must not make a network call.
- Name the fixture after the scenario (`tmdb_movie_with_no_release_date.json`,
  `rt_404_page.html`), not the date or a ticket number — the scenario is
  what future-you will search for.
- If the fixture is large, leave a one-line comment in the test pointing to
  the URL or query that produced it, so it can be refreshed later.

If recording a fixture doesn't make sense (the response is trivial, or the
client is a thin pass-through with no parsing), say so and write a smaller
unit test instead. Don't skip testing the client entirely just because
fixtures feel heavy.

## Commit at every stable state

After each self-contained change reaches a stable state — production code
done, tests written and passing, no leftover compile errors or skipped
specs — make a git commit before moving on. Don't pile multiple unrelated
changes into one commit and don't leave finished work sitting uncommitted
across the next phase. Each commit should be a checkpoint you'd be happy
to revert to.

What counts as a stable state:

- A bug fix with its regression test, both passing.
- A new feature plus the tests covering its golden path and the obvious
  edge cases.
- A refactor that compiles clean and leaves the test suite green.
- A mechanical sweep (rename, package move, import shuffle) where the
  build and tests are still green at the end.

What doesn't:

- Production change without the matching test.
- Half-done work that's "going to need another pass anyway".
- A green compile with skipped or commented-out tests.

Commit messages should describe the *why* in one or two sentences, not
the *what* — the diff already shows the what. The user's recent commit
style (e.g. `Unify ShowtimeCache + EnrichmentCache into a single
MovieCache.`, `Move MovieCache, MovieRepo, MovieService, IdentityMerger
to services.movies.`) is the target tone: a one-liner subject naming the
change, followed by a short paragraph if the motivation isn't obvious.
Use a HEREDOC for the message so multi-line formatting survives. Never
amend a published commit — make a new one.

A long phase that genuinely belongs together (e.g. a multi-file rename
that only makes sense as one atomic change) is fine to commit as a
single big commit. But if a phase has internal milestones — phase A
compiles + tests, phase B compiles + tests, phase C compiles + tests —
each milestone gets its own commit so the history reads as a sequence
of safe checkpoints, not one monolith.

## Extract repeated patterns into a shared abstraction

If you find yourself writing the same shape of code in a third place — a
`FakeRepo extends MovieRepo` defined inside every spec, the same regex
+ `replaceAll` chain copy-pasted across three parsers, a "load fixture
from disk and feed it through this client" helper duplicated per spec —
stop and extract it into a shared place (a `private[services]` helper,
a `test/scala/...` shared base, a method on the most relevant existing
class). The threshold is *three* uses, not two: two copies is sometimes
just two unrelated things that happen to look alike, but three is a
pattern asking to be named.

This is the *other* side of "three similar lines is better than a
premature abstraction" (see the project guidance at the top of this
file). Premature means "I wrote one helper because I imagine the second
caller is coming." Extract-on-third means "the second and third copies
already exist and I'm writing the fourth." Don't preempt — but don't
keep paying the duplication tax once it's clearly recurring.

When extracting:

- Put the shared piece where the most callers can already see it
  (`services.movies` if every caller is in `services.*`; a `test/scala`
  shared object if it's test-only) — don't pull in cross-package
  imports just to satisfy visibility.
- Delete the inline copies in the same commit. Leaving even one behind
  defeats the point and creates drift.
- Name the extracted thing after the *concept* (`InMemoryMovieRepo`,
  not `FakeRepo`; `ProductionLineRegex`, not `parseHelper`) — generic
  names re-attract duplication later.

## Treat cleanup as a phase of every task

Every feature / change / bugfix / refactor ends with a cleanup pass, and
sometimes opens with one too. The work isn't done when the new code
compiles + tests pass — it's done when the surrounding area is at least
as clean as it was when you started, ideally cleaner. The cleanup pass
is part of the task, not a separate ticket.

What to look at, at the end of each task (and at the start when the area
looks crufty enough that touching it without first tidying will produce
worse code):

- **Duplication you just introduced or noticed.** The third copy of a
  pattern — extract it (see "Extract repeated patterns into a shared
  abstraction" above). Don't promise to do it "next time."
- **Dead code the change made obsolete.** A field you stopped writing
  to. A method whose only caller you just removed. A test that asserted
  behaviour that no longer exists. An import that's now unused. A
  one-shot script whose purpose is served. Delete it in the same
  commit, not a follow-up.
- **Comments that lie now.** Doc comments referencing the old name,
  the old flow, or the old caller. Either rewrite or remove.
- **Redundant intermediate variables / helper methods** that the change
  collapsed into one line at the call site. Inline them.
- **Stale tests / fixtures.** A test that exercises behaviour the
  change removed. A fixture file that's no longer loaded by anything.
- **Naming drift.** A class still called `EnrichmentCache` after the
  concept it represented became `MovieCache`. Rename it now while
  you're already in the file.

When to open with cleanup instead of charging straight at the task:

- The function you're about to extend has three near-identical
  branches that you'd be adding a fourth to. Refactor the three first,
  then add the fourth.
- The class you're about to modify has a field, parameter, or method
  that's already dead. Removing it first makes the diff for the real
  change much easier to read in review.
- The test you're about to add would copy-paste setup from two existing
  tests. Extract the setup helper before writing the third test.

The output of the task is the diff. A diff that's half real change and
half "and I also deleted these three dead methods I happened to notice"
is the *right* shape — that's the cleanup pass working. A diff that
only changes the new feature and ignores the dust around it is the
wrong shape.

Skip cleanup only when it would balloon the change beyond what a
reviewer can hold in their head, or when the cruft is genuinely outside
the area you're touching. In those cases, mention it explicitly so we
can decide together whether to tackle it now or later — don't silently
shrug and move on.
