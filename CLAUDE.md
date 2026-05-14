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

`Enrichment.metacriticUrl` / `Enrichment.rottenTomatoesUrl` may only hold a
**canonical** `/movie/...` or `/m/...` URL — the URL of the actual film page.
Search URLs (`/search/<query>` on Metacritic, `/search?search=<query>` on RT)
must never be written to the database or the in-memory cache.

Why: search URLs are unstable (the result set changes as titles get added /
renamed), they cache poorly on third-party sites for years, and most
importantly Mongo persistence makes them sticky — a bad slug stays bad until
someone re-enriches. The view layer (`Enrichment.metacriticHref` /
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
