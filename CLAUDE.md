# Project conventions for Claude

## Scripts must print what they did

When running backfill, migration, or investigation scripts (Scala via
`sbt "Test/runMain …"`, Python one-shots, `curl` probes, Mongo queries — any
script whose purpose is to *find out* or *change* state), the script MUST
print the human-readable data it touched so I can see the result without
needing another query.

Examples of what to print:

- **Investigation**: print the rows / records / API responses you inspected
  (key fields, not raw JSON dumps), so the conclusion is grounded in
  visible evidence. Don't paraphrase data without showing it.
- **Backfill / mutation**: print a `BEFORE → AFTER` line per row changed,
  or at minimum a sample of N rows touched + the total count. Never just
  "done, updated 142 rows".
- **Lookups against external APIs** (TMDB, IMDb, Cinemeta, OMDb, Filmweb,
  Metacritic, RT): print the resolved id/title/year and the field of
  interest, not just "found a match".
- **Mongo queries**: print the documents (or projected fields) the query
  matched, not just the count.

For very large result sets, print the first N (10–20) plus a `(+ M more)`
line. If the data isn't human-readable (binary, opaque ids, hashes),
state that explicitly. This applies to ad-hoc scripts you write inside
the conversation, not to the production app's logging.

## Remove one-shot scripts after they're used

A script written for a specific past fix — "backfill the rows whose
`metacriticUrl` was stored as a search URL", "delete the orphaned
'DZIEŃ DZIECKA W APOLLO' row", "investigate the three Viridiana rows" —
should be deleted in the same commit that finishes the work. Once the
change is in the code and the data has been reconciled, the script will
never re-run; leaving it clutters `test/scala/scripts/` and bit-rots
against the current schema.

What counts as a one-shot:

- Backfills tied to a specific fix where the data is now in the target
  state (`AllCapsTitleBackfill`, `ApolloPrefixBackfill`,
  `CacheKeyYearBackfill`).
- Per-film investigations (`InvestigateViridiana`).
- Phase audits for a migration that has since landed (`MergeKeyAudit`,
  `MongoIdConsolidate`).
- One-shot data-shape migrations (`MoviesCollectionMigrate`,
  `SameTmdbIdMerge`).

What to keep:

- Re-runnable diagnostics with no destructive write (`DuplicateAudit`,
  `MetacriticDiagnostics`).
- Per-service revalidation patterns that rerun on the next client change
  (`MetacriticBackfill`, `RottenTomatoesBackfill`, `FilmwebUrlAudit`,
  `MetascoreBackfill`, `FilmwebReset`, `EnrichmentBackfill`).
- Ad-hoc one-row refresh tools (`RefreshOneFilmweb`).

If unsure, delete. A future change can re-derive the script from a
remaining template in five minutes.

## Parallelize scripts, but don't get rate-limited

Long-running scripts that hit external services (TMDB, IMDb, Filmweb,
Metacritic, RT, OMDb, Cinemeta, Mongo, scraped cinema sites) should run
per-row work in parallel — serial loops of hundreds of HTTP round-trips
are unacceptably slow when 90% of the time is network wait.

Default to a fixed-thread pool of **5–10 concurrent workers** for scripts
hitting a single API. Stay well under each service's limit:

- TMDB: ~50 req/s — 10 workers is fine.
- IMDb / Cinemeta / RT / Metacritic: undocumented; assume a few hundred
  per minute. 5–10 workers fine; back off on any 429/503.
- Filmweb: undocumented; 5 workers comfortable, more risks soft-blocks.
- OMDb (free tier): 1000 req/day — sequential is fine; the limit is
  daily, not per-second.

On HTTP 429 / 503 / `Request limit reached!`, halve concurrency and add
a small sleep between retries. Don't push harder — the host is telling
you to stop.

For Mongo, parallelism doesn't matter much (the driver pools
connections); use the default unless the script does CPU work between
queries.

Always print throughput at the end (`done in 12.3s, ~8 req/s`) so the
next run can be tuned.

## Never persist Metacritic or Rotten Tomatoes search URLs

`MovieRecord.metacriticUrl` / `MovieRecord.rottenTomatoesUrl` may only
hold a **canonical** `/movie/...` or `/m/...` URL — the URL of the
actual film page. Search URLs (`/search/<query>` on Metacritic,
`/search?search=<query>` on RT) must never be written to the database or
the in-memory cache.

Why: search URLs are unstable (the result set changes as titles get
added/renamed), they cache poorly on third-party sites for years, and
Mongo persistence makes them sticky. The view layer
(`MovieRecord.metacriticHref` / `rottenTomatoesHref`) already
synthesises a display-time search link when the stored URL is `None`.

- `MetacriticClient.urlFor` and `RottenTomatoesClient.urlFor` return
  `Option[String]` — `None` when no canonical slug 200s.
- Callers must NEVER reintroduce a `.getOrElse(searchUrl(...))` fallback.
- If a search URL surfaces anywhere in persistence (script, controller,
  service), treat it as a bug and fix the upstream to return None.
  Don't filter at the call site.

## Don't let classes register themselves as listeners

A class must not call `lifecycle.addStopHook`, `scheduler.scheduleAtFixedRate`,
`bus.subscribe`, `addListener`, or any similar self-registration in its
constructor or init block. That side-effect-on-construction makes the
class hard to instantiate in tests (the listener fires the moment you
`new` it), hides the wiring from the call graph, and ties the class's
lifetime to its purpose.

Instead, expose the operation as a plain method (`def start()`,
`def tick()`, `def onSomething(…)`) and do the registration in the
wiring code — the DI module, `AppLoader`, or whichever composition root
owns the lifecycle. Tests can then construct the class freely and invoke
the method directly.

When a bus carries multiple event types, the listener should be a
`PartialFunction[Event, Unit]` rather than a total `Event => Unit` with
a no-op `case _`. The partial function lets the bus filter on
`isDefinedAt` and avoids the bug of an accidentally-exhaustive match
silently swallowing future event types.

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

Do not add code that pins a film's data or behaviour by its title, IMDb
id, TMDB id, year, or any other per-film identifier. No
`Map("tt0241527" -> "harry-potter-and-the-sorcerers-stone")` slug
overrides, no `if (title == "Belle") ...`, no
`imdbId match { case "tt..." => }` special cases, no year-keyed exception
lists. The existing `TitleOverrides` is the narrow exception that proves
the rule — TMDB's Polish-locale search literally cannot surface the
right film — and it is maintained by me, not extended by you.

Per-film overrides paper over upstream data problems, scale linearly
with edge cases the codebase will never finish collecting, go stale
silently when upstream corrects itself, and hide the real bug from the
next reader.

When a film resolves wrong (wrong MC slug, wrong IMDb id, missing
rating, etc.) the fix MUST be one of:

- A general data-driven path that handles this class of problem — e.g.
  consulting TMDB's `/alternative_titles` for the US release title
  rather than pinning HP1's slug; tightening search-scrape acceptance
  rather than blocking a specific bad slug.
- A change to how the data is parsed/normalised/cleaned at the source.
- Accepting that for this row the value stays `None` (and letting the
  view layer fall back to a synthesised search link).

If after honestly looking you cannot find a non-identity-keyed solution,
STOP and ask me — describe the case, the upstream data, and what the
smallest-possible override would look like. I will almost always tell
you to drop it; the few times I won't, I'll be the one to add it. Do
NOT ship the override and ask forgiveness.

## Backfill stored data when ingestion or maintenance logic changes

Whenever you change how a field is ingested, parsed, normalised,
scraped, or otherwise maintained — in a way that would produce a
different value for already-persisted records — you MUST also backfill
the existing rows in Mongo. The new logic only applies to records
touched after the change; without a backfill, the DB is left in a mixed
state where old rows still carry the pre-change value and look
"correct" until something re-enriches them, which may be never.

Examples that require a backfill:

- Changing how a URL is canonicalised, slugified, or validated (e.g.
  the Metacritic/RT search-URL rule).
- Changing the parsing of a scraped field (rating scale, date format,
  title normalisation).
- Adding/removing/renaming a field, or changing which source wins when
  multiple are available.
- Tightening validation so previously-accepted values should now be
  `None`/dropped.
- Fixing a bug in an enrichment client where the buggy output is
  already in the DB.

Concretely: write an ad-hoc script (under `test/scala/scripts/`) that
re-runs the affected logic against every stored row and updates Mongo
in place. Follow the script conventions above (BEFORE → AFTER,
parallelise within rate limits, print throughput). Don't rely on
natural re-enrichment to eventually heal the data.

If a backfill is impractical (e.g. source no longer available), say so
and propose an alternative (invalidate the field, mark rows stale,
schedule a re-enrichment) rather than silently leaving the DB
inconsistent.

## Always add tests for new or changed functionality

Every piece of new or modified behaviour MUST come with a test that
exercises it. Non-negotiable for bug fixes (must fail before the fix
and pass after), new methods or branches, parsing/normalisation
changes, and any logic that decides what to persist or display. "I ran
it once and it looked right" is not a substitute.

**Default to writing the failing test first.** Whenever feasible — bug
fixes, new features, parser/normaliser changes, investigations into
"why is this value wrong" — write the test before the production change
and watch it fail for the right reason. This is the strongest evidence
that:

1. The test actually exercises the new code path (a green-from-the-start
   test often turns out to assert nothing useful).
2. The bug or missing feature is real and reproducible.
3. The fix addresses the root cause, not a symptom that happened to
   disappear.

For investigations: if you suspect a parser/enrichment/data bug, the
fastest way to confirm it is a test that feeds the suspect input
through the real code. If it fails as predicted, you have both the
diagnosis and the regression test in one step.

Skip the test-first step only when genuinely impractical (exploratory
spikes, behaviour observable only via a running server/browser). Still
add a test after the fact. If a neighbouring test is the closest match,
extend it rather than inventing a new style.

For pure logic (parsers, formatters, normalisers, decision functions),
unit tests against in-memory inputs are enough. For composed services,
prefer the existing spec patterns in `test/scala/services/...` that
wire fakes/in-memory implementations.

### Record fixtures for external-service clients

For clients that hit a real external API (TMDB, IMDb, Cinemeta, OMDb,
Filmweb, Metacritic, RT, scraped cinema sites), strongly consider
capturing a real response as a fixture on disk and writing a test that
replays it through the client. Live HTTP in tests is flaky and slow;
hand-written mock JSON drifts from reality and hides parser bugs the
real payload would catch.

When to record a fixture:

- Adding a new client, or a new endpoint on an existing client.
- Changing how a response is parsed (new field, changed shape,
  tightened validation).
- Hitting a real-world payload that exposed a parser bug — capture that
  exact payload so the bug can't regress.

How:

- Save the raw response under
  `test/resources/fixtures/<service>/<case>.<ext>`. Trim noise (huge
  image arrays, tracking ids) only if it doesn't affect parsing.
- Load from disk and feed the parser/decoder directly, OR stub the HTTP
  layer to return the bytes. No network in tests.
- Name after the scenario (`tmdb_movie_with_no_release_date.json`,
  `rt_404_page.html`), not the date or ticket number.
- For large fixtures, leave a one-line comment in the test pointing to
  the URL/query that produced it.

If the response is trivial or the client is a thin pass-through, write
a smaller unit test instead — but don't skip testing the client.

## Commit at every stable state

After each self-contained change reaches a stable state — production
code done, tests written and passing, no leftover compile errors or
skipped specs — make a git commit before moving on. Don't pile
unrelated changes into one commit, and don't leave finished work
uncommitted across the next phase. Each commit should be a checkpoint
you'd be happy to revert to.

Stable:

- A bug fix with its regression test, both passing.
- A new feature plus tests covering golden path and obvious edge cases.
- A refactor that compiles clean and leaves the test suite green.
- A mechanical sweep (rename, package move, import shuffle) with build
  and tests still green.

Not stable:

- Production change without the matching test.
- Half-done work "going to need another pass anyway".
- A green compile with skipped or commented-out tests.

Commit messages describe the *why* in one or two sentences, not the
*what* — the diff already shows the what. Target tone matches recent
commits: a one-liner subject naming the change, optionally a short
paragraph if the motivation isn't obvious. Use a HEREDOC so multi-line
formatting survives. Never amend a published commit.

A long phase that genuinely belongs together (e.g. a multi-file rename
that only makes sense as one atomic change) is fine as one big commit.
But if a phase has internal milestones — A compiles+tests, B
compiles+tests, C compiles+tests — each milestone gets its own commit.

### Auto-commit and push once a change is stable

When a fix/feature/refactor reaches the stable bar above **and** it's
the natural end of the change you were asked to make, commit it AND
push to origin without waiting. Don't sit in a "want me to commit?"
prompt — the default is yes.

This applies double to diagnose-and-fix flows. The moment a real fix
lands for a problem we were investigating (CI failure, prod regression,
OOM, broken test), wrap up by committing and pushing — that's how the
fix actually reaches CI/prod. Don't stop at "compiles locally" and wait
for me to say `push`.

Stop and ask only when something can't be undone cheaply:

- Force pushes / rewriting published history (`push --force`,
  `reset --hard` against an upstream branch, `commit --amend` on
  anything already on origin). Always ask.
- Destructive ops with no easy backout: dropping a Mongo collection,
  truncating a table, deleting branches, `rm -rf` outside `target/`.
  (Killing live Fly machines is NOT in this bucket — see "Short prod
  downtime is fine" below.)
- Committing files that might carry secrets (`.env.local`, credentials,
  API keys). Stage by explicit path; flag and ask before staging
  anything that smells like a secret.
- A diff so large or cross-cutting that a reviewer would balk.

Everything else — code with green tests, CSS tweaks, doc edits,
refactors, multi-commit phases that each compile+test green, even a
manual `flyctl deploy` to roll prod back to a known-good image during
an incident — just do it. If a push triggers CI and CI fails, fix
forward in the next commit; don't undo the push.

### Short prod downtime is fine

Brief downtime in prod — up to ~15 minutes — is acceptable if it gets
the change done sooner. Don't dance around clone-swap orchestrations,
blue/green cutovers, or per-machine canary stages just to keep
`kinowo.fly.dev` answering 200s the whole time. For a hobby-traffic
app, the cost of "page 404s for two minutes while a new machine boots"
is virtually nothing; the cost of building a zero-downtime sequence is
real engineering time the user would rather you spend elsewhere.

Concretely: when moving a Fly machine, swapping a process under live
traffic, redeploying after a config change, or similar prod-touching
lifecycle ops — just `destroy` and `create` (or equivalent). Watch CI
for the deploy to roll, sanity-check, move on. Downtime under 15 min
is in the "everything else" bucket above.

This is about LIFECYCLE — destroying machines, restarting processes,
brief 5xx windows during a redeploy. Destructive *data* ops (dropping a
collection, truncating a table, deleting branches) still need explicit
confirmation per the rule above; downtime is recoverable, data isn't.

If you commit but defer the push for some reason, say so in the same
message so I don't have to ask "did you push?"

## Extract repeated patterns into a shared abstraction

If you find yourself writing the same shape of code in a second place —
a `FakeRepo extends MovieRepo` defined inside every spec, the same
regex+`replaceAll` chain across two parsers, a "load fixture and feed
through this client" helper duplicated per spec — stop and extract it
(a `private[services]` helper, a `test/scala/...` shared base, a method
on the most relevant existing class). The threshold is *two* uses, not
three.

This rule is about **multi-line shapes** — a worker-pool + scheduler
scaffolding, a `NFD → strip diacritics → ł→l → lowercase` chain, a
"parse the script[type=application/ld+json] block" idiom, a parser plus
its regex companion. It is **not** the same rule as "three similar
lines is better than a premature abstraction" — that one targets
trivial inline statements (`val x = a + b; val y = b + c`), where
inlining buys nothing. A multi-line shape that repeats once already has
enough surface area to be a concept.

When extracting:

- Put the shared piece where the most callers can already see it
  (`services.movies` if every caller is in `services.*`; a `test/scala`
  shared object if it's test-only). Don't pull in cross-package imports
  just for visibility.
- Delete the inline copies in the same commit. Leaving one creates
  drift.
- Name after the *concept* (`InMemoryMovieRepo`, not `FakeRepo`;
  `ProductionLineRegex`, not `parseHelper`) — generic names re-attract
  duplication.

## Treat cleanup as a phase of every task

Every feature/change/bugfix/refactor ends with a cleanup pass, and
sometimes opens with one too. The work isn't done when the new code
compiles + tests pass — it's done when the surrounding area is at least
as clean as when you started.

At the end of each task (and at the start when the area is crufty
enough that touching it without tidying would produce worse code):

- **Duplication you just introduced or noticed.** Extract it (see
  "Extract repeated patterns").
- **Dead code the change made obsolete.** A field you stopped writing
  to, a method whose only caller you removed, a test asserting
  behaviour that no longer exists, an unused import, a one-shot script
  whose purpose is served. Delete in the same commit.
- **Comments that lie now.** Doc comments referencing the old name,
  flow, or caller. Rewrite or remove.
- **Redundant intermediate variables / helper methods** the change
  collapsed to one line at the call site. Inline them.
- **Stale tests / fixtures.** A test exercising removed behaviour; a
  fixture no longer loaded.
- **Naming drift.** A class still called `EnrichmentCache` after the
  concept became `MovieCache`.

Open with cleanup when:

- The function you're about to extend has three near-identical branches
  you'd add a fourth to. Refactor the three first.
- The class has a field/param/method that's already dead. Remove it
  first.
- The test you're about to add would copy-paste setup from two existing
  tests. Extract the helper before writing the third.

The output of the task is the diff. A diff that's half real change and
half "and I also deleted these three dead methods" is the *right*
shape. A diff that ignores the dust is the wrong shape.

**Cleanup can — and should — reach beyond the file you'd otherwise
touch.** If, while working on a feature in `MovieCache`, you read
through `MovieService` and notice dead code, a stale comment, or a
duplicated pattern, fix it in the same change. Every file you read is
fair game. The discipline isn't "stay in your lane"; it's "leave
anywhere you looked at least as tidy as you found it".

Extract cleanup commits separately when substantial. A one-line dead
import can ride along; a 200-line rename gets its own commit so the
feature's diff stays reviewable.

Skip cleanup only when it would balloon a single commit beyond what a
reviewer can hold in their head AND there's no clean way to split it.
Then mention what you saw and didn't do — don't silently shrug.

## After every change, audit what it displaced

Once a change is functionally done — tests pass, the diff would land
cleanly — pause and ask: *what did this change just make obsolete?*
The cleanup-as-a-phase rule above is reactive ("notice while looking");
this one is proactive (analyse impact on the surrounding code path,
class, or configuration).

Walk the call graph:

- **A code path the change replaced.** The fallback branch that handled
  the case the new path now handles; the alternative resolver that was
  the workaround for the bug just fixed; the `else` branch now
  unreachable; the helper whose only caller now inlines its work.
- **A class whose functionality is redundant.** Methods are
  pass-throughs; it only existed for a caller you removed; the trait
  it implemented was deleted.
- **A parameter, event type, or config flag with no remaining purpose.**
  A feature flag whose "off" branch is unreachable; an event nothing
  subscribes to; a constructor arg whose consumer was refactored away;
  a default the call site always overrides.
- **A test for behaviour that no longer exists.** A regression for a
  bug now structurally impossible; a fixture nobody loads; an
  assertion specific to the old code path.

When the redundancy is obvious — single-caller method now inlined, an
unreachable branch, an unused import — delete in the same commit. When
it's debatable — removal would ripple wider than this change's scope —
surface it explicitly: "Heads-up, X is now redundant. Want me to remove
it in a follow-up?"

Each change's diff either deletes the displaced code alongside the new,
or names the displacement out loud. Silently leaving the now-dead path
is the failure mode.

## Follow SOLID — especially depend on interfaces, not implementations

The SOLID principles are the design baseline. Each gets its own section
below — short rationale, the smells that signal a violation, and how it
lands in this codebase.

### S — Single Responsibility

> A class should have one, and only one, reason to change.

A class's "responsibility" is *who* asks for it to change. If two
concerns (or stakeholders, or upstream services) can independently
force the class to change, that's two responsibilities — split. The
flip side is cohesion: things that change together live together.

Smells:

- Can't describe the class without saying "and." `Cache` — fine.
  `CacheAndPersistentStoreAndEventPublisher` — three classes.
- Generic noun in the name (`Manager`, `Handler`, `Util`, `Helper`,
  `Processor`) — almost always catch-alls.
- A small bugfix in one feature touches lots of unrelated methods.
- Many private helpers that no two callers share.

In this codebase: `ImdbIdResolver` recovers a missing IMDb id;
`ImdbRatings` refreshes the rating. Two reasons to change (suggestion
endpoint vs GraphQL rating API) → two classes. `MovieCache`,
`MovieService`, `ShowtimeCache` each change for reasons the others
don't care about.

### O — Open / Closed

> Software entities should be open for extension, but closed for
> modification.

Adding a new variant of an existing concept shouldn't require editing
the existing code — *extend* (new subtype, strategy, plugin) without
reopening the closed file. In practice: polymorphism instead of
`switch` / `if-else-on-type`.

Smells:

- A `match`/`switch` on a sealed family that grows by one case per new
  variant, touching every site that matches. Move the per-variant
  logic onto the variant.
- A core class that takes a config flag and forks behaviour inside.
  Move the fork into a strategy.
- Adding a new feature requires editing 5 files, none of which is the
  new feature's own file.

In this codebase: adding a cinema is a new `CinemaXClient` fitting the
existing scrape contract — `ShowtimeCache` doesn't change. Adding a
rating source is a new `*Ratings` class subscribing to the existing
`TmdbResolved` / `ImdbIdMissing` bus events — bus, cache, and service
don't change.

### L — Liskov Substitution

> Subtypes must be substitutable for their base types without breaking
> the program. (Barbara Liskov, 1987.)

A subtype must honour the **behavioural** contract of its supertype,
not just the type signatures:

- **Preconditions can't be strengthened.** If the base accepts any
  String, the subtype can't require a non-empty one.
- **Postconditions can't be weakened.** If the base returns non-null,
  the subtype can't return null.
- **Invariants preserved.** If the base guarantees thread-safety, the
  subtype can't drop it.
- **History constraint.** A subtype can't introduce mutation the base
  didn't permit (immutable base, mutable subtype via the base
  reference).

Smells:

- Override throws `UnsupportedOperationException` or
  `NotImplementedError`. The subtype isn't actually a subtype.
- The `Square extends Rectangle` shape — setting width and height
  independently works on Rectangle and breaks on Square.
- Callers have to know the concrete type
  (`if (repo.isInstanceOf[InMemoryMovieRepo])`). Abstraction is leaking.

In this codebase: `InMemoryMovieRepo` honours `MovieRepo`'s
write-through contract — `upsert` updates the store, `delete` removes
from it, `findAll` returns current contents. A caller using the
`MovieRepo` reference can't tell the difference.

### I — Interface Segregation

> Clients should not be forced to depend on methods they do not use.

Prefer many small, focused interfaces over one general-purpose "god
trait". A consumer that only reads shouldn't compile against the write
API. A test fake that needs two methods shouldn't have to stub fifteen.

Smells:

- A trait with 20+ methods and most callers using 2–3 each.
- Test fakes with many `???` / "should not be called" stubs because the
  interface is wider than the test exercises.
- Methods marked "callers should ignore this" or "only the X impl uses
  this".

In this codebase: keep `MovieRepo` to the persistence contract
(`findAll`, `upsert`, `updateIfPresent`, `delete`, `enabled`, `close`).
Don't bolt on enrichment, scheduling, or display. A new caller that
needs only reads gets a `MovieRepoReader` sub-trait.

### D — Dependency Inversion

> High-level modules should not depend on low-level modules. Both
> should depend on abstractions. Abstractions should not depend on
> details — details should depend on abstractions.

**This is the load-bearing principle for this codebase.** Every
non-trivial collaboration is wired in `AppLoader` (the composition
root); everything else sees only abstractions through constructor
parameters. `MovieService` doesn't know whether `MovieRepo` is talking
to Mongo, an in-memory map, or a flat file — it knows the trait.

Smells:

- High-level class imports a concrete low-level class directly
  (`import services.movies.MongoMovieRepo` from inside `MovieService`).
- Constructors take concrete classes instead of traits.
- Tests have to stand up real infrastructure (Mongo, HTTP server,
  filesystem) because there's no abstraction to swap.
- "I need a feature flag to toggle behaviour X" — usually a missing
  abstraction; introduce a trait with two implementations and pick at
  the composition root.

Related:

- **Inversion of Control.** Accept collaborators, don't construct them
  inside. `class FilmwebRatings(cache: MovieCache, client: FilmwebClient)`,
  not `class FilmwebRatings() { val cache = new MovieCache(); ... }`.
- **The Dependency Rule** (Clean Architecture). Source dependencies
  point inward, toward higher-level policy. Domain doesn't import
  infrastructure; infrastructure imports domain.

Constructors and method parameters take the **abstraction**, never a
concrete class. Production code never references a test subclass
directly — tests swap in via the constructor parameter.

**Never suffix a class with `Impl`.** `MovieRepoImpl`,
`FilmwebClientImpl`, `EventBusImpl` are an anti-pattern — they tell you
the class is "the implementation" without saying of *what kind*, and
only exist because the writer ran out of names. Name after what makes
the implementation distinct:

- `MovieRepo` (trait) + `MongoMovieRepo` + `InMemoryMovieRepo`. Each
  name earns its keep.
- `FilmwebClient` (trait) + `HttpFilmwebClient` if a second backend
  appears. Not `FilmwebClientImpl`.
- If there's truly only one production implementation and naming feels
  awkward, that's a signal you don't need a separate trait yet —
  collapse it into the class until a second implementation shows up.
  The rule is "no `Impl` suffix", not "every concrete class needs a
  trait".

When in doubt: would a future reader of the class name guess what it
*does*? `MongoMovieRepo` — yes. `MovieRepoImpl` — no.

## Share business logic between real and fake implementations

When a trait has both a real and a fake/test implementation, draw the
trait so the business logic lives in **shared** code, not duplicated
across implementations. The two should differ only at the
infrastructure boundary — where data is stored, which HTTP backend is
called, what clock ticks — never in their understanding of the rules.

Whenever you reach for a new fake (`FakeFooClient`, `StubFooService`,
`InMemoryFoo…`), first ask whether the logic you're about to copy
belongs above the trait. If the fake needs to re-implement the same
merge rule, the same write-through ordering, the same "don't publish
on no-op" filter, that logic is on the wrong side of the seam.

How to push it up:

- **Split the trait into two layers.** The outer concrete class owns
  the business logic; it depends on an inner trait that's a narrow
  infrastructure boundary. Only the inner trait gets a fake. Example:
  `MovieCache` (concrete — Caffeine, write-through, event publishing)
  depends on `MovieRepo` (trait — Mongo or in-memory). Tests inject
  `InMemoryMovieRepo` and get the real cache semantics for free.
- **Default methods on the trait.** `trait Foo { def primitive(): X;
  final def derived(): Y = ... }`. Real and fake implement primitives;
  derived behaviour is shared by construction.
- **Extract a helper / pure function** both impls call.

Treat the refactor as part of the change that introduces the fake —
not a follow-up. A fake that re-implements logic the real class has is
worse than no fake: it lets tests pass while real code is broken (or
vice versa).

Signs you've drawn the seam in the wrong place:

- The fake has its own copy of a sort/merge/filter rule the real impl
  also has.
- A behaviour change to the real impl forces a parallel change to the
  fake to keep tests green.
- The fake's body is longer than "store this, return that" — it's
  actually deciding things.
- Two tests against the fake disagree about the rule because each
  patched it differently.

Done right, a fake is boring: a `HashMap`, a fixed list of HTTP
responses, a `Clock.fixed(...)`. The business logic sits above and is
exercised end-to-end with the real outer class.
