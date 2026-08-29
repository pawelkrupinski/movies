# Project conventions for Claude

## Run the relevant test layer locally before claiming a feature is done

When a change touches an area one of the CI test layers below
exercises, run **at least one** of those layers locally as part of
the same task — before reporting the work as done. The list of
layers and which kinds of changes they catch:

- **`sbt testUnit`** — unit specs for controllers, services,
  enrichment, clients, models. Any change under `web/src/main/`,
  `worker/src/main/`, or `common/src/main/` that isn't pure view markup.
- **`sbt itAll`** — `*/src/it/scala/` specs wiring fakes + the real
  cache/repository. Required for any change to enrichment pipelines,
  cache layering, the read-model projection, or anything that crosses
  the `MovieService` ↔ `MovieRepository` ↔ `MovieCache` seam.
- **`sbt web/PageTest/test`** — real Chrome over CDP against
  Twirl-rendered fixtures. Required for any change to
  `web/src/main/assets/js/`, the inline `<script>` blocks in the
  repertoire templates, or the rendered HTML shape those blocks read.
- **Playwright** (`page-tests-playwright`) — mobile + desktop ×
  Chromium / WebKit / Firefox / Edge. Required for visible UX changes —
  card-tap, pill rows, gestures, the empty / loading states.
- **iOS LocalServer** — the real iOS listing parser against a live
  fixture-server render. Required for any change to either side of that
  contract: server-side template/HTML shape or iOS `HTMLParser`.
- **`swift test --package-path ios`** — iOS unit / integration suites
  without the live server. Required for any change to iOS model /
  parser logic regardless of whether you also need LocalServer.

Exact commands, the module-scoping variants, and the `testOnly` / `-z`
narrowing that cuts a run from minutes to seconds are in the
`test-layers` skill.

You should run **all** the layers that match the change. Run them
**in parallel** when there are no dependencies (separate `Bash`
tool calls in the same message), but do **not** split the runs into
new subagents — keep everything in the main session so the test
output and any follow-up edits stay in one context. The only
acceptable reason to skip a layer is that the local toolchain is
missing (e.g. no full Xcode for `xcodebuild test`); say so
explicitly when reporting the work.

CI is the safety net, not the test plan. Pushing and waiting for
CI to catch something is the wrong shape; pushing once the relevant
layers are green locally is the right shape.

## Regenerate the snapshots your change shifts

THREE checked-in snapshot layers guard this repo, and a change that
shifts any of them is not committable until that snapshot is
regenerated and committed alongside it:

- **`expected-*.html`** — the rendered HTML for `/` (per city: Poznań,
  Wrocław, Warszawa) and `/plan`, diffed by `PageSnapshotSpec`. Shifts
  on any Twirl template, markup, `data-*`/CSS-class, `PosterProxy`, or
  inline-JS change. (Comments inside inline `<style>`/`<script>` are
  stripped by `tools.Minify`, so those don't shift it; HTML `<!-- -->`
  comments do.)
- **`expected-schedules.txt`** — the WHOLE-CORPUS assertion, also
  guarded by `FilmScheduleEndToEndSpec`. The spec asserts one anchor
  film field-by-field inline and gives every other film the same depth
  through this file: one block per film with `displayTitle` beside
  every raw `cinemaTitles` spelling, runtime, year, countries, poster,
  synopsis length, cast, director, tmdbId, imdbId, all four ratings,
  the MC/RT/Filmweb URLs, per-cinema slot provenance, and every
  showtime with its room and format. It is the layer that names a
  film the pipeline started or stopped emitting, so it is usually the
  first to fail and the only one that says WHICH films moved.
- **`read-model-snapshot.json`** — the pipeline's projected output,
  guarded by the same spec. Not an assertion so much as a CACHE: the
  page-test servers load it instead of recomputing the ~110s pipeline
  per boot, and the guard exists to stop it going stale (consumers
  fall back to a full boot when it is absent, so a forgotten regen is
  slow, never wrong).

The last two shift on any cinema scraper, enrichment,
`TitleNormalizer`, staging-fold, `ReadModelProjector`, model-field, or
raw-fixture change — and a pipeline change usually shifts the rendered
HTML too, so regenerate all three.

The regenerate commands (delete → run the spec → re-run to confirm
stability) are in the `regenerate-snapshots` skill.

## Parallelize scripts, but don't get rate-limited

Scripts that hit external services (TMDB, IMDb, Filmweb, Metacritic,
RT, OMDb, Cinemeta, scraped cinema sites) run per-row work in
parallel — serial loops of hundreds of HTTP round-trips are
unacceptably slow when 90% of the time is network wait. Default to a
fixed pool of 5–10 concurrent workers, halve concurrency on any
429/503, and print throughput at the end. Per-service limits are in
the `external-api-rate-limits` skill.

## Always add tests for new or changed functionality

**The commit gate — read this first.** Do not commit a change unless
you can write a test that confirms the behaviour *was not there before
and is there now*: a test that fails before your change and passes
after. If no such test can be written, the change is not ready to
commit. This is the exact bar `Commit at every stable state` means when
it calls an untested production change "not stable" — it is a gate, not
a suggestion.

**"Test" means any layer that reaches the behaviour — not just a unit
test.** The fail-before / pass-after gate is satisfied by whichever
layer actually exercises the change: a `swift test` / `sbt testUnit` unit
spec, an `IntegrationTest` spec, a `PageTest` (real Chrome) or
Playwright browser test, or an iOS LocalServer / emulator test. "It's
UI, I can't unit-test it" is NOT grounds to skip — it is grounds to
drop down to PageTest / Playwright / the emulator and assert there
(e.g. pixel-sample the rendered result, per the top-bar memory). A
visible UX change shipped without a browser-or-emulator test has not
met the gate.

Every piece of new or modified behaviour MUST come with that test.
Non-negotiable for bug fixes (must fail before the fix and pass after),
new methods or branches, parsing/normalisation changes, and any logic
that decides what to persist or display. "I ran it once and it looked
right" is not a substitute.

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

The ONLY genuine exception is behaviour that even the browser/emulator
harnesses cannot reach — e.g. sub-16px input-zoom, un-reproducible in
Playwright WebKit and the Simulator alike (see the focus-zoom memory).
In that case test the closest reachable mechanism instead (the
maximum-scale toggle, not the zoom), AND state out loud in the commit
message / report that the behaviour itself is unreachable by every test
layer and why. "Observable only via a running server/browser" is NOT
that exception — we have browser and server test layers, so use them.
Exploratory spikes may skip the failing-test-first step, but still owe
a test before the work is committed. If a neighbouring test is the
closest match, extend it rather than inventing a new style.

For pure logic (parsers, formatters, normalisers, decision functions),
unit tests against in-memory inputs are enough. For composed services,
prefer the existing spec patterns in `*/src/test/scala/services/...`
that wire fakes/in-memory implementations.

### Record fixtures for external-service clients

Clients that hit a real external API (TMDB, IMDb, Cinemeta, OMDb,
Filmweb, Metacritic, RT, scraped cinema sites) get a real captured
response replayed from disk — never live HTTP in tests, never
hand-written mock JSON, which drifts from reality and hides the parser
bugs a real payload would catch. The `record-client-fixtures` skill
covers when to record one and where fixtures live.

## Never leave uncommitted changes stranded — and don't work in the root checkout

This repository is worked by many agents in parallel. The root checkout
(`/Users/pawel/projects/movies`, branch `main`) is shared and must end
every session **clean** (`git status --porcelain` empty). Uncommitted
WIP left in a working tree is the single most expensive mess this repository
hits: it strands work on a stale base, blocks the next agent's
`git rebase`/`pull`, and gets silently swept or lost. One session found
**92 files of uncommitted WIP sitting directly on the `main` checkout**
(a coherent "drop two cinemas" task that was never committed), plus a
half-dozen abandoned worktrees each carrying dirty, never-committed
feature work (mobile day-carousel, tuning screens) — all at risk of
being pruned away.

Rules:

- **No PRs, and NEVER push the feature branch.** The only ref that
  ever reaches origin is `main`. The flow is: rebase your worktree
  branch onto `origin/main` → all relevant layers green → ff-merge into
  `main` → push `main` → delete the worktree and its branch. Before
  typing `git push` or `gh pr create`, run
  `git rev-parse --abbrev-ref HEAD`; if it isn't `main`, you are about
  to violate this rule. (Breached twice — 2026-07-02 and 2026-07-19 —
  each costing a close-PR + delete-remote-branch cleanup.)
- **Do your work in your own worktree, never in the root/`main`
  checkout.** `git worktree add -b <branch> <path> origin/main`. The
  root checkout is for orchestration/inspection only — never edit
  production files there. (See the standing worktree-per-change rule.)
- **Never finish or yield a turn with a dirty tree you own.** Before you
  consider a task done — and before you hand off, pause, or could be
  terminated — either **commit** your changes (preferred; see the commit
  gate below) or, if genuinely not ready to commit, **stash with a
  descriptive label** (`git stash push -u -m "<what + why>"`) so the work
  is named and recoverable rather than a nameless dirty diff. A bare,
  unlabelled dirty tree is the failure mode.
- **An interrupted/exploratory task still owes a checkpoint.** If you
  must stop mid-task, commit a WIP commit on your *own branch*
  (`wip: <state>`) or stash-with-label. Don't leave the next agent — or
  your future self — to reverse-engineer what an orphaned 90-file diff
  was trying to do.
- **Don't strand work on a stale base.** If `git status` shows your
  worktree is behind `origin/main`, rebase before you pile more on;
  don't accumulate uncommitted edits against an old `main` that a
  rebase will then have to untangle.
- **Touch only what's yours.** If the tree holds dirty files you didn't
  create, leave them — they're a co-agent's WIP. Surface them; don't
  `git add -A`, commit, stash, or discard them.

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

- Production change without the matching test. The gate is concrete:
  if you cannot write a test that fails before the change and passes
  after (at any layer — unit, integration, browser, or emulator), the
  change is NOT committable. See `Always add tests`.
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
  (Killing a live Fly *worker* machine is NOT in this bucket — see
  "Worker downtime is fine" below.)
- Committing files that might carry secrets (`.env.local`, credentials,
  API keys). Stage by explicit path; flag and ask before staging
  anything that smells like a secret.
- A diff so large or cross-cutting that a reviewer would balk.

Everything else — code with green tests, CSS tweaks, doc edits,
refactors, multi-commit phases that each compile+test green, even a
manual `flyctl deploy` to roll prod back to a known-good image during
an incident — just do it. If a push triggers CI and CI fails, fix
forward in the next commit; don't undo the push.

### Worker downtime is fine — keep the web tier answering

**Workers** (`kinowo-worker`, `kinowo-worker-de`, `kinowo-worker-uk`)
can go fully down without ceremony. They scrape, enrich, and project on
a cadence; a gap just delays the next cycle and the read model keeps
serving what's already projected. When moving a machine, swapping a
process, or redeploying after a config change — just `destroy` and
`create` (or equivalent). Watch CI for the deploy to roll,
sanity-check, move on. Up to ~15 minutes of worker downtime is in the
"everything else, just do it" bucket above.

**The web / read tier (`kinowo.net`) is different — don't bring it
completely down if there's any way not to.** It's the only part users
see, and a page that 404s is the failure they notice. Prefer a rolling
deploy that keeps at least one machine serving; when moving or resizing
machines, create the replacement *before* destroying the old one.

This is not a licence to build elaborate zero-downtime orchestration
for the web tier either. A brief 5xx window as a rolling deploy passes
through is fine, and so is a genuinely unavoidable short full outage —
a region move, a machine the platform won't let you duplicate. When
that happens keep it short, and say out loud afterwards that the tier
was fully down rather than letting it pass unmentioned.

This is about LIFECYCLE — destroying machines, restarting processes,
brief 5xx windows during a redeploy. Destructive *data* ops (dropping a
collection, truncating a table, deleting branches) still need explicit
confirmation per the rule above; downtime is recoverable, data isn't.

If you commit but defer the push for some reason, say so in the same
message so I don't have to ask "did you push?"

## Extract repeated patterns into a shared abstraction

If you find yourself writing the same shape of code in a second place —
a `FakeRepository extends MovieRepository` defined inside every spec, the same
regex+`replaceAll` chain across two parsers, a "load fixture and feed
through this client" helper duplicated per spec — stop and extract it
(a `private[services]` helper, a `*/src/test/scala/...` shared base, a method
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
  (`services.movies` if every caller is in `services.*`; a `*/src/test/scala`
  shared object if it's test-only). Don't pull in cross-package imports
  just for visibility.
- Delete the inline copies in the same commit. Leaving one creates
  drift.
- Name after the *concept* (`InMemoryMovieRepository`, not `FakeRepository`;
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

## Don't iterate on transient errors

If a tool call fails with an error that smells like a build, cache,
race, or transient infrastructure problem — `No tests found`, `EBUSY`,
`ENOENT` on a file you just wrote, a module-resolution error in a
known-working setup, a Playwright `did not expect test.describe() to
be called here` from a project that ran the same files a minute ago —
**retry once cleanly before iterating**. Each variation ("maybe with
file paths instead of file names", "maybe `--list` to debug") produces
a new shape of the same noise, not new signal, and you spend turns
chasing a phantom.

The cheap probe: nuke the suspect state (`rm -rf test-results/`,
`rm -rf node_modules/.cache/`, `pkill -f playwright`, `sbt clean`,
etc.) and rerun the **original** command. If it reproduces, you have a
real bug to chase. If it doesn't, investigate and fix the test for
intermittent failures — a test that passes on rerun is not healthy,
it's flaky.

Real assertion failures have a specific expectation, value, or
location ("expected X, got Y at line N"). Transients have the shape
"the runner itself couldn't start", "the loader tripped on something
that was fine before", or "the filesystem disagrees with what I wrote
a second ago". Pattern-match on which kind of error you're staring at
before deciding to iterate.

## Follow SOLID — especially depend on interfaces, not implementations

The SOLID principles are the design baseline. What follows is how each
one lands *in this codebase*; the general definitions are assumed.

- **Single Responsibility.** `ImdbIdResolver` recovers a missing IMDb
  id; `ImdbRatings` refreshes the rating. Two reasons to change
  (suggestion endpoint vs GraphQL rating API) → two classes.
  `MovieCache`, `MovieService`, `ScrapeReaper` each change for reasons
  the others don't care about. Treat `Manager`/`Handler`/`Util`/
  `Helper`/`Processor` names as a smell — they're almost always
  catch-alls.
- **Open / Closed.** Adding a cinema is a new `CinemaXClient` fitting
  the existing scrape contract — `ScrapeReaper` doesn't change. Adding
  a rating source is a new `*Ratings` class subscribing to the existing
  `TmdbResolved` / `ImdbIdMissing` bus events — bus, cache, and service
  don't change.
- **Liskov Substitution.** `InMemoryMovieRepository` honours
  `MovieRepository`'s write-through contract — `upsert` updates the
  store, `delete` removes from it, `findAll` returns current contents.
  A caller holding the `MovieRepository` reference can't tell the
  difference, and never needs `isInstanceOf` to find out.
- **Interface Segregation.** Keep `MovieRepository` to the persistence
  contract (`findAll`, `upsert`, `updateIfPresent`, `delete`,
  `enabled`, `close`). Don't bolt on enrichment, scheduling, or
  display. A new caller that needs only reads gets a
  `MovieRepositoryReader` sub-trait.
- **Dependency Inversion — the load-bearing one here.** Every
  non-trivial collaboration is wired in `AppLoader` (the composition
  root); everything else sees only abstractions through constructor
  parameters. `MovieService` doesn't know whether `MovieRepository` is
  talking to Mongo, an in-memory map, or a flat file — it knows the
  trait. Accept collaborators, don't construct them inside:
  `class FilmwebRatings(cache: MovieCache, client: FilmwebClient)`, not
  `class FilmwebRatings() { val cache = new MovieCache(); ... }`. If
  you reach for a feature flag to toggle behaviour X, you're usually
  missing an abstraction — introduce a trait with two implementations
  and pick at the composition root.

Constructors and method parameters take the **abstraction**, never a
concrete class. Production code never references a test subclass
directly — tests swap in via the constructor parameter.

**Never suffix a class with `Impl`.** `MovieRepositoryImpl`,
`FilmwebClientImpl`, `EventBusImpl` are an anti-pattern — they tell you
the class is "the implementation" without saying of *what kind*, and
only exist because the writer ran out of names. Name after what makes
the implementation distinct:

- `MovieRepository` (trait) + `MongoMovieRepository` + `InMemoryMovieRepository`. Each
  name earns its keep.
- `FilmwebClient` (trait) + `HttpFilmwebClient` if a second backend
  appears. Not `FilmwebClientImpl`.
- If there's truly only one production implementation and naming feels
  awkward, that's a signal you don't need a separate trait yet —
  collapse it into the class until a second implementation shows up.
  The rule is "no `Impl` suffix", not "every concrete class needs a
  trait".

When in doubt: would a future reader of the class name guess what it
*does*? `MongoMovieRepository` — yes. `MovieRepositoryImpl` — no.

## Share business logic between real and fake implementations

When a trait has both a real and a fake/test implementation, the
business logic lives in **shared** code, not duplicated across
implementations. The two differ only at the infrastructure boundary —
where data is stored, which HTTP backend is called, what clock ticks —
never in their understanding of the rules. A fake that re-implements
logic the real class has is worse than no fake: it lets tests pass
while real code is broken (or vice versa).

Before writing any `Fake*` / `Stub*` / `InMemory*`, read the
`writing-fakes` skill — how to push logic above the seam, and the
signs you've drawn the seam in the wrong place.
