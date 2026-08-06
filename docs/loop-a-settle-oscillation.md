# Loop A — the settle-beat spelling oscillation

Status: **FIXED** — `MongoStagingFolder.withStitchedCinemas`, regression test
`integration.FoldSpellingAgreesWithSettleSpec`.

The mechanism recorded below under "The mechanism (proven)" was **half right, and its fix
was aimed at the wrong pool.** What is true: a row's spelling is a plurality vote over the
cinema titles on the row, and the vote gets rigged. What is wrong: the votes are not lost to
the divert. They are lost to the **storage split**.

TWO components run that vote and they were not seeing the same pool:

| component | pool it voted on |
|---|---|
| the settle (`MovieCache.canonicalizeBySanitize`) | the STITCHED record — every slot the film has, read back from `movie_slots` |
| the fold (`MongoStagingFolder.foldOnce`) | RAW `movies` documents — and a migrated film's `sourceData` is EMPTY, so the only cinema titles it could see were the ones on the STAGING rows |

So one diverted venue publishing a decorated spelling was an unopposed plurality of one, and
the fold re-keyed the whole film onto it; the settle read the stitched record, saw the
venues that publish it plainly, and re-keyed it back. Neither component is wrong on its own
inputs and neither converges — that disagreement is the beat. The fix makes the fold plan
against the stitched view, so both vote on the same pool.

Two things follow, and they are why the original diagnosis went where it did:

- **It is unreachable without the split.** With `sourceData` embedded, the sibling row
  carries its plain slots into the fold's pool and the two components already agree. The
  in-memory scrape→settle→fold loop self-heals in two cycles — pinned now as
  `services.movies.SettleBeatFixpointSpec`, which is green and was green before the fix.
  That is exactly why `feat/convergence-read-split` was the only harness that reproduced it.
- **`StagingFoldIntegrationSpec`'s "migrated film" case probed this same read and cleared
  it** — but with a SHOUTED variant, which sanitizes to the same key, so no re-key was
  possible either way and the check could not fail. Spellings that sanitize apart are the
  case that moves.

`StableSpellingUnderDivertSpec` has been **deleted, not weakened**. It asserted the right
property (a spelling is a function of the film, not of who is on the row this tick) at a
layer that cannot enforce it: `chooseDisplay`'s entire input *is* who is on the row, so from
a pool of one decorated title nothing distinguishes a mid-oscillation row from a genuine
decorated edition. The only rule that satisfies it — prefer the `fallback` when the pool
lacks it — is the incumbent-key candidate rejected below, and it reintroduces the
"depends on WHO asked" bug design rule 2 exists to prevent (the settle passes `minSpelling`,
a hydrate passes the `_id` prefix). The property now lives where it is enforceable: both
voters get the same pool.

## Verified on the convergence leg — and what still blocks the harness

`feat/convergence-read-split` (`ConvergenceStorage` wired with `screenings`/`slots`) does
catch loop A, and it is the only layer that does. `convergencePolandSample`, fix reverted:

```
tick 1:  7 known film(s) RE-DIVERTED to staging   (all decorated spellings)
tick 2: 25 known film(s) RE-DIVERTED to staging   (12 of them the BARE spelling)
        arekmamapanorama|2026 lost all 39 showtime(s)
        keys VANISHED (Arek.Mama.Panorama) / APPEARED (Przedpremiera: Arek. Mama. …)
```

— the 7-then-25 asymmetry the original diagnosis predicted, exactly. With the fix, the
fixpoint leg and the read-model leg both go green and the run drops 3m06s → 33s.

**The harness still cannot land.** Under the split, the order-independence leg fails with 4
divergences that are NOT loop A and are not fixed by it: which decorated-edition row a
venue's screenings attach to depends on arrival order — `Kino plenerowe: Wartość
sentymentalna …` vs `Kino bez barier: Wartość sentymentalna (AD + CC + PJM)`, and
`Left-Handed Girl …` vs `Plenerowe Pałacowe: Left-Handed Girl …`. Note "FILMS differ" is
gone (the key sets agree now); what remains is screening ROUTING among sibling decorated
rows. Unsplit, all four legs are green. So the earlier note that the harness was "unmerged
only because loop A fails it" was incomplete — there is a second, independent defect behind
it.

Everything below is the ORIGINAL handover, kept because its call chain, its rejected
candidates and its design rules are all still accurate and still load-bearing.

## The symptom

Production PL merges rows on every `SettleReaper` tick — :21 and :51 past the hour,
never a zero tick, ~83 `kinowo_worker_merges_total{reason="canonicalize"}` a day. Each
looping film also re-requests Filmweb, Metacritic and RT on every cycle, so the waste is
external as well as internal. Grafana `fly-overview` panel-32 (merges by reason) and
panel-29 (staging incubation by step) beat together on the same 30-minute cadence.

## The mechanism (proven)

`TitleNormalizer.chooseDisplay` picks a row's spelling by **plurality over the cinema
titles currently on the row**:

```scala
val dominantKey = votePool.groupBy(sanitize).toSeq.sortBy { case (k, ts) => (-ts.size, k) }.head._1
```

A venue that diverts into staging takes its slot **out of `movies`**, so it leaves that
vote pool. Lose enough bare-title venues to a divert and the decorated spelling wins the
vote; the row re-keys onto it; every venue still publishing the bare title now fails to
match the row, so they divert too — removing more bare votes. **The loop rigs its own
vote**, which is why it never converges.

It explains the asymmetry seen in the harness: 7 re-diverts on one tick (all decorated
spellings), 25 on the next (12 of them the *bare* spelling), flipping
`Arek.Mama.Panorama` against `Przedpremiera: Arek. Mama. Panorama | Wakacje z dokumentem`.

The call chain that decides the spelling:

```
FilmCanonicalizer.canonical      builds the pool: slotKeys ++ keys   (canonical:363-396)
  └─ MovieRecord.displayTitle    perCinemaTitles = cinemaData.values.flatMap(_.title)
       └─ TitleNormalizer.chooseDisplay   the plurality vote        (chooseDisplay:287-309)
```

## The proof

**Superseded — this spec is deleted; see the header.** It was:

`common/src/test/scala/services/movies/StableSpellingUnderDivertSpec.scala` — a pure unit
test, no pipeline, no Mongo, no timing. It states the property that has to hold (the
spelling is a function of the FILM, not of who is on the row this tick) and currently
fails:

```
"przedpremieraarekmamapanoramawakacjezdokumentem" was not equal to "arekmamapanorama"
```

**This test is committed FAILING on purpose.** It goes green when the fix is right. Do not
weaken it to get a green run — the property it asserts is the fix.

## The fix — option 4: fix the pool, not the vote

**Superseded.** "Fix the pool, not the vote" was the right instinct aimed at the wrong pool:
the fold's pool is short its own `movie_slots`, not the film's staging rows. Threading the
staging titles in would also have broken hydration, which derives a row's title through the
SAME ladder (`StoredMovieRecord.fromStorage`) and would have kept re-keying the row back —
`MovieCache.rehydrate` migrates a drifted row and deletes its old `_id`
(`MovieCache.scala:1709-1718`), so the settle and the hydrate would have fought instead.
Kept for the reasoning:

Feed `chooseDisplay` the film's **staging rows alongside its `movies` rows**. A diverted
venue's title then stays in the pool, the plurality cannot flip, and the loop never
starts.

Why this one:

- keeps the plurality rule intact — no documented design rule is bulldozed;
- stays a pure function of state (movies + staging), so it is order-independent —
  staging content *is* part of the state, unlike history;
- `MovieService.settle` already holds the `StagingRepository`, so the seam exists.

The threading problem to solve: the pool is assembled in `FilmCanonicalizer.canonical`
(`slotKeys ++ keys`) and consumed through `MovieRecord.displayTitle` →
`TitleNormalizer.chooseDisplay`. The staging titles have to reach one of those two
points. `canonical` is a pure function called from several places, so prefer passing the
extra titles in over reaching for a repository inside it.

### Rejected candidates — do not re-derive these

| Candidate | Why it fails |
|---|---|
| Prefer the **shortest** sanitize group | A venue that abbreviates wins: `Spider-Man` beats `Spider-Man: Całkiem nowy dzień`. |
| Prefer the **token-run base** | Identical failure shape to the above — an abbreviation is a token-run prefix. |
| Keep the **incumbent key** while still represented | Stable, but history-dependent: three passes arriving in different orders keep different spellings, breaking the order-independence assertion. |

### Two design rules the fix must honour, not bulldoze

1. `FilmCanonicalizer.canonical:364-371` votes on **cinema** slot titles rather than
   TMDB's title, precisely so a decorated edition cannot collapse onto the base row.
2. `chooseDisplay:294-302` narrowed its fallback pool so the answer stops depending on
   **who asked** — an earlier violation caused per-boot row rewrites.

## Verification

Branches:

- `fix/loop-a-stable-spelling` — the failing test. Start here.
- `feat/convergence-read-split` — the harness wired to production's storage shape.
  **This is the reproduction**: with it, the fixpoint leg fails on loop A in ~6 minutes.
  It is verified and unmerged only because loop A fails it; land it once loop A is green.

Layers, in the order worth running:

| Layer | Command | Time |
|---|---|---|
| the proof | `sbt "common/testOnly services.movies.StableSpellingUnderDivertSpec"` | seconds |
| unit | `sbt testUnit` | ~1 min |
| integration | `sbt itAll` (needs `MONGODB_URI` → **:28017**, and `MONGODB_DB` unset) | ~35 s |
| reproduction | `sbt convergencePolandSample` | ~30 s warm, ~6 min cold |
| full leg | `sbt convergencePoland` | ~12 min |

Snapshots: regenerate `expected-schedules.txt` and `read-model-snapshot.json` (the
`regenerate-snapshots` skill) and commit them alongside. Leave the DE and UK convergence
legs (27 and 73 minutes) to CI.

## Environment

- Convergence Mongo on **:27117** — `scripts/convergence-local.sh pl` creates it.
- `itAll` replica set on **:28017**, with `MONGODB_DB` unset (pinning one DB makes specs
  drop each other's).
- `TMDB_API_KEY` comes from the root `.env.local`, and `Env` reads from the **working
  directory** — so copy *only that line* into the worktree. Never the whole file: its
  `MONGODB_URI` points at the prod tunnel.
- `gh` auth, for the `convergence-fixtures` release asset.

Pre-authorised by Paweł: snapshot churn is expected and fine; merge
`feat/convergence-read-split` once loop A is green.

## A warning about this document

The mechanism above is proven by a test. The *fix* is not — it is the first candidate
that survived every objection I could construct, after three earlier candidates (two of
them recommended out loud before being disproved) turned out to be wrong on closer
reading. Re-derive option 4 against the failing test rather than trusting it because it
is written down here.

## Adjacent, independent of loop A

- `scripts.ReapOrphanedFilmRows` lives in `worker/src/test/scala/scripts/` and is never
  scheduled — 46 orphaned filmIds in prod, `zaproszenie|2026` alone holding 124 slots.
- `MovieCache.scala:1271` mints a yearless key without checking for a yeared sibling.
- 452 `web_movies` rows have no backing `movies` row; ~140 are decorated variants of a
  row that is still live.
