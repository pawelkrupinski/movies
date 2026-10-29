# 2026-10-29 — UK "Rose (2023)": a search's hit weighed before its record is filed (convergence #2527)

Identity model convergence #2527 (run 3757, commit 9744f8f09) failed `takes-drift-1 (united-kingdom)` on one flip alone.
It was 0 divergences from a whole resolve. The four fixes landed for #2473/#2517 before it (8a50e6d5d, 5ca8d38f8,
e809476a5, ecedac9aa) each cured other rows, and none touched this one:

```
Duke of York's Picturehouse Brighton | Rose (2023): tmdb:901251 [1 boot: OwnMatch] -> tmdb:1069112 [2 fill: OwnMatch]
  -> tmdb:901251 [8 fill+answer: OwnMatch] (back to a film it left)
```

Queen's Film Theatre Belfast flipped the same way. The flicks.co.uk node also holds Grosvenor Cinema Glasgow. 901251 is
Niels Arden Oplev's Danish "Rose" (2022, 105′), the film the listing bills: 106′, Oplev, Sofie Gråbøl, Søren Malling. It
is judged right in `identity-judgements.tsv`. 1069112 is "Rose Parade 2023", a 90-minute US broadcast of the parade.

## Which answer moves it

Seed 1's gate (`IdentityDrift.Share`, 15%, MurmurHash3 of the question id) holds these questions: the search
`q:t\0Rose (2023)`, the record `f:901251` and the record `f:1069112`. The search `Rose` and the director search are not
held. So at boot 1069112 is not reached yet. The step-2 fill files a random half of the held answers asked so far: the
search, but not 1069112's record, which is asked for the first time at that drain. That record is filed only by step 8's
fill 1.0.

The slice `test/resources/fixtures/identity-slices/uk-queen-s-film-theatre-belfast-rose-2023--4f64ad1a` was replayed
(`scripts.IdentitySlice uk --replay`, code 9744f8f09) with chosen requests dropped:

| withheld | take |
|---|---|
| nothing | 901251, 7.9% by dated-title; 1069112 DENIED (`country in {mismatch} AND runtime.gap >= 16`) |
| 1069112's record | **1069112, 52.2% by favoured-calibrated** (title=overlap −3.95, search.rank=1 +3.48, year.delta=0 +3.40, titleYear.delta=0 +2.81) |
| both records | 1069112, 52.2% |
| 901251's record | 901251, 2.7% |
| 901251's record and the search "Rose (2023)" (boot) | 901251, 2.7% |

1069112's OWN record is the answer that decides it. Without that record, the search's only hit carries just its title and
year, and nothing can deny it. The denial is in the record: a 90′ running time against the listing's 106′, and a US
broadcast.

The model equals a whole resolve of what is filed at every step (`IdentityDriftSpec`, below). This is not a cache, a
stage lag or a restart key. The interim take is a faithful function of an interim input: a hit filed without its record.

## Why the input state exists in production

- **The fill.** `LookupFill` asks the records of a newly answered search's films in the NEXT round: "A record a newly
  answered search names is the model's gap after its next drain, and the next round's question."
- **Live reads.** `StoredFirstLookups` asks a new question live, but a record read that fails or is still in its backoff
  (a 429, a timeout) reads `Unknown` while its search answered.

In both cases a search's film is weighed on its hit alone for at least a fill interval. A namesake that its record would
deny can take the listing for that window.

## The same class before

DE "Rose" (prod 10-18..10-22, `IncrementalResolverSpec` "keep apart while their films' records are not answered yet",
the `creditedNamesake` rule in `FamilyScope`): "while the three records went unanswered nothing denied any of them …
and the next fill moved them back". That fix was a rule for its shape: a billing crediting a director whose filmography
reached a namesake denies other directors' namesakes. It is still useful once records are filed, for a record that
credits nobody. It did not reach UK "Rose (2023)", where the namesake is a broadcast with no director at all.

## The first fix: a film is weighed once its record is filed (superseded below)

`Candidate.awaitingRecord`: `Candidate.of` takes the record's `Answer`, and `Unknown` marks the candidate awaiting.
`FamilyScope.pool`, the family's TMDB pool, leaves such a candidate out, as a search not answered yet names no film at
all. The fallback pool is unchanged.

The family still reads the film: `RegionFamily.films` is its members' hits, not the pool. So the record's filing
re-resolves it (`IncrementalResolver`'s `filmReadersOf`, and the slice digest, which hashes the candidate's new
`awaitingRecord`).

Test (fails before the fix with CI's exact flip, on all three listings): `IdentityDriftSpec` "a search filed before the
records of the films it names should move no take back and forth". It replays the pinned slice through
`IncrementalResolver`, holding the search, then 1069112's record, then nothing. It checks the model against a whole
resolve at each step and records the takes in `TakeFlips`.

## Open when this was handed over (2026-10-29 17:20Z)

The fixer that found this ([1500af]) handed over to the #2527 fixer holding the lease ([0e7d14]) at the coordinator's
request, with the fix committed on branch `red-2527` (~/projects/movies-wt-red-2527) and not landed. Still to do:

- the identity and worker unit suites. Watch `IdentitySliceCasesSpec` (a pinned case whose decision rested on a hit whose
  record the recording lacks moves), `IncrementalWholeEquivalenceSpec`, `LateInputEquivalenceSpec`, and
  `AgreementCorrectionSpec` (record 1001 held unknown);
- the drift replay of every country and seed on recording 573: no new flips, UK seed 1's gone;
- the final-take measure (`identity-measure-local` over de, es, pl, uk, us): a hit whose record a recording never got is
  no candidate any more;
- whether the fallback pool needs the same rule. An IMDb-only film's record read through `StoredTmdbLookups.fallbackFilm`
  is `Unknown` until filed.

## What landed: AGREE OR NONE (the owner, 2026-10-29)

The first fix (d51dc845f) cured Rose and moved no final take in any country (measure 3812), but drift 3810 showed two
new flips its parent does not have. Leaving a pending film out guesses its answer as much as weighing its hits does: DE
Metropol's bare "Madoka Magica - Der Film: Rebellion" took the 2013 film while the 1962 "Rebellion"'s record was pending,
and that rival once it was filed; UK Chichester's "The Ties That Bind Us" did the same with its 2008 namesake
(`2026-10-29-flip-burn-down.md` has the slice tables).

The owner's rule: while a candidate's record is pending, a node or cluster takes a film only where both readings take
it -- the pool with the pending films (weighed on their hits) and the pool without them -- else none until the record
is filed. `FamilyScope.of` and `FamilyScope.pooled` compare the two readings' takes (`Acceptance.aloneNamed`,
`Acceptance.pooled`) and mark every candidate UNTAKEN where they differ; the fallback scope is a `FamilyScope` too. It is
a function of what is filed, so a whole resolve and the incremental model decide alike at every step.
`IdentityDriftSpec` replays Rose, Madoka (`drift-slices/de/metropol-madoka-6a9b3bcc`) and Chichester
(`drift-slices/uk/chichester-ties-1231860f`) as their seeds filed them.
