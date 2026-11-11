# 2026-11-11 — sixteen TMDB posters a film instead of eight (dropped)

**Aim.** Raise `PosterEvidence.FilmPosters` 8 -> 16 so the pHash poster vote and veto (`AgreementStage`, `Posters.scala`)
see more of a film's artwork. The groundwork this needed landed separately (`poster-film-side`: a film poster document
records the cap it was chosen under, a re-hash reuses filed hashes by path, renewals drain 64 an apply, young films are
re-read weekly). The cap raise itself is kept as the tag `experiment/poster-cap16` (55e60be9a9).

**Offline evidence (why it looked worth it).** On the research sample's venue-film pairs, recall at <= 8 bits went
52.3% -> 55.8% with no new false positive. On prod's REAL candidate sets
(`~/.cache/movies-posters-research/prodval2-eval.py`, `prodval2-results.txt`; the population of
`2026-11-11-clip-poster-margin.md`: 2,621 clusters, 371 judged, 1,782 unjudged matched ones whose prod take is the weak
label), the <= 4-bit vote went 142 TP / 13 FP -> 155 / 13, the veto on judged takes was unchanged and weak prod-take
vetoes went 8 -> 6. Hashing every poster TMDB keeps added a false positive, so 16 was the candidate.

**Measured on prod's current takes.** `scripts/identity-what-if --answers live`, base origin/main 0c3ad5ea7f vs head
`poster-cap16-land` 55e60be9a9, all five countries, with an overlay patch
(`~/.cache/movies-posters-research/whatif-poster-overlay.patch`, measurement only: the head reads and files its
`poster|film|` documents in a store of its own, read before the mirror's, and films released within 60 days have their
mirror document hidden so they are asked again). 4,104 film documents were actually re-hashed at cap 16 through the
head's own code, and 909 young films re-asked.

| | model | final |
|---|---|---|
| takes moved, all five countries | 0 | 0 |
| gained / lost / new wrong | 0 / 0 / 0 | 0 / 0 / 0 |
| `poster.vote` or veto moves | 0 | 0 |

The +13 TP offline were clusters that other evidence already takes right: the vote agrees with a take that stands
without it.

**Cost it would have carried.** ~8.7k film documents filed at cap 8 asked again: one TMDB `/images` read each and ~6
new posters a film that holds 8 (~56k one-off image fetches), the renewal work doubled for every later re-read, and 92
identity-slice / drift-slice / capture cases re-pinned (`scripts/identity-slice-coverage --repin` for 24 slices, an
`IdentityDrift --slice --widen` re-cut for 3 drift slices whose family lattice asks the new `/images`, and an
`identity-capture.sh --fill` of the de, pl and uk captures).

**Verdict.** Dropped: no take moves on prod's data, for a one-off fetch burst and a permanent doubling of poster
renewal. A poster signal only pays where it decides a cluster nothing else takes; see the CLIP entry's "angles" and
`2026-11-11-venue-poster-multicrop.md` for the venue side.
