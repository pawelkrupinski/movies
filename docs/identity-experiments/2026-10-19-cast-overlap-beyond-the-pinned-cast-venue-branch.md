# 2026-10-19 — cast overlap, beyond the pinned `cast.venue` (branch `experiment-cast-overlap`, tag `experiment/cast-overlap`, recording 38713274617, base `a07939cf6`)

## Coverage: why `cast.venue` gains 0
- **Venue side** (recording, the venue's OWN text — kinoheld / kinoprogramm pages and Webedia / Cineamo ids left out, as
  `namesAreVenues` does): a cast field on PL 23 %, UK 83 %, DE ~0 % (all 2,324 DE cast fields are kinoheld copies of
  kinoheld's own match, as often of a WRONG film as a right one: 41 wrong / 32 right judged), ES 72 %, US 96 % of listings;
  a synopsis on PL 41 %, UK 66 %, ES 97 %, US 1 %.
- **Film side:** the recording's TMDB bodies carry credits for ~97 % of films, but prod's store (`tmdb_films`,
  `local.credits.cast`) holds a cast for only 24 % (PL), 30 % (UK), 44 % (DE), 19 % (US) of records — filed before records
  kept it — and the captures for 5 % (PL 68/1,379), 7 % (UK 254/3,753), 0 (DE, ES, US).
- So `CastEvidence` ("any candidate's cast unknown → nothing") almost never fires where the unified rules were counted
  (the captures), nor in prod. Not covered by other rules — gated out by unknown casts.

## Separation on the judged set (`identity-judgements.tsv`, venue's own text, film cast non-empty)
| level | right | wrong |
|---|---|---|
| none | 66 | 72 |
| one | 31 | 6 |
| two+ | 83 | 12 |
The 12 wrong two+ are multi-film bills (Bear Hunt + Tiger Who Came to Tea, Gruffalo, LOTR triple), a tribute (Dolly
Parton) and a recut (Manhunter: The Final Cut): cast confirms the film is IN the bill, not that it is the bill.
Prose naming no cast is no evidence (prod takes, silver: log-LR −0.15); a cast FIELD naming none is (−3.29).
Joined to prod's traces (takes prod still makes; 102 units with text and cast, only 6 wrong): CV AUC of today's logit
0.705 → 0.781 with cast, log-loss 0.218 → 0.199 — too few wrong takes to trust. Silver candidate-level units (2,699 prod
takes corroborated by director + exact year, 2,701 rivals; cast field only): two+ 2,438 / 307 (+2.07), one 77 / 461
(−1.78: rivals share one actor — sequels, remakes), none 62 / 1,679 (−3.29). Nearly every such take is already decided
by its director: of the judged takes prod makes whose text names two+ of the cast, 1,053 of 1,147 listings also have
`director=same_person`; cast is new information mainly for the 849 prod takes billing no director (two+ 168, one 82,
none 269, unknown 330).

## A: a `cast` listing-film measure in the calibrated logistic — NOT LANDABLE
- `IdentityMeasures.castRelation`: `two_plus` / `one` / `none` beside a cast field, `two_plus` or `missing:prose` beside
  prose only, `missing:film` where TMDB credits nobody or the cast was not fetched; node listing side = union of its
  listings' `VenueNames` (a marker hash records "had a cast field"). Weights the silver log-LRs above, NOT refit by
  `scripts/identity-refit`: its units come from the captures, which hold casts for <7 % of candidates (0 in DE/ES/US),
  so a relearn has nothing to fit. Memory: a `FilmCast` per candidate record (~10 int arrays, ~300 B), nothing per listing.
- `identity-measure-local` (model stage; final stage unmoved, DE and PL unmoved): gained right 6 (UK 5, ES 1), plus
  unjudged ES "La maleta" ×23 (14 off Objetos 2022 onto Therón's 2026 film — reads as a cure) and UK Crescendo
  (L'Objet du délit), Stormy Waters (Remorques 1941), Out of the Woods ×2; LOST right 22 (UK 21: NT-style "Fallen Angels
  by Noël Coward" ×13, NAZA + Q&A ×8 via family moves; US Reel Rock 19 by `cast=none`), unjudged lost 25 (US Met
  "Tosca (2026 Encore)" ×17 by the 2018 record's cast); RIGHT→WRONG 3 (Fallen Angels → Lukas Dhont's "Coward" 2026).
- **Failure modes seen:** stage relays and concerts whose record lists another season's or no cast; a dub's voice cast
  (Spirited Away, Cars) naming none; pooled/family names spreading a rival's two+ across a cluster.

## B: relax `CastEvidence` (unknown-cast candidates left out) — moves nothing
Every country and stage 0 lost / 0 gained: the captures the final stage replays know almost no casts.

- **Next angle:** backfill `credits.cast` into prod's stored records and the captures first (coverage, not the rule, is
  the limit); then try cast only as a POSITIVE tie-break among same-title rivals with no director billed, never `none`
  as a penalty, and keep it out of pooled/family names.
