# An event-cinema relay is another work than a film of its title (identity design B, 2026-11-01)

The owner, 2026-11-01: "Do B". It fixes the last convergence flip, UK seed 17 "Prima Facie".

## What was wrong

TMDB 963568 is "National Theatre Live: Prima Facie" (2022, the National Theatre's recording, Jodie Comer). TMDB 1127282 is
"Prima Facie" (2026, the feature film, Cynthia Erivo, dir. Susanna White). In the drift replay at UK seed 17:

1. Chichester's bare "Prima Facie" boots alone and takes 1127282 (right).
2. Power Station's "NT Live: Prima Facie" arrives. Both search as "Prima Facie" (the lookup query strips the banner),
   so a same-search-form must-link groups them, and the group takes 963568: Chichester WRONG.
3. Kendal's and Bo'ness' bare billings arrive and the group goes back to 1127282: a flip.

Alone, "NT Live: Prima Facie" could also take 1127282 by its search (the synthetic property below shows it). The same
blind spot was in `ListingRetention`: it keeps a listing that left by its search title, banner stripped, so the NT Live
listing and the film shared a retention key.

## The design

A listing billing an event-cinema relay's banner and a listing of the same title billing none are two works: they never
share a take, whatever listings are present and in whatever order they came.

- **What bills a relay** (`services.movies.RelayBanner`): what the codebase already read as one. An opera or ballet house
  (`StageHouse.billed`: "Tosca (ROH)", "Bolshoi Ballet: ...") and the title rules whose `effect` is `relay`
  (title-rules.tsv: NT Live prefixed and suffixed, the Met, RBO, a house's "... 2026/27:" season banner, Exhibition On
  Screen, an "... encore", BTS's tour). Those rows still strip or rename the banner for the LOOKUP (TMDB is searched by
  the clean title and the broadcaster's proper name, as before); the banner is now also kept as an identity FACT, read
  off the listing's raw and clean title alone. A record bills one by any of its titles.
- **Grouping** (`ConstraintEdges`): a cannot-link `relay-apart` between a node billing a relay and one billing none.
- **Candidate scoring** (`CandidateScoring.evidenceDenial`): a relay's listing is denied a record billing no relay, and a
  plain listing a relay's record (`Denial.Kind.NotTheWork`, no credit overrules it).
- **Retention** (`ListingRetention.titleOf`): a relay's title is marked apart, so a relay's listing is kept only while a
  relay of its work is billed, and a film's only while the film is.

All three read the inputs only (no take, no arrival order): a whole resolve and the incremental model decide alike
(identity-no-stickiness).

## The property

`RelayApartSpec`: three works billed both ways (Prima Facie, Hamlet, Carmen; NT Live, NTL, National Theatre Live, RBO,
Royal Ballet and Opera banners), every subset of the listings holding a relay and a plain one, whole and incrementally in
every arrival order: no decision groups a relay with a plain listing, and no film is taken by both. Before: failed in
both forms (Prima Facie, Hamlet, Carmen, e.g. "NT Live: Prima Facie" + "Prima Facie" both on 1127282). After: passes;
each half (cannot-link, denial) alone fails it. `ListingRetentionSpec`: a relay never kept by a film's title or a film by
a relay's.

## The flip, as the corpus replays it

Base drift row uk seed 17 (run 41297259801, base 142c59804): Chichester "Prima Facie" none -> 1127282 (step 3, filled by
cast.venue: Cynthia Erivo) -> 963568 (step 4 scrape, OwnMatch of its node ALONE) -> 963568 -> 1127282 (step 9, joined
by Kendal). So the move to the recording is the bare listing's own match, not only the grouping.

## The slice

`scripts/identity-slice uk --recording 573 --venue "Chichester Cinema at New Park" --title "Prima Facie"
--emit-drift-slice` cuts closed (4 listings: Chichester, Power Station, Kendal, Bo'ness; `--widen 2` adds no context) but
replays CLEAN on the base code on seeds 1-64 and with a `seeds` file of 17: the corpus's step-4 own match depends on
corpus context the slice lacks. Not committed: it does not pin the red.

## The measure (full corpus, five countries, pinned pair 573, base 142c59804)

Two variants, both NOT LANDABLE:

| variant | run | model takes moved (pl / de / uk / us / es) | right-lost (model) | final right-lost |
|---|---|---|---|---|
| B by banner only (af8e5a09..ad869f705) | 41297551190 | 117 / 335 / 1244 / 1315 / 43 | 64 / 16 / 672 / 515 / 0 | 1 / 1 / 32 / 0 / 0 |
| B + broadcast-season, original title, names anywhere (2098fe845) | 41298198021 | 241 / 122 / 2116 / 294 / 43 | 64 / 7 / 692 / 1 / 0 | 0 / 0 / 15 / 0 / 0 |

Cures: 3 (UK "Così fan tutte" off the 2006 film). One new right->wrong (UK Pwllheli "Carrie (1976)" -> Obsession).
What the right-lost are, by kind:

- **Bare billings of a relay** (the design's own cost): PL Kino Nowe Horyzonty "Fleabag", Kinoteka "Mizantrop", UK
  Halstead "Carmen", Chichester "Golden Boy", PL "Dziewczyna z Dzikiego Zachodu" all took their relay's record, right; B
  forbids it, and the NT Live listings of the same work lose their pooled take with them (PL "NT Live: Fleabag" x16).
- **Relays whose TMDB record bills no banner**, or the reverse: Exhibition On Screen records filed under the painter's
  title (US "John Singer Sargent: Fashion and Swagger", DE Rembrandt), BTS's "LIVE VIEWING" billings vs "BTS WORLD TOUR"
  records (UK x many), Berliner Philharmoniker.
- **The broadcast-season widening**: any bare film title that is also a stage work, screening, read as a relay: PL
  "Orlando" (Handel), "Jestem Rocky" (the musical), "Martwe zło 2", "Kaczka po pekińsku" -- hundreds of right film takes.

Conclusion: a listing-kind split read off titles cannot hold "never share a take" without losing right takes in the
hundreds, because box offices bill relays bare and TMDB files relays under plain titles. The Prima Facie flip itself is
a plain listing's OWN match to a relay record whose facts its own (2026, Cynthia Erivo in the venue's text) contradict.
