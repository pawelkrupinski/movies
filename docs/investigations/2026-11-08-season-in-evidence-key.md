# 2026-11-08: the screening season in the evidence key

**The gap (review of a997b41d01):** the resolver groups listings into one evidence node by `Evidence.key`
(`CandidateGeneration.nodeKey`; the incremental `LiveCorpus` keys by the same string), and searches and scores the node as
its head listing's evidence. `StageRelays.closedSeason` (a stated year closing the season its screenings fall in also
searches the season's opening year) reads `Listing.screeningsSeason`, which the key carried only where it bounds what the
title names. So UK Riverside's "Romeo and Juliet (2027)" screening on 25 May 2027 (season 2026/27: searches
"Romeo and Juliet 2026", the RBO 2026/27 relay) and a billing alike screening in October 2027 (season 2027/28) were ONE node,
searched as whichever listing headed it: the first could lose its relay search (the original flip), the second be given the
other season's (a wrong-match risk). `RelayStage.of` (a stated year before the season, an encore's season, the air-day
test) reads the same season outside the key.

## The fix

`Evidence.key` carries `screenings:<season>` wherever the season rules read it: the billing names a stage house or a stage
work (`StageRelays.readsItsScreeningsSeason`), unless `broadcast:`/`screened:` already key that very season (those
listings keep their keys). Every other listing keeps its key, so a film's run past July re-resolves nothing.
`SeasonInEvidenceKeySpec`: two such listings are two nodes, each searched with its own season, in either order; two film
listings across seasons stay one node.

**Still read outside the key (named, not changed):** `ReleaseVeto.ofCurrent` (a namesake released in the season or the year
after) and `ListingShape.creditDenies` (a credited namesake two seasons older) read the screenings' season for every
listing. Keying every listing by its season would split every film's node across the July rollover and re-resolve the
whole corpus each year, for rules whose window (a release year in season..season+1, a gap of two seasons) one season's
shift rarely moves.

MODEL VERSION BUMP: the identity-rules digest moves (resolver source changed); stage-work listings' keys change.

## Cost

Evidence nodes and searches over the prod mirror (snapshot 2026-11-08, detail pages left out), base key -> head key:

| cc | listings | keyed by season | nodes | distinct searches | searches over nodes |
|---|---|---|---|---|---|
| pl | 11113 | 243 | 3280 -> 3280 | 12457 -> 12457 | 19602 -> 19602 |
| de | 32507 | 2202 | 7617 -> 7623 (+6) | 35675 -> 35675 | 52059 -> 52257 |
| uk | 32956 | 4018 | 4978 -> 4979 (+1) | 17132 -> 17132 | 29990 -> 30003 |
| es | 6033 | 137 | 490 -> 490 | 2330 -> 2330 | 2953 -> 2953 |
| us | 89309 | 5446 | 2288 -> 2288 | 9311 -> 9311 | 11409 -> 11409 |

The seven splits are season-named RBO billings with screenings in both 2026/27 and 2027/28 (DE "RBO 2026/27: Romeo und
Julia" x6 shapes, UK "RBO Cinema Season 2026-27: Manon"). No new distinct search: TMDB is asked nothing more, and the
searches over nodes, answered from the resolve's own cache, grow 0.4% in DE and less elsewhere.

## Measured

`scripts/identity-what-if` (prod mirror, synced 2026-11-06, base 3c4f6b0731): every family re-resolved (the key reaches
all); pl, de, uk, es, us model and final: 0 takes moved.
