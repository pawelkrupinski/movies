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

(filled in below from the what-if and the measure)

## Measured
