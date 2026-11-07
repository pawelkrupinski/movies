# 2026-11-07: a relay billed bare or dated by its season's closing year

**The flip class (backlog, kind `population`, UK, slice `identity-slices/uk-empire-theatre-halstead-romeo-and-juliet-06f0c8a2`):**
the RBO 2026/27 relay of "Romeo and Juliet" (tmdb:1702789, aired 25 May 2027) in its 375-listing family:

- Riverside Theatre Woodbridge, "Romeo and Juliet (2027)": final take none -> 1702789 -> none [chain 5];
- Theatr Gwaun Fishguard, bare "Romeo and Juliet": model take 6003 -> none -> 6003 [chain 3], and 6003 -> 91745 -> 6003
  on another chain (shown once the first was cured).

## The mechanism

**Riverside: a search the year hid.** A relay whose title states a year searches its work beside that year
(`StageRelays.stageWorkQueries`). TMDB titles the season's record by the year the season OPENS ("Royal Ballet & Opera
2026/27: Romeo and Juliet"): "Romeo and Juliet 2026" finds it, "Romeo and Juliet 2027" finds nothing. A stated year also
keeps the screened season out of the searches (`Listing.screenedSeason` is only read for a title stating no year). So
Riverside alone never reached 1702789, and the broadcast join (`agreement.Broadcast`, over the films the cluster's own
evidence reaches) could take it only while Riverside was joined to Empire Theatre Halstead's bare billing, whose
season search did reach it. The cannot-link a mislinked "Romeo and Juliet (2013)" sibling (Curzon Clevedon, Flicks' page
of Carlei's film on the relay's own booking URL) draws kept the two apart in the whole family: none -> film -> none.

**Gwaun: a namesake rule blind to the relay.** Gwaun bills the work bare (no year, no credit, no running time) and screens
on the broadcast day. Its own pool holds 6003 (Zeffirelli, 1968) at 42.7% and the relay at 2.6%. Alone, the pooled
unrivalled rule took 6003. When the solver joined it to Clevedon's mislinked billing (some populations only), the cluster
took 91745. When it was joined to Riverside, whose 2027 denies 6003, it took nothing. The agreement stage corrected the
6003 take to the relay by the day, but not the 91745 take, and 91745 was served. `Corroboration.refused`
(an uncorroborated take loses to a namesake the family takes on its own facts) did not count the relay as a namesake.
Its title "Royal Ballet & Opera 2026/27: Romeo and Juliet" names the bare billing only by a piece, so it fell below
`naming`'s tier of the films named whole.

## The fix (structural fix 2: each rule a function of the listing's own facts)

1. `StageRelays.closedSeason`: a stated year that CLOSES the season the screenings fall in also searches that season's
   opening year, as the season's record spells it. It depends only on the listing's year and days.
2. `StageRelays.bareWorksSeasonRecord` and `Corroboration.relayedBare`: for a listing whose whole title is a stage work
   billed bare, the house's record of that work, of the season its screenings fall in, is a namesake as close as any
   film of the title. An uncorroborated take of another film loses to it when the family takes the relay on its own
   facts (the existing `support` rule). Gwaun then takes no film in the model in every population, and 1702789 by
   broadcast in the final take.

The walk (`PopulationMonotonicitySpec`) over the slice: 0 flips; across all 8 chains, Gwaun, Riverside and Halstead take
none in the model and 1702789 in the final take at every step.

## Measured

`scripts/identity-what-if` (prod mirror 2026-11-07, base 362196b15a): UK model, cure 4 ("Cosi Fan Tutte" x3 at the same
bare-billing venues dropped Heinz's tmdb:299734, the other cure Gwaun); UK final, gained-right 1 (Riverside); 0
right-lost, 0 right->wrong, 0 new-wrong. PL: 39 gained, all Helios' "<work> – Sezon Royal Ballet & Royal Opera House"
[2027] (Così fan tutte, Swan Lake, Tosca, Götterdämmerung) on the RBO 2026/27 records, each screening on its record's
broadcast day. That is rule 1, judged right. DE, ES, US: 0 moved.
