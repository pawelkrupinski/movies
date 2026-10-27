# 2026-10-20 — a Cineamo-linked listing dated by its linked TMDB record's year — NOT LANDABLE (local tag `experiment/cineamo-linked-year` in ~/projects/movies, Forgejo recording 528, base `0d2a69060`)

- **Target:** the DE losses of `docs/investigations/2026-10-20-de-release-match-losses.md`: since f5784855d a Cineamo
  listing of an older film states no year (999 DE listings on 528, 948 carrying a `cineamoMovieId`). Eulenspiegel's
  "King Kong" (244m) took the 2005 remake.
- **Rule:** `Evidence.derived` gave a listing whose own field, detail page and title date nothing the year of the TMDB
  record its `cineamoMovieId` names, as the feed's Support-trust year (set aside with the link when `FeedLink` overrules
  it). Resolver-side so a recording replays it; `LiveCorpus.relinked` re-derived a listing when its linked record was
  filed anew.
- **Measured:** `scripts/identity-measure-local --base 0d2a69060 --head 681016b72 --recording 528` (DE and PL; Cineamo is
  DE-only, PL moved nothing). DE model: 90 moved — 0 cures, 9 gained right, 33 gained unjudged (King Kong → 244 among
  the changed), **8 new-wrong, 10 right→wrong, 3 right-lost**, final 1 right-lost.
- **Why it stopped:** the record's year restates the venue's pick, so it agrees with the linked film by construction
  (`FeedLink`'s own warning) and turns a wrong link into a verdict by `year.delta=0 (+3.40)`: Niagara → the 2022 film
  (rank 6), RENT → 2020 (rank 15), Bonhoeffer → 2003, Zeugin der Anklage → 1982, In the Mood for Love → 2015, American
  Werewolf → 2024 — each judged WRONG. The German premiere year it replaced was Cineamo's own entry, not the link.
- **And it cannot reach LOTR:** the regressed "Der Herr der Ringe 2/3 (Extended Version)" (Kinopolis), Capitol, Lohne,
  Kinorama, Lichtburg, Cineplex and CineStar extended billings carry NO `cineamoMovieId` (Cineamo's extended entries name
  no TMDB film), so no link-based year dates them.
- **Next angles:** the premiere as an upper bound ("made no later than", the investigation's proposal; needs the
  premiere scraped as a fact and a new recording); the linked year only where something the feed did not copy confirms
  the link; the numbered-title rule ("Herr der Ringe 2" whose original names a subtitle no title of the candidate holds,
  as `titlesAnotherPart`) against Bakshi's tmdb:123.
