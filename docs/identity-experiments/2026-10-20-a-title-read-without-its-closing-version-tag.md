# 2026-10-20 — a title read without its closing VERSION tag ("– Extended", "(ext.)", "Director's Cut") — LANDED (branch `version-tags`, recordings 528 and 38713274617, base `083645525`)

- **Target:** ~290 DE LOTR Extended listings with no film in prod (worker-de at 083645525): CineStar "(ext.)"'s original
  title "The Lord of the Rings: The Return of the King – Extended" measured `originalTitle=segment` (-2.52) against
  Jackson's film, the learned listing-film cut denied it once f5784855d dropped the year, and its `denies-film` edges
  vetoed Traumpalast, Capitol, Lichtburg, Kinorama, Lohne, Kinopolis and Cineplex's Extended billings with it. The
  user's reviews (`identity-judgements.tsv`, `review_local`) judge every single-film Extended / Director's Cut / 4K
  billing RIGHT on the base film's record; only multi-film bills are "matches no film".
- **Rule:** title-words.tsv `version-tag` phrases (en/de/pl/es); `VersionTags.versionless` drops a WHOLE closing tag (a
  bracket, a delimited piece or the last words, format words beside it; never leaving only an article: "Pink Floyd: The
  Final Cut" stays). `Listing.ownKeys` and `OriginalForm` read the versionless spelling too, so it is `exact` / `match`.
  Feature extraction only: no weight changed (the weekly refit re-tunes them). The veto itself is untouched.
- **Measured** (`scripts/identity-measure-local --base 083645525`): 528 DE model 80 gained (77 judged right, 3 Cinema
  München "(Ext.Vers.) Intermission" judged right), every one a LOTR Extended billing → 120/121/122, incl. Kinopolis'
  "Der Herr der Ringe 2/3 (Extended Version)" ×12; PL 0. 38713274617 all five: UK ICA "Manhunter (4K Restoration)" →
  11454 gained right, nothing else. 0 new-wrong, 0 right→wrong, 0 right-lost. Also cures the pending r5 known regression
  (the 83-minute "Your Name (re-release)").
- **Not reached:** "Herr der Ringe-Extended Edition" ×2 (no instalment named; correctly none) and the trilogy /
  marathon bills (correctly none).
