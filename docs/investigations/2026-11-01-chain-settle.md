# Do one chain's listings of a film settle together? (2026-11-01)

The owner asked whether one chain's venues (Multikino, Cineworld, Cineplex…) billing the same film always end on
the same take, and whether settling a chain's group together would improve anything. Measured; nothing landed.

## Method

- Whole-corpus takes of green convergence run 41296844565 (main, 2026-11-01).
- Listings grouped by chain + normalized billed title. The recording carries no chain film id, so the title was the
  only key.
- Every split group re-run through `scripts/identity-slice` on recording 573, to read the FINAL take. The takes
  artifact's `decisions-<cc>.jsonl` is the model stage only; production's agreement stage then fills unresolved
  bare listings.

## Counts (groups shown at 2+ venues)

Cinesa, Kinepolis and Cinépolis were not matched in ES.

| Country | Chains | Groups | Same take | No take anywhere* | Split |
|---|---|---|---|---|---|
| PL | Multikino, Cinema City, Helios | 278 | 250 | 28 | 0 |
| UK | Cineworld, Vue, Odeon, Everyman, Showcase, Picturehouse, Curzon, Omniplex, Savoy | 790 | 763 | 26 | 2 |
| DE | Cineplex, CineStar, CinemaxX, UCI, Filmpalast, Kinopolis, Traumpalast, CineMotion, Cineworld | 780 | 737 | 29 | 5 |
| US | Regal, AMC, Cinemark, Marcus, Harkins, Alamo, Santikos, Emagine, Cinépolis, Landmark | 897 | 884 | 13 | 0 |
| ES | Ocine, Yelmo | 89 | 85 | 3 | 1 |

\* Events, marathons, relays and secret screenings: consistently no film, not a split.

## The split cases, by final take

- **Savoy "Hocus Pocus":** Doncaster is right (10439, Agreed). Model-stage only.
- **Savoy "Resident Evil":** Corby and Grantham are right (1423191, Filled from the venues' current listings).
  Model-stage only.
- **CinemaxX "Titans":** really split. Dammtor, Raschplatz and Regensburg are bare (no runtime or director) and
  end with no take at 30%; Essen takes 869152 (2026, Prince Joy). 3 listings, unverified against TMDB, left as they
  are.
- **"Festtagsspaß mit Peppa"** (Cineplex/CinemaxX/UCI): two real TMDB records, not a duplicate. 1650896 "Peppa
  Pig: Perfect Holiday" carries the German title; 1778515 "Festive Fun with Peppa" is 60 min, released
  2026-11-14. Both have 0 votes. Most venues take 1650896 and 6 take 1778515. Which is right needs the venues'
  runtime and German release date; a group pin would then settle it.
- **Correct splits:** a chain + title key would wrongly merge these.
  - Yelmo "La invitación": 830788 (2022) vs 950028 (2026).
  - Yelmo "Sentido y sensibilidad": 2026 vs the 1995 re-release.
  - Cinépolis "Halloween" and "Nightmare on Elm Street": different years.
  - Cinema City "Lalka" vs "Lalka (ale to horror)".

## The prototype

The prototype exempted the cannot-links between one chain's listings of the same cleaned title, unless a stated
year or director differs. It was never pushed and has been deleted.

- **Full-corpus takes** (run 41309369705, five countries): 0 lost, 0 changed, 0 gained.
- **Drift** (run 41309516606, seeds 37/38, five countries): unchanged.
- **Split groups unified:** 0.

The cannot-links are not what splits these groups. Model-stage splits come from a thin listing left alone as
ambiguous, and the agreement stage already resolves them.

## Conclusion

Chain listings already settle together in practice: about 2 real splits in roughly 2,800 groups, both explained
above. No chain-specific mechanism is worth adding. Measure chain or venue splits on final takes (after agreement),
never on the model-stage decisions alone.
