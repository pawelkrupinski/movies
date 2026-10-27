# 2026-10-21 — a page feed's copied facts read as no support for the film it copied them from (fundamentals proposal 4.4, "Island") — NOT LANDED (local tag `experiment/page-copied-no-support` in ~/projects/movies, recordings 528, 38713274617 and 573, base `96fb23c6c`)

- **Target:** the wrong-served listings whose only facts are a feed's copy of the found film (proposal
  `docs/proposals/2026-10-21-matching-fundamentals.md` §4.4): DE Augustinum's "Island" (kinoheld's copied director,
  runtime and year of a 3-minute 2023 short), Regio Tales, The Killer, Black Hole.
- **Rule:** `SourceReliability.copiedByPage`: a year, director, runtime or original title held only as a copy from a feed
  that matched the billing to its entry by PAGE (kinoheld.de, kinoprogramm.com: no id another database maps). In
  `FamilyScope`'s scoring such a fact's measure reads `Missing("listing")` wherever its calibrated weight is positive (its
  support for the film it was copied from), and stays in full where it dissents. Narrower than the 10-15 copied-share
  variants: share 0, page feeds only (Cineamo, Webedia and Flicks untouched, so Fallen Angels and Volcanoes 3D could not
  move), and every rule reads the neutralised measures, not the probability alone.
- **Measured (identity-measure-local):**

  | recording | cc | lost | cure | right lost | right→wrong | new wrong | gained right | unjudged moves |
  |---|---|---|---|---|---|---|---|---|
  | 528 | de | 116 | 1 (Island) | 5 (De Gaulle ×3 + 2) | 1 (ISLAND 63° 66° N → the 2016 namesake) | 2 (Helmut Schleich, VOODOO LOUNGE) | 1 (Nuisance Bear) | 111 |
  | 38713274617 | de | 121 | 1 (Island) | 3 (Ich bin Dein, Kohlhiesels Töchter, The Enforcer) | 1 (Alpha (dir. Ducournau) → Rawail's 2026) | 2 (Helmut Schleich, Martin Tingvall) | 1 | 118 |
  | 573 | pl, uk, us | 0 | 0 | 0 | 0 | 0 | 0 | 0 |

  PL, UK, US and ES moved nothing (no page feed there). The ~110 unjudged DE losses per recording are mostly plainly right
  takes whose only facts were kinoheld's copy: Traumpalast "David" (2025) ×5, Bambi "Cloud", "Beyond", "Bon Voyage...",
  "Biedermann und die Brandstifter", the BTS live viewing.
- **Why stopped:** far past the bar. kinoheld copies the RIGHT film's facts far more often than a wrong one's (the
  proposal's own risk note), so reading the copy as no fact turns ~110 right takes into factless billings the rules no
  longer take, for one cure. The right→wrong and new wrongs are where a copied fact's dissent stayed in full while its
  support went: the namesake it dissented from won. Island, the one cure, has no rival at all, so no narrower form keyed
  on a competing namesake reaches it.
- **Next angle:** a copy is evidence that the FEED matched the billing to that film, a claim as good as the feed's
  matching. Weigh it by how often the feed's matches are right (per feed, from the judgements), not as zero; or refuse only
  where the copy names a short or a record far from a feature slot (the 10-15 "short whose every fact is a copy" entry's
  next angle). The catalogue-id half of §4.4 landed as a finding instead (`CatalogueSplits`, 3718a67d4): one id taken as two
  films goes to the Identity findings report rather than denying either side.
