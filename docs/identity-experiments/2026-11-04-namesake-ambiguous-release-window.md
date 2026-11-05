# 2026-11-04 — namesake plausible only inside a TMDB release window — PARKED, Titans flips return (head `82d9dcb5faa`, base `40bacafcc`)

- **What:** on top of the no-window rule (see `2026-11-04-namesake-ambiguous-no-window.md`), a namesake counts as a
  rival only when TMDB dates it (film year or own-country cinema release) within a year of the listing's own screenings;
  unknown dates are never ruled out. A window of N=1 year was picked from the age distribution of the 491 model losses.
- **Measured:** measure 41835145766 (all countries), drift 41835183288 (default seeds 57/58 only), local walks.
- **Result:** right->wrong 0. Served right->none 48 -> 12 (pl 5 Lalka/Dolly, de 4 A Separation, Bad Apples FK x2, Luger,
  uk 3 Birthday Party x2, The Store); model pl 137, de 126, uk 34, us 0. The prior cures (Cosi x3) and the de gains went,
  new cure Rose (Metropol Duesseldorf), one new pl unjudged (Kino Eva Lalka). The population walk showed 14 flips against
  4: the Titans flips (CinemaxX Dammtor, Raschplatz, Luxor Heidelberg: none -> film -> none) and Halstead/Fishguard R&J came back.
- **Why parked:** with the second Titans namesake out of the window the listing has ONE plausible work, so it leaves the
  namesake rule and the family's population-dependent rules (feed veto, a sibling's director, copied-original-apart)
  decide again. Drift CI was clean only because it never replays seed 47. Seeds 47/64 de also ran 75 min and timed out
  (run 41898541448; "takes recorded" stalls, cause not established).
- **Code:** local tag `experiment/namesake-ambiguous-window` (`82d9dcb5faa`). Next step taken: the rework.
