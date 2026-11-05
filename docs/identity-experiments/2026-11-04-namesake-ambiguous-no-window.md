# 2026-11-04 — a namesake-ambiguous listing takes no film, no window — CLOSED 2026-11-05 (was: parked candidate) (base `385b1bdb4`, head `b030bb899e2`)

- **What:** a listing whose title names several TMDB works (namesakes) that its own facts do not tell apart is held
  apart from its family and takes no film (`NamesakeAmbiguity`, `ResolverRun.decideWithNamesakes`), unless a feed link
  or pin decides alone (`linkDecides`). Per-listing classification, so a node mixing venues does not borrow a sibling's
  director. Commits `76ebe25f0c3` (property), `49284ecba98` (rule), `b030bb899e2` (deciding link, 7 population-monotonicity
  known rows removed: Titans x6, Fishguard R&J).
- **Measured:** full measure 41795872892 (all countries) and drift 41795902042; local `NamesakeTakeSpec` and
  `PopulationMonotonicitySpec` walks over all slices.
- **Result:** right->wrong 0 in every country. Titans flips cured (seed 47 clean; Buddies at Lichtblick still flips, it
  is not a namesake case). Cures: model de Alpha, uk Cosi fan tutte x3; final de gained Der nackte Wahnsinn. Cost: served
  (final) right->none 48 (pl 11, de 30, uk 7), model right->none pl 149, de 255, uk 86, us 1. 947 namesake-ambiguous
  listings, 0 moved, 0 reclassed across arrival chains. NOT LANDABLE under the right-lost rule.
- **Why parked:** 48 served losses are mostly classics whose old film is the right answer next to a recent namesake
  (current-vs-old 9, old right film 13, another 2024+ namesake 20, unknown 6). The owner (2026-11-05) changed the approach
  to held takes plus own facts. This variant may still land later ON TOP of own facts, which would supply the evidence
  that lifts the held listings: parked as a candidate, not dropped.
- **Code:** local tag `experiment/namesake-ambiguous` (`b030bb899e2`); branch `identity/namesake-ambiguous` (kept, rebased
  as `1edd9a713847` on `40bacafcc`, with the two variants below on top). Identity unit tests were not updated for the
  design reversal (28 failures in 4 classes, e.g. `IdentityResolverCasesSpec`).
- **Re-checked 2026-11-05 (parked-items review) -- CLOSED:** the hope above was that own facts would lift the held
  listings. With Kinoheld's second-feed facts, Buddies (own facts beat `venues.current`), Unmatched C/D and StageRelays
  on main, b030bb899's three commits cherry-picked onto `a57790c72` and run through `scripts/identity-what-if`
  (snapshot 2026-11-05T19:00Z): DE 2100 takes moved, every one film -> none: cure 6, right-lost 110, unjudged 1984,
  right->wrong 0; 2097 move away from prod's served take (6.5 % of DE listings). The validation run on the old base
  (08:00Z) had DE right-lost 107: the new own facts recover nothing (Kinoheld separates only 3 of the 30 DE served losses).
  `scripts/flip-timeline` over the past 7 days of prod finds no reversal at all for the rule's flip targets (Titans at
  Dammtor / Regensburg / Raschplatz / Luxor, Casablanca Kamp-Lintfort, Fishguard and Halstead R&J, Lanternhouse
  Nosferatu) and only a DEPLOY/RESTART one for Torrington "Dracula": their flips live in the population walk and the
  drift replay, which the per-family convergence check (`FamilyConvergence`) now owns. A rule that unmatches ~2100 DE
  takes to cure 6 is not the fix; the class goes to the held-take line (owner approach 2026-11-05 #1). The window,
  rework, monotone-rules and prominence variants below stop with it.
