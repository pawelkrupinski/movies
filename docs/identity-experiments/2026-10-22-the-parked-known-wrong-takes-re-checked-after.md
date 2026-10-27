# 2026-10-22 — the parked known-wrong takes re-checked after WS2 and WS4 (base `873b00e50`, pinned 573 and 528, all five countries)

Main's own takes read off the takes store (pinned 573, code identical to `873b00e50`: `takes-decisions-<cc>` of run 1207
and `takes-final-<cc>` keyed by main's capture), then one fix measured (CI runs 1261 pinned, 1263 recording 528).

- **DE Kino Central "Pornfilmfestival: The Dating Game" → 835113 Woman of the Hour (2024): CURED, LANDED.** Still served
  (PooledMatch 50.7%, unrivalled-calibrated). Cause: the bare billing names 835113 (TMDB's German alternative title "The
  Dating Game") and Du Feng's 2025 documentary 1400769 alike, no fact tells them apart, and the pooled rule leaves
  namesakes the facts fit alike to the ranking's popularity. Rule (`ListingShape.festivalsOlderNamesake`, read in
  `unrivalledCalibratedWhy`): a bare billing under a film festival's banner takes no namesake when one its title names
  alike is newer — the catalogue rule's reasoning (a festival bills its year's entries) below its ten years. Survey of
  every pooled festival-banner take on main: 4 (Only Good Things ×2, Uchronia, Dating Game); only Dating Game has a
  newer alike namesake. Measured: 573 (run 1261) and 528 (run 1263), all five countries: each the one cure, nothing else moved in any country or stage (LANDABLE).
- **DE Augustinum Diessen "Island" → 1192466 (3-min 2023 short): still served (OwnMatch 93.1%), PARKED.** kinoheld's page
  `island-2023` copies the short's year, runtime and director, and its title IS the billing, so neither the
  copied-title veto (which cures today's Zum Dorfkrug and Heilige Cnaipe: Vetoed on main) nor `entryGuessed` (no year,
  no runtime) reaches it. The listing most likely bills Stefan Erdmann's touring Iceland show ("Island" is German for
  Iceland; ~25 DE venues bill "ISLAND 63° 66° N"), held apart from it by the copied facts. Every generic form tried
  (refusing copied-fact shorts: 57 right lost; copied-fact weight share; page-copied-no-support) cost right takes. Still
  needs the slot length (a 3′ short cannot fill an evening slot), which no listing field carries.
- **DE CITY 46 "THE BLACK HOLE" → 9570 (1979): still served (OwnMatch via City-Filmtheater's own match), PARKED.**
  Recording 573 now holds Siimets' film (931219 "Must auk", among CITY 46's candidates at 5.7%, no search rank), but the join is the
  "other node already holds another film" half of `copiedOriginalJoinsAnotherFilm`, which lost 5 right takes
  (Traum-Kino "TOGETHER APART", Andreasstadel "Departures": operators linking a namesake record). Nothing tells CITY
  46's right link from those wrong ones: no festival banner in the billing, no fact of its own.
- **UK Beehive Honiton "Into The Shadows" → 190762 (2009): model none (BelowThreshold 3.3%), still taken by the
  agreement (wiki, rt lean) in the capture's final takes, PARKED.** Looked at: Agreed takes over a bare billing whose
  TMDB search holds two or more exact namesakes — 22 DE + 1 UK right, all the newest of their namesakes, Into The Shadows
  the only one an older one (2023 and 2025 namesakes exist). That split is fitted to this one row (the 10-18 synopsis
  entry's warning), so not built; an event marker reaching the agreement is still the angle.
- **UK Magic Lantern Tywyn / Riverside Woodbridge "Notre-Dame de Paris" → 11594 (1956): not served on the recordings
  (Vetoed on 573 at main), PARKED as prod-only.** Prod's incremental split; no gate reaches it. Scope stays `served,rows`
  until prod is seen taking none (the rows predate the fix).
