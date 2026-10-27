# 2026-10-20 — an old Cineamo premiere kept as the year's UPPER BOUND ("made no later than") — LANDED (recording 528 with the bound laid from the live Cineamo API as `528ub`, base `6aa9c99b2`)

- **Rule:** `CineamoClient` keeps a premiere it no longer reads as the year (f5784855d) as `Movie.releaseYearUpperBound`;
  `IdentityMeasures.yearBound` reads it as the listing's published year against a film whose closest release is more than
  `PublishedAdjacency` after it, and only there (`year.delta` -2: -1.1, -3 and less: -6.9, the learned cannot-links as for
  a stated year). A film of the bound's year or before stays as a yearless listing weighs it: no equality reward.
- **Data:** recordings hold the parsed scrape, not Cineamo's payload, so 528 cannot derive it. `528ub` is 528 with the
  bound on 994 of the yearless DE Cineamo listings, from the premiereDate the live API gave (662 venues, ~14 h after the
  02:03 scrape). A new recording carries it once the scraper is deployed.
- **Measured:** 528ub DE+PL: 2 moves — King Kong (Eulenspiegel) 254 → none (cure; 244 leans at 7.9%, below the cut),
  Drei Männer im Schnee (Cineplex Bayreuth) none → 54769 (gained right); 0 new-wrong, 0 right→wrong, 0 right-lost.
  38713274617 all five countries: 0 moves. Rope, Vertigo and Momo (a wrong Cineamo link, premiere 1986, the 2025 film
  taken) do not move.
- **Not reached:** LOTR Extended (bounds 2001–2003 do not exclude Bakshi's 1978 film; 501 listings, 0 moved) — the
  numbered-title denial's case. Kinoprogramm's "Kinostart" chip and Filmstarts' THEATER `releases[].releaseDate` are
  the same kind of fact, unparsed today; not added.
