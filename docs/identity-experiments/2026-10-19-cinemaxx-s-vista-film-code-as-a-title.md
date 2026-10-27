# 2026-10-19 — CinemaxX's Vista film code as a title-gated catalogue id (recording 38713274617, base `82de60cf0`)

- **Tried:** the next angle of the `experiment/booking-ids` entry. Read the one `HO\d{8}` code off a listing's
  cinemaxx.de booking URLs (`CatalogueSources.CinemaxX`). Only at CinemaxX's own venues ("CinemaxX …"), never off a
  `MultiFilmBill` marker or "+" bill. Added as `CatalogueId("cinemaxx", code)` and title-gated as a feed's id
  (`CatalogueSources.TitleGated` → `ConstraintEdges.catalogueKey`; kept out of `TitleLinks`' block keys).
- **Coverage:** 1,333 listings at 29 CinemaxX-owned venues. 1,332 carry exactly one code, 87 codes in all.
- **The narrowing's premise is false:** CinemaxX's own venues reuse a code for another billing too. Examples:
  "Edmund und Luzie" / "Pettersson und Findus", "Lord Of The Rings Triple" / "Die Gefährten", "Panem-Mockingjay 1+2" /
  "Mockingjay Teil 1" (the bill guards miss "1+2"), and the Busan listings under the Buenos Aires code. HO00001648
  names BOTH Ang Lee's "Sinn und Sinnlichkeit" (1995, tmdb 4584, 7 venues) and Oakley's 2026 film (tmdb 1503762,
  CinemaxX Essen, billed 2026 / Georgia Oakley: its take is RIGHT). Only the title gate keeps these apart.
- **Result (`identity-measure-local`, DE):** model stage 33,703 listings: lost 0, changed 0, gained 0. Final stage:
  0 / 0 / 0. 0 new wrong, 0 unjudged. `identity-slice` confirms the head reads `ids cinemaxx:HO00001648` on Essen, and it
  stays apart from the 1995 film (year delta, denies-film). Moves nothing.
- **Why stopped:** payoff 0. Beside an agreeing title the title's own must-link already joins these listings, so the
  code adds nothing; without the gate it would join different films. Landing would also need the scraper to capture
  the code into `externalIds`: prod's lean listing read (`LeanListing.leanFilm`) drops showtimes, so prod never sees
  the booking URLs. The recording's full corpus does.
- **Code:** `experiment/cinemaxx-code` (code, plus `CinemaxXCodeSpec`).
