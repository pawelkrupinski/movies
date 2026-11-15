# 2026-11-15 — a relay billing a stale season takes the relay its screenings air on its day — PARKED (tag `experiment/relay-stale-season-airday`, head `c5e911a81`)

- **Why:** the 2026-11-15 census's "relay" bucket (prod mirror, all five countries). Most of it is the Paris Opera's
  2026/27 relays (US "Opéra National de Paris: La fanciulla del West" ×186, UK/DE/PL/ES Bayadère, Notre-Dame, Romeo,
  Swan Lake): TMDB holds no record of them, so NO take is right; the same-relay link (2a3a2ddbe) already makes each one
  film. The one big group with a record is UK Flicks' "Royal Ballet & Opera 2024-25: Romeo and Juliet" ×16 (+1 US): a
  Flicks page kept from 2024/25 (dated 2025, 210′, credits MacMillan) whose screenings are 25 May 2027 — the day TMDB's
  "Royal Ballet & Opera 2026/27: Romeo and Juliet" (1702789) airs, which 300+ UK venues bill — and its 30 May encore.
  Its season vetoes 1702789 (`SeasonsApart`), and the 2024/25 record (1451897) is never searched.
- **What:** `Listing.airDays` also for a title naming a season more than `SeasonEncoreLag` (1) before its screenings'
  season (`namesAStaleSeason`, kept past a detail-page year, as old as the title); `CandidateScoring.evidenceDenial`
  lifts `SeasonsApart` for `RelayStage.airsItsRelay` (its house's record of its work, its screenings' season, released
  within `AirDayWindow` of a screening day); `relayRecord`'s published-year check skips such an aired record. Test:
  `OneRelaySpec` "A relay billing an older season…" (air day → 1702789; another day → never 1702789; a 2025-26 season
  and a work-less season get no air days).
- **What-if** (prod mirror 2026-11-15T05:13Z, base b50bcb58c, all five countries): UK model and final 12 moved, all the
  air-day listings, none → 1702789; US, DE, PL, ES 0 moved. The 4 encore-only listings (30 May, 5 days off) stay
  unmatched.
- **Why parked:** `identity-judgements.tsv` holds `uk * "Royal Ballet & Opera 2024-25: Romeo and Juliet" tmdb:1702789
  wrong` (review page, 2026-10-08), so the 12 count as new-wrong. The screening days say it is the 2026/27 relay; the
  call is the owner's, filed as a review candidate (`identity-review-candidates.tsv`). Resume, untouched, if the owner
  re-judges it right.
