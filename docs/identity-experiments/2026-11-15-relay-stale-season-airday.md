# 2026-11-15 — a relay billing a stale season takes the relay its screenings air on its day — LANDED (2026-11-15; parked as tag `experiment/relay-stale-season-airday`, `c5e911a81`, now deleted)

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
- **Why parked:** `identity-judgements.tsv` held `uk * "Royal Ballet & Opera 2024-25: Romeo and Juliet" tmdb:1702789
  wrong` (review page, 2026-10-08), so the 12 counted as new-wrong. Filed as a review candidate for the owner.
- **Landed (2026-11-15):** the owner re-judged that row RIGHT: the billing's "2024-25" is a stale Flicks page; the screening
  day decides the relay record, and every screening is on 1702789's air day or its encore. The row was edited in place,
  the candidate removed, and the commit rebased onto main (clean over 4d7de313a / 1bbd187c6, today's RelayStage changes).
  What-if (prod mirror snapshots 2026-11-15T20:46-20:52Z, base d7bad6100, all five countries; see the unmatched notebook §17):
  UK model and final +12 gained-right; US, DE, PL, ES 0 moved; CLEAN. `scripts/fail-before` on OneRelaySpec: yes/yes.
- **Left as is:** the 4 encore-only listings (30 May, 5 days after the air day) stay unmatched. TMDB records one release
  day and no encore; `AirDayWindow` (3 days: a venue's own date, a time zone apart) is not widened, since an encore on no
  air day is the case `Listing.airDays` deliberately leaves to its stated year. UK "The Royal Ballet Romeo and Juliet
  (Filmed live 2025)" ×2 does not move either (its "Filmed live" still reads as `namesAnotherWork`; §15).
