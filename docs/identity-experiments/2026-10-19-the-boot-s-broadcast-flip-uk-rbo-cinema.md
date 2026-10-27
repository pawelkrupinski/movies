# 2026-10-19 — the boot's broadcast flip (UK "RBO Cinema Season 2026-27: Macbeth"), base `1a2e33add`

## Never wait on a year-only record no screening day's relay can be — parked
- **Seen:** the first projection after deploy 8b75b70 (2026-10-15 23:49) lost the Met's Macbeth (tmdb:1703622, judged
  right) at 90 UK venues, basis Vetoed; the next projection 40 s later took it back as Broadcast. Mechanism (read from the
  code): `AgreementStage.rereads` is the process's, so at every boot `undatedOf` makes `Broadcast.take` wait (`Left`) on
  each candidate record whose stored `local.release_date` is a bare year, until the re-read is filed
  (`noteFiled(recordReadId)`); the re-read often files no day (mirror: 29,430 year-only records, 10,568 re-fetched and
  still year-only, e.g. 1702769 local "2027", english "2027-02-03"), so the wait recurs at every boot.
- **Tried:** skip the wait for a record whose year no screening day's `RelayRunDays` reaches back to.
- **Result:** the pinned case (a 2018 "Royal Opera House: Macbeth" year-only) passed before the change too: that record
  does not fit the credited billing, so it never held the take. Which year-only record held prod's take is not known:
  the recording's model takes the Met's record itself (OwnMatch), so the slice cannot replay prod's Vetoed + Broadcast.
- **Next angle:** find why `readRecordAgain` re-files the localized partial without its day, or keep the re-read's
  outcome across boots (a `StoredVerdict`-like record per re-read film), or let a broadcast take stand as last decided while
  its re-read is outstanding (as `d006d2c3e` did for corrections). `IdentityBillingTakeOscillating` names it when it recurs.
- **Code:** `experiment/broadcast-airable-year`.
