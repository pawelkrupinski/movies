# 2026-10-18 — facts in a venue page's prose (recording 38713274617, base `807e7f287`)

## Read a page's prose as facts at intake (Binti, Into The Shadows, Unrest)
- **Tried:** the TitleFacts angle on the synopsis: what a venue's description states in a structured-looking way
  ("Running Time: 2 Hours (Including Interval)", a director credited) read into the listing's own fields. Checked
  against the three known-wrong rows with `scripts/venue-replay` (the recording holds each synopsis) and the TMDB records.
- **Result:** no intake fact separates any of them. PL Janosik "edukacja MH: Binti": its bilety24 synopsis states no
  year, runtime, credit or two-word name — only plot ("Belgii", "Konga", "Eliasa"). UK Beehive "Into The Shadows":
  "2 Hours (Including Interval)" holds the 2009 documentary's 90 minutes (a 120′ slot with an interval), and a longer
  venue time vetoes nothing (`Agreement.runsShort`) by design. An interval as an event marker was scanned, not built:
  152 UK synopses bill one — nearly all relays taking their stage-production records rightly, and right film takes
  among the rest (Dementia Friendly screenings of "Tea For Two", "Holiday Inn", "Lullaby Of Broadway"; "The Odyssey
  (2026) [35mm]"; "It's Nice Up North + Q+A"; the Candyman double bill); carrying it to the agreement would need a new
  `Listing` field the checked-in captures do not hold (a recapture, GitHub suspended). Unrest's synopsis credits no
  director ("Harvard PhD student Jennifer Brea is struck down…"), so no intake credit reads her either.
- **What landed instead (Unrest):** not intake but the agreement's `billsAnothersDirector`: the venue's own synopsis
  naming WHOLE (`VenueNames.names`) the director of a namesake a family weighed, and no director of the agreed film,
  refuses the agreement as a billed surname already did. identity-measure-local: UK final 2 cured (Southsea "Unrest";
  Kiln "Somali Film Festival: Lifeline", judged right 10-13 but the synopsis is Ilhan Abdullahi's Canadian-Somali
  film, tmdb 1545459, not Alfuqaha's 2025 thriller — re-judged wrong), 0 right lost, every other stage and country 0.
- **Next angle:** Into The Shadows needs an event marker reaching the agreement (an interval with no relay, no film
  word, no double bill — measure it after a recapture holds the field); Binti needs the synopsis compared with each
  namesake's overview, which no signal reads.
