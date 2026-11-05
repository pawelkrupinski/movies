# 2026-11-05 — a relay's work words drop "screen" (`ScreeningWords`) — LANDED 2026-11-05 after a re-run under keyed fills; the DE seed-8 Geister flips were the harness's (head `f36cb6581ae3`)

- **What:** `StageRelays.Relay.of` filters `ScreeningWords("screen")` out of a relay's `workWords`, so UK Lowther Pavilion's
  "On Screen: The Royal Ballet SWAN LAKE (12A)" no longer reads "screen" as its work and denies the house's 2026/27 record
  (measure 41438444822: Swan Lake took the 2024 relay). Commits `06baf92b3fda` (as a RelayWords member, which broke the
  "EOS" abbreviation, measure 41458006277) and `4c5cf3b4fee3` (the final form); C's rule judgements beside them.
- **Measured:** C as a whole (`f36cb6581ae3` on `fb13eb12d2fc`) was measure 41898404243 LANDABLE (right->wrong 0), but its
  drift 41898476540 was RED on DE `takes-drift-1` seed 8: four NEW flips, Geister/GEISTER at Bali-Kino, Theaterstadel, UFA
  Palast Düsseldorf and Zeise Kinos, tmdb:1447206 -> 1218694 at step "6 gone" -> 1447206 (cause answer, population); the base
  flips Titans x2 and Buddies there instead.
- **Bisect:** `129b6c3700e7` (the title shapes only) drift run 41956587308 CLEAN; `4c5cf3b4fee3` (adds the StageRelays
  change) run 41958722825 RED with the same flips. The shapes are not the cause. The drift slice of the Geister listing
  (`identity-slice de --venue Bali-Kino --title GEISTER --emit-drift-slice`, 12 seeds) replays CLEAN: the step needs the
  corpus's population.
- **Likely mechanism (not confirmed):** emptying a relay's `workWords` flips `namesAnotherWork`'s guard
  (`relay.workWords.nonEmpty || relay.workIds.nonEmpty`), a population-dependent denial that moves pooled answers when a
  listing goes.
- **Why parked:** a population-dependent relay change is not patched per listing; it waits for the held-take work (owner,
  2026-11-05). The Lowther On Screen relays stay unmatched meanwhile.
- **Code:** local tag `experiment/stage-relays-screen` (`f36cb6581ae3`, C whole); branch `unmatched-recover/banners` holds it.

- **Correction (same day):** the first bisect (129b6c3 clean, 4c5cf3b red) spanned TWO changes, the Roman-numeral narrowing
  of `caseSplit` (`1d4ed4`, before the StageRelays commits) and the relay one. A second bisect on the rebased shapes
  branch isolates it: shapes without the numeral commit, drift run 42013538022, CLEAN (DE seed 8: Titans/Buddies
  pre-existing only); the same plus the numeral commit (`7005d9bb1b30`, no StageRelays change), drift run 42003442640, RED
  with the same four Geister flips. So the Geister flips follow the numeral narrowing (a shape-set change moving a DE
  title's searches, hence the family's pool), and the relay change is parked for its own reason (population-dependent
  denial, held takes), not for these flips.
- **Root cause (later the same day): neither change.** The drift harness drew each fill's held answers from the stream
  that also draws every re-scrape's half, one number per held question; any change asking one question more or fewer
  (the numeral narrowing stops searching "Halloween" for Babylon's "Halloween II [OV]") re-rolled seed 8's arrangement
  country-wide. With fills drawn by question id (`IdentityDrift.Share.filed`), main and the shapes branch with the
  numeral commit show the SAME DE flips (seed 8 Buddies + Titans x2, seed 64 Buddies; drift runs 42027698490 and
  42027914659): no Geister flip. The relay change may deserve a re-run under the fixed harness.
- **Re-run under keyed fills (2026-11-05), landed:** the relay change ALONE on main `80b59d0da8e1` (no numeral commit,
  no shapes): drift 42039542135 (DE and UK, seeds 8 and 47) CLEAN -- DE seed 8's head flips are the base's (Titans at
  CinemaxX Dammtor and Regensburg), no Geister flip, seed 47 none; full measure 42044743504 LANDABLE (UK model: Lowther
  Pavilion's 2023 Nutcracker take cured to the house's 2026/27 record, 8 of its "On Screen" RBO/ROH relays gained right;
  right-lost 0, right->wrong 0, every other country 0 moved). The red that parked it was the drift harness's arrangement
  shift; the held-take wait it was parked for was dropped by the owner, so it landed on main after C (rebased onto
  `f805949e59d1`; `scripts/identity-what-if` there moved only UK Lowther's Carmen, Götterdämmerung and Tosca to their
  2026/27 records, a subset of the measure's moves, every other country 0).
