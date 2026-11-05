# 2026-11-05 — a relay's work words drop "screen" (`ScreeningWords`) — PARKED, it opens DE seed-8 Geister/Titans flips (head `f36cb6581ae3`)

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
