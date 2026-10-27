# 2026-10-21 — the relay stage (WS1 of `docs/proposals/2026-10-21-matching-fundamentals.md`, recordings 573 / 528 / 38713274617, base `c24cbe06d`)

## "MetOpera" as a stage-house word (and a name of the Met in `stage-houses.tsv`) — not landed
- **Tried:** make US/UK Flicks' run-together "MetOpera" banner a house everywhere, so the relay stage (`RelayStage`) reads
  "MetOpera: Carmen (2009)" as the Met's.
- **Result:** 573: US +514 right (Carmen 2009), but UK final lost 3 right "MetOpera: Samson et Dalila" (Luxe Wisbech,
  Plaza Dorchester, Tyneside): a stage house's billing asks the search a new "<work> <season>" query, the agreement's
  broadcast join then reads its candidates blind and takes nothing. Same with `--live-gaps`.
- **Why stopped:** the word reaches every reader of `StageHouse.Words` (search, screen-namesake, another-work denials),
  not just the relay stage.
- **Landed instead:** `RelayStage` alone reads a camel-cased banner as its words ("Met Opera"); every other reader sees
  the billing as billed. Code: `experiment/relay-metopera-house-word` (`711e65a00`).
