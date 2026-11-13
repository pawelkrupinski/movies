# 2026-11-13 — one house's relay of one work in one season is one film (`same-relay`) — LANDED, then fixed forward

- **Why:** the 2026-11-13 corpus audit (`~/.cache/movies-split-audit/splits-2026-11-13.tsv`) found the Paris Opera's
  2026/27 relays split by wording, none with a TMDB record: UK "La Bayadère" 10 web films ("Opéra De Paris Ballet -",
  "The Opéra National de Paris:", "(Opéra de Paris)" …), "Notre-Dame de Paris" 6, DE "Der Glöckner von Notre-Dame" 3-4,
  PL "Jezioro łabędzie" / "Romeo i Julia" 4 each. No film joined the spellings and their titles share no form.
- **What (2a3a2ddbe76):** `RelayStage.relayKeys` keys a billing naming a house in particular by (stage work, season: the
  season it names, else its screenings'; none when its title dates another season's performance); a block key each, and
  a `same-relay` must-link (tier 3, ahead of the bare work's title-segment links, which left each spelling ambiguous)
  between two billings naming one house (`StageRelays.namesOneHouse`); no link onto another house's record a member
  holds. The Paris Opera's PL/DE names joined `stage-houses.tsv`.
- **Recording 43331601375 (identity-slice):** UK Bayadère Paris spellings 7 clusters → 1 (19 listings), Notre-Dame 6 → 1
  (19), DE Glöckner 4 → 1, UK Hebden's "THE NUTCRACKER The Royal ballet" joins the 2026/27 record. The Royal Ballet's own
  "La Bayadère" (9 Feb), the 2021 and 2024/25 Nutcrackers and the bare work's billings stay apart.
- **The landed version's CI measure (run 43377416936, pinned recording 573): NOT LANDABLE, UK model right-lost 4**
  (final takes 0 moved): Uckfield "Macbeth (MET)", Northallerton "RBO: MACBETH (MET)", Stowmarket / Corby Met "Manon".
  Two causes, each replayed both ways on 573:
  1. Banners naming two houses ("RBO/MET presents MACBETH", "RBO: MACBETH (MET)", "Met Opera in Association with RBO")
     keyed as the RBO's formed a relay of their own apart from the Met's record. Fix: `namesTwoHouses` — a billing whose
     houses, banner words ("MET") or house initials ("RBO") name two houses has no relay key.
  2. The landed version also lifted the learned listing-listing cut between any two billings of one relay. Lifted between
     "The Metropolitan Opera- Manon" and the Met's "Met Opera 2026-27: Manon" spellings it moved Stowmarket and Corby out
     of the Met's cluster. Dropping the lift entirely lost UK Notre-Dame's fold (8 clusters) and, on prod's corpus,
     Hebden's "Swan Lake The Royal Ballet" (right). Fix: lift only between billings naming the house ALIKE
     (`StageRelays.nameOneHouseAlike`: a shared word, initials, two names of one house — not "Met" opening "Metropolitan").
  Tried and dropped on the way: pairing relay keys only inside a family (`<family>|r:…`, no family growth) — lost the
  gains (Kendal's Così, Hebden's Nutcracker, DE's Paris "Schwanensee" spellings) and fixed nothing the two fixes do not.
- **The fix's measurements:** 573 slices: Met Macbeth 332 listings (base 329), Manon 170 (base 170), Uckfield and
  Northallerton back on the Met's record. `scripts/identity-what-if` on prod's corpus, all five countries: vs main
  (2a3a2ddbe76) 0 moved anywhere; vs the pre-change base ac133c24eeb (other agents' commits included) right-lost 0,
  right→wrong 0, new-wrong 0 — UK Kendal "Così fan Tutte RBO The Royal Opera" gained right, DE 9 Paris "Schwanensee"
  spellings gained the Paris Opera's record (judged right beside it), DE Cinecitta's "Opera de Paris: Romeo et Juliette"
  cured off the 2023 Bastille Gounod record. CI measure of the fix: run 43394141008.
- **Not cured by wording:** UK Riverside Woodbridge's bare "La fanciulla del West" / "The Nutcracker" (no house: both
  the Met and the Paris Opera relay Fanciulla in 2026/27); PL Multikino's Paris Opera "Romeo i Julia" takes the RBO's
  record in recording 43331601375 (`StageRelays.anotherProduction` keeps another house's same-season record a candidate,
  for Flicks' "RBO Cinema Season" billing the Met's productions) — a review candidate row, the owner's call.
