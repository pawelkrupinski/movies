# 2026-10-21 — a bare one-venue billing among namesakes as popular is not "unrivalled" (recordings 528, pinned)

- **Tried:** in `AcceptanceCalibratedRules.unrivalledCalibratedWhy`, refuse a take when the listing states no year,
  running time, director or original title, no other venue corroborates it, and another eligible candidate the title
  names by the same pieces is at least a quarter as popular. Target: PL Terminal Kultury Gocław "Binti Edukacja Młode
  Horyzonty" → 855156 (Shamte 2021; right 577285, Migom 2019, as popular).
- **Result:** cures Binti (unit case). The e2e fixture corpus (08-06-2026) lost PL Kino Amok's bare "Omen" → 794, the
  1976 film, which the venue's page states (Donner, 1976, judged right 10-21) though its scraped listing carries no
  fact: a right take lost. Not pushed; the whole-corpus measure (CI run 810 / 817) was still queued.
- **Why it stopped:** a venue's unread page facts are what tell the namesakes apart; "states nothing" reads the scrape,
  not the venue. **Next angle:** read Kino Amok's (and biletyna's) detail facts into the listing, then the bare-namesake
  refusal costs nothing there.
- **Code:** `experiment/bare-one-venue-namesakes` (57c0ca15b).
