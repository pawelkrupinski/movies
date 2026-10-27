# 2026-10-21 — a spaced "//" as a double bill's join (recordings 528, 38713274617)

- **Tried:** `MultiFilmBill.billPieces` splitting on a spaced "//" as on a spaced "+", so every double-bill rule read
  DE Filmhaus Kino Köln's "SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE" (judged 10-13: two films) as two works.
- **Result (`identity-measure-local`, DE, base `3718a67d4`):** cured the one listing, but lost 7 takes on 528 (10 on
  38713274617), none judged: DE venues join ONE film's German and original titles with "//" — Central im Bürgerbräu's
  "DIE VENUS VON PARIS // LA VÉNUS ÉLECTRIQUE", "NUR GETRÄUMT // JUSTE UNE ILLUSION", Ostpassagen's "La Haine // Hass".
- **Second form, not landed:** a candidate denial when some piece's words are none of the film's titles' — still lost 7
  on 38713274617: TMDB files "Die Venus von Paris" and "Down and Out in Paris" under no title of those records.
- **Landed instead:** `MultiFilmBill.slashesAnotherWork` as a candidate denial: a film is one work of a "//" bill when
  neither piece is its own or original title and a piece is none of its titles. A bilingual billing carries the
  original title as one piece; Filmhaus Köln's bill carries neither of "片袖の魚"'s own titles.
