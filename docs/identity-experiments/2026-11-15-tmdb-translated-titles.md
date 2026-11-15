# 2026-11-15 — a local title TMDB holds only as a translation (counted, not built)

## Read candidates' `/translations`, or search TMDB in the country's language, for unmatched local billings — DROPPED on the count
- **Asked:** PL "Oczy Ghany" (1525208), "Jedna kobieta, jeden stanik" / "1 KOBIETA, 1 STANIK" (1517105), "Munch: Miłość,
  duchy i wampirzyce" (978505), "Żar" (1658185), "Diuna: Część pierwsza" (438631) stay unmatched; the guess was that
  TMDB files these local titles as translations or alternative titles the resolver does not read.
- **Today:** `/search/movie` already carries `language=<deployment>` (`TmdbClient`), and TMDB's search matches every
  translation and alternative title whatever the language; the resolver's record reads the localized title, the original
  and the en-US `alternative_titles` (`TmdbFilmRecord`).
- **Found:** none of the five films holds a Polish title at all — not as a translation, not as an alternative (1525208:
  only `ru`; 1517105: `ko`, `zh`; 978505: `en cs es ko pt`; 1658185: `lt`; 438631's `pl` is "Diuna", its "Part One" only in
  English and other languages). The billings are the venues' own renderings of the English titles; only translating the
  billing would reach them, not TMDB data.
- **Counted (local prod mirror, 2026-11-15):** 147 unmatched BelowThreshold clusters (PL 60, DE 60, ES 27 of
  313/908/27) — every candidate's `/translations` (369 TMDB calls, 5 workers) against every title the listing searched:
  0 exact matches the resolver does not already see (8 apparent PL hits were the `pl` translation the pl-PL answer
  already carries; one DE hit, Schauburg's "Little Shop of Horrors" → the 1960 film, is the wrong film, the 1986 one is
  meant). 55 NoCandidate clusters have no candidate whose translations could help; their searches already ran in the
  country's language. Recoverable: ~0, well under the 15-listing bar — no code written.
- **Next angle:** none on TMDB's side; a curated title-rule row per festival film is the only route for the cases above.
