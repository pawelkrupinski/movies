# 2026-11-15 — a Polish part-number tail or spelled-out number against TMDB's title (counted, not built)

## Fold "Część pierwsza" onto a record titled without it, or "jedna … jeden" onto a record's digits — DROPPED on the count
- **Asked:** after [2026-11-15-tmdb-translated-titles.md](2026-11-15-tmdb-translated-titles.md) found that the PL
  local titles are TMDB data gaps, are any of the unmatched PL billings a SHAPE a code change could reach, not a missing
  translation? The two shapes seen: "Diuna: Część pierwsza" (TMDB 438631's `pl` title is "Diuna", its "Part One" only
  in English), and "Jedna kobieta, jeden stanik" (the official Polish title is "1 kobieta, 1 stanik": AfryKamera's
  catalogue and Filmweb).
- **Counted (local prod mirror, 2026-11-15, `scripts/unmatched_census.py --countries PL --since 2026-09-01`, 727
  unmatched PL listings in 370 clusters):** every unjudged cluster was read and each film-like billing was looked up on
  Filmweb's search and TMDB.
  - A part-number tail TMDB lacks: **0 listings.** "Diuna: Część pierwsza" is no longer billed. The Dune marathons
    ("Diuna - maraton" ×36, "NOCNY MARATON FILMOWY: MARATON DIUNA" ×27, "Maraton: DIUNA I + DIUNA II",
    "Minimaraton: Diuna + Diuna 2") are events joining several films, not a tail. Every billed "Diuna: Część druga / trzecia" (271 listings) already takes its
    film. Filmweb titles the 2021 film "Diuna" too, so "Część pierwsza" is the venues' rendering.
  - Spelled-out numbers against a title's digits: **2 listings** (Kino NCKF EC1, Kino Zamek: "Jedna kobieta, jeden
    stanik"). They can be reached only once TMDB holds "1 kobieta, 1 stanik", and they are well under a handful.
  - Everything else of the "local title" kind (25 listings, 16 films) is a Polish title TMDB does not hold: AfryKamera,
    Watch Docs, Pięć Smaków and FOKUS: GRANICE festival films, Filmweb-titled releases, and an NHEF catalogue film. The
    fix is TMDB data: the PL-title upload list made the same day, listing each title with its official source.
    "Muzeum Prado: kolekcja cudów", "Munch: Miłość, duchy i wampirzyce" and "Diuna: Część pierwsza" have no official
    source (Filmweb carries the Italian, the English and "Diuna"), so they stay off that list as venue renderings.
- **Built:** nothing. Neither shape earns a rule. The instalment grammar
  (`services.billing.NumberedTitles.SpelledInstalments`, "pierwsza" → 1) already reads the Polish part words.
- **Next angle:** if a re-release bills "<film>: Część pierwsza" again at scale, the fold is a listing-side shape: the
  title before a part-ONE tail, kept only where the record's own alternative titles hold a "Part One" variant. Re-count
  first.
