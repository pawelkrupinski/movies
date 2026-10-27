# 2026-10-19 — OmeU listing vetoes an English-original film (branch `experiment-omeu-veto`, tag `experiment/omeu-veto`, base `b6e058316`)

## Deny a candidate whose TMDB `original_language` is `en` when the listing's showtimes are (all / mostly) OmeU — not built (offline: loses far more than it cures)
- **Idea:** the `experiment/language-genre` study's one surviving form — "OmeU" (original, ENGLISH subtitles) implies
  the original is not English (13 wrong : 3 right contradicted among judgement rows) — as an agreement/unified veto,
  measured per showtime as that entry asked. Code: `scripts/ml-identity/omeu-veto/` (`run.sh`): prod's served DE takes
  from the local prod mirror (`screenings` → `movies.tmdbId`, 627 listings with an OmeU showtime: 570 all-OmeU, 12
  majority, 45 minority), each film's `original_language` from recording 38713274617's TMDB bodies, verdicts from
  `identity-judgements.tsv`.
- **All-OmeU listings taking an `en` film: 72** — judged 6 wrong (5 "Pornfilmfestival: …" at Moviemento / Kino Central,
  "The Killer" 2024 at Savoy), 2 right ("Shoshana", "LOVE & WAR"), 64 unjudged. By inspection the unjudged are almost
  all right: BTS "ARIRANG" concert relays (26 listings), "NAZA" (Palestinian doc, TMDB `en`, 9), "Bridget Jones's
  Diary", "Pride", "The Red Shoes" ×2, "Der Teufel trägt Prada 2", "Sonnenaufgang" (1927), "Mama", "Mārama", "Lady",
  "Black Burns Fast", jazz docs (Monk, Ornette, Milford Graves); only 2 wrong (AFRIKAMERA / LT KINO shorts → tmdb 25132
  "Shorts" 2009). German venues bill OmeU on English films (English subtitles for an international audience, or just
  "OV"). **Majority variant:** +5 listings, all NAZA (right). The sibling's cures ("CLOSE YOUR EYES", "The Tea
  Alchemist") are not among today's takes.
- **Guard tried:** exempt documentary/music films (TMDB 99 / 10402): 27 listings left — 5 judged wrong + 2 unjudged
  wrong vs Shoshana + ~19 unjudged right (Bridget Jones, Pride, Prada 2, Red Shoes, Mama, Mārama…). Still a net loss.
  TMDB's own `original_language` is also wrong on small films ("Barrio Triste" and "Narciso, film esperienza" read `en`
  though spoken es / it), so the signal is noisy on exactly the art-house titles OmeU marks.
- **Why stopped:** right losses outnumber cures ~8:1 (guarded ~3:1); no resolver code written, identity-measure-local
  not run. The cured wrongs have better-aimed fixes: "Pornfilmfestival:" is a festival programme prefix (a title rule,
  then the bare title's own match), "The Killer" a remake-vs-original choice (year/director evidence).
