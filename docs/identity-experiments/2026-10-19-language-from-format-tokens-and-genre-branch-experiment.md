# 2026-10-19 — language from format tokens, and genre (branch `experiment-language-genre`, recording 38713274617)

## Weigh the listing's format tokens against TMDB `original_language`, and its genres against TMDB genres — not built (offline: no gain)
- **Tried (offline only, no resolver code):** language constraints from the showtimes' `format` tokens and the title
  (PL NAP/DUB/LEK/ORG/napisy/dubbing → not `pl`; DE OmU/OV/OmdU → not `de`, OmeU → not `en`; UK/US Subtitled/SUB/DUB
  → not `en`, HINDI/TAMIL/… → that language; ES VOSE/VO/DOB → not `es`), levels match / contradicts / unknown against
  the candidate's TMDB `original_language`; listing genres mapped onto TMDB ids (documentary 99, animation 16, plus
  music, horror, comedy, family, sci-fi), levels agree / contradicts (venue says doc/anim, film neither) /
  contradicts:film (film doc/anim, venue's genres not) / neutral / unknown. Code: `scripts/ml-identity/language-genre/`
  (`run.sh`).
- **Data:** neither field is in the resolver's data. `TmdbNormalizer.minimal` keeps neither `original_language` nor
  `genres`, search hits keep id/title/original title/year/popularity only, the captures hold neither, and the prod
  mirror's `tmdb_films.record` holds no genres. The recordings' raw `/movie` bodies (and search results:
  `original_language`, `genre_ids`) do, so adding them needs NO new lookup: keep two fields in `minimal` (~8 bytes a
  film: a 2-letter code and a genre bitmask), refill the captures from a recording, and let the stored records gain them
  as they are re-fetched (or re-fetch). The listing side is free: formats and genres are already scraped (a byte and a
  bitmask per listing).
- **Coverage (listings carrying it):** language PL 5,645/10,593, DE 4,479/33,489, UK 3,359/36,748 (791 a named
  language), US 246/100,152, ES 4,319/6,705; genres PL 1,647, DE 6,043, UK 25,855, US 97,063, ES 6,210 (doc/anim:
  377 / 1,380 / 3,490 / 10,218 / 1,408). On the failure classes it is nearly absent: of the ratchet's (38420977752)
  31 WRONG takes, 2 carry a language contradiction (DE "CLOSE YOUR EYES" OmeU vs `en`, "The Tea Alchemist" OmeU), 0
  carry listing genres; the remake ("Sparrows" `xx`, "Hocus Pocus" `sk`) and translated-title ("BARWY OCHRONNE /
  CAMOUFLAGE" OmeU vs `uk`: a *match*) classes bill no token that tells the namesakes apart.
- **(B) confidence** (`identity-unified/training.tsv.gz`, 24,267 labelled contender rows, judgements override, grouped
  5-fold logistic, hand ×5): base AUC 0.9987 / hand AUC 0.9643, LL 0.1837, Brier 0.0355; +language 0.9987 / 0.9643,
  LL 0.1839, Brier 0.0354; +genre 0.9987 / 0.9640, LL 0.1849, Brier 0.0357; +both 0.9987 / 0.9645, LL 0.1846. Nothing.
  The calibration's own counts make language perverse: contenders at `contradicts` are 31 right : 64 wrong, ABOVE the
  prior (27 %), so a refit weighs a contradiction ~+0.3. Every one of today's 31 contradicted takes is right: German
  films billed OmU/OmeU (dialect, Swiss, English-subtitled screenings: "Mittagsstunde", "Der Himmel über Berlin"), Polish
  films with NAP/DUB (accessibility subtitles: "Ojczyzna", "Przytul mnie"), UK relays with SUB/Subtitled (NT Live, ROH).
  Genre contradictions on today's takes: 220, all right (venues list animated films under "Family", music biopics under
  "music").
- **(A) takes, simulated at the model stage** (best contender, accept at logit ≥ 0.27 ≈ the showRatings cut):
  language moves 1–2 clusters (right↔right); genre with its in-logit fit loses 78 right for 15 gained, 2 cured, 2 NEW
  WRONG; with calibration-style weights gains 50 (weak-labelled, mostly what later stages take already) for 4 right
  lost and 9 NEW WRONG (DE "München Mord: …" → "Der beste Papa der Welt", ES "Carmen (OPERA LIVE)" → Carmen 1921). Not
  landable; identity-measure not run, since the data is not plumbed and the offline fit leaves nothing to measure.
- **Why stopped:** neither signal separates right from wrong beyond what the model already reads; format tokens mean
  "subtitled screening", not "foreign original", too often; genres are too coarse and inconsistently listed. Memory
  would be cheap; the value is not there.
- **Next angle:** only OmeU-vs-`en` held up on the judged set (13 wrong : 3 right contradicted) — as a narrow agreement
  veto it would cure 2 WRONG + 2 UNJUDGED ("Pornfilmfestival: The more the merrier") for 1 right lost ("Shoshana"),
  but a listing's showtimes mix OmU and OmeU and English-language documentaries screen OmeU: measure it per showtime,
  and only after `original_language` is in `minimal`.
