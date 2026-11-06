# 2026-11-06 — a "N: Subtitle" search variant for every title search — DROPPED before code (no opportunity)

- **What:** apply globally the variant that fixed Skarpetki 4's Filmweb link (`FilmwebClient.searchQueryVariants`,
  `82023af55d`: a billed "skarpetek 4. Do roboty!" is also searched as "4: Do roboty!", a word required before the
  number): add it to the queries TMDB, IMDb and the agreement families (Wikidata, RT, Metacritic) ask
  (`IdentityMeasures.searchQueries`) in all five countries.
- **Measured (read-only, local prod mirror, synced 2026-11-05 22:00 PL / 2026-11-06 07:45 others):**
  - Billed titles of the shape word, number, `. ` / ` - ` / ` – ` / `: ` / `, `, subtitle: PL 81, DE 107, UK 37, US 14,
    ES 0 (title × film); unmatched among them PL 7, DE 20, UK 13, US 4, none a punctuation miss (festival and event
    numbering, TV episodes, "Niezwykłe" for "Niesamowite", a number TMDB's title lacks: "Der Herr der Ringe 1: …").
  - Cached answers asked under BOTH punctuations: TMDB (`tmdb_queries`) 38 pairs (PL 16, DE 18, UK 4), identical answers
    in all 38. Agreement families (`identity_family_answers`): Wikidata, RT, Metacritic, Filmweb and IMDb 20 pairs
    (PL 14, DE 2, UK 4), the same hits in all but UK "Part 2 - / – Liberté" (dash vs en dash; Metacritic's extra hit is
    Harry Potter) and IMDb result order.
  - Live searches, 2026-11-06: TMDB (Skarpetki 4, Checker Tobi 3, Konferenz der Tiere 2, Bibi Blocksberg 2, Mookuthi
    Amman 2), Wikidata `wbsearchentities` (Skarpetki 2 and 3, Checker Tobi 3, Bibi Blocksberg 2, Konferenz der Tiere 2,
    Der kleine Eisbär 2) and RT (Mookuthi Amman 2) return the same answer for the period/dash and colon spellings.
- **Why dropped:** TMDB, Wikidata, RT, Metacritic and IMDb tokenise the punctuation away; only Filmweb's search does not.
  The variant would only add queries (API cost, listings awaiting answers) for no gain. The matched films' missing
  Wikidata/RT/Metacritic links come from the sources not listing them (Skarpetki, Checker Tobi) or from a number the
  source's title lacks ("Die Tribute von Panem 2 - Catching Fire" vs "Die Tribute von Panem – Catching Fire"), a different
  idea.
- **Next angle:** the identity Filmweb family (`FilmwebFamily.titled`, PL) searches without `searchQueryVariants`; its
  answers for both Skarpetki 4 spellings date from 2026-10-29 (both empty), so whether sharing the variants there adds an
  agreement needs a re-asked answer. It would corroborate a film TMDB already takes; it moves no take by itself.
