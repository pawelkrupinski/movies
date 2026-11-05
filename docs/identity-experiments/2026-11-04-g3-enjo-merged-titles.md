# 2026-11-04 — merge every agreeing record's titles into the agreed record — PARKED, it moves no take (Abaton "Enjô – Conflagration", tmdb:97149)

- **What:** Abaton's "Enjô – Conflagration" (tmdb:97149) has been unmatched since G3 (`d68bb5652fd`). IMDb's resolve
  ties exactly: Conflagration (1958, tt0051584) against Burn (2026, tt39150067, original title "Enjô"), both
  0.42502405. Before G3 the tie-break chose 1958 because its titles covered both billed pieces; since G3 a tie neither
  takes nor leans. The remaining taker is RT alone (Filmweb and Wikidata lean; TMDB corroborates), and review-site-only
  takes are never taken (`AgreementTakes.taken`, `namesFilms = false`). The reported refusal, BilledPiece, comes from
  agreement reading only the lead taker's record (now RT's "Conflagration" alone). The experiment merges the titles of
  every agreeing record into the agreed record.
- **Measured:** `AgreementSpec` test fails before / passes after; `AgreementSpec` + `AgreementStageSpec` 95/95.
- **Result:** BilledPiece no longer refuses, but the take stays none: nothing here moves a take.
- **Why parked:** it moves no take for this listing, so it is not worth landing alone. Open questions for the owner:
  (1) the scorer gives both films equal probability although the billing names 1958's English title; changing the
  scoring is identity design and needs the full measure; (2) which family's title leads the merged record is arbitrary.
- **Code:** local tag `experiment/g3-enjo-merged-titles` (`Agreement.scala`, `AgreementSpec`).

## Measured: billed-pieces coverage as a score (2026-11-05)

- **Proposal:** the open question (1) above, as a rule: among a billing's candidates, the one whose titles (TMDB
  title, local, English, original, alternative) cover more of the billing's pieces scores higher. Pieces are
  `BillingPieces.pieces` (colon, pipe, or a dash or slash spaced both sides); a title covers a piece when its
  normalised key equals the piece or a run of adjacent pieces joined ("Frida. Viva la Vida" covers "Frida" +
  "Viva la Vida"); a title equal to the whole billing covers all.
- **Method (read-only, nothing run through the resolver):** the local prod mirror (synced 2026-11-05 02:49Z),
  `identity_traces` of pl / de / uk / us / es (56,453 family traces with >= 2 pieces), candidates and probabilities
  from the trace, titles from `tmdb_films`; counts are distinct raw titles ("clusters"), listings in brackets.
  Ground truth: `identity-judgements.tsv` where a row exists (about 10% of clusters), else a reading of the titles.
  **Blind spot:** the Enjô tie lives in the IMDb family resolve (0.42502405 both), which the mirror's traces do not
  store, and Abaton Enjô is not in the mirror, so the counts are the TMDB model's candidate sets, a proxy.
  Also: exact-key coverage only (no fuzzy), TMDB alternative titles only (not IMDb AKAs), and the plain delimiter
  split counts event pieces ("Nostalgic Cinema:") as pieces.
- **Unmatched several-piece billings: 1,574 clusters.** 864 have no candidate, 503 exactly one, 207 are near-ties
  (>= 2 candidates within 80% of the top probability; 466 listings, mostly noise at about 3% top probability).
  Of the 207 the rule decides 25: 4 right (Frida via the joined variant, "Hacer Mucho con Poco - Do More With
  Less", "Alim | UFF", "NT Live: Hay Fever"), 20 wrong (3 judged in the file, 17 by eye), 1 unclear; 182 stay tied
  (equal coverage, mostly 0 vs 0). A low-probability candidate whose title equals one piece ("Namibia", "Bhutan",
  "Workshop", "Afternoon") is the event or topic piece, not the film.
- **Matched several-piece billings (4,408 clusters, 52,791 listings):** 113 clusters / 831 listings have a rival
  covering strictly more pieces than the current take. 24 clusters (63 listings) are judged right in the file, so
  certain right-to-wrong flips; of the 87 unjudged, about 75 more are right by eye, about 8 unclear, about 4 cures.
  One pattern: the right take is a specially titled record that covers none of the pieces, the rival is an older
  namesake whose title is a piece.
- **Examples:**
  - Royal Ballet & Opera 2026/27: Swan Lake / "The Royal Ballet: Romeo and Juliet" (2027 season records) would flip
    to the older "The Royal Ballet" 2024 / 1451897 (2025) namesake records.
  - Maja "THEATER: WOLFGANG BAUER - NAMIBIA" would take Namibia (2005); Hall of Fame Kamp-Lintfort "Live-Event:
    Stefan Erdmann - Bhutan" would take Bhutan (1928): lectures, not films.
  - Theatr Mwldan "Forest Coal Pit / Everybody to Kenmure Street": a double bill, the rule picks one film.
  - Also: "Der Herr der Ringe: Die Gefährten (Extended Version)" (31+ listings) would flip to the 1978 animation.
- **Conclusion: net harmful, not to be built.** About 4 right and 20 wrong among the unmatched, against about 4
  cures and about 24 certain plus about 75 probable right-to-wrong flips among the matched. Enjô-like cases (a
  same-title namesake tie with a stated coverage margin) are rare: only Hacer Mucho and Frida in this corpus. Enjô
  stays unmatched; unmatched beats wrong. If ever revisited: restrict to an exact tie with a stated own-title fact
  on one side, and measure on the recording.
