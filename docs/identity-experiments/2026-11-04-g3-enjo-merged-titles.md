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
