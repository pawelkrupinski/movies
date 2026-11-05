# 2026-11-04 — `certified-title`: the venue's own certificate and running time rule namesakes out — CLOSED 2026-11-05 (head `752f3f1505f5`)

- **What:** `Corroboration.certifiedAlone` + `AcceptanceFactRules.certifiedTitleWhy` (branch `identity/cert-rule`,
  WIP `a3c3f74580aa`): among the namesakes a bare title names, a namesake TMDB rates on another step of the venue
  country's scale (`Certificates.Apart`), or too short for the slot, is ruled out; one left standing is taken. Target:
  UK Plough Arts Centre Torrington "Dracula" (87', 12A) -> Hammer's 1958 film.
- **Measured:** right -> wrong: Rex Berkhamsted "Ghostbusters" is billed 12A (the BBFC's current re-rating), TMDB records
  the 1984 film's original PG, so the rule ruled the right film out and took a namesake rated 12A.
- **Why closed (review 2026-11-05):** (1) the error is on TMDB's side: a re-rated old film keeps its original
  certificate in TMDB, so restricting the rule to venues with an authoritative certificate source does not help (Rex's
  12A WAS authoritative); (2) the restriction that would be safe -- trust a TMDB certificate only for a recent release,
  which no re-rating has reached -- leaves the target nothing: Torrington's namesakes (1931, 1958, 2025/26) are old or
  uncertified, and the owner has decided Torrington "Dracula" stays unmatched (a genuine 3-way namesake); (3) G3's
  recovery (Enjo) already excludes certificates. No remaining target, a demonstrated wrong-take mode.
- **Code:** local tag `experiment/certified-title` (`752f3f1505f5`); branch `identity/cert-rule` (unmerged).
