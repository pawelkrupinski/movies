# 2026-11-02 — a venue's text naming another film's director as a credit AGAINST a film — DROPPED (pinned 573, base `f43b1fcd5`)

- **What:** `CreditMeasures.creditedByText` read the venue's own text (synopsis, cast field: `VenueNames`) both ways
  for a listing crediting no director: the text naming one of a film's directors whole was `director=same_person`
  (kept), and the text naming whole a director of another film of the pool, and none of this one's, was
  `director=different` (−5.67). Asked for as "a positive as well as a negative signal" beside ES taquilla's "Hope"
  (Sødahl's 2019 film vetoed as a namesake of Na Hong-jin's 2026 one).
- **Measured:** `scripts/identity-measure`, run 41461223304, base `f43b1fcd5` vs `affc221f5` (both branches): US no
  move; UK lost 4 right takes and 2 more in the agreement (Chapter Cardiff "Out of the Woods" ×2), each a listing now
  denied its film or kept apart from its own cluster by `director=different`: Riverside Woodbridge "Rocky 50th
  Anniversary" (the text names Sylvester Stallone, Rocky II–IV's director, not Avildsen), DCA "Ken Russell's The
  Devils (4K Restoration)" (Russell is the title's, so left out, and the text names a director of "Aria" in the pool),
  Magic Lantern Tywyn and Chapter Cardiff "Pressure". DE, ES, PL never ran: the CI lane stayed queued for hours.
- **Why dropped:** a whole name in a synopsis is a person the film is ABOUT or stars, as often as its director; a name
  that directed another candidate says nothing against this one. Only the positive reading holds: a film's own director
  named is its credit. A negative reading would need the text's role for the name ("directed by", "su directora",
  "Regie", "reż.") in every country's language — not built.
- **Code:** local tag `experiment/text-director-against` (`714f0f1f26`).
