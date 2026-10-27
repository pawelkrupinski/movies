# 2026-10-23: a withdrawn match re-taken by the group vote. CURED for Notre-Dame de Paris and The Thing With Feathers (prod-latest 1622)

- **Cause, shared by both prod-only cases below:** on 1622 each node's own match is withdrawn by a title-linked sibling's
  denial (`withdrawn:sibling-denies-film`). Tywyn's and Woodbridge's bare "Notre-Dame de Paris" are withdrawn from 11594
  by the ballet billings ("a screen namesake of the work its stage house's billing names"), and Mal seh'n Kino's "THE
  THING WITH FEATHERS" from 1278923 by the Cineamo-linked "Das Ding mit den Federn" ("a short, its feed linking
  1127628"). Then the solver splits the denying sibling off (a learned listing-listing cut), and the cluster of withdrawn
  nodes left over votes the same film back (PooledMatch, unrivalled-calibrated). On 573 the sibling stayed in the
  cluster (Vetoed / 1127628).
- **v1, NOT landable:** the vote refused any film a member had been withdrawn from. Prod-latest DE (run 1777): 1 cure, 43
  right-lost, 198 unjudged lost ("Sinn und Sinnlichkeit" ×~150, "Glaube", "Es"). Those withdrawals come from a sibling's
  own facts or its billing ('Glaube' [1998], 'ES (ULRICH SCHAMONI)'), which say only that the sibling is another film.
- **Landed:** `Denial.Kind.NotTheWork` marks the two denials that say the film is not the WORK billed (a screen
  namesake of a stage house's work; a short beside a feed's link to a feature). A cluster whose EVERY member was
  withdrawn from a film by such a denial votes no film (`ClusterVoting.onlyWithdrawn`). Two exemptions, each one a
  measure showed: a member whose own TMDB-keyed feed links the film (DE "Nosferatu" ×2, Cineamo 426063m, beside a
  "Nosferatu … Ballett Extra" billing), and, for the screen-namesake denial, a member crediting the film's director, as
  that rule spares its own listing (UK "These Violent Delights" ×10 {Christopher Hampson}, Scottish Ballet's record, 573);
  and any family whose member keeps the film by its own match (573's uncredited Savoy "These Violent Delights" ×7).
  All three listings take none, and so does DE Studio-Kino's bare "Das goldene Zeitalter" (the 2015 short, agent-judged
  wrong). Not moved by it, but likely wrong (for the user): DE Cineplex Lörrach "Der Glöckner von Notre Dame" → 10545
  (screens only on the Paris Opera Ballet relay day, kinoheld copying Disney's credits) and Kulturbühne Versmold "GRAF
  VON BOTHMER: NOSFERATU mit Chor" → 426063 (a live silent-film concert, i.e. the 1922 film).
  The Thing With Feathers does not reach its right film 1127628: kinoheld's copied director (Oelofse) keeps it apart
  from the Cineamo-linked sibling (`learned-director-in-different-and-venue-in-same`). The slice case holds that row as
  `pending:`. Next angle: a copied credit should not cut a listing-listing link at the same venue.
