# 2026-11-13 — an ambiguous bare node joins the film's side past a cut-only denial — DROPPED (tag `experiment/thin-denial-joins-film`, head `0b3620c73e2`)

- **What (Option A for PL "André Rieu. Pada śnieg!"):** a `denies-film` cannot-link whose only ground is a node denying
  the other's film by the probability cut alone (`Scored.deniedByCutOnly`) became `denies-film-thinly`. It still kept its
  two sides apart, but `ConstraintSolver` rule 3a no longer called a film-less node reaching both ambiguous: when exactly
  one of the roots it reached held a film and only thin denials kept them apart, it joined that film's side. Built for
  the 64 plain Rieu listings that reached both the three venues holding tmdb:1707785 and Cinema City's 30, which denied it.
- **Measured:** `scripts/identity-what-if --base 92af130e8c4 --head 0b3620c73e2` (snapshot 2026-11-13 ~02:20Z, all five
  countries): PL 0 moved, UK 0, US 0, ES 0; **DE model 349 moved, all one title: "Godzilla Minus Zero" tmdb:1381071 ->
  none** (prod serves 1381071; the final stage took it back, final 0 moved). Unjudged, but the film prod serves at 349
  venues: a model-stage right loss. Slice (recording 43166417765): 71 listings take 1707785 (the plain spellings and
  the retransmisja ones join the film's side), but Cinema City's 30, Nove Kino's 4, Kino Iskra and Kino Muza Myślenice
  stay apart, taking none: 11 web films for the concert.
- **Why dropped:** it treats the symptom (the sides) and leaves the cause (two thin readings of the record that DENIED
  it) in place, so the denying venues still take nothing; it loosens rule 3 everywhere a cut-only denial holds a bare
  node back (the Godzilla loss, a model-stage regrouping the first round's partition votes over -- the population read
  the 2026-11-12 pooled vote failed `PopulationMonotonicitySpec` on); and the joined nodes skip their edges to
  non-ambiguous neighbours in that tier, an order-shaped detail.
- **What landed instead (Option B):** the denials themselves were weakened at the evidence: a country mismatch against
  a concert's director-less record weighs half (`concert-mismatch`), and an original title matches a record whose title
  adds only the record's own year. On recording 43331601375 the slice takes 1707785 for 109 listings as one cluster
  (6 web films left); what-if: PL 1 moved (Muza Myślenice, gained, judged right), every other country 0.
- **A third denial, prod only (2026-11-13):** prod's stored Kino Muranów page read "około 2 godzin i 50 minut" as 2
  minutes before dc6c2bef1f6 parsed hours, so a learned listing-listing cannot-link (`runtime.delta >= 81`) kept every
  170-minute listing off Muranów's. Not a rule to change: it heals on the page's next read.
