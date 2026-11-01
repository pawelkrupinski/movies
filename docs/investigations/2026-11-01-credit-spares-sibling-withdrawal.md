# A credit spares a node from a sibling's "not the work" withdrawal (2026-11-01)

Red: Identity model convergence #629, `takes-drift-2 (united-kingdom)`, seed 42, recording 573. One flip, "back to a
film it left": Keswick Alhambra's "Scottish Ballet: These Violent Delights" {Christopher Hampson CBE, Oscar Sansom} [95′].

## What happened, step by step (seed 42)

1. Up to step 8 Keswick took tmdb:1462368 ("These Violent Delights", 2026, Hampson, 70′, Scottish Ballet's record) by
   its own match: its credit, the search's first hit, a corroborating venue.
2. Step 9: Kiln Cinema Kilburn's same-titled billing arrives, crediting nobody. Its banner "Scottish Ballet" is a stage
   house, so its own evidence denies 1462368 as "a screen namesake of the work its stage house's billing names"
   (`CandidateScoring.evidenceDenial`, `Denial.Kind.NotTheWork(creditOverrules = true)`). That rule spares a billing
   crediting the film's director, and Keswick's own scoring is not denied. But `Families.siblingDenying` withdrew
   Keswick's own match all the same: a title-linked sibling's denial. Keswick went to the group vote with Kiln. Their
   pooled facts (95′ against 70′, 37.9%) do not carry the film, so `ClusterVoting.vote` split nobody off, and the pair
   was Vetoed together.
3. Step 13: Forum Hexham's 70′ billing arrives. Now the pooled facts carry the film, Kiln is split off (a denies-film
   cannot-link), and Keswick votes 1462368 again.

The whole resolve takes 1462368 for Keswick both with Hexham and without. The incremental state followed the arrival
order.

## The rule changed

`Families.siblingDenying`: a sibling's `NotTheWork(creditOverrules = true)` denial no longer withdraws a node whose own
best scoring credits the film's director (`IdentityMeasures.sameDirector`). This is the exemption `Denial.Kind.NotTheWork`
already documents ("one `creditOverrules` lets a billing whose credited director is the film's keep it"), and
`ClusterVoting.onlyWithdrawn` already applies it at the vote. Only the withdrawal ignored it. Nothing else loosens:

- a `NotTheWork(creditOverrules = false)` denial (a short beside a feed's link to a feature) still withdraws;
- an uncredited node is still withdrawn, and an all-uncredited family of the banner still takes nothing;
- the node keeps only its own match, which its own evidence accepted before the sibling arrived.

## The brief's alternative, tried and dropped

The first recommendation: split the denying member off `ClusterVoting.vote` whatever the pooled facts say. Tried on the
pinned slice (no Hexham), it does not cure the take. The rest, Keswick alone, must still carry the film by its own facts
(`EvidenceWeights.carriedByOwnFacts`), and 95′ against 70′ reads 7.4%. Dropping that check too would let any
denial's leftovers take a film that their own facts read low, on the database's ranking alone. That is looser than before.
The withdrawal was the cause. The vote only followed from it.

## Proof

- `IdentitySliceCasesSpec`: `uk-keswick-alhambra-these-violent-delights-nohexham` (recording 573's family less Forum
  Hexham's row, the state at step 9) is red on main and green after. `…-hexham` (the whole family) is green on both.
- `IdentityResolverCasesSpec` "A stage house's relay of a work": the credited pair beside an uncredited billing keeps
  the record (red on main), and the guard: uncredited billings, one with a runtime, still take nothing.
- Measurement: below.
