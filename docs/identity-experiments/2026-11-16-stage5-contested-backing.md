# 2026-11-16 — stage 5, narrow: a bare cluster whose family two films' own statements contest — PARKED (head `2c77abc5f`)

- **Why:** DE Burgtheater Ratzeburg's bare "Adventskino: Der Grinch" (with Beluga, Uetersen, Steinhaus) flipped six times
  between 8871 (2000) and 360920 (2018) in prod on 2026-11-10: `ClusterVoting.familyMajority` lent a near-50/50 venue
  majority whose Wilson bound straddled the 0.3746 cut (17/32 → 0.389, 15/29 → 0.370). The arrival walk
  (`PopulationMonotonicitySpec`, the drift slice of 97d260d67) finds it as A → B → A. The owner approved stage 5 on
  2026-11-16; the 2026-11-05 variants (`2026-11-02-stage5-contested-title-family.md`) were closed for reading a
  sibling's film against a bare listing's own vote and losing ~40 right takes.
- **Variants** (all on `ClusterVoting.ownBacked`, the films the cluster's title siblings take ALONE on a fact of their
  own, with their venues; only facts-free clusters):
  - **A** (tag `experiment/stage5-contested-majority-a`): a contested family (ownBacked ≥ 2 films) lends its majority
    only when the bound clears ONE HALF; the cluster then votes on its own pooled evidence. What-if DE, PL: 0 takes
    moved. Arrival walk: the Grinch walk becomes 8871 → none → 8871 (the cluster's own vote, at 50.7%, reads the
    siblings' venues too), and a NEW walk appears (UK Old Market Hall Shrewsbury "Dracula (12A)" 11868 → none → 11868).
    Rejected: any share threshold is crossed both ways by arrivals (flip-unification (b1)).
  - **C**: contested = two films each own-backed at ≥ 2 venues (a count, so monotone in arrivals); such a cluster takes
    no film, majority nor vote. Breaks `IdentityResolverCasesSpec` "The Omen" (78 Cineworld venues' bare 1976
    re-release beside 2 + 4 credited siblings: right, lost). Rejected.
  - **C'** (tag `experiment/stage5-contested-cprime`): C's predicate, but only the majority is withheld; the own vote
    stays. The Omen passes and the Shrewsbury walk goes, but the Grinch still walks film → none → film through the vote.
  - **D** (head `2c77abc5f`): contested = two films each own-backed at ≥ max(2, the cluster's own venues) venues; such
    a cluster takes NO film. All specs green: IdentityResolverCasesSpec (Omen, Sense and Sensibility; the new Grinch
    population case fails at the base, 8871 → 360920 → 8871), every PopulationMonotonicity part (Grinch walk gone, no
    new walk), every drift-slice shard.
- **D measured** (what-if, prod mirror 2026-11-16 05:19, base 07bc7a6ba): CLEAN in all five countries (0 right-lost,
  0 right→wrong, 0 new-wrong); model takes moved: DE 5, UK 2, PL/US/ES 0; all unjudged, all film → none: DE
  Grinch ×4 (Ratzeburg, Beluga, Uetersen, Steinhaus; 8871), DE Royal Filmpalast "Die Tribute von Panem" (1300968), UK
  Plough Torrington "The Cat in the Hat" (1117898), UK Theatr Colwyn "Miracle on 34th Street (U)" (10510). Final: UK
  Colwyn moves 10510 → **11881** (the agreement stage fills the 1947 film), unjudged; the venue's page refuses us (403).
  CI measure, run 43814658448 (pinned recording; UK row decided, the rest cancelled after 90 min queued): **NOT
  LANDABLE**, UK lost 3 accepted takes: Old Market Hall Shrewsbury "Dracula (12A)" 11868 → none, Ilkley Cinema and
  Pavilions Teignmouth "Fallen Angels" 1702350 (the Coward relay, 96 venues) → none; final takes 0 moved. The very
  losses that closed the 11-05 variants: a relay or restoration billed bare beside a few venues stating a namesake.
- **Why parked:** D cures the flip but loses right takes. Colwyn's family is 29 of 33 venues for the
  1994 film against 2 venues stating 1947: D drops that clear majority because two venues' facts contest it, and the
  agreement stage then fills the minority film, a likely new wrong no judgement yet decides. The same predicate
  would drop prod's bare "Sense and Sensibility" (836 venues for 2026 against 9 for 1995). A monotone predicate (counts
  only) cannot tell a clear majority from a split; a share-aware one is crossed both ways by arrivals (A). The open
  question for the owner: is "unmatched beats a clear majority whenever ≥ 2 venues state another film" acceptable, with
  the agreement stage barred from re-filling a contested bare cluster's film?
- **Code:** annotated tags (pushed) `experiment/stage5-contested-majority-a`, `experiment/stage5-contested-cprime` and
  `experiment/stage5-contested-backing-d` (D also carries the Grinch population case in `IdentityResolverCasesSpec` and
  strikes the backlog row). The Grinch flip stays in `identity-flip-backlog.tsv`.
- **Next angle:** the losses are bare relay or restoration billings ("Fallen Angels" beside the Coward relay, "Dracula
  (12A)" beside the 4K restoration), where siblings' stated facts back a namesake at a few venues. A contested family
  that tells those apart needs the relay/event kind as a fact with provenance (flip-unification (d)), not venue counts.
