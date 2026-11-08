# 2026-11-08: a cluster's members withdrawn from a film bring it no vote

Written after the fact for d71b2d61a9 ("Identity: a cluster's members withdrawn from a film bring it no vote",
`identity/src/main/scala/services/identity/ClusterVoting.scala`, `ClusterVotingSpec`). That commit changed a resolver
rule but landed without a design note or a full-corpus measurement. This entry is both.

## What the rule decides

`ClusterVoting` is the GROUP vote for clusters in which no member matched a film alone: the cluster votes on its
members' pooled evidence (`votedFor`). `onlyWithdrawn(cluster, film)` vetoes that vote for a film the cluster's members
were WITHDRAWN from. A node is withdrawn from `film` (`withdrawnFrom`) when:

- it took `film` alone (`scope.takenAlone`);
- it is not the family's own best match for it (`families.bestOf`);
- its own feed does not link it (`FeedLink`);
- a sibling's billing denied that it is the work billed (`Denial.Kind.NotTheWork`, for example a screen namesake of the
  work the sibling's stage house bills), and `Authority.spares` does not spare it.

Withdrawn nodes are sent to the group vote so that the sibling's denial still holds there. The solver may split the
denying sibling off, though, and the cluster left behind could then vote the withdrawn film straight back.

**Before d71b2d61a9** the veto held only when EVERY fact-stating member was withdrawn
(`stating.forall(withdrawn)`). Members publishing no fact were left out, as they still are (drift slice
`dammtor-titans-0bd19a4c`). A cluster that also held one fact-stating member that took nothing alone was not vetoed. It
voted on the whole cluster's pool, and the withdrawn members' evidence was in that pool.

**After d71b2d61a9:**

- When every stating member is withdrawn, the veto holds as before.
- When only some of them are, the rest (the cluster without the withdrawn members) are pooled on their own. The veto
  holds unless that pool, through `acceptance.pooled`, accepts `film` by itself.
- When none is withdrawn, there is no veto.

`keptByASibling` still lifts the veto when a family member keeps the film by its own match.

## Why a withdrawn member's evidence must not vote

A withdrawal is a sibling's statement that this member's own evidence for the film is a namesake, not the work. Pooling
that evidence back in lets the cluster count a match the family has already refuted. The members that took nothing
alone add their facts (runtimes, days) to a pool whose winner came from the withdrawn take. The vote then reflects
whatever the withdrawn members brought, not anything the other members show. The rule keeps the withdrawal and still
lets the other members win the film when their own evidence is enough.

## The case: UK "Notre-Dame de Paris" (prod mirror 2026-11-07)

The Paris Opera Ballet's 2026/27 relay of Roland Petit's "Notre-Dame de Paris" (14 Mar 2027) is billed by:

- **Film Theatre Leiston**, "Ballet - Notre-Dame de Paris": the ballet billing that denies the 1956 film as a screen
  namesake.
- **Magic Lantern Tywyn**, "Notre-Dame De Paris" [107′]: took The Hunchback of Notre Dame (1956, tmdb:11594) alone.
  Leiston's billing withdrew it.
- **Riverside Theatre Woodbridge**, bare "Notre-Dame de Paris", no facts.
- **Plough Arts Centre Torrington** [90′, 24 Apr 2027] and **Quad Derby** "Notre-Dame De Paris (Cert. TBC)" [125′]:
  neither took anything alone.

Prod's solver put Magic Lantern, Riverside, Plough and Quad in one cluster. Plough and Quad state facts and were not
withdrawn, so the old veto did not hold. The four voted tmdb:11594 straight back, with Magic Lantern's withdrawn take in
their pool. Prod served the 1956 film at Magic Lantern and Riverside (judged wrong, known-wrong `served`, 10-18). Plough
and Quad were judged wrong for tmdb:11594 in the same commit (`AGENT-JUDGED 2026-11-07 (A2 withdrawn-vote)`).

No recording reproduces prod's split, so `ClusterVotingSpec` builds that cluster directly. It checks the premise
(Magic Lantern took 11594 alone and is not `bestOf`, and Plough and Quad took nothing alone), then checks that the vote
does not contain 11594.

## Measured

`scripts/identity-measure --base d71b2d61a9^ --head d71b2d61a9` (full measure, every country the footprint reaches;
the change is shared resolver code, so all five). Run
[42491109614](https://github.com/pawelkrupinski/movies/actions/runs/42491109614), recording 573 (the pinned pair).
Base 977d57bba7, head d71b2d61a9. Classified with main's `identity-judgements.tsv` of 2026-11-08.

| cc | listings | model takes | final takes |
|---|---|---|---|
| pl | 10804 | 0 moved | 0 moved |
| de | 34210 | 0 moved (lost 0, changed 0, gained 0; 0 new suspects, 13 standing) | not replayed (below) |
| uk | 36649 | 0 moved | 0 moved |
| us | 97570 | 0 moved | 0 moved |
| es | 5933 | 0 moved | 0 moved |

No take moved in any country, so there is no right-lost, right-to-wrong, new-wrong or unjudged take to judge. That
matches the commit's note that no recording reproduces prod's split: the rule fires only on a cluster the recording's
solver never forms. The rule's one known case is the prod-mirror Notre-Dame cluster above, pinned by `ClusterVotingSpec`
and judged in `identity-judgements.tsv`:

- Plough Torrington and Quad Derby are `wrong`, `open` for tmdb:11594.
- Magic Lantern Tywyn and Riverside Woodbridge are `wrong`, `known-wrong` (`served`) for tmdb:11594. Strike them once
  prod's projection serves them none.

The measure's verdict is NOT LANDABLE, for DE only and not for anything this change does. The DE final takes could not
be replayed on EITHER side. `UnmatchedClustersRatchet` stopped at `java.lang.RuntimeException: no venue named Kino in
Dorfen` (`ListingBson.of`, reading recording 573's stored unmatched clusters) in the base's ratchet log and in the
head's. Recording 573 still names a "Kino" in Dorfen that the roster at 977d57bba (and at d71b2d61a9) lacks, so the
DE final half has no result on either side. A change that moves no DE model take is not expected to move a DE final take, but this run does not prove it.

What-if could not run first. `scripts/identity-what-if` overlays main's `WhatIfAnswers.scala`, which needs testkit's
`RefusingLookups` (ff8fa4f9d0, landed after this commit), so the base ref did not compile. The CI measure was therefore
dispatched with `--no-what-if`.
