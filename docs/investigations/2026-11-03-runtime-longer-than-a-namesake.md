# 2026-11-03 — a venue's runtime LONGER than a namesake's does not tell them apart

**Owner's decision (red #792):** keep 0f68213beda; do not widen sibling denial; decide Mowlem's "Purbeck Film
Festival - Hen" from its OWN facts, or leave it unmatched -- a mechanism, not a special case; full measure right->wrong 0.

## The case
Purbeck's listing states 110 minutes and nothing else (no year, director, text, original title). The namesakes:
tmdb 1534155 "Hen" (2025, Afrikaans, 108′, released in South Africa only) and tmdb 1382513 "Hen" (2026, György
Pálfi, 96′, UK release 2026-02-20) -- the film judged right, taken on their own facts by two other nodes of the family.
`Corroboration.facts` counted 110′ as a *discriminating runtime* for 1534155: within `RuntimeBand` (10) of 108, and
Pálfi's 96 outside it. So the node was "corroborated", the uncorroborated-namesake refusal never applied, and it took
the 2025 film (OwnMatch 38.3%) once 0f68213beda left Keswick's sibling denial no longer withdrawing it.

## The rule
A namesake FITS the venue's running time when the venue's minutes are within `RuntimeBand` of it **or longer**: a slot
runs the film and more (adverts, trailers, an introduction) -- the doctrine `Agreement.runsShort` and
`SuspectTakes.runtime` already apply ("longer says nothing"). A runtime corroborates a film only when it fits that film
and no namesake. The node's own fit is unchanged (within the band). 110′ fits both "Hen"s, so it tells neither apart:
Purbeck is an uncorroborated namesake take beside a film two nodes take on their own facts, and the model takes none;
the agreement then takes Pálfi's film by the venue's own poster (2 bits, no other candidate within 4).

No population dependence is added: the facts read are the listing's own runtime and the candidates' records; the
existing `support` count decides only as it did before.

## Measured
`scripts/identity-measure --all-countries`, run 41609683835, base `d7b0f8493` vs head `12e168682`, pinned recording 573:
PL, DE, UK, US, ES -- 0 lost, 0 right->wrong, 0 new wrong; the one move is UK model: Purbeck "Hen" tmdb:1534155 -> none
(a cure); every country's final takes unchanged. Pinned: `identity-slices/uk-mowlem-theatre-purbeck-film-festival-hen-*`
(IdentitySliceCasesSpec: fails before, passes after); drift slices (IdentityDriftSlicesSpec) all clean.

## 2026-11-04 — a node's own facts stand for its film whatever the solver joined it to (Main #2128, convergence #834)

**Owner's decision:** keep 84cba51ce03 (the solver's ambiguity kept across tiers; Torrington "Dracula" cured); the Hen
family takes Pálfi's film from its OWN facts, else unmatched; no arrival-order flip.

After 84cba51ce03 Keswick's "KFC Presents: Hen" {György Pálfi} reaches two components in a stronger tier (Mwldan's
"Hen", Pálfi's film, and Mowlem's bare "Purbeck Film Festival - Hen", the Afrikaans one by search rank) and stays alone
in the weaker title-segment tier that used to join it to Mwldan. Alone its credit is below the model's rating cut, so its
cluster takes no film -- and `Corroboration.support` counted a node only for the film its CLUSTER took. Pálfi's film fell
to one node on its own facts, under `EvidencedNamesake`, the uncorroborated-namesake refusal no longer applied, and
Purbeck took the 2025 film (judged wrong).

**The rule.** A node of a cluster that took no film still counts for the one film its title names that a fact of its own
corroborates (`ownlyNamed`; none when its facts corroborate no namesake or several). The facts are the node's own; only
how the solver grouped them no longer decides whether they count. Keswick's director credit counts for 1382513 again,
Purbeck's own facts (110′ fits both Hens) tell neither apart, the refusal applies, and the agreement takes Pálfi's film
by Purbeck's own poster. Lighthouse Poole's "PFF: Hen" has no fact of its own and the solver leaves it apart: none.
Keswick (below the cut alone), Poole and Torrington "Dracula" lose takes judged right to 84cba51ce03 -- recorded
`accepted-right-loss` under this decision, for the owner to confirm.

Pinned: `identity-slices/uk-mowlem-theatre-purbeck-film-festival-hen-*` re-pinned (it lacked the answers the new
solver's lookups read); IdentitySliceCasesSpec and its population walk pass; slice --orders 24 / --modes 12 clean.
