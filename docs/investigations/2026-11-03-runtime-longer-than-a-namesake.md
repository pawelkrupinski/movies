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
