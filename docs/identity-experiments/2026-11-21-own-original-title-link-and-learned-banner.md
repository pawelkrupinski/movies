# 2026-11-21 — an original title links only to the OTHER title (PARKED); house-production needs a banner naming its house (LANDED)

- **Why:** the identity code review (review/identity) found `TitleRelation.sameOriginal` matching a node's original
  title against its OWN whole title, so a listing billing its title as its own original ("Dune" / "Dune") drew a tier-3
  `original-title` must-link to any title it shared a block key with.
- **What:** `a.originals(b.whole) || b.originals(a.whole)` (plus the shared-originals clause as before).
- **First what-if** (prod mirror, all five countries, 2026-11-21): 2 takes moved. DE Kino Central "Tronthaim: Berlin -
  Die Sinfonie" left tmdb:68594 (Klein's 1957 "Berlin - Ecke Schönhauser…", reached by `join:original-title`) for none:
  a cure (Ruttmann's 1927 film, tmdb:222, is the work). UK Lowther Pavilion "On Screen: La Bayadere" gained tmdb:142479,
  La Scala's 2006 "La Scala Ballet: La Bayadère", judged wrong for every other Bayadère relay: a new wrong take, so the
  change alone was not landable.
- **Why Lowther took it:** `house-production` (`StageRelays.billsUnderItsHouse`) accepted the record because Lowther's
  programme banner "On Screen" was LEARNED as La Scala Ballet (`BannerHouses`: its works' searches find La Scala's
  records), a banner sharing no word with the house and abbreviating none of it.
- **The owner's choice (a), 2026-11-21:** a guard so a relay billing cannot take that record, as a general rule:
  `namesTheHouse` — house-production now needs the banner to spell the house, share or contain one of its words (3+
  letters: "NT Live" / National Theatre Live, "MetOpera" / the Metropolitan Opera), or abbreviate it ("RBO"). The
  learned map still decides which house a naming banner is; `BannerHouses.other` (another house's denial) is unchanged.
- **Second what-if, both changes** (all five countries, whole, final): 2 moved, both cures, 0 new wrong, 0 right lost —
  the DE Tronthaim cure kept, the Bayadère gain gone, and UK Lowther "On Screen: Notre-Dame De Paris" (prod's take now)
  leaves La Scala's 2013 record (tmdb:790330, judged wrong at every other Paris Notre-Dame relay) for none. The three
  takes are judged in `identity-judgements.tsv` (AGENT-JUDGED 2026-11-21).
- **Not tried:** honouring the `*` judgements of one relay family in the resolver (the judgements are gate data and the
  projection's withdrawal, by exact billing): the banner guard cures the class without reading them.

## Reverted (2026-11-21, Main #2806-#2808)

The `TitleRelation.sameOriginal` half alone added a new A -> B -> A take along an arrival chain in the committed slice
`identity-slices/uk-keswick-alhambra-ntlive-hayfever-5cf7064a` (PopulationMonotonicityPart2Spec): Lichfield Garrick's
"NT Encore - Hay Fever" and Regal Fordingbridge's "NT Recorded Live: Hay Fever" go none -> tmdb:1791571 -> none. With the
parent's `TitleRelation.scala` the slice passes; with the parent's `StageRelays.scala` alone it still fails. The whole
commit (604134d1a) was reverted -- the banner guard only compensated for the link change -- so its two cures wait on a
version of the original-title link that keeps the walk monotone. Reproducer:
`scripts/sbt-warm 'worker/testOnly services.identity.PopulationMonotonicityPart2Spec -- -z "uk-keswick-alhambra-ntlive-hayfever"'`.

## Relanded: the banner guard alone; the link change parked (2026-11-21)

Each half measured alone on the reverted base: `TitleRelation.sameOriginal` alone fails the Hay Fever slice (the A -> B
-> A above); `StageRelays.namesTheHouse` alone passes PopulationMonotonicityPart1-4Spec, every drift slice
(`scripts.IdentityDriftSlices*`), IdentitySliceCasesSpec and MatchedTakesLabelSpec. The guard is what cured UK Lowther
"On Screen: Notre-Dame De Paris" (tmdb:790330), so it relands alone with that cure; DE Kino Central's "Tronthaim:
Berlin - Die Sinfonie" (tmdb:68594, by `join:original-title`) waits on the link change. Why the link change walks
A -> B -> A was not found: the self-original links had glued the Hay Fever relay spellings at tier 3, and without them
"NT Encore" and "NT Recorded Live: Hay Fever" join the NT Live cluster only while the listings bridging them are present.
A version keeping the walk monotone needs that bridge understood first (the slice above, `--orders`); the plain fix is
kept as the reverted 604134d1a.
