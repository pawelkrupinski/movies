# 2026-11-10 — a piece-named node its own search ranks a namesake above the film takes none (parked)

**Aim.** Cure the population flip of UK Lighthouse Poole's "PFF: Hen - Cert 15" (the `population` row of
`identity-flip-backlog.tsv`, slice `identity-slices/uk-mowlem-theatre-purbeck-film-festival-hen-736301b7`): its model
take goes none@{Poole} -> tmdb:1382513@{Poole, Mwldan} -> none@{Poole, Mowlem, Mwldan}. The owner's decision of
2026-11-04 (`accepted-right-loss` in `identity-judgements.tsv`) is that it takes none: it bills no year, director,
runtime or id of its own.

**Tried 1** (`experiment/pff-hen-piece-outranked`, ported from the abandoned `cure-uk-pff-hen` WIP 3a3ff318ef):
`Corroboration.refused` also disputes a node whose title names the film taken only by a piece (`naming` tier 1:
segment / decorated) when its OWN search ranks an undenied namesake above that film -- the node's own ranks decide, no
population count. It cures the slice (PopulationMonotonicitySpec fails before with the row removed, passes after), but
breaks 4 pinned right takes in `IdentityResolverCasesSpec`: "A decorated spelling should take the film its plain
siblings' own evidence matched" (PL Lalka 2026), "A spelling a learned venue decoration wraps ... corroborated by the
venues listing the title it wraps" (Mistyczka), "A house spelled unlike its records" (NT Live: The Importance of Being
Earnest), "A listing that publishes only its title should follow its title family's clear majority film" (Sense and
Sensibility 2026). Each is a decorated spelling whose own search ranks an older namesake first and which rightly takes
the film its siblings' own facts name. "PFF: Hen" is the same shape; only the family's size and its siblings'
evidence tell it apart, and that is population again.

**Tried 2** (`archive/baseline-23780-banner-segment`, `ReleaseVeto.ofBanner`, ported unchanged onto 354b7c1587; it
applies cleanly): a banner segment's namesake with no release in the country is vetoed beside one released there in
the screening season. It does NOT cure the slice: Poole still flips (now to none@{all four}), and Mowlem's "Purbeck
Film Festival - Hen" gains a none -> film -> none flip of its own.

**Measured** (identity-what-if of Tried 1, base 354b7c1587, head 751d89957c, mirror snapshot 2026-11-10 ~15:10-15:30Z;
the vault was locked, so answers came from the local what-if store alone):

| | model | final |
|---|---|---|
| UK | right-lost 1 (Hebden Bridge "COSI FAN TUTTE Royal Opera" 1702775 -> none), unjudged 6 | right-lost 1 (same), unjudged 10 |
| PL | right-lost 2 (Kosmos "KF Ambasada: Diably" 31767, Swiatowid "OPETANIE + TRASA PLAKATOWA" 21484 -> none), unjudged 15 | right-lost 1 (Opetanie), unjudged 6 |
| DE | unjudged 7 | unjudged 7 |
| ES, US | 0 moved | 0 moved |

No right->wrong or new-wrong, but every right loss is a banner/decorated spelling the same mechanism serves rightly,
for one flip cured.

**Verdict.** Parked. The flip is the borrowed-film mechanism every decorated spelling relies on (a bare node takes the
film its siblings' facts name); a node-local rule that removes it for "PFF: Hen" removes it for Lalka and Sense and
Sensibility too. The row stays in the backlog. A new angle would need a fact the Hen family lacks and the four pinned
cases have (e.g. a sibling that names the film WHOLE with a year or director, which Mowlem's banner-segment billing
does not) -- but that still reads a sibling, so it must be walk-proved monotone (PopulationMonotonicitySpec) first.
