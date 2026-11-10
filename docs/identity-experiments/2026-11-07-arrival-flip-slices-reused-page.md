# 2026-11-07 — five arrival-flip drift slices and "a reused page's denial withdraws no sibling" — SUPERSEDED (the fix and three slices landed); Hochland's Olympia slice PARKED, still flipping on main

- **What:** WIP branch `arrival-flips-wip` (2026-11-07): drift slices of five families the family explorer found
  flipping as listings arrive (recording 573 with its fills), and the `Families.siblingDenying` change that a sibling
  whose page reads as REUSED (`readsAsReusedIn`) withdraws no one (DE Abaton's "De Gaulle Zeit des Widerstands").
- **Landed since, from other commits:** the Families change, byte for byte, with the Abaton slice (`4bb80014f2`); the
  Bambi "Les Misérables" slice (`7aea8ee50e`, a billing crediting a director borrows no film another director made); the
  Leiston "Disney Jr Cinema Club 2026" slice (`3524e8269d`, re-cut in `137dfa02a6`, the edition-year rule).
- **Re-walked on main `d974d81054`** (2026-11-10, `PopulationMonotonicityPart*Spec`, `FamilyConvergenceSlicesSpec`, the
  two remaining slices dropped into the tree): both answer their lattice.
  - ES Artesiete Las Terrazas "Stray Kids World Tour <Run It> (Diferido)": no A -> B -> A. Not committed: it witnesses
    no flip on main (nor a fix), and costs ~0.5 MB of fixtures.
  - DE Hochland-Kino "Olympia 1936 / 2026 - Leni Riefenstahl: OLYMPIA 1 - FEST DER VÖLKER" STILL FLIPS, model, lattice
    2^4: tmdb:677 (Olympia 1, right) -> tmdb:236353 ("Der große Sprung", 1927) -> tmdb:677; and the same series'
    "Premiere: Kurzfilm AUSGEGRENZT" none -> 236353 -> none. The venue bills a programme, "Olympia 1936 / 2026 - <film>",
    four listings (DER GROSSE SPRUNG 112′, OLYMPIA 1, the short AUSGEGRENZT, RIEFENSTAHL 116′); the shared programme
    prefix ties them, so with DER GROSSE SPRUNG present and RIEFENSTAHL absent its film is pooled onto its siblings.
    Not the reused-page mechanism: the landed Families change does not cure it.
- **Why parked:** the Hochland slice cannot land red (PopulationMonotonicitySpec fails a new flip; a `population` backlog
  row needs the owner's OK), and its fix is identity design, not this branch's: the likely angle is a programme-prefix
  title rule for "Olympia 1936 / 2026 - " (a `programmePrefix` row of `title-rules.tsv`, `scripts/title-trace` to see
  it fire, `identity-measure --footprint` to measure), or a family rule that a shared programme prefix alone does not
  link siblings for pooling. Either is measured whole-corpus before landing. Re-cut the slice with main's cutter
  (`scripts/identity-slice de --venue Hochland --title "OLYMPIA 1" --emit-drift-slice`) rather than reusing the tag's.
- **Code:** tag `experiment/arrival-flips-wip` (`68643d2313`): all five slices as cut on 11-07 and the Families change.
