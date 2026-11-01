# A kept listing never joins a present listing whose feed link the present sources overrule (2026-11-01)

The owner chose option "B" on 2026-11-01: "a kept listing may not join a cluster whose feed link is overruled toward
another film". The Madoka-style help stays. There, kept dated facts keep a bare present listing on its film when
nothing overrules it.

## The case: DE "Eternity" (prod-latest 41265286306)

- Cineamo links the bare "Eternity" [2025] at 18 venues to David Freyne's 2025 romcom (1259102m), copying its year.
  Those venues screen Susanne Regina Meures' 2026 *Eternity* (TMDB 1769681).
- Two other sources date that film 2026 and credit Meures: Zazie's listing, and Caligari's kinoheld page
  `/film/eternity-2026`. Without a kept listing, Cineamo's link is overruled ("2 other source(s) back 1769681 and none
  backs 1259102"), and the 18 take none.
- Caligari's kinoheld listing of Freyne's film [2025] {David Freyne} (`/film/eternity`) left its board on 2026-10-31
  10:43Z. It is KEPT (`ListingRetention`) because "Eternity" is still billed.
- Kept, it counted as a source backing 1259102, so the link was no longer overruled. Its year agrees and the 18 state
  no director, so it joined their cluster. All 18 took 1259102, in production from 10:49Z and in every row replaying
  the kept set.

The owner's first idea was to compare stated year and director. It could not tell these apart: the 18 state the year
Cineamo copied, which agrees with the kept listing, and they state no director. The contradiction came from the other
present sources.

## The rule

- `Listing.kept` is an input fact, set by `ListingRetention.kept`. It is inside equality and hash, and `ListingBson`
  carries it. It is not state: the set is the one `ListingRetention` already computes.
- The overrule is the present sources' to say. `FamilyScope.overrule` counts only present listings' backing
  (`VenueBacking.sources` and `corroborating`, `presentOnly`; `Backer.Kept`). It also reads the link's own claim off the
  node's present listings.
- `ConstraintEdges.KeptOverruledApart` is a cannot-link. It keeps a node of kept listings apart from a node whose feed
  link the present sources overrule.
- A kept listing still backs its film and corroborates wherever nothing overrules a link. That is the Madoka case.

## Proof

- `IdentityResolverCasesSpec`, "A kept listing should never join a present listing whose feed link…": on main the
  linked listings take 1259102. Now they take none, and the kept Freyne listing keeps 1259102.
- `IdentityDriftSpec`, "a bare title beside a kept dated listing of its film": the Madoka drift slice. Filmhaus
  (Langerringen) leaves, kept and marked, and Metropol stays on 212162.
- `KeptOverruledApartSpec` checks two properties: over 32 seeds no overruled present listing takes the kept listing's
  film; and the incremental model, told in shuffled batches, decides as the whole resolve does.

## Measurement

- **The fix alone**, on the pinned recordings (identity-measure run 41321711292, all five countries): no take moved and
  LANDABLE. No recording holds kept listings yet.
- **Over the kept input** (run 41323419852, prod-latest 41265286306 with prod's kept set captured beside it): the base
  is the kept input on main, and the head adds this fix. Only DE moved: the 18 "Eternity" listings went from
  1259102 (Freyne, wrong) to none. Meures' film is 1769681, and none is not wrong. There were no other moves in any
  country and no new suspect. LANDABLE.
- **Drift** (`identity-drift-ci --against main`, run 41321787149): both seeds in all five countries, DRIFT CLEAN.
