# 2026-10-28 — a sibling taken alone withdraws a bare take its title reads only by another title — PARKED, it withdraws a take the venue's own feed links (pinned 573, base `86d10e36f`)

- **What:** `Families.siblingDenying` let only a title-linked sibling taken by NO rule alone withdraw a node's alone take.
  The experiment lets a sibling taken alone withdraw it too, when the node's title names the sibling's film exactly, the
  node's take reads the title otherwise (another country's title, the original one), no other venue corroborates that
  take, and the sibling's denial compared a fact the node did not publish. Written by red fixer #2375 for convergence's
  DE seed-1 drift flip (CinemaxX's bare "Titans" ×3 taking "Remember the Titans", whose Danish title it is, whenever
  Luxor's untaken sibling was absent): `docs/investigations/2026-10-28-drift-flips-bare-listings-de.md`.
- **Measured** (red fixer #2432, recording 573): the drift replay's DE seed 1, locally: the Titans flips 3 -> 0.
  `scripts/identity-measure-local --base 86d10e36f --head e9de7bf54` over de, pl, uk and us: two model takes move, uk
  and us none.
  - DE Metro-Kino im Schloßhof's "Billie" 649137 -> 1544034: a likely NEW WRONG take. The listing runs 96′ and its
    Cineamo link names 649137, the 2020 Billie Holiday documentary; 1544034 is Sheri Hagen's 108′ "Billie" (2026),
    which Hansa Kino bills with her credit.
  - PL Kino ODEON's "Amelia" 8915 -> 194 ("Amélie", "Amelia" in Polish; DKF Zamek bills Jeunet's that week): likely
    right, unjudged.
- **Why parked:** it trades a flip for a wrong take, and wrong is worse than missing. Next: keep a take the node's own
  feed links (with no overrule setting the link aside) out of the withdrawal (CinemaxX's bare node links 869152, the
  right film, so its withdrawal of 10637 stands), judge Kino ODEON's "Amelia", measure again, and replay DE seed 1.
- **Code:** tag `experiment/drift-flip-titans-sibling-withdraws` (`e9de7bf54`): `Families`, `ClusterVoting`,
  `ResolverDecisions`, an `IdentityResolverCasesSpec` case, the pinned slices `de-werkstattkino-e-v-camouflage` and
  `es-bahia-mar-escape-room-2` (re-pinned to the 2026 film it then takes), and a judgement row for Werkstattkino's
  "CAMOUFLAGE".
