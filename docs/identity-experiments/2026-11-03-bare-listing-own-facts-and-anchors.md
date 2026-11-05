# 2026-11-03 — bare listings take no borrowed film: own-fact singling out, then anchored unanimity — CLOSED 2026-11-05, merged into the namesake / held-take line (head `025aa73ccbdc`)

- **What (branch `identity/bare-borrow`):** a listing that states nothing of its own (bare) takes no film its family
  lends it. Phase 1 (`d3432fc5a3`, tag `experiment/bare-listing-own-title`): bare -> no film. Phase 2 (`d7b37ae8a7a`,
  `ea53b48a16a`): `BareListing.singledOut` narrows the TMDB namesakes by a stated original title, a release in the
  venue's country and the screening season. Phase 3 (`c9dd40f16f7`, `025aa73ccbd`): anchored unanimity -- take the film
  of a pinned / feed-linked / own-fact-matching sibling in the same country and season, only where its facts contradict
  every other plausible work.
- **Measured:** 41548110532 (phase 1): R>W 0, served right-lost 35 (pl 7, de 21, uk 7), model lost 1390, cures 2.
  41581998806 / 41605092315 (phase 2): served 35 unchanged, model R>W 7 then 0. 41608397725 / 41629344976 (phase 3):
  model right-lost pl 113 / de 45 / uk 59, model R>W 1 (Pershore "Royal Ballet: The Nutcracker" 1702761 -> 746190:
  the anchor's pool lacked the bare listing's 2026/27 record), served still 35. ~27 of the 35 are genuine own-fact
  near-ties (Hanussen 1955/1988, Gavagai, Birthday Party x5, The Store, Turksib, Luger, Lalka/Dolly) with no anchor.
  Drift 41629386910 red UK seed 3 Lanternhouse "Radiohead X Nosferatu" 653 -> none -> 653: never attributed, possibly
  the shared-rng harness artefact fixed in 80b59d0da8e1 (DRIFT-ARTEFACT-AUDIT #8). 38 identity unit tests left
  un-updated (design reversals).
- **Why closed (review 2026-11-05):** (1) the served losses did not move across four variants: the cost is the class
  itself, not the variant; (2) the anchor reads a sibling's presence in the same season -- a population read the
  owner's rules exclude; (3) the general form of the rule is the namesake rule (`2026-11-04-namesake-ambiguous-*`: a
  listing whose own facts cannot tell TMDB's namesakes apart), which every bare listing falls under, and the owner's
  approach of 2026-11-05 (#3) moves the "what stays bare" decision to the held take. Kinoheld's second-feed facts
  (landed 11-05) separate only 3 of the 30 DE served losses (Bad Apples x2, Broken Circle), so new own facts do not
  reopen it. Pursue the class under the namesake / held-take line, not here.
- **Code:** local tags `experiment/bare-listing-own-title` (`b66eea37a784`, phase 1) and `experiment/bare-listing-anchored`
  (`025aa73ccbdc`, phases 2-3); branch `identity/bare-borrow` (unmerged). A stash on it holds only regenerated fixtures.
