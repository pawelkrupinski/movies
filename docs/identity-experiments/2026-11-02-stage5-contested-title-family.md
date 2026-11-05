# 2026-11-02 — stage 5: a bare listing whose title family is contested takes no film — CLOSED 2026-11-05 (head `19a1a3d6551b`)

- **What:** `ClusterVoting.rivals` (branch `flip/stage5-contested`): a bare listing takes no film while a title-related
  sibling took ANOTHER film alone on its own facts. Three variants: strict (rival backed at more venues, `cb1940380ae9`),
  narrowed (rival of the same kind, readable by the bare's own title, stated by >= 2 sources, `49e21c357d45`), and an
  unmeasured "statements" variant (max of sources and sibling nodes, `19a1a3d6551b`, with the Beehive Honiton Addams
  drift slice).
- **Measured:** strict, measure 41430307403 (old base): removed the Addams population-walk flip but lost 6 judged right +
  34 unjudged (CinemaxX BTS "In São Paulo" x27, Fallen Angels x10, Shaun the Sheep Newton Stewart, Sinn und
  Sinnlichkeit Lumos, Shrewsbury "Dracula (12A)"). Narrowed, measure 41440533030 (5 countries): 0 takes moved, 0 cures,
  Addams flip still in the walk; `PopulationMonotonicitySpec` passed on both, curing no known flip (Titans x4,
  Torrington, Fishguard R&J, DCA These Violent Delights).
- **Why closed (review 2026-11-05):** every variant decides from how many sources or venues back a sibling's film, a
  population read the owner's rules exclude ("own facts plus TMDB"); the variant that cures the flip loses ~40 right
  takes, the one that loses nothing cures nothing. The flip class it targeted is now owned by the per-family convergence
  check (`FamilyConvergence`, which finds Addams-shaped A -> B -> A walks) and the held-take direction (owner approach
  2026-11-05 #1/#3: what stays bare is decided by the held take, not a per-case rule). Not worth a new angle: an own-fact
  version of "contested" is the namesake rule (`2026-11-04-namesake-ambiguous-*`).
- **Code:** local tag `experiment/stage5-contested` (`19a1a3d6551b`); branch `flip/stage5-contested` (unmerged, `git
  branch -d` refuses). Notes: scratchpad `stage5-notes.md` (2026-11-02).
