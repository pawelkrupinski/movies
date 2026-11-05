# 2026-11-05 — approach B: a namesake listing takes the work TMDB prominence leads by a margin — PARKED, no flip-safe margin recovers a served loss (head `5c4308469c94`)

- **What:** `NamesakeAmbiguity.prominent`: take a work whose TMDB popularity bucket leads every other by
  `ProminenceMargin` (margin 6 buckets, i.e. >= 64x popularity, in the wip commit); ported per-listing reading from the
  rework, no window. Base `b030bb899e2` rebased as `1edd9a713847`.
- **Measured:** offline sort of 1324 listing rows (2f86 served 50, model 574, right->wrong 9, new-wrong 1; b030 served 48,
  model 642): recording popularity (run 41775760543), live TMDB popularity and vote_count (5 Nov), mirror `tmdb_films`;
  flip slices read with the branch's resolver. No CI dispatched. Constraint: the identity store keeps popularity only as a
  log2 bucket (`PopularityBucket`); vote_count is stored nowhere (`TmdbNormalizer.minimal` drops it).
- **Result:** the flip listings' popularity lead is Titans 3 (Remember the Titans vs Titans 2026), ICA/Woking "The Devils" 3,
  Dracula 2, R&J 1, Cosi 2, so a margin <= 3 decides them; the margin must be >= 4. Casablanca (Kamp-Lintfort), Bad Apples
  and Metropolis lead by exactly 3. Recovery (served/wrong): m=3 5-7/0; m=4 served 0, model 71/19; m=5 model 31/0 on the
  recording but 32/17 wrong on the live numbers a day later (drift 0 -> 17 wrong in one day); m=6 stable: model ~23 recovered,
  0 wrong, 0 served, no flip listing decided. Vote counts (log2): m=5 served 10/0 but model 99/49 wrong; m=8 still 59/19 wrong
  (new releases lose to an older namesake with votes); Titans 3127 vs 0 votes takes Remember the Titans at any margin.
  The 9 right->wrong revivals had a popularity lead 0-3 and a vote ratio 3x-811x.
- **Why parked:** the only drift-stable margin (6) recovers 0 of the 50 / 48 served losses; the margins that would
  recover them either decide the Titans flips or pick wrong from one day's numbers. Popularity is a moving population
  read itself, so it conflicts with the owner's held-takes direction.
- **Code:** local tag `experiment/namesake-prominence` (`5c4308469c94`); branch `identity/namesake-prominence` kept
  (`git branch -d` refuses: unmerged into main).
