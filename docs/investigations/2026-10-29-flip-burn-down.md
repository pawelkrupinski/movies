# 2026-10-29 — The known-flips burn-down

The owner, 2026-10-29: "fail build only on new flips, focus in eliminating all old flips and then fail on all flips".
The drift gate passes a flip named in `test/resources/fixtures/identity-flips-baseline.tsv` (8e04674aa) and fails every
other one. That file only shrinks. This notebook lists each baselined flip, the incremental mechanism behind it, its
fix and its status. A fix removes its rows from the baseline in the same commit. When the file has no rows left, the
gate is strict again.

Before the baseline existed, five flips had already been cleared by mechanism fixes (the corrections budget and the
inline ask, 8a50e6d5d and 5ca8d38f8). Two more were cleared by rule fixes, each measured on the full corpus (Titans
5f8b24cc3, CAMOUFLAGE ecedac9aa). Details: `2026-10-28-drift-flips-bare-listings-de.md`.

## The baseline (8e04674aa, from convergence #2527, run 3757)

| Country | Seed | Listing | Takes | Mechanism | Status |
|---|---|---|---|---|---|
| uk | 1 | Duke of York's Picturehouse Brighton, "Rose (2023)" | 901251 → 1069112 → 901251 | An input still waiting for its answer: a search's hit weighed before its record is filed (below) | **FIXED: agree or none** (rows removed) |
| uk | 1 | Queen's Film Theatre Belfast, "Rose (2023)" | the same | the same flicks.co.uk node (Grosvenor Cinema Glasgow is the third listing) | the same |

## UK "Rose (2023)": the record-awaited fix, measured, and why it did not land

The diagnosis is red fixer #2527's (branch `red-2527`, commit d51dc845f, with
`docs/investigations/2026-10-29-drift-flip-record-awaited.md` on that branch). The fill files the search
`Rose (2023)` at step 2, but "Rose Parade 2023" (1069112)'s record only at step 8. Between those steps the parade wins
on its search hit alone, and only its record (a 90′ US broadcast) denies it. The model equals a whole resolve of
what is filed at every step. So this is no cache, lag or restart key: the interim take is a faithful function of an
interim input.

d51dc845f, "a film is weighed once its record is filed": `FamilyScope.pool` leaves out a candidate whose record is
`Unknown`. CI on that tree, recording 573:

- **Measure 3812** (every country, whole pinned pair, base 4f19e4eeb): 0 model and 0 final takes moved in pl, de, uk,
  us and es. LANDABLE.
- **Drift 3810** (all 10 rows): it cures UK seed 1 (Rose: 0 flips) but adds **two new flips**, both of which are 0 on
  its parent 4f19e4eeb in main's run 3811 with the same fills:
  - DE seed 1, Metropol-Kino-Center "Madoka Magica - Der Film: Rebellion": 212162 [boot] → 42997 [2 fill, PooledMatch]
    → 212162 [4 scrape, Filmhaus (Langerringen)'s listing joins];
  - UK seed 2, Chichester Cinema at New Park "The Ties That Bind Us": 1096642 [boot] → 1460982 [8 fill+answer] →
    1096642 [14 settle, corrected by families and poster].
- Unit tests: `identity/Test/test` passes (1136), and so does `worker/Test/test` once the GitOps manifests are fetched.

### What the slices show (the take for every subset of the seed's gated answers filed, at d51dc845f)

| Slice (recording 573) | Everything filed | The rival's record held (excluded by the fix) |
|---|---|---|
| Metropol's "Madoka" alone (before Filmhaus arrives), seed 1 | 42997, "Rebellion" (1962) | 212162 (when f:42997 is held) |
| Metropol with Filmhaus | 212162 | 212162 |
| Chichester "The Ties That Bind Us", seed 2 | model 1460982, corrected to 1096642 | model 1096642 (when f:1460982 is held) |
| flicks.co.uk "Rose (2023)", seed 1 | 901251 | **none** (when f:901251 is held) |

Both new flips come from the exclusion itself, not from a pool it missed. The Madoka move is in the family's TMDB pool:
with 42997's record held, the pool holds 212162 alone. So making the rule uniform (the fallback pool, PooledMatch)
cannot clear them. Before the fix, an awaited candidate was weighed on its hits, and those hits read the same way as
its record does here, so no take moved. With the fix, leaving the candidate out creates a third interim state. It is
right for Rose, but for Madoka and Chichester it is a take the filed state then leaves. The exclusion also opens
"901251 → none → 901251" for Rose wherever 901251's own record is asked late.

Chichester is not a separate correction lag. The model's take at step 8 is the whole resolve of what is filed then.
The agreement's correction of it rests on family answers (wiki, poster) that the seed's gate files at settle.

### The decision this needs (identity design, not a red fix)

A pending answer forces a choice about what the model reads in its place. Each choice is a resolver rule, and each
moves interim takes differently:

1. **Weigh the hit as if its record agreed** (main today). Rose flips; Madoka and Chichester do not.
2. **Leave the candidate out until its record is filed** (d51dc845f). Rose is cured; Madoka and Chichester flip; final
   takes are unchanged everywhere.
3. **Decide only what both readings agree on, else none.** By the tables above: Madoka none → 42997 → 212162 and
   Chichester none → 1460982 → 1096642, so neither flips; Rose 901251 → none at step 2 → 901251 is still a flip through
   none, unless its boot state is pending too. Not built. In production it costs listings shown unmatched while a
   record is pending.
4. **Keep Rose on the baseline** as an interim-input flip the model cannot avoid without a rule, and decide whether the
   flip predicate should count a move whose interim take is the whole resolve of the then-filed inputs.

The full-corpus drift run of any of these is about 45 min on CI (`scripts/identity-drift-ci --seeds both`), and the
final-take measure is `scripts/identity-measure --all-countries`. Until the owner chooses, the two Rose rows stay on
the baseline. d51dc845f must not land: its land item `20261029-192949-movies-wt-red-2527-161583`, queued by the stopped
fixer with `Fixes-Red: 2527`, would add two new flips.

## The owner's decision and the fix: AGREE OR NONE (landed with the rows' removal)

The owner chose option 3. While a candidate's record is pending, a take only where the pool with the pending films and
the pool without them agree, else none until the record is filed (`FamilyScope.of` and `.pooled`, the fallback scope
alike; `2026-10-29-drift-flip-record-awaited.md`). On recording 573:

- `IdentityDriftSpec`: Rose, Madoka and Chichester, each replayed as its seed filed it, flip in none. Rose and Madoka
  fail with the rule switched off; Chichester holds the exclusion's regression.
- Drift 3854 (all 10 rows, both seeds): 0 flips, 0 divergences, 0 known flips left. DRIFT CLEAN.
- Measure 3855 (every country, the pinned pair): 0 model and 0 final takes moved in pl, de, uk, us and es.
- Interim "none": against main's drift 3811 (same seeds), the boot projection takes 12-15% fewer clusters in every
  row (UK seed 1 1141 -> 975, DE seed 2 2562 -> 2193, US seed 1 1689 -> 1451). The gap closes as answers are filed, and
  every row's last projection takes the same count (UK 1787, DE 3119, US 2027, PL 759, ES 237). That is the rule's price:
  after a deploy brings new questions, a listing whose pool has a pending record is shown unmatched until the fill
  files it.

The baseline is empty: the drift gate fails every flip again.
