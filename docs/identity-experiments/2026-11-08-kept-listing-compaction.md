# Kept listings at 90 days for today's 7-day cost: compaction measured, nothing fits (2026-11-08)

The owner (2026-11-07): "see if there's a solution that keeps them but doesn't cost extra memory or time / cpu". The
target was departed listings kept as identity inputs for 90 days (docs/design/identity-listing-retention.md) at no more
than ~10% over today's 7-day resolve time and model heap. Two attempts came before this one:

- full nodes at 90-day volume: DE resolve 23 s to 54 s, model heap 153 MB to 323 MB, UK past worker-uk's 640 MB heap;
- evidence-only kept listings (branch `kept-evidence-only`, docs/design/identity-kept-evidence.md): cheap, but departure
  flips came back (DE 25, UK 38) and Giessen "Black Water (GLOBALE)" went right to wrong.

## What was measured

`scripts.KeptListingsBench --composition --model --histogram` (tag `experiment/kept-compaction`), CI run 42418350496,
recording 42332112286, the synthetic departure history of `experiment/retention-windows` (recordings 10-20..11-06).
Only REAL kept listings are counted for composition. A node is one evidence key, venue-free (`Evidence.key`,
`CandidateGeneration.nodeKey`), so listings with identical evidence are already one node and one solver variable.

What the kept listings add, by window:

| cc | days | kept | share a present node | kept-only nodes (listings) | by (title shape, facts) | by title shape |
|---|---|---|---|---|---|---|
| DE | 1 | 470 | 356 | 75 (114) | 74 | 74 |
| DE | 6 | 3,603 | 2,989 | 437 (614) | 427 | 399 |
| DE | 9 | 6,525 | 5,235 | 805 (1,290) | 779 | 693 |
| DE | 18 | 12,124 | 9,266 | 1,579 (2,858) | 1,490 | 1,193 |
| UK | 6 | 4,341 | 3,064 | 318 (1,277) | 301 | 208 |
| UK | 9 | 6,159 | 4,430 | 385 (1,729) | 359 | 232 |
| UK | 18 | 14,536 | 9,176 | 1,079 (5,360) | 979 | 550 |

At 18 days, DE's 1,579 kept-only nodes differ from the nearest present evidence of their title shape as: facts 720,
title shape billed by no present listing (related by `TitleRelation` only) 683, spelling only 167, dates only 9. UK:
650, 318, 108, 3.

Cost, one repeat each (wall time is noisy on the runner, ±15% and worse):

| cc | days | replicas | kept | nodes | resolve wall s | model heap MB |
|---|---|---|---|---|---|---|
| DE | 0 | 1 | 0 | 7,518 | 20.4 | 152 |
| DE | 6 | 1 | 3,603 | 7,955 | 37.7 | 157 |
| DE | 9 | 1 | 6,525 | 8,323 | 140.6 (noise; 22-30 in run 42379154337) | 172 |
| DE | 18 | 1 | 12,124 | 9,097 | 95.4 (noise; 23-25 in run 42379154337) | 185 |
| DE | 9 | 5 | 32,625 | 15,087 | 41-46 (run 42379154337) | 256 |
| DE | 18 | 5 | 60,620 | 19,805 | 51-59 (run 42379154337) | 321 |
| UK | 0 | 1 | 0 | 5,021 | 23.7 | 117 |
| UK | 18 | 1 | 14,536 | 6,100 | 23.9 | 139 |
| UK | 18 | 5 | 72,680 | 12,747 | 61.3 | 236 |

Model heap by class, DE 18 days against none (`--histogram`): +33 MB for 12,124 kept listings, ~2.7 KB each. Of that,
strings are 15.7 MB (250k `String` and their `byte[]`, ~20 a listing), `Object[]` 2.1 MB, `int[]` 1.5 MB, `Listing`
1.4 MB, keys 0.6 MB. Per-node objects (`Evidence`, `EvidenceNode`, `LiveCorpus$Node`: 1,579 each) are under 0.5 MB.

## What it says

1. **CPU follows distinct claims, and they are already compacted.** 76% (DE) and 63% (UK) of kept listings share a
   present listing's evidence and add no node. The kept-only nodes are distinct claims: folding them by (title shape,
   facts) saves 5-9%, by title shape alone 25-50% (and that drops the facts that decide takes, e.g. Köln's "BLACK
   WATER (2025)" year). Direction A has nothing left to fold: today's evidence key is already the film-claim key.
2. **Distinct claims grow linearly**: DE ~85 kept-only nodes a day of history (75, 437, 805, 1,579 at 1, 6, 9, 18
   days), UK ~60. Linear to 90 days: DE ~7.9k kept-only nodes, 7.5k to ~15.4k nodes, which costs what the 9x5 point
   costs (15.1k nodes, 41-46 s against 20-30 s): +50-100%, not +10%.
3. **The bench overstated 90-day nodes** (replicas of every kept listing, those sharing a present node too, became new
   nodes: 19.8k against ~15.4k linear). The conclusion does not change.
4. **Heap follows listings**, ~2.7 KB each, mostly strings. Linear to 90 days (~60k kept in DE, ~73k in UK) is +160 MB
   DE and +120 MB UK. Interning a kept listing's strings against its node's (direction C) saves at most the strings,
   ~1.3 KB, leaving ~1.4 KB a listing: still ~+80 MB DE. Not built, not measured.
5. **Memoising scoring (B)** helps only the incremental drains, not a boot or a whole resolve. The JFR profile of
   2026-11-07 put 50-65% of the growth in per-node scoring. With all of a kept node's scoring free, DE 90 days would
   still be ~+30-50%. It would also need a cache keyed on the pool's records and the corpus reads a score depends on,
   which is more state (the owner: "don't make it more stateful than it was").

Mirror check (prod's `identity_history` and `identity_retained`, read-only, synced 11-06 14:10Z): the history covers
only 8.6 days. Kept listings folded ~4.4:1 (DE) and ~10:1 (UK) into distinct (title, year, directors, runtime,
original title) claims, which agrees with the bench. About half of the kept set is one departure burst on 2026-11-04
~13:28Z, possibly the archive deploy rather than real delistings. So prod cannot yet say whether kept listings
plateau. Retention keeps only titles a present listing relates to, so a plateau is plausible. But no measurement shows
one: the synthetic 18-day history grows linearly.

## Verdict

No direction meets all three bars (departure flips 0, no right take lost at 7 days, 90-day cost within ~10% of
7-day). The binding constraint is CPU: the 90-day window brings ~2x today's distinct film claims, each a node the
resolver must score and solve. Stability needs exactly those claims (the evidence-only attempt's flips). The best
trade measured is today's design with a longer window, at linear cost. At 18 days, DE's resolve stayed within the
runner's noise (23-25 s against 20-31 s at none, run 42379154337) and its model heap was 185 MB against ~162 MB at
7 days (+14%; UK 139 against ~124, +12%). So ~14 days is the most that fits ~10%.
That is the re-measure already planned for ~2026-11-25 on real dated departures (docs/design/identity-listing-retention.md).

Angles left:
- **Real history first.** If real departures plateau (titles stop being billed, so their kept listings go), 90 days
  may cost what ~3 weeks of linear history costs. Re-run `--composition` on prod's dated `identity_retained` archive
  once it holds 3+ weeks.
- **Heap.** Intern kept strings against their node's: ~half the heap of a kept listing, at no CPU or take cost. Worth
  building if the window grows at all.
- **Window by role.** A kept listing that shares a present node's evidence costs heap and no CPU. Keeping those longer
  than kept-only claims needs the heap fix above. It is also not "keep them all": kept-only claims are where the
  stability lives.

Code: tag `experiment/kept-compaction` (the bench with `--composition` and `--histogram`, on main of 2026-11-07).
