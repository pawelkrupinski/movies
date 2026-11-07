# 2026-11-07 — DE "Titans": the rival is a phantom listing, no identity rule built — PARKED, owner decision (base `362196b15a`)

- **What was asked:** cure the 15 Titans rows of `identity-flip-backlog.tsv` (CinemaxX Dammtor / Raschplatz /
  Regensburg, Luxor Heidelberg "Titans, Malayalam/english", Filmwelt Grünstadt; slices
  `drift-slices/de/{dammtor-titans-0bd19a4c,cinemaxx-titans-inherent-08f3a19c,cinemaxx-regensburg-titans-4d5c957f}`,
  `IdentityDrift de --seed 8`) with a monotone design, without repeating the parked namesake / bare-borrow line
  (`2026-11-03-bare-listing-own-facts-and-anchors.md`, `2026-11-04-namesake-ambiguous-*.md`, `2026-11-05-namesake-*.md`).
- **Measured:** the whole 2^7 lattice of the slices' one family (the three slices hold the same seven listings), the
  model take and the final take, at main `362196b15a`. A scratch `worker/Test/runMain` printed every subset's take and
  `DecidedBy` per listing (not committed: it was `PopulationMonotonicitySpec.walked`'s `bothTakes` with a print). No
  CI dispatched: no rule was built.

## What the lattice says

The seven listings are D(ammtor), E(ssen), R(aschplatz), Rg (Regensburg), G(rünstadt), K(ino am Raschplatz), L(uxor).

| take | listings present | by |
|---|---|---|
| D, R or Rg alone | 10637 "Remember the Titans" (wrong) | `accept:favoured-calibrated`, the title alone at 46.5 % |
| L alone | none | `veto:named-only-by-the-title-its-feed-cineamo-869152m-copied-from-869152`; leans to 869152 at 3.1 % |
| D + L | none, both | L's copied-title denial withdraws D's 10637 |
| anything with E or K, no G | 869152 at every member | E's and K's kinoheld page credits Prince Joy (88.6 %) |
| D + E + G (+ L) | D none, E 869152 | `withdrawn:sibling-denies-film`: G's Ruffin credit denies 869152 |
| D + R + Rg + G + L, final | 1690206 at D, R, Rg, G | `fill:venues.current` (4 open venues) |
| all seven, final | 869152 at D, R, Rg; none at G and L | `fill:venues.current`; L `vfeed` |

Every flip of the backlog's rows (model none -> 869152 -> none at D, R, Rg, L; final 1690206 -> none -> 1690206 at G)
needs G. **Without G the family has no A -> B -> A in either take** (every chain of the 64 subsets of the other six).
Without K it still flips as today.

## G is a phantom

Filmwelt Grünstadt's "Titans" is not a Grünstadt screening. Its two showtimes book
`ticket-cloud.de/Luxor-Heidelberg/Show/2450683/` and `…/2450684/`, the same shows Luxor Heidelberg's own listing books
(whose Cineamo link is 869152m, the Malayalam film). Kinoheld lists them under Filmwelt Grünstadt's cinema id and attaches
another of its film pages, Ben Ruffin's "TITANS AWAKENING" (judged WRONG for G 10-23). So the "contesting namesake" that
makes the family's take come and go is one screening counted twice, the second time under a wrong film record.
Kino am Raschplatz's K is likewise CinemaxX Raschplatz's own show (`cinemaxx.de/…/1304/HO00001731/43478`), but it agrees
with the film and moves nothing here.

It is systematic. On the prod mirror (DE, synced 2026-11-06 14:10Z), 131 booking URLs are shared between two venues'
showtimes. Nearly all are a kinoheld-fed venue carrying another cinema's show: Adria Filmtheater Steglitz / Cineplex
Titania 33, Cinema+Studio Lippstadt / Cineplex Germering 22 (two cities), Kino am Raschplatz / CinemaxX Raschplatz 16,
Filmwelt Grünstadt / Luxor Heidelberg 15 (two cities), Filmhaus Langerringen / Bad Wörishofen 10+4, Kino in Dorfen /
sKino im Jakobmayer 6. Each phantom is one more venue billing a title. That raises `venues.current` counts, and its
copied page facts make it a rival.

## Why no identity rule

- **An own-fact rule is all that removes it.** Dropping G only when L is present (de-duplicating screenings across venues)
  is a population read. Replayed over the lattice it still flips D, R and Rg 869152 -> none -> 869152 in both takes,
  because G stands while L is away. Only a rule over G's own data removes every flip. The model's input holds no
  showtimes (`ListingRetention`: days only), so that rule belongs in the scraper or the roster: a kinoheld show that
  deep-links another cinema's booking page.
- **A scraper or roster fix strikes no row.** The slices freeze the recorded rows, so the walk keeps seeing G until
  someone re-cuts the slices from a recording made after the fix. Re-cutting them from such a recording removes the evidence
  rather than curing the mechanism.
- **The mechanism stays.** A bare listing borrows a credited sibling's film and loses it when a credited rival arrives.
  The parked line already covers this: the no-window namesake rule (48 served right lost), bare-borrow (35), the
  monotone-rules sort (empty) and prominence (no safe margin). The held-take proposal (`identity-no-drift.md`, "Held
  takes") is the owner's open decision. A genuine rival would flip it the same way. Here the rival is an input bug.

## The owner's decision

1. Fix the data: a kinoheld show deep-linking another cinema's booking (ticket-cloud tenant, `cinemaxx.de/…/<house>`,
   cineplex checkout) is dropped, or the bad kinoheld cinema ids are corrected in the roster. This is a scraper task with
   its own fixture test. It cures prod's input and the inflated venue counts, not the rows.
2. Then either strike the 15 rows as an input artefact (`Owner-Approved:`, the slices kept as the class's witness) or
   keep them as the held-take line's test cases.
3. Or decide the held-take proposal, which is the only design that covers a genuine rival as well.

- **Code:** none.
