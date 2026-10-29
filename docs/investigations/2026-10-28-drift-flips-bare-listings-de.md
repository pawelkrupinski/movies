# 2026-10-28 — Germany's drift flips: a bare listing's take hangs on which sibling listings are present (OPEN)

Identity model convergence #2375 (run 3558, and #2335, run 3509, on the same code) fails `takes-drift-1 (germany)`
and `takes-drift-2 (germany)` on FLIPS alone: 0 divergences from a whole resolve, and takes that went back to a film
they left (`services.identity.TakeFlips`). Both are model takes of listings that bill a bare title. Both are real
resolver behaviour, not artefacts of the replay. Prod meets the same states whenever a sibling listing's run ends
first or starts later. Neither is fixed here: each fix moves the model over whole corpora and needs the full measure
(below). PL's `takes-drift-1` row (Kino Eva's "LALKA", changed with its inputs unchanged) is a false flag, cleared by
the flip predicate's fix ("Flip check: a model take moving at a step that moved listings moved with its inputs"). The
Titans fix below was written, replayed and measured whole-corpus, and parked: it moves another take to a likely wrong
film ("The Titans fix, measured").

## CinemaxX "Titans" (seed 1): a namesake by a Danish alternative title, withdrawn only by a sibling taken by nothing

```
CinemaxX Dammtor | Titans: tmdb:869152 [1 boot: OwnMatch] -> tmdb:10637 [2 fill: OwnMatch] -> tmdb:869152 [9 scrape: OwnMatch]
  -> none [13 scrape: BelowThreshold] -> tmdb:869152 [16 settle: Filled by venues.current: 'Titans (2026)'] (back to a film it left)
```

CinemaxX Raschplatz and Regensburg flip the same way: the three bare listings (no year, runtime or director) are one
model cluster. 869152 is the 2026 Malayalam "Titans". 10637 is "Remember the Titans" (2000, "Gegen jede Regel").
TMDB lists "Titans" as its Danish alternative title, so the bare title names it as `title=alternative`.

Slice: `scripts/identity-slice de --key 'CinemaxX Dammtor|Titans' --recording 573`. It holds seven listings:

- the bare CinemaxX three;
- "Titans" {Prince Joy} at CinemaxX Essen and Kino am Raschplatz, both billed with the Malayalam original title;
- "Titans" {Ben Ruffin} at Filmwelt Grünstadt;
- "Titans, Malayalam/english" at Luxor Filmpalast Heidelberg, whose Cineamo link names 869152.

What the model takes in each variant of the slice:

| slice | model take of the bare three |
|---|---|
| whole | none (30.2%, BelowThreshold). 10637 is the best rejected candidate at 46.5% (search.rank=1 +3.48, popularity.log2=3 +1.74, title=alternative +1.71, venues.corroborating=0 −1.38, rivals=2 −0.72). The blocker is `withdrawn:sibling-denies-film`, from Luxor's listing. |
| 869152's and 647690's records read as unknown | none, as whole. A gap in the answers alone does not move it. |
| without Luxor's listing | **10637 (46.5%, OwnMatch) by favoured-calibrated**, "kept apart from 'Titans' {Prince Joy} ×2: denies-film; different-films" |
| without any sibling | 10637, OwnMatch |

`Families.siblingDenying` withdraws a node's alone take only through a title-linked sibling "itself taken by no rule
alone". Luxor's listing is such a sibling: it is named only by the title its feed copied. The Prince Joy listings are
taken alone, as 869152. Their own facts deny 10637, but that withdraws nothing. So the bare three take "Remember the
Titans" whenever Luxor's listing is absent. That holds even beside the chain's own Essen listing crediting the
Malayalam film's director.

In the replay, venues arrive at boot and at steps 4, 9 and 13, and answers are filed at steps 2, 8 and 12. That walks
the bare cluster through 869152, 10637, 869152 and none, and the stage then fills 869152.

## Werkstattkino "CAMOUFLAGE" (seed 2): a bridge listing leaves, and the listing's own wrong feed link stands

```
Werkstattkino e.V. | CAMOUFLAGE: tmdb:1735950 [1 boot: OwnMatch: node Published(Gröben-Lichtspiele,Camouflage,Some(2026))]
  -> tmdb:56085 [6 gone: OwnMatch: node Published(Werkstattkino e.V.,CAMOUFLAGE,None)] -> tmdb:1735950 [10 scrape] (back to a film it left)
```

1735950 is the Ukrainian "Camouflage" (Камуфляж, released 2026-10-08). 56085 is "Camouflage - Alles nur Tarnung"
(2001). Gröben-Lichtspiele's listing is among the 3% of listings that leave at step 6 and return at step 10.

Slice: `scripts/identity-slice de --key 'Werkstattkino e.V.|CAMOUFLAGE' --recording 573`.

- **Whole.** The model takes 1735950 for five listings: Gröben's "Camouflage" [2026]; "Camouflage" [2026] {Mykyta
  Gibalenko} at Lichtspielhaus Deggendorf, Monopol and SchanzenKino 73; and Werkstattkino's. Werkstattkino's listing
  comes with Cineamo's link 56085m, which Cineamo mislinks to the 2001 film. The trace reads "feed cineamo 56085m
  links 56085, overruled: 2 other source(s) back 1735950 and none backs 56085 — its copied facts set aside". It is
  then taken as 1735950 at 58.5% by favoured-calibrated (venues.corroborating=4, +4.93).
- **Without Gröben's listing.** Werkstattkino's listing stands alone. It takes **56085 at 85.7% by
  favoured-calibrated** (search.rank=1 +3.48, title=original +2.70, originalTitle=match +1.63 from the feed's copied
  facts, venues.corroborating=0 −1.38), with 1735950 at 62.5%.

Why the overrule needs Gröben's listing (red fixer #2432, the slice with and without it). The backing that overrules a
feed link is the WHOLE corpus's, read by title key (`CorpusContext.titleGroups`, `VenueBacking.sources`), not the
node's family's: the three Gibalenko listings count without Gröben too, but as ONE source. Deggendorf, Monopol and
SchanzenKino 73 bill the same Filmstarts entry (one poster, one synopsis), and a feed entry copied onto many venues is
one claim. Gröben's own Cineamo entry (`cineamo:1735950m` [2026]) is the second source `FeedLink.OverrulingSources`
asks for. Without it the link stands, one source against one, as FeedLink intends ("Kohlhiesels Töchter": a link only a
single source contradicts stands). The pool then reads 56085 and 1735950 alike (62.5% each); the node's alone take of
56085 is withdrawn by Kino Krokodil's "Camouflage" [2016] {Imge Özbilge}, an 8-minute short kept apart from it, and
the node's own pooled vote (of itself alone) takes 56085 again at 85.7%. Prod (mirror synced 2026-10-28T17:30Z) takes
Werkstattkino's listing as 1735950 now, with Gröben's listing present.

## What a durable fix needs

1. **Titans.** Let a sibling taken alone also withdraw a node's alone take, when two things hold:
   - the sibling's own facts deny the node's film;
   - the node's title names the sibling's film exactly, while the node's take rests on a weaker relation (alternative,
     original, decorated).

   The node then goes to the group vote. Here that should give none, as at full corpus, and the stage's
   `venues.current` fill takes 869152. Keep the "Hamlet" shape, two exact namesakes, out of it. Pin the
   without-Luxor slice as an `IdentitySliceCasesSpec` case, judging CinemaxX's bare "Titans" first. Tried: "The Titans
   fix, measured" below.
2. **CAMOUFLAGE.** A rule decision, either form changing shared resolver code: let one source overrule a TMDB-keyed
   feed's link when it backs a current release the node's title names, billed at several venues that week, and nothing
   backs the linked film (the counter-case: two old films, each named once); or keep a bare listing in the family its
   title key joined while the bridge is away.

### The Titans fix, measured (parked)

`Families.siblingDenying`: a sibling taken alone withdraws a node's alone take when the node's title names the
sibling's film exactly, the node's take reads the title otherwise, no other venue corroborates that take, and the
sibling's denial compared a fact the node did not publish. Code: tag `experiment/drift-flip-titans-sibling-withdraws`
(with an `IdentityResolverCasesSpec` case and two pinned slices). Red fixer #2432, recording 573:

- the drift replay, DE seed 1 (local): the Titans flips 3 -> 0; seed 2 keeps CAMOUFLAGE;
- `scripts/identity-measure-local --base 86d10e36f --head <the fix>` (de, pl, uk, us; uk and us move nothing):
  - DE Metro-Kino im Schloßhof's "Billie" 649137 -> 1544034. The listing runs 96′ and Cineamo links it to 649137, the
    2020 Billie Holiday documentary; 1544034 is Sheri Hagen's 2026 "Billie", 108′, which Hansa Kino bills with her
    credit. Likely a NEW WRONG take: the fix withdrew a take the venue's own feed links.
  - PL Kino ODEON (Żyrardów)'s "Amelia" 8915 -> 194. 194 is Jeunet's "Amélie", "Amelia" in Polish, which Poznań's DKF
    Zamek bills that week; 8915 reads "Amelia" only as the original title of "Amelia Earhart" (2009). Likely right,
    unjudged.

Before it lands: keep a take the node's own feed links (and no overrule sets aside) out of the withdrawal (CinemaxX's
bare node links 869152, the right film, so its withdrawal of 10637 stands), judge Kino ODEON's "Amelia", and measure
again.

Each changes shared resolver code, so the whole-corpus verdict is `scripts/identity-measure` (every country). The DE
rows' own check is the drift replay on CI, which lays the recording. `{fixtures}` is the laid pair, so the capture
and the allow-lists come from the checkout, as `scripts/ci/identity-drift-check.sh` passes them:

```
F=test/resources/fixtures
scripts/identity-ci-run --recording 573 --countries de -- "worker/Test/runMain scripts.IdentityDrift {cc} --recording {fixtures} \
  --families $F/identity-unmatched/{cc}.json.gz --allow $F/identity-drift-allow.tsv --flips-allow $F/identity-flips-allow.tsv \
  --labels $F/identity-judgements.tsv --seed 1 --out {out}"
```

This is the bare pair with no fills; the leg's own flips can differ a little. Use `--seed 2` for CAMOUFLAGE.

## The variants, by hand

`identity-slice` has no option to drop a listing. To get the variants above:

1. Copy the slice dir it prints (`$KINOWO_IDENTITY_CACHE/slice/573/slices/<key>/`). Never edit the shared one.
2. Drop the venue's row from `rows.json.gz`, and its keys from `slice.json.gz`'s `core`. To read an answer as
   unknown, drop its request from `http.jsonl.gz` instead.
3. Replay the copy under the checkout's compiled tools:

```
java -Xmx3g -cp "$(cat target/identity-tools/classpath)" scripts.IdentitySlice de --replay <copy> [--replay <copy2>] \
  --labels test/resources/fixtures/identity-judgements.tsv
```

## The other rows of the same runs (not diagnosed here)

- **UK, seed 1:** "Cosi Fan Tutte" at Empire Theatre Halstead, "Rose (2023)" at Queen's Film Theatre Belfast, "The
  Ties That Bind Us" at Chichester (a correction lag).
- **UK, seed 2:** "Cosi Fan Tutte" at Theatr Gwaun Fishguard and Riverside Woodbridge, Rio's "Rocky Horror", and two
  that the predicate fix clears (CBeebies "Treasure Island", GFT's "Candyman (2021)").
- **US, seed 2:** "NT Live: Hamlet" ×2, withdrawn only at a deploy.
- **PL on a Mac:** replayed locally (recording 573, seed 1), PL also flags Kino Oaza's and Przedwiośnie's "Ktoś całkiem
  obcy" (corrected at the step-5 re-scrape, "inputs unchanged") with or without the predicate fix, where CI's replay of
  the same base and recording does not (run 3553): a case-insensitive APFS keeps one of each recorded answer that
  differs only by case (`scripts/identity-measure-local`'s header). Judge PL's rows by CI's replay.

The correction-lag ones are in `docs/identity-experiments/2026-10-28-drift-replay-stage-hears-tmdb-filings.md`.
