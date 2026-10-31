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

## 2026-10-30: the drift slices (conv-guards' IdentityDriftSlicesSpec) on the two new slices

The slice replay (`IdentityDrift <cc> --slice <dir>`, 12 seeds, a booted listing leaving and returning) flipped both
slices that drift 3854 and IdentityDriftSpec passed.

- **UK Chichester "The Ties That Bind Us", seeds 3 and 7: a stage lag, fixed.** 1096642 at step 4; at step 8 (fill +
  answer) the record of 2008's 1460982 is filed and the model takes it. The agreement's correction of it waits on the
  wiki, IMDb, Rotten Tomatoes and Metacritic records its families' searches name, and lands only at the next queue
  round (settle). There were two causes. First, the stage judged its first pass by `loadedAt == version`, where the
  version is the families' answers' and moves only when one is filed. After the step-7 deploy, step 8's apply was
  still "first", so nothing was asked inline. Now `firstPass` is the stage's first apply. Second, a new correction's
  inline ask ran one round. Now it follows the questions its answers lead to (`InlineRounds`), as a new cluster's does.
  Both are needed: either alone still flips.
- **DE Metropol "Madoka Magica - Der Film: Rebellion", 11 of 12 seeds: real, identity design.** 212162 while Filmhaus
  (Langerringen)'s listing is present, 42997 "Rebellion" (1962) by PooledMatch when it leaves (step 6), and 212162 when
  it returns. The whole resolve of the listings present agrees at every step: Metropol's bare listing alone, every
  answer filed, takes the 1962 film its title overlaps over the 2013 film it names by an alternative title. This is
  the CAMOUFLAGE shape (a sibling's presence decides a bare listing). Only a resolver rule changes it, which is
  outside red fixing. The corpus replay meets it only when Filmhaus is among the 3% that leave.

### Madoka: the owner's two options, measured (2026-10-30, identity design), neither landed

The owner said "try 2 then settle on 1 if doesn't work". Option 1 to try: a candidate whose title the listing names as
its franchise outranks one matched only on a shared word. Option 2 to settle on: a pooled vote that ties at the top
takes no film.

- **The tie.** Metropol alone measures 42997 ("Rebellion", 1962, released in English as "H.M.S. Defiant") and 212162
  (2013; its forms include "Puella Magi Madoka Magica - Der Film 3 - Rebellion") as `segment` alike, both at 46.3%.
  The ranking's order then falls back to the lower id. With Filmhaus present, 212162 gains the venue's title as a
  form, measures `alternative`, and wins at 95.4%.
- **Option 1 does not work.** Marked untaken, it leaves Metropol with no film, because an untaken film stays a rival.
  As a denial (a film named only by a piece is denied when another carries every word of the title), it takes 212162
  alone but breaks 12 `identity/Test` cases: season and house productions, where the house's record carries the whole
  billed title and the season's record is named by a piece.
- **Option 2 does not work.** `Acceptance.tiedAtTop` stopped the calibrated pooled rules on a top tie. The slice
  passed and so did the identity suite, but measure 3949 was NOT LANDABLE: 2 right DE final takes lost to none. They
  are Abaton's "Enjô - Conflagration" (97149, Ichikawa's 炎上, 1958) and Filmrauschpalast's "SPECIAL: 2. Dangerous
  Encounters – Hong Kong im Filmrausch: MADE IN HONG KONG" (26005). Ties the id order happens to break right are real.
- **Status.** The Madoka slice still flips under IdentityDriftSlicesSpec. That needs the owner's next decision.
- **Open question, not acted on.** Both venues' kinoheld links use the slug
  `puella-magi-madoka-magica-der-film-walpurgisnacht-rising`, which is "Walpurgisnacht: Rising" (TMDB 822653, 2026).
  Their metadata is Rebellion's: the title, Filmhaus's 2013, 116′, the original title 叛逆の物語, and the poster.

### Madoka: what landed, a better tie-break (the owner's third call, 2026-10-30)

Only when the pooled vote ties EXACTLY at the top of the takeable does the order change: the candidate whose best title
form carries the larger share of the listing's title tokens goes first, and the TMDB id is the last resort
(`Acceptance.tieBroken`, the calibrated pooled rules). Metropol's five tokens ("madoka magica der film rebellion") are
all carried by 212162's "Puella Magi Madoka Magica - Der Film 3 - Rebellion" and one of them by 42997's "Rebellion", so
Metropol alone takes 212162. Results:

- the slice: `IdentityDriftSpec` passes, and all 10 drift slices pass (IdentityDriftSlicesSpec on conv-guards' branch);
- the suites: identity 1142, worker 4046;
- `identity-drift-ci`, all 10 rows: DRIFT CLEAN;
- `identity-measure`, every country: 0 model and 0 final takes moved. Enjô and Made in Hong Kong are unchanged.

## 2026-10-30: the precise `none` (a229501a7), and 138 flips it revealed, baselined at its birth

The review of 58ea51d22 found the flip check blind to a take dropping to no film and coming back at any step where
some answer, anywhere, was filed. Now a `none` is skipped only when an answer its decision reads was pending. Drift
3991 (all rows) then showed 138 flips the blind predicate had hidden. The owner (2026-10-30): "baseline them, then burn
them down". They were baselined at the check's birth (`New-Check:`), generated by `identity_flips_baseline.py from-run
3991 --owner flip-fixer`. The ratchet now admits a tightened check's birth rows, and nothing else.

| Country, seed | Listings | Shape | Status |
|---|---|---|---|
| us 1, us 2 | "Parkway Drive: Home" ×63, ×60 | none → imdb:tt47657111 (the IMDb fallback, at fill) → none (a scrape) → imdb | **fixed**: its nones were pending (the fallback's IMDb record), but the decision did not say so; it now names the records its pools await |
| uk 1 | "Royal Ballet: Swan Lake" ×4 | 1702782 → none (2 fill) → 1702782 | open |
| de 1 | CinemaxX "Titans" ×3 | 869152 → none (13 scrape) → 869152 (Filled at settle) | open |
| pl 2 | "Piesek" ×2 | 1319522 → none (8 fill+answer) → 1319522 | open |
| uk 1 | "Starlight Express - Live On Stage", "Hocus Pocus" | X → none → X | open |
| de 1, de 2 | "Do You Love Me", "Scott & Lila" | X → none (6 gone) → X | open |
| pl 1, pl 2 | "Siostry (1972)", "Festiwal Korelacje: „Zemsta”…" | none → X → none → X | open |

### 2026-10-30: what the pending detection clears, and what stays

- Drift 4133 (the tip, with the awaited-record line and held venue pages) names six rows fixed: UK "Royal Ballet: Swan
  Lake" ×4 and "Starlight Express - Live On Stage", and DE "Do You Love Me". Their `none`s were pending answers (records,
  venue pages). The rows are removed, and Starlight's and "Do You Love Me"'s drift slices are committed.
- The replay now also counts a cluster the agreement stage still waits on (`AgreementStage.waitingOn`, not settled) as
  pending. This targets PL "Siostry (1972)", PL "Zemsta" and UK "Hocus Pocus": Agreed → none while a re-read waited →
  Agreed.
- **Real, sibling leave and return** (identity design, like Madoka): DE CinemaxX "Titans" ×3. The stage's
  `venues.current` fill holds while enough venues bill the title, drops to none when one CinemaxX listing leaves, and
  comes back with it. Also DE Hasetor "Scott & Lila": NoCandidate alone, 716004 while Filmmuseum München's linked page
  is present.
- **Open, found by slicing:** the widened UK Swan Lake slice shows a DIVERGENCE on seed 3. Empire Halstead's "Swan Lake"
  and Riverside Woodbridge's "Swan Lake (2027)" serve none incrementally where the whole resolve takes the 2026/27
  broadcast 1702782 (at tick and at settle). Its slice is not committed while it replays red.

### 2026-10-30: the structural plan, steps 2–4 (typed outcome)

Following `identity-flips-structural-2026-10-30` (adopted by the owner), a decision now carries `awaits` and a typed
`outcome`: Taken | NoFilm | Pending(awaits). The stage's `outcomeOf` adds its own waits, and the drift replay reads the
type instead of prose. Measure 4162: 0 model and 0 final moves in every country. Drift 4161 (all rows): DRIFT CLEAN. PL
"Piesek" ×2 is fixed: its `none` at the fill was a pending wait. Its rows are removed and its slice committed. Still
on the baseline:

- DE "Titans" ×3 and "Scott & Lila": sibling leave and return (class D).
- PL "Siostry (1972)", PL "Zemsta", UK "Hocus Pocus": Agreed → none → Agreed. The stage drops an attempted question from
  what it waits on (the settle rule) and decides without it, then agrees once the answer lands. That is class B, for the
  plan's stage-outcome and budget-invariance steps.

### 2026-10-30: class B in the stage (Siostry, Zemsta, Hocus Pocus): an agreed take pending, served as the model's

The slices showed what the corpus hid. After a restart, the stage reads an agreed cluster's candidates again. It then
needs those candidates' poster hashes, or an IMDb id found in TMDB, before the agreed take stands
(`AgreementTakes.taken`: `Take.Pending`). Meanwhile it serves the model's no film, and nothing typed it as a wait.
Siostry and Hocus Pocus then diverged at every checkpoint after the deploy.

- **Step 4 (the stage emits Pending).** A take left pending keeps what it asked (`AgreementStage.takeWaits`), and
  `outcomeOf` reads it. It also reads the verdict's own gaps, an attempted question included: a failure still in
  backoff is pending, not absent (P1).
- **The replay (P1).** An incremental take that is Pending at a checkpoint is no divergence. A step served Pending is no
  change with inputs unchanged (TakeFlips).

What is served is unchanged. The three rows are removed and their three slices committed (replaying clean on 12 seeds).

## 2026-10-30: class D, the lifetime of a listing's identity facts (the owner's decision, identity design)

The owner, 2026-10-30: "Drop showings, keep movie details from all cinemas data until there are no showtimes in any
cinema in the country", and "update the details when it comes back".

- **The rule (`ListingRetention`).** A listing that leaves loses its showings. Its identity facts stay in the model's
  input while a present listing of the country bills its title (the model's `t:` key, read before any resolve). They go
  once none does. A listing that comes back replaces what was kept of it, so the current data always wins.
- **Where it applies.** The rule is data, applied where listings are fed to the model: the worker's drain
  (`IdentityModelService`), the drift replay and the equivalence spec's histories. So a whole resolve of the inputs and
  the incremental model read the same set.
- **The replay.** A venue scraped again never bills a listing it dropped. A step that moved a title's listings is an
  input change for every listing billing that title, because the stage's `venues.current` fill reads the title's venues.
- **Effect.**
  - DE Metropol's bare "Madoka" keeps Filmhaus's dated facts while Filmhaus is away.
  - CinemaxX "Titans" keeps its fill.
  - Hasetor's "Scott & Lila" keeps Filmmuseum München's linked page.
  - All three slices replay clean, and "Scott & Lila" now has a committed slice.
- **Tests.** IdentityModelServiceSpec covers a listing kept while another venue bills its title, updated when it comes
  back with changed details, and dropped with the last.
- **The baseline is empty:** the four class-D rows are removed, and the drift gate fails every flip again.

### Class D follow-up: a kept listing keeps its days

- **Showings versus days.** The model's input holds no showing (time, screen, booking), only the DAYS a listing screens
  on. Those days are facts: they date the broadcast a relay screens.
- **Days dropped.** The first cut dropped the days too. In the full drift rows (run for 2dc6b7cce), UK "Royal Ballet and
  Opera: Tosca", "Alice's Adventures in Wonderland" and "ROMEO AND JULIET" then flipped 11 times: a kept bare
  listing joined the 2024/25 staging while its venue was away, and came back to the 2026/27 one.
- **Season only.** Keeping the season alone (`ScreeningDays.ofSeason`, run 4209) left the same 11 flips.
- **Companions tried and dropped.** Matching kept listings by the cluster they left beside, as well as by title, broke
  the Titans slice. It added a cross-title read that the replay's inputs-moved signal does not see.
- **The rule now:** a kept listing is held whole. The showings drop from the cinema data the site serves, not from the
  facts the model reads.
- **Not a class-D flip.** The cut Alice slice (UK, 503 listings, 54 requests missing) replays red at Halstead and
  Woodbridge with retention OFF too: the broadcast take "changed with its inputs unchanged at 9 scrape". It is a slice
  artefact, like the open Swan Lake one, so it is not committed.
- **Film and stage-cluster signals, tried and dropped.** With whole kept listings, DE "Titans" (CinemaxX Dammtor,
  Raschplatz, Regensburg) went from none to tmdb:869152 at a scrape read as "inputs unchanged". Marking moved inputs
  by the films the moved listings were taken as, and by their stage clusters, did not clear it (runs 4272, 4278).
- **Inline filings are filings.** Nothing of Titans moved at step 13. A diagnostic run (4281) showed the answer store moving 8265 -> 8446 under that scrape: its apply filed, by
  an inline ask, the answer the fill read. The replay now also compares each listing's stage verdict RECEIPT
  (`StoredVerdict.reads`, answer by answer). A receipt moving is an input moving. A take moving under the same receipt
  is still a flip. This is the receipt predicate of the structural plan's property 7. The receipt holds the verdict's
  listings digest too, so it also covers a stage cluster gaining or losing a listing.
- **Result.** Drift is clean on all 10 rows with the receipt signal. The measure moves no take in any country
  (31d296369; the later commits touch the replay alone). The baseline is EMPTY.


## 2026-10-31: drift slices cut closed (the owner: a slice holds everything its listings depend on)

- **The cut.** `IdentityDrift.cut` replays the slice with the recording and prod's filed answers behind it, and keeps
  everything read. It then replays the slice alone, and repeats while the slice alone lacks a request or finds a
  divergence or flip the recording-backed replay does not. A cut that does not close fails (`CUT NOT CLOSED`). The
  filed IMDb finds were read unnoted and never kept. All committed slices are now closed and lack nothing.
- **Alice is not a cut artefact any more.** Cut closed (504 listings), it replays clean on 12 seeds. It is committed.
  With the receipt signal reverted, it replays the old red again (seed 7, "changed with its inputs unchanged at 9
  scrape"), so it has teeth. The old cut's red was that flip, fixed by the receipt signal.
- **Swan Lake is not a cut artefact either.** Cut closed (455 listings, recording 573), it replays red on seed 3 only:
  Halstead "Swan Lake" and Woodbridge "Swan Lake (2027)" are incremental none (Vetoed) where the whole resolve takes
  1702782 (Broadcast), at `settled` and `tick-1`. The fresh stage after `deploy-1` takes it again. The slice alone and
  the recording-backed replay find the same red: it is these listings' own incremental = whole divergence. The
  country's replay runs seeds 1 and 2 only, so it never plays this sequence. Not committed. The closed slice is kept
  in `~/.cache/kinowo-identity/drift-slices-open/uk/halstead-swan-lake-64d7a829`, and replays in ~10 s:
  `IdentityDrift uk --slice <dir> --seed 3`.

## 2026-10-31: Swan Lake's seed-3 red was the replay's, not the stage's: TMDB filings unannounced

- **Root cause.** At the fill the stage read Halstead's candidate searches blind (`AgreementCandidates.candidatesOf`,
  `blindAt` = the answers' version) and keeps a blind read while that version stands. In the worker every TMDB filing
  is published on the answer feed (`TmdbStore.updateAll`), and the family store moves the stage's version for a search a
  kept verdict or the last apply read, or a find or record it waits on (`FamilyAnswerStore.agreementKeyOf`, wired by
  `IdentityProjectionWiring.agreementStage`). The replay's `Gate` filed the model's searches and records unannounced and
  wired no stage to the feed, so the version moved only on a family filing. In the slice no family answer was filed
  from the fill to settle, and the blind read (no candidates, so no broadcast take) stood until a deploy's fresh stage.
- **Fix (harness, 5bb668f72).** The replay announces each filing under the TMDB store's key and wires each new stage as
  the live one is. No resolver or stage code moves. The slice is committed and replays clean on 12 seeds, and is red on
  seed 3 with the announcement off.
- **What the faithful replay shows.** The Cosi slice now flips on seed 11. Fishguard "Cosi Fan Tutte" goes none ->
  1702775 (Broadcast, step 8, Pending on record 455831) -> 299734 (PooledMatch once Halstead's listing joins, step 9,
  Pending on an IMDb search and 455831) -> 1702775 (Corrected, step 12, the search filed). A whole resolve of the step-9
  inputs takes 299734 too, so this is class A: an interim input. The flip predicate counts a film served Pending, while
  the structural properties read it as "pending". That is identity design: either the stage serves none while a
  correction it cannot read yet is pending (agree or none), or the predicate skips a Pending film as it skips a Pending
  none. It is baselined at the tightened check's birth (`New-Check:`). The corpus rows replay clean.
