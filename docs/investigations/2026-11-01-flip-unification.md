# Flips, divergences and drift: one catalogue, one structure, one plan (2026-11-01)

The owner, 2026-11-01: "investigate all the flips flops and drifts and see if you can design structural solution that
would unify / prevent them from happening". This is analysis and design only. No code changed and no CI was run for
it. Every number below is quoted from a measurement already on record (commit, run or notebook named beside it).

It builds on, and does not repeat:

- `~/Desktop/claude docs/identity-flips-structural-2026-10-30.md`, the structural plan the owner adopted. It introduced
  the typed outcome, the properties and the migration steps. Steps 2–6 have landed since.
- `2026-10-29-flip-burn-down.md`, which records each baseline row and the step 6 and settle decisions.
- The 2026-11-01 notebooks: `relay-apart-from-film`, `kept-overruled-apart`, `credit-spares-sibling-withdrawal`,
  `chain-settle`, `grouping-coherence` and `incremental-whole-divergences` (some of them still on landing branches).
- `~/projects/movies-wt-owner-convergence/RED-NOTEBOOK.md`, the convergence family's red log.

Design rules applied throughout: **no stickiness** (an incremental take is what a whole resolve of the same inputs
gives, and no take is ever kept because it was taken before), and **unmatched beats wrong**.

## 0. Summary

- **47 cases** since 2026-10-18, plus 2 latent id ties. Each is counted once, by its root mechanism; a case is one
  listing group or one prod incident.

  | Class | Root mechanism | Cases | Fixed by a mechanism-level change | Point patches | Open |
  |---|---|---|---|---|---|
  | P | A decision read a question that was not yet answered | 18 | 17 (typed awaits, step 6, settle) | 1 (Torrington, half-general) | 0 |
  | S | The stage's result depended on its budget, order or object identity, not on its inputs | 9 | 1 (typed Pending in the stage) | 8 (one budget or loop at a time) | 0 |
  | H | The replay or the flip check differed from the worker | 7 | 2 (shared stage composition, receipts) | 4 | 1 (APFS, judged by CI) |
  | N | A rule over the title's **present listings** is not monotone as listings arrive or leave | 4 | 1 (retention, for departures) | 2 | 1 (Addams) |
  | L | Evidence of one work, or of an absent listing, counted for another | 6 | 0 | 5 | 1 (Peppa) |
  | V | One denial read differently by two code paths | 1 | 0 | 1 (Keswick, landing) | 0 |
  | T | A tie broken by an id | 1 (+2 latent) | 0 | 1 | 2 latent |
  | ? | Not reproduced | 1 (Hilltop) | | | 1 |

- **The hypothesis holds for 30 of the 47 cases** (P, N, L, V, T): one decision rule is evaluated on inputs that are still
  changing. In each traced case, a whole resolve of the inputs present at that step takes the same interim film.
  The other 16 (S and H) are not a function of the inputs at all. They are mechanism bugs, which the no-stickiness
  equivalence properties now catch.
- **Inside those 30, the changing input is one of two kinds, and the code treats only one of them structurally:**
  - **Answers (18 cases).** Each question eventually closes: it is answered or given up on. Since 10-31 the decision
    names every open question it reads (`awaits`), it is decided on what is known, and it is marked Pending and
    provisional until the questions close (step 6 and the settle rule). Flips of this kind can no longer fail the gate.
  - **The population (12 cases, primary or secondary).** This means which listings of a title are present. The
    population never closes, nothing types it as pending, and every rule that reads siblings decides on whatever set
    is present. That is where every red since step 6 came from:
    - Torrington: a neighbour's unread page;
    - Ladykillers and Black Water: the retention key;
    - Keswick: a sibling's denial;
    - Prima Facie: a banner-stripped search form;
    - Eternity: a kept sibling's backing;
    - Addams: a venue majority.

  No red since step 6 was a pure own-cluster answer flip.
- **Recommendation.** One principle: a decision's dependence on other listings is explicit, typed and monotone. It is
  delivered in five stages. The first moves no take:
  1. a **population-monotonicity property** and report, computed on whole resolves of each real family over arrival
     chains;
  2. **one title relation** shared by retention, grouping and backing;
  3. one **evidence provenance table** for who may support, overrule, deny or withdraw;
  4. **read-set closure**: a decision awaits every open question of every listing it read, not only its cluster's;
  5. for listings that borrow their take from siblings, a **contested family takes no film** instead of following a
     majority. This one is the owner's decision; it is measured first.

  Typed works (c) are rejected on the existing measure: Prima Facie's design B lost 672 right UK model takes.
- **Expected effect.** On the catalogue, stages 2–5 would have prevented 11 of the 12 population and evidence cases.
  The exception is Peppa, which is a data question. P, S and H are already structurally closed or guarded. Stage 1 turns
  the rotating-seed discovery (one or two new flips a run, often impossible to slice) into a per-family count in the
  takes check that only shrinks.
- **First step:** stage 1. `PopulationMonotonicity` runs over the takes check's whole resolve: each family's listings are
  added in sampled orders, everything filed, and the report counts the listings whose take goes A → B → A. It also
  classifies each drift flip as answer-driven, population-driven or a lag. It is report-only and moves no take.

## 1. The catalogue

Columns:

- **Take path**: the take sequence, with the step that moved each take.
- **What moved it**: the mechanism behind the move.
- **Fix**: the fix, with its kind. *mech* = how the code computes; *rule* = what the resolver decides; *harness* = the
  replay or the check; *typing* = the decision says what it awaits.

Rows 1–25 are summarised from the 10-30 plan's case table, which has more detail. Rows 26–47 are new.

### 1.1 Up to the 10-30 plan

| # | Listing | Where | Take path | What moved it | Fix | Class |
|---|---|---|---|---|---|---|
| 1 | Chichester "The Ties That Bind Us" | UK prod, pod starts | 1460982 ↔ 1096642 | a correction read before TMDB answered was stored as standing | bc5755ca4 mech | S (P) |
| 2 | the same | UK s1, s2 | 1096642 → 1460982 → 1096642 | the corrections budget (200 an apply) | 8a50e6d5d mech | S |
| 3 | the same, slices | UK s3, s7 | the same | `firstPass` misjudged after a deploy; the inline ask ran one round | c949f0e97 mech | S |
| 4 | "Così fan tutte" ×3 | UK s1, s2 | pooled at step 9, corrected at settle | the corrections budget | 8a50e6d5d mech | S |
| 5 | "NT Live: Hamlet" ×2 | US s2 | withdrawn only at a deploy | the corrections budget | 8a50e6d5d mech | S |
| 6 | Rio "Rocky Horror" | UK s2 | model take, then agreed | the inline ask read only its first gaps | 5ca8d38f8 mech | S |
| 7 | Cineworld "La Fanciulla del West" ×73 | UK prod | none ↔ 1703629 every 2–6 min | the verdict depended on decision-object identity | e11dbea02 mech | S |
| 8 | PL and UK corrections backlog (E4) | PL, UK | corrections never stood | one attempt cleared every check; searches filed under one id | 3a44cb4dd mech | S |
| 9 | flicks.co.uk "Rose (2023)" ×2 | UK s1 | 901251 → 1069112 → 901251 | a hit weighed before its record (which denies it) was filed | agree-or-none aed6950bb, replaced by step 6 typing (636ca5fb5) | P |
| 10 | DE "Rose" | DE prod 10-18..22 | flipped after each deploy | the same as 9 | GapHold, reverted twice; `creditedNamesake`; now step 6 | P |
| 11 | Metropol "Madoka" (d51dc845f's regression) | DE s1 | 212162 → 42997 → 212162 | excluding a pending candidate made a third interim state | aed6950bb, then step 6 | P |
| 12 | Metropol "Madoka", slices | DE, 11 of 12 seeds | 212162 → 42997 → 212162 as Filmhaus leaves and returns | an exact tie at 46.3%, broken by the lower id | bfa0b5296 rule (`tieBroken`), then retention | T (N) |
| 13 | CinemaxX "Titans" ×3 | DE s1 | 869152 → 10637 → 869152 → none → 869152 | a bare listing took "Remember the Titans" by its Danish title whenever a sibling was away | 5f8b24cc3 rule (a sibling taken alone withdraws), then retention | N (L) |
| 14 | Werkstattkino "CAMOUFLAGE" | DE s2 | 1735950 → 56085 → 1735950 | overruling a feed link needed a second source, and that source was the sibling that left | ecedac9aa rule | N |
| 15 | Hasetor "Scott & Lila" | DE s2 | NoCandidate ↔ 716004 | Filmmuseum München's linked page present or not | 4100740a0 retention | N |
| 16 | Zeise "Do You Love Me" | DE s1 | X → none (step 6, gone) → X | an unread page, then a departure | typed awaits, retention | P (N) |
| 17 | "Royal Ballet: Swan Lake" ×4, "Starlight Express" | UK s1 | X → none → X | records and pages pending, untyped | 49d1c9e78 typing | P |
| 18 | "Parkway Drive: Home" ×123 | US s1, s2 | none ↔ tt47657111 | the fallback's record pending, decision said Vetoed | 0ea5cd50e typing | P |
| 19 | "Piesek" ×2 | PL s2 | 1319522 → none → 1319522 | a wait at the fill, untyped | acb25f358 typing | P |
| 20 | "Siostry (1972)", "Zemsta", "Hocus Pocus" | PL s1, s2; UK s1 | Agreed → none → Agreed | the stage re-read posters or a find after a restart and served the model's none meanwhile | b1c4114b1 typing (stage Pending) | S (P) |
| 21 | "LALKA", "Treasure Island", "Candyman (2021)" | PL s1; UK s2 | "moved with inputs unchanged" | the predicate's inputs-moved missed reads beyond the members | 5df3295bd harness | H |
| 22 | "LALKA / DOLLY" ×3 | PL s2 | whole 1309083, incremental none | the replay re-asked what production's asker skips | 7b586af0f harness | H |
| 23 | "Ktoś całkiem obcy" ×2 | PL s1 (Mac) | "inputs unchanged" | APFS case folding dropped one of two recorded answers | none (CI judges PL) | H |
| 24 | the 138 hidden flips | all | A → none → A unseen | any `none` was skipped whenever any answer anywhere was filed | a229501a7 harness | H |
| 25 | cluster keys across replay deploys | all | false moves | the replay lost the stage's keys | 91cfbe7e8 harness | H |

### 1.2 Since the 10-30 plan

| # | Listing | Where | Take path | What moved it | Fix | Class |
|---|---|---|---|---|---|---|
| 26 | "Tosca", "Alice's Adventures in Wonderland", "Romeo and Juliet" (RBO) ×11 | UK rows | 2026/27 ↔ 2024/25 staging | a kept listing kept its facts but not its screening days, which date the broadcast | 7def2dde0 rule (kept whole) | L |
| 27 | CinemaxX "Titans" at a scrape; Alice seed 7 | DE; UK slice | "changed with inputs unchanged" | an inline ask filed an answer the replay did not count as an input | a49478fb9 / receipts (31d296369) harness | H |
| 28 | Halstead "Swan Lake", Woodbridge "Swan Lake (2027)" | UK slice s3 | incremental none, whole 1702782 | the replay filed TMDB answers unannounced, so the stage's version did not move | 5d534c116 harness (the worker's announcing) | H |
| 29 | Fishguard "Cosi Fan Tutte" | UK s11 | none → 1702775 → 299734 → 1702775 | interim takes on a pending record and search; the whole resolve agrees | 259c2383a step 6 (Pending is provisional) | P |
| 30 | PL "Lalka" (pool 1001 → 1002), "A Star Is Born" | spec histories | taken, then moved | a taken cluster with open searches or records was typed Taken | c21619eb8 typing | P |
| 31 | Campus West "Verity" | UK s10 (#549) | 1283515 → tt44935209 → 1283515 | an Agreed take while its own record was pending | step 6 | P |
| 32 | Kinocenter Giessen "Black Water (GLOBALE)" | DE s9 (#549) | none → 14138 → 454615 → none → 1436969 | interim takes, answers pending | step 6 | P |
| 33 | the same | DE slice s29 | 1436969 → 454615 → 1436969 | Köln's "BLACK WATER (2025)" left and was not kept: retention keyed the whole sanitized title | 5959ba7bf rule (retention by search form) | L (N) |
| 34 | Centrum 3D Przemyśl "DKF – Zniknięcie" | PL s22 (#555) | 1644 → 8740 → 1644 | a same-search-form must-link grouped the 1988 Sluizer listing in at the fill and split it off at a scrape; the whole resolve does the same at every step | step 6 (interim takes Pending) | P (N) |
| 35 | Lanternhouse "Andre Rieu: Let it Snow (TBC)" | UK s21 (#555) | 1707785 → 295151 → 1707785 | re-scraped alone, then rejoined at settle; whole equals model | step 6 / settle (measured clear) | P (N) |
| 36 | "Karla" ×11 | DE s23 (#556) | 1480424 → 9695 → 1480424 | fill, then scrape; not traced | step 6 (measured clear) | P |
| 37 | Maltings Berwick "Fallen Angels by Noël Coward" | UK s24 (#556) | 1702350 → 1437981 → 1702350 | moves at scrapes; not traced | step 6 (measured clear) | P? |
| 38 | "Pip and Posy's Cinema Show" ×2 | UK s24 (#556) | 1045638 → none → 1045638 → none | settled past a failure in its retry backoff | 18e697aa5 rule (settle only when answered or given up on) | P |
| 39 | Alamo Seaport "The Irregular at Magic High School" | US s24 (#556) | 1310387 → 431821 → 1310387 | a pooled interim take at a fill | step 6 | P |
| 40 | Hilltop "Rebuilding" | UK s6 (#579) | 1211442 → none → 1211442 | a fill and answer, then a scrape; the slice replays clean on 64 seeds | none: not reproduced | ? |
| 41 | Plough Torrington "Dracula" | UK s37 (#595) | 11868 → none → 11868 | Quad Derby arrived with its page unread; Torrington's must-links reached two components held apart, so the solver left it alone (`ConstraintSolver`'s ambiguous rule), Decided, awaiting nothing of its own | 589a6e3ea mech (awaits include questions of nodes its must-links reach outside the cluster) | P (N) |
| 42 | Nerve Centre "The Ladykillers \| Cinematic Breakfast" | UK s32 (#624) | 5506 → 5516 → 5506 | Keighley's "The Ladykillers (1955)" left and was not kept: retention read the present title whole, while the model joins by segment | 8dfe17752 rule (a present listing bills its pieces) | L (N) |
| 43 | Keswick Alhambra "Scottish Ballet: These Violent Delights" | UK s42 (#629) | none → 1462368 → none → 1462368 | Kiln's uncredited billing arrived; `Families.siblingDenying` withdrew Keswick's credited own match, though `ClusterVoting.onlyWithdrawn` spares a credited node; Hexham's arrival restored it | cc3282031 rule (withdrawal honours the credit), landing | V (N) |
| 44 | Chichester "Prima Facie" | UK s17 | none → 1127282 → 963568 → 1127282 | NT Live's listing searches as "Prima Facie" (banner stripped), so a same-search-form must-link grouped it with the film; the bare listing's own match also reached the relay record | e9337f0be + 5aa4dc396 rule (own text denies a relay record); design B (typed relay works) measured NOT LANDABLE | L (N) |
| 45 | DE "Eternity" ×18 venues | DE prod 10-31 10:49Z onwards | none → 1259102 (wrong) | Caligari's kept Freyne listing counted as a source backing Cineamo's link, so the link was no longer overruled | 871125cf7 rule (`KeptOverruledApart`; only present listings back) | L |
| 46 | Beehive Honiton "Nostalgic Cinema: The Addams Family" | UK s58 | 2907 → 481084 → 2907 | `ClusterVoting.familyMajority`: 5 of 7 venues cleared `RateBounds.lower95`, 5 of 8 did not; whole equals model | none: open, the owner's decision (options in §5) | N |
| 47 | "Festtagsspaß mit Peppa" | DE corpus, ES prod one-offs | split 1650896 ×344 / 1778515 ×33 | two real records; a feed linked the other one (Cinefactory "Fun_with_Peppa") | none: needs a pin | L |
| – | Kino Apollo "Faust 2 (2026)" | spec seed 7 | grouping follows id order while Pending | a grouping tie | latent (tie audit) | T |
| – | Abaton "Enjô", "Made in Hong Kong" | DE | right by id order | ties the id order happens to break right | latent (measure 3949) | T |

Not counted: Kino Parczew's showtime-less "nieczynne" card (intake, not identity; 80c27314a), the chain-settle splits
(they are stable splits, not flips), and the 2 latent ties.

## 2. The classes, by root mechanism

### P: a decision read an unanswered question (18)

The rule is evaluated on an evidence set that is still growing. Every fix until 10-29 picked a value to read in place of
the unknown:

- the hit, as if its record agreed;
- nothing (exclusion);
- the last take (GapHold);
- agree or none.

Each moved a different set of interim takes. The owner's 10-31 decision ended that. The decision is made on the inputs
as they stand, the open questions are named (`ResolverDecision.awaits`), and the take is Pending, served provisionally,
until they close. Step 6 removed agree-or-none and the withheld fallback. The settle rule closes a question only when
it is answered or given up on (8 failures, about 16 h). 17 of the 18 cases were then cleared by these
**mechanism-level** changes, not by rules. Rows 29 and 31–39 needed no case-specific code at all.

The one exception is Torrington (41). The open question was **another node's**: the decision's outcome depended on
it, but it was outside the cluster, so it was not named. 589a6e3ea widened `awaits` to the nodes that the cluster's
must-links reach. That is half general: it covers the solver's reach, but not the other places a decision reads a
sibling (§3.2).

### S: the stage was not a function of its inputs (9)

Budgets, inline-round limits, a first-pass flag and identity-hashed memos each made the stage's take depend on how much
it had read and in what order. Each fix was a point mechanism patch: read more, sooner. The 10-30 plan's P3 (a budget
delays, never changes) is now property 3 of `IncrementalWholeEquivalenceSpec` (production budget against unlimited),
and the stage types its own waits Pending (b1c4114b1). No S case has appeared since 10-30. One known residue remains:
a stored correction that stands unread under the budget is served as last decided (`AgreementStage.outcomeOf`'s `held`
clause, plan step 8).

### H: the harness or the check differed from the worker (7)

The replay re-implements the worker (the identity-solid review's first finding). Two fixes shared production's code:
the stage composition, and the filings announced on the answer feed. Four were point patches to the predicate or the
replay. The receipt signal replaced the guessed inputs-moved signal with the verdict's own read set. No H case
has come from the predicate since then; Swan Lake (28) was the replay's wiring. The SOLID plan's item 9, the replay
driving the real wiring over fakes, removes the rest of this class. It is outside this document.

### N: a rule over the present listings is not monotone in them (4 primary, 8 more secondary)

These are rows 13–16, 46, and secondarily 12, 33, 34, 35, 41, 42, 43 and 44. A rule reads the title's other listings,
and its result is not monotone as that set grows or shrinks. Concretely, the rules that read siblings on main are:

| Rule | Reads | Non-monotone because | Cases |
|---|---|---|---|
| `ClusterVoting.familyMajority` | linked siblings' takes, counted by venue | a lower-95% bound on a venue share is crossed both ways as venues arrive; ties go to the smaller id (`minBy (-size, filmId)`) | Addams |
| `ClusterVoting.vote` / `onlyWithdrawn` | the cluster's pooled facts | a pooled runtime or year crosses the cut as members join | Keswick, Ladykillers, Black Water s29 |
| `Families.siblingDenying` / `withoutSiblingDenials` | siblings' denials and takes alone | one arriving sibling can withdraw a member's own match | Keswick, Titans (whose fix added another such withdrawal) |
| `ConstraintSolver` (ambiguous nodes left alone) | must-link reach across components | a new node can make a listing's reach ambiguous | Torrington |
| `ConstraintEdges.sharedSearchForm` | other listings' search forms | a must-link appears when a listing of another work searches the same form | Prima Facie, Zniknięcie |
| `FamilyScope.overrule`, `VenueBacking.sources` / `corroborating` | other venues' backing of a film | the second overruling source can be the one that left | CAMOUFLAGE, Eternity |
| the agreement's `venues.current` fill | how many venues bill the title | the fill holds only while enough venues bill it | Titans |
| `Acceptance.tieBroken` and the pooled vote | the pool, which siblings' titles widen | a sibling's title form turns a tie into a win | Madoka |
| `ListingRetention.titleOf` / `billed` | which present listing bills a leaving one's title | its key differed from the model's link relation | Ladykillers, Black Water, Prima Facie's retention arm |

Departures are handled structurally by `ListingRetention` (the owner's 10-30 rule): a leaving listing's facts stay
while its title is billed. That made departure flips rare, but it created class L's retention cases. Arrivals have no
structural treatment. Every fix for them was a point rule:

- Titans: a sibling taken alone withdraws;
- CAMOUFLAGE: one source overrules for a current release;
- Madoka: a token-share tie-break;
- Keswick: withdrawal honours the credit.

Each rule is itself a new read of the siblings.

### L: evidence counted for the wrong work, or kept beyond its listing (6)

Rows 26, 33, 42, 44, 45 and 47:

- retention dropped a kept listing's days (26);
- retention used a different title key from the model's (33, 42);
- a banner-stripped search form made a relay and a film share a key (44);
- a kept listing vouched for a feed link (45);
- a feed linked the other record (47).

All five landed fixes were point rules in different files: `ListingRetention` ×3, `RelayRecords`, `FamilyScope.overrule`
with `ConstraintEdges.KeptOverruledApart`. The common fault is that **nothing says what a piece of evidence is**: the
listing's own statement, a present sibling's, a kept sibling's, a feed's copy, or a search's form. Without that, nothing
says what such evidence may do.

### V: one denial read differently by two paths (1)

Keswick (43). `Denial.Kind.NotTheWork(creditOverrules = true)` documents that a credited director keeps the film.
`ClusterVoting.onlyWithdrawn` honoured it; `Families.siblingDenying` did not. This is the same fault as L, seen in the
code: the permissions of a denial are re-derived at each reader.

### T: an id broke a tie (1, and 2 latent)

Madoka (12). Property 4 (relabelling) holds for decided takes in generated histories. The tie audit (plan step 7) has
not been done. `familyMajority`'s `minBy` on film id is one more id ordering in a decision path that the 10-30 list
did not name.

## 3. The common structure

### 3.1 The hypothesis, tested

"Most flips are one decision rule evaluated on a population or evidence set that is still changing." To check it, for
each case: is the interim take what a whole resolve of the inputs present at that step gives?

- **Yes, traced:**
  - Rose (9), Madoka (11, 12), Cosi s11 (29);
  - Zniknięcie (34) and Rieu (35), traced projection by projection in the red notebook;
  - Addams (46);
  - Torrington (41), where the whole resolve equals the model;
  - Keswick (43), whose step-9 set is the "nohexham" slice case, red on main.
- **Yes, by the fix's form** (the fix is a rule over inputs, measured with 0 incremental-only moves): 13, 14, 15, 26,
  33, 42, 44, 45.
- **No:** all 9 S cases (the stage's state, not its inputs) and all 7 H cases (the harness).

So **30 of 47** cases are faithful functions of interim inputs, and **16** are not (they are bugs). The hypothesis
holds for the majority, and the split matters. The 16 are prevented by the no-stickiness equivalence properties, which
exist now. The 30 cannot be prevented by any equivalence property, because whole and incremental agree on them.

### 3.2 Two kinds of changing input

| | Answers | Population |
|---|---|---|
| Closes? | Yes: answered or given up on | Never: a venue can bill the title tomorrow |
| Named in the decision? | Yes, in `awaits` (search, page, pool record, family, poster) | Only Torrington's must-link reach |
| What the decision does meanwhile | Decides on what is known; Pending, provisional | Decides on whatever is present, typed Decided |
| Property that guards it | 1 (monotone under filing), 2 (any filing order) | **none** |
| Cases | 18, all now Pending-covered | 12 (primary or secondary), the source of every red since step 6 |

The asymmetry is the finding. The 10-30 plan's principle P2 ("as inputs only move from Pending to Known, a take only
refines") is enforced for answers. There is no counterpart for listings. A take can go A → B → A while listings only
**arrive**: Addams, Keswick, Prima Facie and Torrington did, with no departure at all. That is not "an input reverted"
(the 10-30 plan's excuse for class D). The resolver's decision function is non-monotone in its population: f(S) = A,
f(S ∪ {x}) = B, f(S ∪ {x, y}) = A.

### 3.3 Why the point fixes keep coming

Every N and L fix so far added or narrowed one sibling read. The rules that read siblings (the table in §2, nine of
them) each decide on their own:

- what a sibling's evidence counts for;
- whether a kept sibling counts;
- whether a sibling's denial reaches a member.

Each rule's answer was locally right. Two rules disagreed (Keswick), or one counted what another excluded (Eternity).
The rotating seeds then find the next combination at a rate of 1–2 per run (notebook: #549, #555, #556, #579, #595,
#624, #629 in about 30 h). Reproducing them was expensive:

- Rieu reproduced only on slice seed 14 of 40;
- Torrington only in a slice widened to 153 listings (seed 35);
- Ladykillers only on slice seed 136;
- Keswick, Hilltop and Prima Facie never as a drift slice. Keswick was pinned as a hand-made subset case instead.

So each one cost a CI round and hours before diagnosis. A population property over each family's subsets finds the same
shapes directly (§4 (e)).

## 4. The candidate designs, evaluated

The counts are of the 47 catalogued cases that a design would have prevented, meaning the flip could not have happened.
Typing a take Pending does not count as prevention.

### (a) Decision closure: decided only once the inputs are closed

- **Status.** Implemented for answers: typed `awaits` (acb25f358, c21619eb8), step 6 (636ca5fb5, 2b7dbdf6e, d432c45da)
  and settle (18e697aa5). It is generalised over the existing `Question`/receipt graph, as the brief asks.
- **Would have covered.** All 18 P. Torrington needed the reach extension.
- **The population cannot be closed.** No listing is "all answered" with respect to listings that do not exist yet.
  What can be closed is the **read set**: the decision awaits every open question of every listing whose evidence it
  read, sibling or member. Today only the cluster and the must-link reach are in `awaits`. The siblings read by
  `familyMajority`, `siblingDenying`, `overrule` backing and the `venues.current` fill are not.
- **Prevents.** Torrington's kind: a sibling's unread page or record decides another listing's take. That is 1 case
  directly, plus the secondary P in 34 and 35. It does not stop a sibling's arrival itself from moving a take: Addams,
  Keswick, Prima Facie and Eternity had nothing open.
- **Cost.**
  - Measured for the narrow form: +1.0% Pending listing-projections in UK s37 (679,550 → 686,164), still Pending at
    the end 19,071 → 19,377 (589a6e3ea).
  - For the settle rule: +2 to +30 Pending per projection, 0 left at the end.
  - Step 6 as a whole: 0.0–0.9% of Pending listing-projections served another film than the final, per kind (run
    41226360347).
  - The full read-set form should cost a few times the narrow one. That is an estimate, to be measured.
- **Complexity.** Low. `ResolverDecisions` already unions the awaits. Each sibling-reading rule reports the nodes it
  read through one `Reads` accumulator.

### (b) Order-independent group decisions

There are two variants, of different strength.

- **(b1) Margins.** Require a margin that further members cannot overturn. **No such margin exists in an open
  population**: enough arrivals overturn any share. `familyMajority` already uses a lower-95% bound, and Addams crossed
  it in both directions. Counting independent statements instead of venues (a chain once, as `GroupingCoherence` does)
  lowers the volatility but does not remove it. It prevents perhaps Addams, and nothing provably. **Rejected as a
  structure.**
- **(b2) Own evidence decides; the group only fills, and only uncontested.** Two rules:
  1. A member's own accepted match, one its own facts do not deny, can be moved by siblings only through a
     provenance-table action that its own facts confirm (see (d)). No sibling can withdraw it on the sibling's own
     facts. That is the Keswick rule made general.
  2. A member with no deciding facts of its own (a bare listing) borrows a take from its title family only when the
     family is **uncontested**: no other work is backed by a sibling's own stated facts (year, director or runtime).
     A contested family gives the bare listing **no film**. This is the Addams option 1 that was put to the owner.

  **Monotonicity.** As listings arrive, a bare listing's take can only go x → none, when a contesting sibling arrives.
  It cannot come back without a departure (which retention neutralises while the title is billed) or an answer
  (which (a) makes Pending). So A → B → A on arrivals becomes impossible for borrowed takes. For own-evidence takes
  it is impossible by rule 1.
- **Prevents.** Addams (46), Titans (13), CAMOUFLAGE (14), Scott & Lila (15), Madoka's sibling arm (12), Keswick (43),
  and Torrington's solver arm (41, where a contested reach leaves the bare node with none, which it would keep). That
  is 7. The relay arm of Prima Facie is half covered, since its own match was to a relay record.
- **Cost.**
  - Right takes lost by bare listings in contested families: Beehive's right 1991 "Addams", and similar ones. **Not
    measured.** `chain-settle` found only 2 real chain splits in 2,834 chain groups, so contested families of a chain
    title are rare. Mixed-venue repertory titles (classics strands beside a remake) are where the loss will be. It
    is measured before any decision (§5, stage 5).
  - It aligns with unmatched beats wrong; it costs coverage.
- **Complexity.** Medium. One predicate (`contested(family)`) and a precedence in `ClusterVoting` and `Families`. The
  rules it replaces (`familyMajority`'s bound, Titans' sibling withdrawal) become smaller.

### (c) Typed works before grouping (film, relay, event, re-release, edition)

- **Already measured, as Prima Facie's design B** (runs 41297551190, 41298198021). Classifying a listing's kind from
  its title moved 1,244 / 2,116 UK model takes and lost 672 / 692 right ones; final right-lost was 32 / 15. Box
  offices bill relays bare, and TMDB files relays under plain titles, so a kind read off titles is wrong too often to
  partition on.
- **Prevents.** Prima Facie (44) and the relay arm of retention, which is 2, at a cost of hundreds of right takes.
- **What survives.** Kind as a **fact with provenance**, used only where the listing's own text contradicts the
  record. That is the narrow rule that landed (`RelayRecords.contradictedByOwnText`). It belongs inside (d).
- **Rejected as a partition.**

### (d) Evidence provenance and one permissions table

- **The idea.** Every piece of evidence a decision reads carries its provenance. A single table says what each
  provenance may do.

  | Provenance | support a film | corroborate (count as a source) | overrule a feed link | deny itself a film | withdraw another member's match | keep a listing (retention) |
  |---|---|---|---|---|---|---|
  | own statement (listing text, stated year/director/runtime) | yes | yes | yes | yes | – | – |
  | present sibling's own statement | yes (bare members, uncontested) | yes | yes | – | only if the member's own facts agree (b2.1) | yes |
  | kept sibling (left, title still billed) | yes (its own node) | **no** | **no** (Eternity) | – | **no** | – |
  | feed copy (a feed's year, link or runtime copied to venues) | weak | counts once per feed | is what gets overruled | – | no | – |
  | search form (banner stripped, normalised) | links by key only | no | no | – | no | – |
  | record (TMDB, IMDb, wiki) | yes | – | – | via own-fact contradiction | – | – |

  The table's cells are today's ad-hoc rules, written once:

  - `Backer.Kept` plus `presentOnly` is the "kept: corroborate no" cell;
  - `creditOverrules` is the "withdraw: only if own facts agree" cell, which `siblingDenying` missed;
  - `KeptOverruledApart` is the "kept: overrule no" cell.

  Every reader asks `Authority.may(provenance, action)` instead of re-deriving it.
- **One title relation**, a sub-part of (d) that is cheap and separate. `ListingRetention.billed`, the must-link keys
  (`Families.blockKeysOf`, segment and search form) and the backing's title groups become one function,
  `TitleRelation.linked(a, b)`. Its property: whatever the model links by title, retention keeps by the same
  relation.
- **Prevents.**
  - Eternity (45) and Keswick (43) by the table.
  - Ladykillers (42), Black Water s29 (33) and Prima Facie's retention arm (44) by the one relation.
  - Tosca/Alice (26) by "keep a listing: whole", which is now the rule.

  That is 6. Peppa (47) only partly: a feed copy is weak, but two real records still need data.
- **Cost.** Behaviour-preserving as a refactor, measured to move 0 takes first. Each inconsistency the table exposes is
  then a rule change, measured on its own.
- **Complexity.** Medium-high. It touches `Families`, `ClusterVoting`, `VenueBacking`, `FamilyScope.overrule`,
  `ConstraintEdges`, `ListingRetention` and the stage's fills. It is also the natural home of the identity-solid plan's
  "rules → orchestrator constants" back-edges.

### (e) A property harness over arrival orders, for every rule

- **Today's properties cover filings, not arrivals.** `IncrementalWholeEquivalenceSpec` checks, over generated
  histories:
  - monotone under filing;
  - any filing order;
  - budget invariance;
  - relabelling;
  - incremental = whole.

  None of them adds listings to a fixed answer set and checks that each take is monotone. The per-fix specs
  (`RelayOwnFactsSpec`, `KeptOverruledApartSpec`, `ListingRetentionSpec`) enumerate arrival orders for one case each.
- **The property.** For each title family, with every answer filed, whole resolves are computed along sampled arrival
  chains (∅ ⊂ S₁ ⊂ … ⊂ S). The property fails when any listing's Decided take goes A → B → A. Departures are run the
  same way, with retention applied. There are two sources:
  - generated families (the existing `Case.generated`), as a unit property in seconds;
  - **every real family of the recording**, as a report in the takes check.

  Both need only whole resolves of one family, a few milliseconds each. Incremental equality is a separate property
  that already exists.
- **What it would have found before CI did.** Addams, Keswick (the "nohexham" subset), Prima Facie (the {Chichester,
  NT Live} subset), Titans, CAMOUFLAGE, Madoka and Scott & Lila: 7, each on its own family's data.
- **Its limit.** Some takes read corpus context beyond the family (`CorpusContext`: houses and venue titles). The Prima
  Facie slice replayed clean because the corpus's step-4 own match needed that context. A family-only chain therefore
  under-reports. The report runs inside the whole-corpus resolve, varying one family's population with the rest fixed,
  which keeps that context.
- **Cost.** Report-only, 0 takes moved. CI time: families × chains × one family resolve. At 8 chains of a 2–20 listing
  family, that is an estimated tens of seconds per country (to be measured; `GroupingCoherence` runs in 0.1–3.4 s on
  the same corpus).

### Summary of the five

| Design | Prevents (of 47) | Classes | Takes moved | Pending cost | Complexity | Verdict |
|---|---|---|---|---|---|---|
| (a) closure, answers | 0 more (18 already typed) | P | 0 | measured, small | done | keep |
| (a) closure, read set | 1 (+2 secondary) | P via N | 0 (label only) | ~+1% (narrow form) to a few % (estimate) | low | **stage 4** |
| (b1) margins | 0 provably | N | some | – | low | reject |
| (b2) own-first + contested → none | 7 | N, V | bare listings in contested families, unmeasured | none | medium | **stage 5, the owner's call** |
| (c) typed works | 2 | L | 1,244 UK model / 32 final right-lost (measured) | – | high | reject |
| (d) provenance table + one title relation | 6 | L, V | 0 for the refactor, then one measured rule each | none | medium-high | **stages 2–3** |
| (e) arrival-order property and report | 0 directly; finds 7 | N, L, V | 0 | none | low-medium | **stage 1** |

Stages 2–5 together cover 11 of the 12 population and evidence cases, all but Peppa. The overlaps between (b2) and (d)
(Keswick, Madoka) are counted once.

## 5. Recommendation: one principle, five stages

**The principle.** A take's dependence on other listings is explicit, typed and monotone:

1. **explicit**: a decision records which listings' evidence it read (the read set);
2. **typed**: every piece of that evidence carries its provenance, and one table says what it may do;
3. **monotone**: adding a listing can only refine a take (a member's own decided take stays; a borrowed take may only
   drop to none when contested), and an open question anywhere in the read set makes the take Pending.

This is the 10-30 plan's P2, extended from answers to listings. It changes no rule about what a film is. It changes
how siblings may weigh in.

| Stage | What | Moves takes? | Proof | Expected cases prevented |
|---|---|---|---|---|
| 1 | **`PopulationMonotonicity`**: (e) as a unit property over generated families, and a report in the takes check over every real family (count per country, top listings); IdentityDrift labels each flip answer-driven, population-driven or lag by comparing a whole resolve at the step | No | the property fails on the parents of 5f8b24cc3 (Titans), ecedac9aa (CAMOUFLAGE) and cc3282031 (Keswick), and on main for Addams | 0, but the backlog becomes a number |
| 2 | **One title relation** shared by retention, must-link keys and backing | Expected no (retention affects departures only; the measure cannot see it) | `TitleRelationSpec`: model-linked ⇒ retention-kept; fails on the parents of 8dfe17752 and 5959ba7bf | 3 (33, 42, 44's retention arm) |
| 3 | **Provenance table** (`Authority.may`): refactor every sibling read to it, 0 moves; then reconcile the inconsistencies it exposes, each measured | Refactor no; reconciling yes, measured | `ProvenanceSpec`: no kept evidence overrules or corroborates; no sibling withdraws a member's own-confirmed match; fails on the parents of 871125cf7 and cc3282031 | 3 (43, 45, 26) |
| 4 | **Read-set closure**: every sibling-reading rule adds what it read to `awaits` | Pending label only | property 1 extended to sibling questions; fails on the parent of 589a6e3ea | 1 (41) + the secondary P |
| 5 | **Own-first, contested → none** (b2), only after stage 1's report says how many listings it touches | Yes | stage 1's property with the arrival chains strict (no A → B → A for any Decided take); `identity-measure --all-countries`, final right-lost judged | 7 (12, 13, 14, 15, 41, 43, 46) |

**Expected reduction, per class.**

- P: already 18/18 typed. Stage 4 removes the remaining way, a sibling's open question, for a Decided take to move on
  an answer.
- N: arrival flips of Decided takes become impossible by construction after stage 5. Before it, stage 1 counts them and
  the gate can hold a shrink-only baseline of them, exactly as the flips baseline did.
- L, V: one table and one relation make "two readers disagree" impossible. A new rule that reads siblings must name its
  cell in the table.
- S, H: unchanged here. They are guarded by properties 1–4 and by the SOLID plan's item 9 (the replay drives the real
  wiring).
- T: the tie audit (plan step 7), plus `familyMajority`'s id `minBy`.

**Proving it.** Each stage brings a property with teeth (it fails on the parent of the fix it encodes) and the existing
measures:

- `identity-measure --all-countries` for model and final takes, with right-lost judged;
- the drift rows over 64 seeds per country (`identity-drift-ci --seeds`) for flips and divergences;
- `pending-<cc>.tsv` for the Pending price.

Stage 1's report is the progress metric: listings with an arrival-non-monotone take, per country, only shrinking.

**Trade-offs against the owner's rules.**

- **No stickiness.** Every stage reads inputs only:
  - retention is an input fact;
  - provenance is a property of evidence;
  - closure is a label;
  - "contested" is computed from the listings present.

  Nothing reads a previous take. Whole and incremental stay equal, and the existing properties keep checking it.
- **Unmatched beats wrong.** Stage 5 is this rule applied to bare listings: a contested title gives no film rather than
  a majority's guess. Its price is right takes that a majority happened to guess right (Beehive's 1991 film). One
  existing tension stays and is named here rather than solved: step 6 serves provisional films while Pending, and
  0.0–0.9% of those differ from the final take (US catalogue ids up to 324 of 329). That was the owner's 10-31 choice
  (a flag, not a frozen take).
- **Owner decisions needed.**
  - Stage 5's rule, after stage 1's count and the measure.
  - Peppa's pin.
  - Whether population-driven A → B → A flips fail the gate before stage 5 lands (recommended: a shrink-only baseline
    from stage 1, as on 10-29).

## 6. The first implementation step

Stage 1, in one change, with no take moved:

1. `identity/src/test/.../PopulationMonotonicitySpec`: over `Case.generated` families with every answer filed, whole
   resolves along 8 sampled arrival chains and the matching departure chains with retention applied. Each listing's
   Decided sequence must contain no A → B → A. It starts as a known-failures list: the parents named in §5 must fail
   it, and today's main lists Addams' shape.
2. `integration.IdentityResolveDump` and the takes check: the same chains per real family, inside the whole-corpus
   resolve, varying one family's population at a time. The output is `population-flips-<cc>.json` and a
   `kind=identity.population_flips` metrics line, report-only, like `grouping_suspects.py`.
3. `IdentityDrift`: each flip gets a `cause` column. A flip is `answer` if the whole resolve at the step equals the
   take and the step filed answers; `population` if it equals the take and listings moved; `lag` if it differs. The
   whole resolve runs only for flipped clusters. The red notebook asked for this on #629, so every flip red names its
   class without a manual trace.

After that, stage 2 (one title relation) is the cheapest stage that prevents cases. It has no owner decision attached.

## 7. Implementation log

The owner approved all five stages on 2026-11-02 ("do it"). Each stage lands on its own, with its measurement here.

### 7.1 Stage 1: the population property and report (landed; no take moved)

**What landed.**

- `PopulationMonotonicity` (identity): arrival chains (an order of a population's listings cut into at most N nested
  prefixes), the A → B → A test over decided takes (Pending skipped), and a corpus report that walks each family of two
  listings or more inside the whole-corpus context. The family's absent listings are taken out of the context's title
  groups and reached-by entries (`PopulationMonotonicity.Without`); the corpus's learned venue titles, houses and banners
  stay as the whole corpus learned them.
- `PopulationMonotonicitySpec` (worker): every committed slice (25 drift slices, 71 identity slices) walked along 8 chains
  of at most 8 steps. Known flips are listed in `test/resources/fixtures/identity-population-flips-known.tsv`. The list
  only shrinks: a new flip fails the spec, and so does a known row whose listing no longer flips.
- `IdentityResolveDump --population N` and the takes check: on the head, 4 chains within 300 s, writing
  `population-flips-<cc>.json` and a `kind=identity.population_flips` line in the verdict. Report only.
- `IdentityDrift`: each flip carries a `cause` column. The seed is replayed with its flipped listings probed, and a whole
  resolve is run at every step where one of them moved. The cause is `answer` or `population` when the model took what
  the inputs decide (`answer+population` for both), and `lag` when it did not, or when nothing moved.
- Report only: `ClusterVoting.ownBacked` and `Resolution.contested`, the facts-free clusters whose title family backs two
  films or more by its siblings' own statements. Stage 5 reads these.

**Teeth.** Two checks:

- On #629's Keswick slices, without that fix, the walk finds 7 A → B → A listings. Among them is Keswick Alhambra's
  none → 1462368 → none. With the fix, 1 remains (DCA's "+ Recorded Q&A" billing).
- The cause column labelled main's UK seed-42 red (Keswick) `answer,population`.

**Known at birth (6 + 1).**

- CinemaxX "Titans" ×4 (none → film → none);
- Plough Torrington "Dracula" (11868 → 138 → 11868);
- Theatr Gwaun Fishguard "Romeo and Juliet" (6003 → none → 6003);
- DCA "These Violent Delights + Recorded Q&A", held once #629's slice lands.

**The report over the latest nightly recording** (run 41363092880, 4 chains, everything walked except families over 300
listings):

| cc | families ≥2 | walked | >300 | flipped families | listings flipped | bare | shapes | a3 apart | contested bare (taking a film) |
|---|---|---|---|---|---|---|---|---|---|
| pl | 386 | 383 | 3 | 3 | 6 | 2 | none→film→none 5, film→film→film 1 | 0 | 105 (105) |
| de | 1,329 | 1,306 | 23 | 4 | 5 | 4 | none→film→none 5 | 0 | 18 (10) |
| uk | 647 | 605 | 42 | 6 | 9 | 5 | none→film→none 6, film→none→film 2, film→film→film 1 | 0 | 3 (3) |
| us | 940 | 867 | 73 | 0 | 0 | 0 | – | 0 | 0 |
| es | 143 | 142 | 1 | 0 | 0 | 0 | – | 0 | 0 |

- "a3 apart" is 0 everywhere: each family walked alone against the corpus context takes, at its whole population, what
  the corpus resolve takes.
- The flipped listings include:
  - UK Beehive Honiton "Nostalgic Cinema: The Addams Family" (2907 → 481084 → 2907, bare);
  - UK "Miracle on 34th Street" ×3 (facts);
  - UK Riverside Woodbridge "La Bayadère 2027" (bare);
  - PL Met "Samson i Dalila" ×4 (none → 1703624 → none);
  - PL Kino Światowid "BILLY ELLIOT" (bare, 298931 → 71 → 298931);
  - DE Cinecitta "RB&O Live: Alice im Wunderland" (bare);
  - DE "Das Neinhorn" (bare).
- **How many bare listings stage 5 would touch.** That depends on what "well supported" means:
  - "two films backed by any sibling's own statement": 126 listings across PL, DE and UK, 118 of them taking a film
    now. Nearly all of these are PL "Lalka" and "Rozważna i romantyczna", where one venue's stated fact backs an old
    film against 186 and 107 venues backing the new one.
  - "each film backed by two venues or more" (stage 5's rule): 5 listings taking a film:
    - DE "Der Grinch - Adventskino" ×2 and "Adventskino: Der Grinch" (8871 by 4 venues, 360920 by 8);
    - UK Beehive "The Addams Family" (2907 by 2, 481084 by 5);
    - UK "Miracle on 34th Street (U)" (10510 by 7, 11881 by 3).

**Measure.** `identity-measure --all-countries`, run 41362724489: 0 model and 0 final takes moved in pl, de, uk, us and
es. LANDABLE.

**Drift.** Rows at seeds 41 and 42 in all five countries, run 41362718212: 9 of 10 rows clean. UK seed 42 is red on
Keswick Alhambra's flip. That is main's open red, fixed by #629 (waiting), and its cause reads `answer,population`.

### 7.2 Stage 2: one title relation (no take moved)

**What landed.**

- `TitleRelation` defines how two listings' titles relate, once. It gives a title's forms: its sanitised whole, search
  form, original titles, delimited segments, its backing group key and the groups its backing reads. From those it gives
  the must-link tier between two titles, and the family block keys its cut keeps (`TitleRelation.Cut`, moved out of
  `TitleLinks`).
- Its readers:
  - `ConstraintEdges`' title must-links. The segment's candidate-reading exception (`namesBeside`) stays there.
  - `TitleLinks.titleLinked`, the sibling relation `Families.siblingDenying` reads. It is the same tier with one clause
    left out: two titles that share only an original title. The edges draw that link, then cancel it with
    `copied-original-apart` where the original is a feed's copy.
  - The backing's groups (`FamilyScope.backingGroups`, `CorpusContext.titleOf`) and what a family's resolve reads of
    them (`CorpusContext.Reads`). The latter now includes the shapes the backing reads, so an incremental family is
    re-resolved when a group it backs by moves.
  - `ListingRetention`. A leaving listing is kept while a present listing relates to it by title: the model's title
    must-link (without its exceptions), a shared family block key (cut against the whole titles and banners of the
    listings present, leaving and kept), or one backing the other's film by title group. `titleOf` and `billed` are
    gone. The drift harness keeps the agreement stage's own key for "a title's venues moved" (`IdentityDrift.titleOf`).
- `TitleRelationSpec`, over every committed slice: retention keeps every pair the slice's resolve joins by a title
  must-link, and every pair where one listing backs the other's film by title group.
- Teeth:
  - with the retention rule from before 8dfe17752, the Ladykillers slice drops 7 such pairs;
  - with the rule from before 5959ba7bf, the Black Water slice drops 2 more.
- What retention now keeps that it did not: relay billings the model joins only through a venue page's original
  title, which retention cannot see. RBO "Swan Lake", NT Live "Hay Fever", Rieu's "Let it Snow" and "Carmen" are now
  kept through their shared block key.

**The one move, found and removed.** The first measure (run 41365658407) moved 35 DE model takes:

- CineStar "Tribute von Panem - Mockingjay: Teil 1" ×35 went to none; the final takes were unmoved.
- The pinned slice de-fallen-angels-by-noel-coward-adria-zorn's seven Coward billings lost their own match.

The cause: `siblingDenying` read the unified tier's shared-original clause, so "Der Zorn der Engel" (copied original
"Fallen Angels") withdrew those matches. The sibling relation now leaves that clause out (c527df039).

**Measure.** Run 41374801177, base = stage 1: 0 model and 0 final takes moved in pl, de, uk, us and es. LANDABLE.

**Drift.** Rows at seeds 5 and 6 in all five countries, run 41376690109: all 10 clean.

### 7.3 Stage 3: one evidence-provenance table (no take moved)

**What landed.**

- `Authority` holds the table, once. Every piece of evidence one listing gives another has a provenance: the listing's
  own statement, a present sibling's, a kept sibling's (it left, its title is still billed), or a feed's copy. The table
  says which acts each provenance may do: back a film, corroborate a sibling's film, count toward overruling a feed link,
  join an overruled listing, withdraw a member's own match.

  | provenance | back | corroborate | overrule | join an overruled | withdraw |
  |------------|------|-------------|----------|-------------------|----------|
  | own        | yes  | -           | yes      | -                 | -        |
  | present    | yes  | yes         | yes      | yes               | yes      |
  | kept       | yes  | yes         | no       | no                | yes      |
  | feed copy  | yes  | once a feed | once     | yes               | yes      |

- The withdrawal rows (`Authority.spares`, `sparesEvery`) say when a member's own statement answers a sibling's denial:
  - its title names the record's production (house and season, or its relay's record): every denial is spared;
  - its banner bills the record's house, against another house's season;
  - its credit of the film's director, against a "not the work" denial that a credit overrules. This is #629's fix
    (Keswick), absorbed as a row.
- The rule sites read the table instead of deriving it:
  - `VenueBacking`'s venue and source counts (`presentOnly` became the act counted for);
  - `FamilyScope`'s overrule of a feed link;
  - `ConstraintEdges.KeptOverruledApart`, the kept listing's join;
  - `Families.siblingDenying`, a sibling's withdrawal;
  - `ClusterVoting.onlyWithdrawn`, the group vote's withdrawal. It and `siblingDenying` now read one row where they read
    two, which is how Keswick's two paths came to disagree.
- `ProvenanceSpec`: the table's rows (a kept listing backs, corroborates and withdraws, and never counts toward an
  overrule or joins an overruled listing; the credit row spares Keswick's credited billing alone), and a scan of the
  identity code. A listing's or backer's kept mark, a credit or house sparing a denial, or a present-only flag read
  anywhere but where provenance is defined fails it. On the commit before, it names the two readings of the credit row,
  the kept marks in `ConstraintEdges` and `FamilyScope`, and `VenueBacking.presentOnly`.

**Measure.** Run 41381954546, base = stage 2: 0 model and 0 final takes moved in pl, de, uk, us and es. LANDABLE.

**Drift.** Rows at seeds 9 and 10 in all five countries, run 41386009340: all 10 clean. An earlier run at seeds 53 and 54
(41368259117) was clean too.

### 7.4 Stage 4: a decision awaits its whole read set (no take moved)

**What landed.**

- `ResolverDecisions`: a cluster's decision awaits every open question of its FAMILY, not only its own cluster's and
  its must-links' reach (589a6e3ea, #595 Torrington). A decision reads its family's evidence beyond its cluster: the
  family's pool (every member's searches), a sibling's denial withdrawing a member's own match, the family majority a
  facts-free cluster follows, a sibling's own match the group vote may not take back, and the solver's reach. So the
  take is Pending while any search, venue page or awaited record of its family is open. Pending only: the film is
  served all the same, so no take moves.
- `ReadSetClosureSpec`, over every committed slice:
  - the outcome property: the slice's answers withheld and filed in four rounds of two seeded orders, and its
    listings arriving along arrival chains with their pages unread and then read; no decided take may move by a
    filing. It passes on the commit before as well, because the slices' moves were Torrington's, already covered by
    the reach;
  - the read-set property, which has the teeth: for each family the whole resolve splits into clusters, one listing
    per cluster has its venue page withheld; where its own take then awaits that page, no listing of its family may
    be decided. On the commit before it fails on 7 slices: UK CBeebies "Treasure Island" (two slices), Halstead
    "Così", the two BTS "Arirang" slices, RBO "Romeo and Juliet" and Penrith "Carmen".

**Measure.** Run 41389164653, base = stage 3: 0 model and 0 final takes moved in pl, de, uk, us and es. LANDABLE.

**Drift.** Rows at seeds 9 and 10 in all five countries, run 41411465401: all 10 clean. An earlier run at seeds 57 and
58 (41368390631, UK seed 58 being the Addams seed) was clean too.

**The cost of the wider waits.** Stage 3's drift at the same seeds (41386009340) against stage 4's, with the mean listings
Pending per projection:

| cc | seed 9: stage 3 → 4 | seed 10: stage 3 → 4 |
|----|---------------------|----------------------|
| de | 2730 → 2753 (+0.8%) | 2777 → 2787 (+0.4%)  |
| uk | 17166 → 17204 (+0.2%) | 17160 → 17210 (+0.3%) |
| pl | 6795 → 6800 (+0.1%) | 6803 → 6817 (+0.2%)  |
| us | 39612 → 39636 (+0.1%) | 40238 → 40239 (0%) |
| es | 2458 → 2458 (0%)    | 2748 → 2748 (0%)     |

The listings ever Pending rose by at most 70 (DE seed 9). The minutes a listing stays Pending (p50 and p90) did not
move in any country.

### 7.5 The solver's ambiguity holds through the weaker tiers (Torrington)

**Mechanism.** `ConstraintSolver` rule 3 judged ambiguity tier by tier. Torrington's bare "Dracula" reaches the 1931
film (138) and the 1958 film (11868) by same-title links, so that tier left it alone; the next, weaker tier's
same-search-form link reached the 1958 film only and joined it. With "Dracula (1931)" present but "DRACULA (1958)" not
yet, the strong tier was unambiguous and took 138: 11868 → 138 → 11868 as the listings arrived.

**Fix.** A node a tier finds ambiguous stays left alone in every weaker tier: weaker evidence does not settle what
stronger evidence found contested. Monotone: a node's reach only grows as listings arrive, so it only becomes
ambiguous, never un-ambiguous.

**Proof.** `ConstraintSolverSpec` "leave a node a stronger tier found ambiguous alone in the weaker tiers too" fails on
the parent and passes here (seconds). `PopulationMonotonicitySpec`: Torrington's row removed from
`identity-population-flips-known.tsv`; no new flip in any committed slice. Torrington's take at full population is now
none (11868 → 138 → none along one chain).

**Measure.** Run 41459539675 (base f43b1fcd5e, all five countries): one MODEL take moved, Torrington's "Dracula" 11868 →
none; no final take moved. Judged right (87 minutes and a 12A on Halloween fit Hammer's 1958 film, not the 1931 one
nor Besson's 2025 film), so a right → none at the model stage. Accepted under "unmatched beats wrong": the bare title's
same-title links reach both the 1931 and the 1958 films, and only a weaker search-form link chose between them.

**Cumulative measure.** See §7.8's end: with §7.5–7.7 landed together, only Torrington's model take moved.

### 7.6 A sibling's arrival no longer withdraws a borrowed take (DCA "+ Recorded Q&A")

**Mechanism.** DCA's "These Violent Delights + Recorded Q&A" bills no fact of its own that fits (its runtime is the
ballet's plus the recorded Q&A), so it took the ballet's record (1462368) from its family: none → 1462368 → none along
the arrival chains. Two arrivals each took it back:

- Picture House Uckfield's "Scottish Ballet: These Violent Delights + Q&A" searched "Q&A", which put the film "Q&A" in
  the family's pool; the calibrated vote's rival check ("a film the title names alike fits its facts better") then
  refused the pooled film. `eventNamed` missed the rival: "Recorded" was no event word, and it compared pieces by
  equality, not as runs of the event's words.
- Kiln Kilburn's "Scottish Ballet: These Violent Delights" denies the record as a screen namesake of its house's work,
  which withdraws Kendal's uncredited own match (`Families.siblingDenying`). Kendal still takes 1462368 by the vote,
  but DCA, already in round A's cluster of the film's accepted nodes, had no film for the family round's `sameFilm`
  exemption (`filmOf` read only accepted and voted nodes), so the learned listing-to-listing cannot-link between DCA
  and Kendal (their runtimes 20 minutes apart) stood and kept DCA out.

**Fix.**

1. A film a piece joined as an event names (`eventNamed`) stands as no rival in the calibrated vote either, as it
   already stood as no runner-up; `eventNamed` reads a piece as a run of the event's words; "recorded" is a
   side-event word.
2. `filmOf` also gives a node the film of its round-A cluster when every accepted member of that cluster took that one
   film: a node the first round put with one film's nodes holds that film for the `sameFilm` exemption, whichever of
   its siblings' own matches a later denial withdraws.

**Proof.** `IdentityMeasuresSpec` (the "+ Recorded Q&A" piece) and `PopulationMonotonicitySpec`: DCA's row is removed
from `identity-population-flips-known.tsv` (the spec fails with only the first fix, its Kiln arm still flipping), and
no other committed slice gained a flip. DCA's take at full population moves none → 1462368, the right film.


**Backed out (same day).** The whole-corpus measure (run 41465290558) found the first fix's price: UK Riverside
Woodbridge's "Dracula + Q&A" (Hammer's 1958 film, judged right by its synopsis) went to Besson's 2025 film. That
listing publishes no fact; the "Q & A" (1990) rival had been all that kept the IMDb and RT families' calibrated vote
from guessing between the two "Dracula"s on rank alone, and once it voted, `families.current` outranked the venue's
cast tie-break. Guarding the skip by the listing's own facts, or by the rival's listing being a sibling's, brought
DCA's flip back, so the rival check is restored as it was. Without it the second fix alone made a new
film → none → film flip for DCA in the `…-nohexham` slice, so it is backed out too; `eventNamed`'s token runs and the
"recorded" word stay (no take moved by them). DCA's row is back in `identity-population-flips-known.tsv`: it is a
named remainder, the facts-free rival check standing in for a near-tie that no rule yet names.

### 7.7 A facts-free member neither keeps nor withdraws a withdrawn vote (Titans, Grünstadt)

**Mechanism.** `ClusterVoting.onlyWithdrawn` refuses a cluster's vote for a film every member took alone and a
sibling's NotTheWork denial withdrew. Filmwelt Grünstadt's "TITANS AWAKENING" took 1690206 alone; Luxor's feed-linked
short withdrew it (none). When the bare "Titans" node joined its cluster, the rule no longer held for every member
(the bare node's alone take, on its title only, was 10637), so the pooled vote re-took 1690206; Essen's arrival
dropped it again: none → 1690206 → none.

**Fix.** The rule reads only members that publish a fact (and needs one): a facts-free member's title is already the
others', so it brings no evidence that answers the denial.

**Proof.** `PopulationMonotonicitySpec`: Grünstadt's row removed, no new flip in any slice. Its take at full population
is none, unchanged.

**What stays in the slice (class b, inherent).** CinemaxX Dammtor and Raschplatz's bare "Titans" (one node) and Luxor's
"Titans, Malayalam/english": none alone (or 10637 on the title alone), 869152 once Kino's credited Prince Joy billing
arrives (the only film the family states), none once Grünstadt's Ben Ruffin film makes the family contested (the
solver's ambiguity). At full population they take what they take with the contest present, none; the borrow at the
middle populations is right with the data then present, and no rule over the present listings can know that a
contesting sibling will come.

**Cumulative measure.** See §7.8's end: with §7.5–7.7 landed together, only Torrington's model take moved.

### 7.8 A sibling's learned denial against a film it credits withdraws nobody (Oxide Ghosts, It's Nice Up North)

**Mechanism.** A learned listing-film rule (`LearnedCannotLinks`: "runtime.gap>=6 AND year.distance>=2", "runtime.gap>=36
AND title in {none,overlap}") denies a film on the listing's own runtime or year. Read from a SIBLING, that denial
withdrew members' own matches (`Families.siblingDenying`) and drew a denies-film cannot-link (`Families.denies`). UK
"Oxide Ghosts: The Brass Eye Tapes + Q&A": Cultplex's billing credits Michael Cumming but dates it 21 years off and runs
30 minutes over (the Q&A), so its rule fired and withdrew MAC's and Quad's own matches; "It's Nice Up North (20th
Anniversary + Q&A)": Hexham's +40 minutes withdrew the others' match crediting Graham Fellows. Each went none → film →
none as the family arrived, and ended none, wrong.

**Fix.** Learned denials carry their own kind (`Denial.Kind.Learned`). One whose own scoring credits the film's
director (`Authority.splitDenial`: the sibling's facts split, its credit naming the film) neither withdraws a member
nor counts as the sibling denying the film. It still holds for the sibling itself. A function of each sibling alone:
monotone.

**Proof.** `ProvenanceSpec` (two "Oxide Ghosts" billings crediting Cumming, one [1996]) fails on the parent, passes
here. Two new drift slices from recording 41379008920 (`quad-derby-oxide-ghosts-…`, `gala-cinema-durham-it-s-nice-up-north-…`)
fail `PopulationMonotonicitySpec` on the parent and pass here; their full-population takes move none → 475376 and
none → 197993.

**Backed out (2026-11-02).** The cumulative measure (run 41500994372) found one new wrong take: DE Centralkino
Wittenberg "Panem-Mockingjay 1+2", a double bill of both parts, went none → 131631 (Mockingjay Teil 1). The billing
credits Francis Lawrence, so its own learned runtime denial ("runtime.gap" against Teil 1) became a split denial and no
longer kept the double bill out of Teil 1's family. A slice bisect over the branch (`identity-slice de --venue Wittenberg
--title "Panem-Mockingjay 1+2" --recording 573 --fills …`) names this commit as the first to move it. Restoring only
`Families.denies` still left it wrong: the withdrawal exemption alone lets it in. A director credit cannot tell a Q&A's
+30′ from a second feature, and no fix by the evidence was quick, so the code is reverted. The two drift slices stay;
their three listings are known rows in `identity-population-flips-known.tsv` until a fix can tell an event's length
from a double bill's.

**Cumulative measure (2026-11-03, §7.5–7.8 as landed).** Run 41550488381 (base 2d37f0531, head e94ef5df8, all five
countries, both stages): one take moved, Torrington's MODEL "Dracula" 11868 → none (§7.5, accepted: the title reaches
both the 1931 and 1958 films and only a weaker link chose); no final take moved; no right → wrong, no new wrong. Drift
run 41534283799 (seeds 11, 12, ten rows): clean. The §7.8 run before the back-out (41500994372) had also gained the
four "It's Nice Up North" billings (judged right, 197993) and Wittenberg's double bill (wrong); both went with it.

### 7.9 Titans: inherent bare-borrow flip, baselined by owner decision (CinemaxX Dammtor, Raschplatz)

Convergence #728 (DE takes-drift-1, seed 47; red 9400367d430f): the one node of bare "Titans" at CinemaxX Dammtor and
Raschplatz goes 869152 → none → 869152. Reproducer: `drift-slices/de/cinemaxx-titans-inherent-08f3a19c` at seed 20 (its
`seeds` file), the seven DE "Titans" listings, about 20 s. A slice can't replay seed 47 itself, because
`IdentityDrift.sequence` shuffles the slice's own venues. The cause, step by step:

1. **Borrow.** At boot, Dammtor, Raschplatz, Kino am Raschplatz (kinoheld, Prince Joy credited → 869152, 66%) and
   Luxor are present. The bare node's own 10637 (46.5%) is withdrawn by the Prince Joy node's denial
   (`Families.scala` ~34), so the cluster takes Kino am Raschplatz's 869152 (`ResolverDecisions.scala` ~30).
2. **Rival.** Essen and Grünstadt arrive. Grünstadt's Ruffin node (TITANS AWAKENING, 1690206) must-links by the same
   title to the bare node and cannot-links the Prince Joy node. The solver leaves the bare cluster (2 venues) alone,
   where 869152 has 3.5%: BelowThreshold. Unmatched beats wrong. Nothing was pending and `contested` played no part.
3. **Fill.** Regensburg arrives. The cluster now has 3 venues, so the `venues.current` fill (WidelyBilled 3,
   `UnifiedEvidence.scala` ~296) gives 869152, which is also the whole resolve's take (own match refused, fill only).

Each take is the honest one for the listings present. No stale state or unanswered question is involved, so no
mechanism fix exists. Curing it means changing a rule over the present listings, for example counting co-titled venues
outside the cluster toward `venues.current`. That is identity design, to be measured on the full corpus later. The
owner (2026-11-02) chose to baseline the two rows as a named exception. `scripts/ci/commit_rules.py` (`owner_approved`)
admits a baseline row only from a commit carrying `Owner-Approved: <date> <why>`, with each row's note reading
`OWNER-APPROVED <date>: <why>`. The rows go when that rule lands. Seed 31 of the same slice flips Regensburg the same
way, and it is NOT baselined.
