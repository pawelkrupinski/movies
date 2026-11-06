# No drift by construction (design and audit, 2026-10-25)

## The principle (the owner's, 2026-10-25)

**No stickiness, no order dependence.** Every change re-evaluates everything exactly as if all the data had been there
from the start. That covers GROUPING (which listings form a cluster) as well as takes.

- State may be only persisted INPUTS, or a CACHE of a pure function of them. A cache must give the same result if it is
  dropped.
  - Inputs are: listings; the answers to questions (the question memory and its outcomes count); pins; TMDB store
    entries; venue facts; the clock's year.
- Anything that would have affected a decision had it been there from the start must reopen that decision when it
  arrives later. That includes an ABSENCE: an answer missing, failed, or not asked yet because a prerequisite was
  missing.
- Ids and keys (FilmIds, cluster keys, staging rows) may stay stable for NAMING only. They never influence a grouping
  or a take.
- The definition of done is the equivalence harness: incremental, in any order, with any restart points, equals a whole
  resolve over the final inputs, for grouping and takes alike. Equality is exact: no tolerance and no allow-list.

Once this holds, the nightly live drift check (`scripts.IdentityLiveDrift`, alert `IdentityIncrementalDrift`) and the
convergence drift step (`scripts.IdentityDrift`) become weekly, and then retire. What remains for that is listed under
"What still blocks retiring the checks".

## The differential gate (the owner, 2026-11-04)

"A gate that only fails on what the change adds." Supersedes the shrink-only known-flips baseline (2026-10-29) and the
rotating seeds (2026-10-31): 3 of 7 red streaks to 2026-11-04 ended with no identity change -- an old flip failed
whichever unrelated commit drew its seed (`~/Desktop/claude docs/convergence-reds-root-causes-2026-11-04.md`, fix B).

- **Fixed seeds.** Each country's `takes-drift-1` / `takes-drift-2` rows replay the seeds
  `test/resources/fixtures/drift-gate-seeds.tsv` assigns them (`scripts/ci/drift_seeds.py`), the same on every run;
  a dispatch may pin others (`scripts/identity-drift-ci --seeds`).
- **Head against base.** `scripts/ci/identity-drift-check.sh` replays the head's seeds, then its base's on the same
  seeds (the parent on main, the merge-base on a branch; no replay when the identity code is the same, and a stored
  result -- release `identity-drift` -- when one exists), and `scripts/ci/drift_diff.py` judges: a flip only the head
  shows FAILS, naming the listing; one both show is `pre-existing (backlog)`, never failed; one only the base shows is
  `removed:`. Divergences (incremental != whole end state) fail outright, as before.
- **One seed, one arrangement, whatever the code asks** (2026-11-05). Head and base are comparable only if a seed hands
  both the same arrangement: the venues, who leaves, each re-scrape's half, and which held answers each fill files. The
  fills drew one number per held question from the arrangement's own stream, so a head asking ONE question more or fewer
  re-rolled everything after it: DE seed 8 (drift 42003442640) flipped Geister at four venues under a Roman-numeral
  narrowing that reads no Geister title, while two Titans and Buddies flips "went away" -- the same reshuffle a parked
  StageRelays change had shown. A fill now draws each question by its own id (`IdentityDrift.Share.filed`), and the
  stream draws the arrangement alone.
- **The scenario guard** (2026-11-05). That coupling went unseen for two weeks, so it is now checked on every row: each
  replay writes its SCENARIO per seed (`scenario-<cc>.tsv`, `IdentityDrift.Scenario`: the venue arrangement -- the
  listings dealt, the venues' order, boot, arrivals, who leaves, each re-scrape's half -- and every fill draw) and prints
  its digest (`## <cc> scenario [seed N]: arrangement …, K fill draw(s) …`); the base's is kept beside its flips
  (`drift-{green,calc}scenario-…` on the store). Before any flip is judged, `scripts/ci/drift_scenario.py` compares
  what does not depend on the change's own questions: the arrangement whenever both were dealt the same listings, and
  the draw of every question BOTH ask. A difference is a HARNESS ERROR (the row exits 2, `harness error: …` in
  `verdict.txt`, an `::error` titled `identity drift harness`), never flips: fix the harness, not the code. A base stored
  before the guard has no scenario and is not compared (said). The nightly explorer replays the head alone and only
  prints the digest; a per-family check replacing the rows carries no seed and needs none.
- **The backlog.** `test/resources/fixtures/identity-flip-backlog.tsv` is every known flip, a row each -- kind `drift`
  (a seed found it) or `population` (`PopulationMonotonicitySpec`'s arrival walk did) -- with its owner. It decides
  no drift verdict: it is the work list. A fix removes its row. Rows arrive only by the explorer's filing (below), a
  tightened check's birth (`New-Check:`), or the owner's named decision (`Owner-Approved:`) -- `scripts/ci/commit_rules.py`.
- **The explorer.** `.github/workflows/identity-drift-explorer.yml` dispatches main's drift rows nightly on random seeds
  off the gate (`drift-explore: auto`: DE 4, UK 6, US 6, PL 8, ES 24 seeds a night), the head alone, never red, and
  publishes the flips no backlog row names as `new-flips.tsv` on the release `identity-flip-explorer`. Actions cannot
  push main, so agentctl adopts the asset: one item per flip, and one landed commit adding its backlog row and its seed
  to the gate (`scripts/ci/drift_explorer.py file`), the only commit `commit_rules.py` lets add a drift row freely.
- **Coverage.** No run replays fewer seeds than the rotation did (2 a country a run; ES 4 now), the base side adds
  replays, and the explorer adds the seeds above every night.
- A newborn check's own findings go TEMPORARY on `identity-flips-allow.tsv` (`New-Check:`), never a flip that already
  exists.

## Fixing a drift or flip red (the owner, 2026-10-29)

- **A red fix names and fixes the incremental state that diverges** — a lag, a budget, the queue's order, a cache, a
  restart's key — so the incremental path ends where a whole resolve does. **Red fixers add no identity resolver rule**
  as a flip or drift fix: a new or changed rule (IdentityResolver, FeedLink, FamilyScope, the acceptance and scoring
  rules, the agreement's rules, the title rules) is identity design, measured on the full corpus in every country,
  outside red fixing. A `Fixes-Red:` commit touching rule code carries `Identity-Design: <doc>`, the design doc that
  measured it, a file of the commit's tree (`scripts/ci/commit_rules.py`, pre-push's `driftguards`). Mechanism files — AgreementStage's state
  handling, the incremental resolver, the stores and keys — need none.
- **Known flips wait on the shrink-only baseline** ("Known flips" above); new flips and every divergence fail.
  Neither allow-list is edited to get green.
- **A drift or flip red is not closed until its slice is committed**: `scripts/identity-slice <cc> --venue V --title T
  --emit-drift-slice` cuts the listing into `test/resources/fixtures/drift-slices/<cc>/<name>/`, widened with every
  answer the incremental sequence reads, and `scripts.IdentityDriftSlicesSpec` replays every slice through that
  sequence on 12 seeds in the unit suite (Tier 1 of the drift gate; `IdentityDrift --slice`, at least one listing
  leaving and returning). The fix's push carries the slice, or `Drift-Slice: none -- <why>` when the red has no
  listing (a CI script, a timeout); a fix already on main without either is answered by a later commit's
  `Drift-Slice-For: <sha> none -- <why>` (main is never amended). **The legs' seeds rotate** (the owner, 2026-10-31): `takes-drift-1` / `-2`
  replay their run's pair of 1..64 (`scripts/ci/drift_seeds.py`), a red names its seed and reproducer, and a red seed
  found anywhere becomes a committed slice — `docs/design/identity-slice.md`, "A red seed becomes a slice".
- **The harness carries its self-test**: a change to IdentityDrift, IdentityLiveDrift, TakeFlips or
  identity-drift-check.sh comes with a capture-replay case in the same commit — IdentityDriftSpec,
  CorrectionFlipReplaySpec, TakeFlipsSpec, identity-drift-check-test.sh — proving the replay does what the worker does
  (a third of the week's drift reds were the judge's: 91cfbe7e8, 7b586af0f, 5df3295bd). A pure refactor says
  `Drift-Harness-Refactor: <why>`.

## The A/A self-test (review 2026-11-06, P5; the owner approved it)

The differential gate compares head and base under one harness, so a harness that is not a function of its inputs
(the shared rng that changed what a change asked) reads as the resolver's defect: 13 convergence episodes, ~71 red
hours. The capture-replay case above proves one case; the A/A run proves the whole replay:

- **When:** a push to main whose range (since the previous convergence run's head on main, else the parent) changes a
  path of `.github/harness-aa-paths.txt` -- IdentityDrift, IdentityLiveDrift, ReplayOrder, TakeFlips,
  identity-drift-check.sh, drift_diff.py, drift_seeds.py. Main's convergence preflight decides it
  (`scripts/ci/harness_aa.py decide`). Never in pre-push or `agentctl land`: it runs after landing, in CI only.
- **What:** each leg adds one row, `takes-drift-aa (<country>)`: identity-drift-check.sh on the row-1 seeds with
  `DRIFT_BASE=<head>` (no base replay: base and head are one commit) and `DRIFT_TWICE=true` (the head replayed twice,
  the drift and flips TSVs diffed), storing nothing.
- **Verdict** (`harness_aa.py judge`): two replays that differ fail as `HARNESS FAULT, not the resolver's`; a
  divergence both replays agree on is the head's own, a notice here and takes-drift-1/2's verdict.
- **Cost:** nothing on a push that touches no harness path (no row is created); on a harness push, five rows, each one
  replay pair of the row-1 seeds (the step's twice ceiling, 140 min). A preflight that proves a reuse skips every row,
  this one included.
- **Gating:** the rows are `unproven` in `.github/gating-checks.tsv`; after the first green run on main,
  `scripts/ci/gating_checks.py prove --workflow-file identity-model-convergence.yml --run <id> --number <#n>`.
- **Not yet covered:** the takes rows and identity-measure. `identity-takes-check.sh` ends at once on base == head
  ("nothing can have moved"), so their A/A needs a switch inside that harness; left for a later change.

## The harness

`identity/src/test/scala/services/identity/agreement/IncrementalWholeEquivalenceSpec.scala` runs in unit and CI on every
commit, in about 20 s.

Each case runs 40 seeds, and each seed is a random history:
- listings arrive in batches and leave;
- TMDB's searches and records, the families' questions, the posters and the IMDb finds are filed a round at a time;
- some questions are ATTEMPTED (asked and failed) before their answer lands, or for good;
- the clock crosses a new year;
- the model and stage restart over their stores, with the cluster keys kept, lost or renumbered.

It holds four properties:

1. **Stepwise.** After EVERY event, the incremental takes equal a whole resolve of the inputs at that moment. The
   whole resolve is the model's resolve of the listings held, then a fresh stage applied once. The comparison covers
   every cluster's members and its film, fallback and basis.
2. **Settled under the production correction budget.** At the end of a history, the settled incremental equals the
   settled whole resolve.
3. **A restart alone moves no take.** This holds with keys kept, lost or renumbered, and with the restart's first
   apply reading no model take afresh (a deploy's budget spent). This is the run 1530 class.
4. **Reach.** The cases do reach Agreed, Filled, Corrected and the model's matches, so the properties are not vacuous.

The cases:
- generated corpora (`GeneratedIdentityCorpus` with family records, venue posters and finds of the films drawn);
- the shapes production met: US "SEVENTEEN World Tour" (a fill of a current release across a new year), and PL Kino za
  Rogiem's "Lalka" (a model take corrected by its poster and two families).

**Teeth.** On forgejo/main 50dfc4ad4, the harness fails:
- stepwise and settled fail on the fill case: the verdict filled in December stood into January;
- restart fails on the correction case: renumbered keys plus a spent budget put the model's take back. This is the run
  1530 class: a correction found by a key that no longer names it.

The model alone has its own property harness, `IncrementalResolverSpec`. It checks every event of a random sequence
against `IdentityResolver.resolveWith`, plus every arrival order of the cross-family cases. The new harness drives that
same model.

### Per input kind: `LateInputEquivalenceSpec`

`identity/src/test/scala/services/identity/agreement/LateInputEquivalenceSpec.scala` holds one input kind at a time to
the principle. Its gates and runs are in `LateInputs.scala`, beside it. It decides without the input, lets the input
arrive, and requires the incremental takes to equal a whole resolve with the input there from the start. The kinds:
- family answers, TMDB finds and records;
- searches, new hits included, and people (director searches);
- posters, catalogue ids, venue pages, synopses (TMDB descriptions) and broadcast days;
- pins, and listings joining and leaving.

Every case runs:
- under each ABSENCE its kind can have:
  - not filed (`Unknown`, never asked);
  - asked and failed (`Unknown` and attempted, so the stage settles without it);
  - not existing yet (a `Known` empty answer, such as TMDB holding no such record or hit);
- with no restart, a restart while the input is withheld, and a restart after it arrived.

An arrival reaches the stage as it does in the worker:
- The model is told what it asked (`AnswersChanged`).
- The stage's version moves as `FamilyAnswerStore.agreementKeyOf` moves it:
  - always for a family, poster or catalogue answer;
  - for a find or a TMDB record only while the stage waits on it, or while a kept verdict read it (`AgreementStage.reads`);
  - for a search only while a kept verdict read one;
  - never for a venue page.
- The stage's `changes` names each filing.

Every case must have TEETH: the late input moves the whole resolve.

The production drift rows are also cases in this spec. They come from `scripts.IdentityLiveDrift` over the 2026-10-25
recording. Each keeps its row's titles, films and families, with its inputs reduced to what decides it. The prod
captures are not in the repo.
- Dolly ×10, Rental Family ×8, Ice Cream Man ×4, Jump Scare, Miss Mary, Karmele and I.K.U: the families agree, and the
  IMDb id's find lands after the verdict (attempted first, or answered "none" first).
- CBeebies Panto ×11: a pantomime's own record new in TMDB, which the model decides apart as no film and no fill takes.
- Ostrze ×2: evidence against a take that arrives after it (the venue's poster, hashed late).
- Run 1530 is the "corrected Lalka, a cluster before it gone" listing case. The random histories' restarts also cover
  it.

A case the incremental path does not cure yet is marked `pendingUntilFixed`, and `LateInputEquivalenceSpec.Pending`
names it with its root cause. On the no-drift-2 stack (e57becb42) there is one: a film new in TMDB (CBeebies). The
model's decision is identical to the whole resolve's, yet after the film arrives the live stage takes no fill, where a
fresh stage fills it. The verdict the live stage files then stands across a restart.

**Teeth.** The spec was also run with the version moving by the pre-fix rule (finds and records only while waited on,
searches never). It then fails on:
- late finds answered "none" first;
- late broadcast records and days;
- late TMDB descriptions.

**Runtime.** The whole spec takes about 3 s, inside the unit tier. No heavier variant needs an `it` tier yet.

## Audit: every piece of state the incremental path carries

Each item has a verdict:
- (a) cannot affect a take or grouping: a cache or a name, with its proof;
- (b) an input the whole resolve must read too;
- (c) a dependence on history, which is removed.

Every map, set or Memo the identity code keeps is reviewed in `NoDecisionKeyedByMutableIdSpec`: one keyed by a cluster,
listing or family id carries the content digest it was decided for and compares it on read, or is a pure name index,
one pass's scratch, or a store's rows. A new one fails the spec until it is reviewed. Its first sweep (2026-10-26) found
two waits kept by cluster key alone: `waiting` outlived its shape when a cluster went back to one whose verdict stands,
and `correctionWaits` when a reshaped take's budget left it unread. Both now carry `Waiting.listings`.

**One file per item** in `docs/design/identity-no-drift/state/`, named `<NN>-<slug>.md` (`01b-...` for an item
between two): a `# <NN>. <state>` title, then `- **Where:**` and `- **Verdict:**` lines, then the fix or proof.
Add a file for new state; edit one in place when its verdict moves. Items are files, not rows of one table, so agents
auditing in parallel never touch the same line (`SharedFileLayoutSpec`). Count a verdict with
`grep -l 'Verdict:.*(b)' docs/design/identity-no-drift/state/*`.

Runtime inputs are held structurally (2026-10-26): `IdentityRuntimeInputsSpec` fails on a collection the identity path
opens with no `ResolveRole` in `services.MongoCollections` (an input and the key it is in, or state and why it decides
nothing), a resource a closure source names that the closure does not digest, and a stage parameter neither versioned,
receipted nor inert; `NoRuntimeLookupsInIdentitySpec` bans reflection, the environment, files and classpath reads
outside `ClasspathResource` in identity code.


## Backfill of prod's stored verdicts

The change edits `AgreementStage.scala`, so `AgreementStage.RulesVersion` moves. On the first apply after the deploy,
no stored verdict stands: `v.rules == decidedUnder` fails for all of them. Every one of them was cached with an
incomplete receipt, and every one is decided again with its full receipt, a budget at a time:

- Agreement verdicts: `BackfillPerApply` (500) an apply. Until a later apply's budget reaches one, it stands as it
  was, and the stage reports a backlog so the next apply goes on. Before this change, every deploy touching the stage
  decided every agreement verdict again on its first apply, unbounded: the boot pass that OOM-killed worker-us.
- Corrections: `CorrectionsPerApply` (200) an apply, as before. Until then each stands as last decided. A take with no
  decided correction (never read at its shape, or waiting on an answer) spends none of it: it is read at once, never
  served as the model took it meanwhile (2026-10-29, the drift replay's flips, convergence run 3656).

**Order.** Both budgets are spent in the clusters' content order: by the least listing key of each cluster, which no
two clusters share (`AgreementStage.inContentOrder`). The model hands its decisions over in that order already, and
the stage sorts any other order itself. Which verdicts an apply decides again, and so the interim takes, are a
function of the current clusters. They never depend on the order the store's verdicts or the model's decisions were
read in, or on arrival. Spec: `AgreementStageSpec` "decide again the same verdicts stored under other rules first,
however they and the decisions are read".

**A verdict decided again to a wait is dropped.** The new rules may decide a cluster to a wait (a family's question
not answered yet). Its old verdict used to stay stored. On the next apply it then took the budget again ahead of the
clusters behind it, or, with the budget spent before it, stood again and took its cluster back to the old take: a
flip that depended on how far the budget reached. It also held the gauge above 0 for as long as the cluster waited.
Now it is dropped with the apply that decided it again: a cache of nothing under these rules. Spec: `AgreementStageSpec`
"drop a verdict stored under other rules once it is decided again to a wait, spending no later budget on it".

**Prioritising the takes that would move: not done.** Whether the new rules change a take is known only by deciding
it, and that is what the budget pays for. Every cheap signal (a verdict that takes a film, a short receipt) predicts
nothing about the new rules. Spending part of the budget on a pre-check would only slow the backfill down. The
lever that matters is how short the window is, so the stage keeps the window short.

**Time to zero.** The projection applies the stage on every scrape, answer or stored-film change (debounced), and at
the hourly reconcile. A projection the stage left a backlog is not settled (`IdentityProjection.settled`). It is tried
again by `EventTrigger` after 1, 2, 4 and 8 min, then every 15 min (`RetryAfter`, doubling to `RetryAtMost`). Before
this, a backlog was read on only as events came. For `N` verdicts stored under other rules in one country, the
backfill takes `k = ceil(N / 500)` applies. With no event at all, that is about `15 + 15·(k − 5)` min once `k ≥ 5`:
- 2,500 verdicts take 15 min;
- 5,000 take ~1¼ h;
- 10,000 take ~4 h;
- 12,500 take the 6 h of the `IdentityAgreementBackfillStalled` alert (`for: 6h`, `> 0`).
Scrapes and answers only shorten it, since every one is another apply. Prod reached 0 within hours the last time.

The corrections drain alongside, 200 an apply. Each apply's cost is bounded by the two budgets, never by the corpus.

Progress shows as `kinowo_worker_identity_agreement_clusters{state="incomplete"}`, the verdicts decided under other
rules still to decide again. It reaches 0 per country once the backfill is done. Spec: `AgreementStageSpec` "decide
verdicts stored under other rules again a budget an apply at a time".

Takes move where prod's stored verdicts were wrong; the landing check lists them. Not measured yet: peak heap on the US
corpus during the backfill (CI, `identity-measure` on the US recording with a stored-verdict boot).

## Simplifications

These are evaluated against the principle: "every decision is a pure function of recorded inputs, cached by its
dependency set". Each is its own refactor commit, landed with 0 moved takes. None is landed yet.

| Item | Change | Est. net lines |
|---|---|---|
| `unreached` | Derived from the found verdict's receipt (its unanswered entries); the load-time rebuild goes (1613e6f6a) | −3 (done) |
| Fill blind wait (`Waiting.blindAt` for a fill) | Subsumed by the receipt: an unanswered TMDB read is in it and reopens the verdict. It stays only for the staging screen's `onTmdb` | −20 |
| Family-only `unanswered` list | Deleted in this change: every family read, unanswered too, is in the receipt | −4 (done) |
| `adopt` (legacy verdicts by first listing) | Deleted in this change: found by digest | −20 (done) |
| Merge union | Deleted in this change | −6 (done) |
| `IdentityDriftCheck` (unwired since the OOM) and `AgreementStage.forget` | Deleted (ddd1c1951) | −309 (done) |
| Stage-life `rereads` | Deleted (f28034e6d): the question memory says | −12 (done) |
| `StoredAsWritten.Keys` and the drift replay's key plumbing (91cfbe7e8) | Keys decide nothing now, so the replay need not keep them | −20 |
| Correction receipt as the verdicts' | One typed receipt (`Map[Dependency, Long]`, one codec), one validity rule (`Memo.clean`) for verdicts, corrections and candidates (141b3f2db, 603c81e77) | done |
| `IncrementalResolver` as the whole resolve plus a dependency-keyed cache | NOT recommended now. It already is a digest-keyed cache over slices (item 1); its update algorithm only decides which caches to re-check, and `IncrementalResolverSpec` holds it equal every event | — |
| Staging events derived from waits | Possible once waits derive from receipts plus memory. A later step | — |

## Unifications (2026-10-25)

1. **Every external read a typed `Question`, every receipt typed** (fd1c1bda1, 141b3f2db). `ObservationReads` names a
   model question by `Question`. A receipt is `Map[Dependency, Long]`: a family `Answer`, an `Attempted` question, a
   `Lookup` of a `Candidate` / `VenuePage` / `FilmRecord` aspect through the venues' or TMDB's lookups, a question `Asked`,
   the `Year` — one codec (`Dependency.encode`/`parse`, total; stored as `[id, digest]` pairs, never field names). A
   correction's receipt is its verdict's. The receipt key is `(Question, how it was read)` rather than the bare Question:
   the stage reads one TMDB record through two lookups (the venues' store and TMDB's store-else-live) and in four aspects
   (record, release day, cast, description), whose answers may differ until a filing; collapsing them would miss an
   invalidation or read TMDB live for an aspect the decision never read. The worker's filing feed still names filings by
   string (`Dependency.filing` maps a dependency to it).
2. **One dependency-keyed `Memo[K, V]`** (603c81e77): verdicts, corrections and the candidates cache. Not moved, and why:
   - `IncrementalResolver`'s families: their dependencies are `CorpusContext` slice keys digested together (one `Long` a
     family), not Questions; moving them needs a digest per key (worker-us heap) or a combined-digest memo, and
     `resolve(listings, memo)` means replacing the expansion loop (Single path step 4) — not done. `IncrementalResolverSpec`
     still holds it equal to a whole resolve every event.
   - `LiveCorpus`: a materialised view with its own fact graph, not a cache of outcomes; `LiveCorpusSpec` checks it equal to
     `CorpusContext.of` every event.
   - `VenueSlotMemo`: content-addressed (its key is the digest of everything a slot is built from): already keyed by its
     dependency set, nothing to invalidate. Its priors (the previous slots) are an input it reads — see item 7.
   - Poster distances: read per apply (`AgreementStage.Read`), never kept.
3. **One stable-name assigner**: `StableNames` names FilmIds and cluster keys; `ClusterKeys` deleted (2232a8c6a).

## Single path

**Verdict: feasible with caveats.** Running a literal whole resolve on every tick costs far too much. But the incremental model is already nearly "whole resolve, sharded by family, with a cache". So the one-algorithm design means tightening that cache's keys and deleting the memories that are not caches. It does not mean replacing the model.

### 1. Where the incremental path differs from `IdentityResolver.resolve`

- **The decision code is the same.** `IncrementalResolver.update` calls `IdentityResolver.resolveRegion` (IncrementalResolver.scala:222 → IdentityResolver.scala:367), which runs the same `Stages`/`ResolverRun` against the shared `CorpusContext`.
- **Grouping differs.**
  - The whole resolve gets its families from a single `FamilyClosure` over every node's block keys.
  - The incremental path gets them by expansion: re-resolve the touched and stale families, then pull in any family that now shares a key (`joined`/`back`, :225-240), and repeat until nothing changes. In practice that takes at most 2 rounds.
- **Restore trusts stored families.** A stored family is reused if all its listings are still held, its node keys are the same and its slice digest matches (:124-128). `storeId` (the smallest listing, IdentityModelStore.scala:20) only names the document.
- **Agreement memories that are not caches** (AgreementStage.scala):
  - `unreached`: the "settled before, settles again" short-circuit (:636), and its transfer on merge (:337-341);
  - `askedNow`: asked once per stage life (:178);
  - `rereads`: asked once per stage life (:193-197);
  - the correction budget of 200 per apply (`CorrectionsPerApply`, :92), which made interim takes depend on read order.
    Since 2026-10-29 it defers only a decided correction's reading again, which stands as decided meanwhile: a take with
    none decided is read at once (state/21).
- **Gaps in the verdict cache key.** `held` is keyed by the listings digest, the rules and the family-read digests (:584-590). The key leaves out:
  - the fill's TMDB reads (`filledOf` → `IdentityResolver.evidenceOf`, AgreementTakes.scala:30; the code says "its reads are the families' only", :656);
  - `thisYear` (:642).
- **Projection.**
  - `IdAssigner`/`FilmIdCounters` hand out ids by path, on purpose.
  - `ProjectionDrafting` keeps the previous record's data (:146-157), and `heldLinks` keeps what it holds (:204).
  - VenueSlots reads the previous film's slots.
  - `LiveProjectionIndex` is already a proven cache (`ScopedProjectionEquivalenceSpec`).

### 2. What has to be cached, and the cost

Measured costs without caches:

| What | Cost |
|---|---|
| US boot, whole resolve | 342–583 s |
| Agreement, first apply with no stored verdicts | 3.8 GB |
| Same, with corrections | 7.6 s / 5.0 GB |
| PL agreement phase, re-resolving every waiting cluster | 24–48 s / 540 MB |
| Whole projection tick | 36 MB |
| Quiet projection tick | 5.3 MB |

Clustering is cheap (near-linear union-find). The cost is per-family scoring and acceptance, plus `Agreement.verdict` and poster distances. Use `IncrementalResolver.Timings` to get a per-phase breakdown.

The caches needed:
- **Model:** `RegionFamily` keyed by (sorted listing set and node keys, Slice digest, `rulesVersion`).
- **Corpus context:** `LiveCorpus` keyed by the node set.
- **Agreement verdict:** keyed by (listings digest, every read's digest including the fill's TMDB reads, cast and productions, `decidedUnder`, year).

The dirty indexes (`readersOf`, `askersOf`, `filmReadersOf`) stay as a speed-up that must be proven equal to full validation: a periodic full-validation tick with a drift counter, like the projection's "every 12th tick is whole".

### 3. Is grouping order-dependent?

The whole resolve is not. `ConstraintSolver` sorts its input; `FamilyClosure` numbers families by their smallest listing; `Stages` takes the smallest listing per key; `pack` breaks ties by smallest listing.

On the incremental side, the order dependence is limited to interim states: batches of a large title component (:212), and restore trusting stored families. `--drift` found no grouping divergence. Ids depend on the path, which is acceptable only as a persisted input that never feeds a take.

### 4. Migration, step by step

1. **Delete the dead adopt path (0 moved takes):** `IdentityDriftCheck.scala` (139), its spec (80), `IncrementalResolver.reResolve`, `AgreementStage.forget`.
2. **Close the verdict key gaps:** record the fill's TMDB, cast and production reads, and add the year. Moved takes are listed.
3. **Remove the agreement memories that affect takes:**
   - the `unreached` short-circuit;
   - the merge transfer and `StableNames.absorbed`;
   - key `rereads`/`askedNow` by question and answer state.
   The owner HAS signed off: the no-stickiness principle reverses the earlier "a merge never makes a settled half wait anew". List the clusters that wait again.
4. **One entry point:** `IncrementalResolver.update` becomes `resolve(listings, memo)`, where the partition is computed by closure over the memo's block keys and a memo entry is valid only if its key compares equal. Event invalidation becomes an index over the memo, with a full-validation tick and a drift counter. `restore` becomes memo validation. Gated by `--drift` and `IdentityLiveDrift`.
5. **Projection record stickiness:** derive `heldLinks` and the base record from current inputs only. Keep `IdAssigner`/`FilmIdCounters` as documented persisted inputs.

**What can be deleted:** about 150 of `IncrementalResolver`'s 414 lines, `IdentityDriftCheck` and its spec (219), about 40 lines of `AgreementStage`, and `StableNames.absorbed`.

### 5. Risks

- The full-validation tick on US could cause OOM: budget it like `WholeProjectionTick` and skip it under `HeapPressure`.
- `CorpusContext.Reads.of` (IdentityResolver.scala:382) coverage must be complete. Mutation teeth must cover every new key component.
- Step 3 produces interim no-matches after merges.

> Status against this plan (no-drift agent, 2026-10-25): step 1 done (ddd1c1951). Step 2 done: the fill's TMDB, cast and
> production reads, the lean record and `thisYear` are in the receipt; corrections and the candidates cache have
> receipts too. Step 3 done for the `unreached` short-circuit, the merge transfer and `rereads`. `StableNames.absorbed`
> only names a merge to the staging screen now, and `askedNow` only spares a second inline ask (asking, never a take).
> Step 4 is not started. Step 5 is done but for the `searchTitle` and the venue slot's prior-slot carry (item 7).

## One dependency index for reading and notification (2026-10-27)

The owner approved this generalization on 2026-10-27.

### The bugs, one shape

Each of these was something read without the read itself saying when to look again:
- **The year.** It was digested in one form when recorded (its text) and in another when compared (`Known(<year>)`), so
  no verdict stood a re-read (fixed by hand in 9627359ec).
- **Over-unchecking.** An attempt marked anywhere unchecked every verdict whose receipt *named* an attempt, not those
  whose recorded *value* moved: ~58 s and 11 GB an apply on DE and UK (fixed by hand in d16de25b2).
- **The venue page.** Its detail fed the venue slot, but no change was reported to the scoped projection (fixed by hand
  in 5c27f0498, through a side set `VenuePageIndex.moved` the projection drains).
- **Inline asks.** Whether a cluster was "new" was keyed by its cluster key (fixed by hand in 6eb4deba2 to key plus
  shape digest).

Each fix was local. The point of this section is that one mechanism makes the class impossible.

### The mechanism

**1. Every input read is a typed `Dependency`.** The kinds are:
- `Answer(family question)`;
- `Attempted(question)`;
- `Lookup(via, aspect, question)`, covering TMDB records and searches and the venues' detail pages;
- `Asked(question)`;
- `Year`.

A reader reads through one API, and the read records `dependency -> digest` into the reader's receipt.

**2. There is one digest function.** It is `Dependency.digest(answer)`: the digest of the `Answer` a read returned.
Every kind is read as an `Answer`:
- the year as `Known(<year>)`;
- an attempt as `Known(<bool>)`;
- an unanswered question as `Unknown`.

Recording and comparing both go through `Dependency.digest`, and comparing re-reads through the one reader
(`AgreementStage.reread`, the year included). So the form recorded and the form compared cannot differ.
`DependencyReadSpec` holds this for every kind.

**3. One index answers "who read it".** `Memo` keeps, for each indexed dependency, the entries whose receipt names it.
A change event says a dependency moved: a filing on the feed, a mark in the question memory, or the year turning.
It reaches exactly the entries that read that dependency, and of those, only the ones whose recorded digest differs
from what a read gives now. It never reaches an entry merely because its receipt names the dependency.

This works two ways:
- **Pull.** `clean` reads the feed's filings since an entry's check, and re-reads only the dependencies they name,
  comparing each by value (`again`). A filing that changed nothing leaves the entry standing at the cost of that one
  read. Only a moved one sends it to be decided again. Before this, a verdict one filing named was read again whole
  (`stands`). Every TMDB search is filed under one id (`QueryFiled`), so any search filed anywhere re-read every DE
  verdict that read a search, at ~4 s and 1.5 GB an apply (2026-10-27). Corrections already compared their TMDB
  lookups this way (run 1641).
- **Push.** `moved(kind, now)` is for the inputs no store files: the year and attempts. It walks the index's
  dependencies of that kind, reads each once, and unchecks the readers whose digest moved.

`DependencyIndexSpec` holds both to full validation over random receipts and changes of every kind. After any change,
an entry is clean exactly when every dependency it read still digests as recorded. This is the property "any
dependency change puts every entry that read it into the next apply", plus its converse, "and no other entry".

**4. The same events drive the other consumers.** Each consumer reads the change events and resolves them to its own
readers:

| Consumer | Readers | What moves them | Before |
|---|---|---|---|
| Memo uncheck (verdicts, corrections, candidates) | memo entries | any dependency in their receipt, by value | per kind: `uncheckAll` (year), the `uncheckWhere` receipt scan (attempts), filings (by presence) |
| Projection scope | listings, so their clusters and films | a venue page's filing on the one feed (`VenuePageIndex.keyOf`, the key the model reads a page by), through a view of the projection's own | the side set `VenuePageIndex.moved`, drained by `takeMoved` |
| Inline-ask newness | cluster shapes, by their listings' digest | a shape no verdict, check, wait or inline ask holds, under any key | key plus digest, looked up by the cluster's key |

### Why not a push index of every receipt entry

A push index from every dependency to its readers would cost memory. Each verdict's receipt is about 20–60 entries,
and the index would hold a set entry for each one, with tens of MB more held on worker-us. The pull side already gives
"every entry that read it", because each apply asks each entry it meets. What the pull side lacked was the comparison
by VALUE and a form shared with the recorder, and both now hold.

So the push index holds only the kinds no filing announces: attempts, the year, and the TMDB reads whose filings the
feed must be told to count (`reads`). The cost is one set entry per indexed receipt entry. Prod DE and UK verdicts hold
1–3 attempts and one year each.

### Steps

Each step lands with its specs. `IncrementalWholeEquivalenceSpec`, `LateInputEquivalenceSpec` and
`ScopedProjectionEquivalenceSpec` stay green, and the footprint shows 0 moved takes.

1. **One digest, one reader.** `Dependency.digest`, with the year read through `reread`, so `digestNow` has no case of
   its own. Spec: `DependencyReadSpec`.
2. **A typed, value-aware index in `Memo`.**
   - Its readers are kept by `Dependency`, not by filing count.
   - `moved(kind, now)` replaces `uncheckAll` on a year turning and the `uncheckWhere` receipt scan on an attempt mark.
   - A verdict's and the candidates cache's `clean` compare a filed dependency by value, as corrections' did (step 2b).
   Spec: `DependencyIndexSpec`.
3. **Projection scope from the one feed.** `IndexedVenuePageFacts.takeMoved` reads the venue-page filings through a feed
   view, as the agreement reads its own, and the side set `VenuePageIndex.moved` goes. When the feed no longer holds
   every filing since the last call, it returns None, and every listing on a page is a change. Specs:
   - `VenuePageIndexSpec`: a settle's filing reaches the view once; a dropped filing gives None;
   - `ScopedProjectionEquivalenceSpec`'s venue-page case, both ways.
4. **Ask-newness by shape.** `askedNow` is keyed by the shape digest alone. `newCluster` asks the digest indexes
   (`heldByDigest`, waits by digest), not the key, so a renamed or renumbered cluster whose shape a verdict holds is
   not asked inline again. Spec: `AgreementStageSpec`.
5. **Later, not in this change: attempts filed on the feed.** The question memory would publish `tried|<question>`
   on the feed, and the stage would drop `attemptsMarked`, `awaiting` and `awaitingMarks`. This waits for a harness
   change: every spec world that marks attempts would publish them too.

## One family index (2026-11-04)

The owner approved this on 2026-11-04, after the family replay prototype (a family plus its read set, replayed in
isolation over its population lattice, reproduced 15 of the 16 known flips still live on main).
`services.identity.FamilyIndex` records, for every family of a resolve:

- **members**: each listing key with the digest of its whole input (every field, the stage's too);
- **block keys**: the title and film keys it merges under; a listing outside it carrying one means a merge;
- **context reads**: every read of the corpus-learned context (`CorpusContext`) by the digest of its VALUE: a record with
  its venue titles (`candidate`, CorpusContext.scala:211), a banner's house (`houses`, :94), a piece's spread
  (`segmentSpread`, :136 -- the raw count, so `repeatedSegment`'s threshold of 2 is covered, which `CorpusContext.Slice`
  does not hold), a title group, a whole title, a reached set, a search's ranking;
- **answers**: every lookup as its typed `Dependency`, digested by `Dependency.digest` (an unanswered one too);
- **pins** naming a member;
- **takes and rules**: each member's take and its `DecisionTrace` rule ids (`rulesOf`, the stage's): the ids the
  per-rule registry is keyed by.

It is recorded by resolving each family ALONE against the corpus's context (A3) through recording wrappers of the
context and the lookups (one read function each, shared with the replay: what is recorded is what was read). A measure:
nothing reads it to take.

Query API: `familyOf(listing)`, `readersOf(ContextRead | Dependency)`, `holdersOf(blockKey)`, `firing(ruleId)`,
`stale(now)` (each moved input named).

**Completeness** (`FamilyIndexCompletenessSpec`, every drift and identity slice): (1) every recorded read digests as
recorded against its own corpus; (2) A3: each family alone takes what the corpus took; (3) each family REPLAYED from its
recorded reads alone -- no corpus, no lookup beyond them, a read outside fails (`replayAlone`) -- takes the same; (4) for
corpora differing by a removed or added population (another family, one listing, a quarter, a half), every family whose
entry stands keeps its members together and alone, and every take. A failure names a read the index lacks.

**The final take** (`services.identity.agreement.FamilyFinalReads`, worker tests): a fresh, unbudgeted stage settled
over the family records its verdicts' receipts (`StoredVerdict.reads`: family answers -- the venue programme,
`agreement/VenueListings.scala:46`, searches, records --, attempts the ask memory holds, the year) and every lookup the
stage made (Broadcast's namesake records, casts, descriptions, release days). Stored verdicts found by digest
(`AgreementStage.scala:518`, `:638`) and apply budgets are path state and stay out: the spec proves the take independent
of them. The spec holds the final take as it holds the model's: each family settled alone takes what the corpus's stage
took; its reads digest alike in the corpus's stage; and a family whose model entry and stage reads stand keeps its
final take, over the population corpora, corpora with a stage-only lookup withheld, and the corpus a year on (dropping
the stage reads from the comparison fails it: Purbeck "Hen" moves a year on). Then: (b) affected-only recompute on the local prod mirror.

**(a) The per-family convergence check** (`services.identity.FamilyConvergence`, worker tests; `worker/Test/runMain
services.identity.FamilyConvergence --cache FILE SLICE...`): each family's population lattice walked -- every subset of
7 or fewer listings, levels 0-1 and n-1..n up to 63, seeded orders beyond -- the model against the corpus's context with
the family's absent listings out, the final take by a fresh stage per subset; a flip wherever a listing's takes go A -> B
-> A along nested subsets, or A -> B one listing apart. Each verdict is cached under the rules version, the entry's
digest and its final reads, with its CLOSURE: the reads a subset made beyond the whole family's (context reads by the
whole corpus's value, lookups, the year). A run walks again only a family whose key or closure moved: over the 29 drift
slices a first run walks 50 families (66 flips, Buddies, Durham, Oxide Ghosts and Titans among them) and a second walks
none. A family answer only a sub-population asks -- no settle of the whole family files it -- is in the closure too
(kind `answer`), digested as it reads asked alone of the corpus's answers (`FamilyFinalReads.AnswerOracle`). A drift
slice is cut over its families' lattice as well as the incremental sequence (`IdentityDrift.cut`, `latticeOf`), so a
subset's question is the slice's answer, never a gap read as unknown: before that cut, 5 of the 29 slices lacked the
lattice's answers (Hasetor "Scott & Lila" 5, Halstead's three 2-3, Torrington 2). `FamilyConvergenceSlicesSpec` (heavy1)
walks every slice and holds that it lacks nothing. A CORPUS run (`--corpus <cc> --recording DIR --families CAPTURE
[--shard i/n]`) walks a recording's whole country, the families split into shards by their first member's key.

**The family explorer: a weekly, NOT a gate** (the owner, 2026-11-06, option A). The corpus walk was costed as the drift
gate's replacement and refused: per commit it adds +36% runner minutes (37 of the last 40 identity commits reach every
family -- code outside the identity model, the check's own tools, shared resolver code, the agreement stage -- so they
re-walk everything: a cold DE walk is 822 s, UK 898 s, PL 370 s, US over 48 min on the laptop, about twice that on a hosted
runner), and production rarely shows the flips it finds (reachable states only: prod's arrivals are not every subset of a
family). So `.github/workflows/identity-family-explorer.yml` walks it every week (Sunday; first as a nightly), outside THE GATE (no row in
`.github/gating-checks.tsv`, no call from the convergence workflows); the seed drift rows `takes-drift-1|2` keep gating
unchanged.

- **Rows.** One a country shard (`scripts/ci/family_shards.py`: DE 11, UK 5, PL 2, US 11, ES 1), each under about 10 minutes
  on a hosted runner. The first night's cold run (500 runner-minutes; DE 14-18 min a row, UK 11-14, US 4-41 with four of
  fourteen timed out at 40) split the families by COUNT, so a few huge ones dominated. They are now split by MEASURED
  COST (`test/resources/fixtures/family-costs-<cc>.tsv`, `FamilyConvergence.balance`: longest first into the lightest row;
  refresh with `family_shards.py costs --from <downloaded artifacts> --out test/resources/fixtures`), and a family of 64
  or more listings (the sampled tier: 93 US families cost 9,081 of the 20,255 recorded seconds) samples 1 arrival order
  instead of 8, another each ISO week. Coverage traded away: a giant family sees 1 random order a week (8 subsets)
  instead of 8, though every order any week draws is as good as any other and a week's orders differ; the lattice and
  level tiers (every family of 63 or fewer) are walked in full.
- **Weekly.** 37 of 40 identity commits reach every family, so a nightly walk's cache saves little, and each night pays every
  row's setup again: seven nights cost about 7 x 290 runner-minutes, one weekly full walk about 290 (against the first
  run's 500). Every family is walked at least once a week; a flip a commit introduces shows up within seven days.
- **Warm.** `scripts/ci/identity-family-nightly.sh` keeps the verdict cache (and the commit it was made at) on the release
  `identity-family-explorer-store`, under a rules key that names no code. A walk re-walks only the families the code
  changed since then can move (`scripts/ci/family_reach.py`); an unattributable change walks every family.
- **Output.** `scripts/ci/family-explorer-publish.sh` joins the shards into the release `identity-family-explorer`:
  `family-flips-<cc>.tsv` (listing, flip chain, the rules the walk fired), `family-flips-ranked.tsv` (every flip by deciding
  rule, most listings first: structural fix 2's list, the rule burn-down), `family-flips-new.tsv` (new since the last walk, also
  on the run's summary) and a dated copy of the ranking (30 kept). A country with a shard missing keeps last night's file.
- **No backlog rows.** The explorer does not file its flips (Actions cannot push main, and these are explored, not owed): a
  flip enters `identity-flip-backlog.tsv` (kind `family`) only through a named decision, as any other row.

## Held takes (proposal, 2026-11-05; not implemented -- the owner decides)

The approach change of 2026-11-05, item 1: a take a population-dependent rule decides is HELD, Pending, until the
population it read has settled; flips are fixed as a class this way, not a listing at a time by a new rule. Why: the
rule registry (`RuleContract`, 4d9ad4ba61de) declares 71 of 131 rule ids kind (iii) and no accepting rule but `pin`
(i) or (ii); in prod's last 7 days every take change outside a deploy (1,567 of 1,567) was decided by a kind (iii)
rule (`accept:joined` 685, `pooled:no-film` 348, `pooled:unnamed` 170). The resolver is population-dependent almost
everywhere, so per-listing fixes reshuffle flips (the Titans / Hen / Dracula chains of 11-01..11-04).

### What "settled" means

A pure function of inputs and the clock, so incremental == whole still holds (the principle above):

- **The population a family read**: the listings its `FamilyIndex` entry's context reads draw on (record venue titles,
  houses, segment spreads, title groups, reached sets, rankings: `readersOf` inverted to the listings behind each read),
  plus its own members and its block-key holders.
- **Settled at t** when none of those listings first arrived or left within `(t - T, t]` -- the population has been
  quiet for T -- and every answer the take reads is answered (the existing Pending model, `ResolverDecision.awaits`).
  A listing's first-seen and leaving times are inputs (`ListingRetention` keeps a leaving listing's days already); the
  clock is an input as the year is. NOT a count of projections: that depends on the tick cadence, not the inputs.
- **Held**: a take whose deciding rule (`DecidedBy.of`) is kind (iii) or undeclared, while its family is unsettled, is
  `TakeOutcome.Pending(Population(since))`. A take a kind (i) or (ii) rule decides is never held: it cannot go back.
- **Bound**: a family is held at most H (proposed 24 h) from its oldest unsettled arrival; past H it is decided as it
  stands and named (`held-expired`, a metric and the drift report). Families that never go quiet -- an event billed at
  100+ venues arriving over weeks, a scraper whose listings flap -- otherwise stay Pending forever.

### Why it removes the flip class -- and where it does not

`TakeFlips.Flip.reasons` drops every step served Pending before it looks for A -> B -> A
(`takes.map(_.served).filterNot(_.pending)`), `PopulationMonotonicity.returns` skips `None`, and
`FamilyConvergence.flipOf` reads a Pending take as no take. So Pending -> film -> Pending is no flip, nor is
film -> (Pending, other film) -> film. A held take decided only at settled moments flips only if two SETTLED populations
in a row disagree and a third agrees with the first: an intermediate population that lasted T or longer.

- In prod, that is the timing of real arrivals: of 407 non-deploy reversals in the last 7 days, the intermediate take
  stood < 15 min in 45 %, < 1 h in 71 %, < 2 h in 74 %, < 4 h in 85 %, < 24 h in 91 % (p50 51 min, p90 13.8 h).
- **In a time-free check it removes nothing.** The per-family lattice (`FamilyConvergence`) and the seeded drift
  replay give no time between arrivals. With a quiet-T hold, any sub-population can last T (the timeline may pause
  there), so a lattice walk that respects the hold sees every flip it sees today; a replay whose clock never moves
  holds every family to its end and sees none (vacuous). The gate must carry time: the replay advances the clock by
  the recording's own first-seen gaps (or a gap distribution drawn from prod), and its end state is compared at a clock
  past every hold. Without that, the hold only relabels flips as Pending.

### What it costs

- **What is served does not change** (the owner, 2026-10-31: a Pending take is decided on the inputs as they stand and
  served, flagged provisional; never a previous take). The cost is the time a take stays provisional: every kind (iii)
  take -- in prod nearly all -- is decided T after its population's last arrival. The film on the card and in the API is
  the same; `provisional: true` lasts longer (prod mirror, `identity_history` 10-29..11-05, family = normalized title,
  DE: time from a listing's arrival to its family's first quiet T -- T 30 min: p50 0.5 h, p90 4.3 h; T 2 h: p50 3.1 h,
  p90 17.5 h, 1 % never settle in the week; T 6 h: p50 10 h, p90 36 h, 14 % never; T 24 h: 59 % never; UK alike), and prod's flip watch (which counts decided takes) stops counting the
  short reversals. If the owner wants the intermediate film itself hidden, the held take must serve none (agree or
  none, dropped 2026-10-31) -- that would add T to time-to-match (today p50 35 min, p90 ~99 min) for new listings.
- **Final takes**: none move. At a settled moment the take is the whole resolve of the current inputs, as today; a family
  past H is decided as it stands. Only the moment a take counts as decided moves.
- **Compute**: one settledness read per family per projection (a max over first-seen/leaving times of the read set's
  listings), from the family index the projection already builds.

### Interaction with the namesake rule (step 3) and the Pending model

- Step 3 lets a namesake-ambiguous listing be decided only by kind (i)/(ii) rules -- in practice "pinned, else no
  film", which cost 12-50 served right takes. With the hold, a namesake-ambiguous listing may be decided by kind (iii)
  rules again, held until its family settles: the losses come back while the population-dependence stays out of the
  decided sequence -- to the extent T covers the intermediate populations. Approach item 3 ("what stays bare is decided
  by the held take") is this.
- The existing Pending reasons (search, page, record, family question) stay; Population is one more `awaits` kind,
  and `IdentityDrift`'s pending tally (provisional != final) measures the price per kind, as it does today.

### Failure modes

1. A family that never settles: bounded by H; counted.
2. A gate made vacuous by a clock that never moves (above): the replay must model time, and the check must fail a run
   whose end state is still held.
3. A read set the index lacks (a sub-population's family answer, "Not in the closure yet" above): the family settles too
   early; the completeness spec names the read.
4. Long-lived intermediate populations: still flip, by design. The backlog's own families are of this shape: "It's Nice
   Up North" arrived 10-12, 10-12, 10-13, 10-23; "Oxide Ghosts" 10-14, 10-20, 11-02, 11-04 -- days apart, so no T of
   hours holds them, and H (24 h) releases them before the next sibling. Prod's 407 short reversals are another
   population (mostly a bounce within a tick or an hour) than the backlog's lattice flips.
5. Hiding a bad rule: a hold must not become the fix for a rule whose decision is wrong at the FINAL population; the
   ratchet still judges final takes.

## Predicting flips (backtest, 2026-11-05)

The owner (2026-11-05): postpone or hide flipping takes only once they can be predicted. Backtest over the local prod
mirror's `identity_history` (read-only), DE, UK, ES and US. The history holds 7 days (2026-10-29 00:08Z to 11-05
04:10Z), not a month. Training is on changes before 11-03 00:00Z, using only reversals that had completed by then;
the test is 11-03 to 11-05 04:10Z. A **reversal** is a non-deploy take change A to B whose next change goes back to A.
DE had 116, UK 96, ES 1 and US 2. US history records takes for only 3 listings, and PL has no history in the mirror.
Scripts, data and full tables: the flip-prediction state directory (`FLIP-PREDICTION-STATE.md`, `flip-prediction/*.py`).

**What the reversals are.** 151 of the 215 (70%) are one pattern. A scrape-triggered projection's model stage drops
the take to none (`pooled:no-film`, from `accept:joined` or a fill). The next projection, 5 to 7 s later and
triggered by `answer` or `answer+scrape`, restores the same film through a later stage (`fill:venues.current`,
`poster:vote`, `agreed:*`, `fill:family.feedLink`). The middle take lasts under a minute: the later stage is waiting
on answers the new population asked for. Of the rest, DE has 20 `accept:joined` changed-and-back within 24 h, and UK
has 9 `poster:vote` gains undone and a few fills. 18 last longer than 24 h. The prod-wide "71% under 1 h" is mostly
this sub-minute class.

**Predictors.** They are scored two ways: at the change (will this new take reverse within 24 h?) and ahead, at
00:00Z of each day (will this listing reverse today?).

| predictor | framing | precision | recall | lead | flagged |
|---|---|---|---|---|---|
| R-lost: the model stage takes none where a film stood | at the change, whole week (nothing fitted) | 0.57 | 0.77 (151/195) | 0 (at the change) | 37 changes/day |
| R-lost | at the change, test days | 0.39 | 0.89 (40/45) | 0 | 47/day |
| P3 risk score (logistic: rule, previous rule, stage, kind, family size, corroborating venues, rivals, probability near the cut, bare title, prior flips, family churn) | at the change, test | 0.07 (base rate 0.065) | 0.82 at >= 0.7 | 0 | 258/day |
| P2 scheduled departures (a sibling or the listing leaves for good within 24 h) | at the change, whole week | 0.39 | 0.11 (22/195) | <= 24 h | 8/day |
| P2 | ahead, test | 0.000 | 0/24 | - | 258 listings/day |
| P3 risk, top 5% of listing-days | ahead, test | <= 0.001 | 0.17 to 0.62 (unstable between fits) | 2.8 to 5.5 h | ~9,000 listings/day |
| "flipped in the last 7 days" | ahead, test | 0.002 | 0.17 (4/24) | median 5.5 h | 875 listings/day |
| P1 counterfactual (`FamilyCounterfactual`, branch `flip-counterfactual`: each family of 2+ at 00:00Z re-resolved with each member removed and up to 10 outside listings added, model take) | ahead, DE 11-03/04 | 0.000 | 0/6 | - | 218 listings/day |
| P1 | at the change, DE 11-03/04 + UK 11-03 | 0.00 | 0/26 | - | 15 changes in 2 country-days |

The ahead framing has ~107k live listings a day, with 24 listing-days that reversed in the test. Ahead of time,
nothing tried predicts a reversal at a usable cost: the risk score flags ~9% of all listings every day and its recall
swings between fits. The counterfactual flags 0.7% of the listings it covers and caught none of the six reversals. The
reversals are not one-sibling perturbations of the 00:00 population: they come from listings that arrive during the
day (all 21 UK reversals on 11-03 were BTS live-viewing listings that arrived at 11:45) and from the two-phase write
above. A start-of-day snapshot does not hold either. At the
change, one deterministic rule (R-lost) catches three quarters. The fitted score adds nothing over it: test
precision sits at the base rate, and the test window held mass waves (DE `accept:joined` re-keys on 11-03, UK
`pooled:unnamed` gains on 11-04) that the training days did not have.

**Settle trade-off** (whole week; DE+UK+ES+US; 195 reversals undone within 24 h, 1,177 non-deploy changes):

| policy | reversals never shown | takes shown late |
|---|---|---|
| blanket display settle 1 h (every new take) | 159/195 (82%) | 1,018 takes, 1 h each (gains included: every new film appears an hour later) |
| blanket 4 h | 177/195 (91%) | 1,000, 4 h each |
| settle only R-lost changes, 1 min | 150/195 (77%) | 114 losses, 1 min each (the previous film stays up a minute longer) |
| R-lost, 1 h or 4 h | 150 / 151 of 195 | 114 / 113 |

Recommendation: settle only R-lost changes, i.e. hold a model-stage loss for about a minute. That hides 77% of
user-visible reversals, against 82% for a blanket 1 h settle, and delays 9x fewer takes by 60x less time. It never
delays a gain. The structural form is the same rule stated exactly: do not publish a model-stage loss while answers
the projection asked for are still unfiled, i.e. the decision's `awaits` / `unanswered`, which the history does not
record yet. The remaining ~23% (model-model re-keys, poster votes undone, departures days apart) is not predictable
ahead from history features; the per-family convergence check and own facts remain the route there.

**The prod monitor** (2026-11-05). The worker counts the same reversals as its projection writes them
(`services.identity.TakeReversals`), so every identity deploy has a before/after flip rate with no reader of the history:
`kinowo_worker_identity_take_flips_total{country,pattern,class,dropped_by,restored_by,lasted}` (pattern
`film-none-film`, `none-film-none` or `film-film-film`; `class` as `scripts/flip-timeline` names it -- `DEPLOY/RESTART`,
which the backtest leaves out, `LATE-ANSWER` (an answer-triggered restore: the timeline's upper bound, as it also wants the
late ask), `SIBLING/POPULATION` (the two decisions hold other listings) or `RULE`; the stages, `DecidedBy.stage`, that
dropped and restored the take; `lasted` `1m`, `1h` or `24h`) and the time to reverse, `kinowo_worker_identity_take_flip_seconds{country,pattern}`, drawn on the worker
dashboard's "Identity flips" panel against the deploy marks. It holds each listing's last film change for 24 h (at most
20,000 listings a country), so it compares with the backtest's 195 a week undone within 24 h, not its 215; and it
forgets on a restart, so a flip whose two changes straddle a worker restart is not counted (the history would count one
whose second change is not the boot's own deploy projection). Those, and flips over 24 h, are what a scheduled read of
the local mirror's `identity_history` would add.

## A fill that counts venues counts only those its film is open at (the owner, 2026-11-05: "Buddies, properly")

The `venues.current` signal (`UnifiedEvidence`: at least `Agreement.WidelyBilled` = 3 venues bill the title, the film this
year's or last) is the one fill input that is the POPULATION itself: how many listings arrived. It counted every venue of
the cluster, those whose own facts deny the film too, so a film the listings' facts had left unmatched at two venues was
taken at three -- DE Lichtblick's "Buddies" (convergence #800, seed 64; backlog rows `drift` and `population`):

| listings present | take | by |
|---|---|---|
| Lichtblick alone | tmdb:1629957 | the lean fill on kinoheld's copied year and credit |
| Lichtblick + Pollux, or + Lichtwerk | none | vetoed: Cineamo's 81 minutes deny a 5-minute short |
| all three | tmdb:1629957, at ALL three | `venues.current`, two of its three venues denying it |

**The rule:** a venue counts toward a film's `venues.current` only when no node of its listings denies that film on its
own facts (`IdentityResolver.NodeEvidence`: a year, a director, a running time against TMDB's record). Each venue's part
is its own facts and TMDB, population-independent; the count only grows as listings arrive, so the signal is monotone in
the population and adds no A -> B -> A of its own. An IMDb-only contender (no TMDB candidate to deny) counts every venue,
as before. One more thing keeps the fill from taking MORE than the plain count did: a fill takes only the one contender
its rule fires on, and counting every venue fired on every current film at once; so while another current film is
counted below three venues by its denials, this one is not taken either (UK "MET: Silent Night" x3 at Phoenix Leicester
and two Savoys: the Met's 2026/27 relay is denied there, and a 2026 "Silent Night" would have been the one current film
counted). Locally the unmatched-cluster ratchet is green on every capture with it.

**First tried and dropped (same day):** `venues.current` only for a cluster holding a BARE listing (no
`publishesAFact`), as `ClusterVoting.familyMajority` borrows nothing for a cluster that states facts. Locally it lost 44
right takes of the unmatched-cluster capture: Cinema City's "HYBE CHEERING PARTY" x16 and "SEVENTEEN WORLD TOUR 'NEW_'"
x16 (PL), "Paddington Bear: Winter Warmers" x11 (UK) -- IMDb-only events whose every venue states its running time. A
running time the film's record never contradicts is no reason to stop counting its venues.

**Buddies' facts:** Lichtblick e.V. screens at Pollux Paderborn (both bill it 2026-11-24 19:15, 81 minutes); Cineamo links
Pollux's and Lichtwerk's billings to 259433m, Bressan's 1985 "Buddies" (81 minutes). Lichtblick's 2026 and Rahul Hansraj
are kinoheld's copy of tmdb:1629957, a 5-minute student short: the 10-13 judgement RIGHT is re-judged wrong, 259433
right at the three venues (AGENT-JUDGED). With the rule, Pollux and Lichtwerk take 259433 or none, Lichtblick none from
two venues on: none of the takes comes back. Lichtblick alone still takes the short by the lean fill (the parked
copied-credits class, `docs/identity-experiments/2026-10-23-kinoheld-s-copied-credits-a-short-taking-a.md`); the
projection withdraws it by its judgement.

## What still blocks retiring the checks

1. Item 7's open parts (the first draft's `searchTitle` -- since Main #2617 the IMDb-id resolver falls back to every venue
   title, so it only orders the first ask -- and the venue slot's prior-slot carry), and receipts of the question memory's outcomes for a
   settled family question (an attempt swept from the memory leaves a settled verdict standing; a whole resolve would
   wait).
2. The deploy's backfill, its progress metric (`state="incomplete"`) reaching 0 in every country.
3. Input kinds the harness does not generate yet:
   - venue detail pages that move;
   - catalogue ids and pages;
   - synopses;
   - broadcast days and productions (stage works);
   - pins;
   - stale answers that are re-asked and move;
   - `republished` fields only the stage reads;
   - a TMDB search answered with a NEW candidate later.

   Each needs a generator in `IncrementalWholeEquivalenceSpec` (a `Case` or a `World` gate).
4. After the deploy, the live drift check's 39 rows reading zero. Those rows are DE Dolly ×10, Rental Family ×8, Ice
   Cream Man ×4, Jump Scare, Miss Mary, Karmele and I.K.U.; UK CBeebies Panto ×11; PL Ostrze ×2. Dolly's class is the
   lean record (item 8). Panto's prod line is "one piece of the billing names ..." from a verdict decided on other
   inputs, which item 8 should clear. Ostrze (a stored family.director fill that a fresh stage vetoes) is unexplained
   until it is re-checked after the deploy.

Then, as one commit: move the live drift check and the convergence drift step to weekly. That commit is prepared but
not landed; the owner approves it.
