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
- Corrections: `CorrectionsPerApply` (200) an apply, as before. Until then each stands as last decided.

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
  - the correction budget of 200 per apply (`CorrectionsPerApply`, :92), which makes interim takes depend on read order.
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

## What still blocks retiring the checks

1. Item 7's open parts (the first draft's `searchTitle`, the venue slot's prior-slot carry), and receipts of the question memory's outcomes for a
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
