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

## Audit: every piece of state the incremental path carries

Each item has a verdict:
- (a) cannot affect a take or grouping: a cache or a name, with its proof;
- (b) an input the whole resolve must read too;
- (c) a dependence on history, which is removed.

| # | State | Where | Verdict | Fix / proof |
|---|---|---|---|---|
| 1 | Stored families plus their slice digest | `IncrementalResolver.families`, `IdentityModelStore` | (a) cache | Reused only when the digest of every corpus fact the family read matches (`CorpusContext.slice`, a superset). On restore, nodes and claims are checked too. `IncrementalResolverSpec` checks every event against a whole resolve. |
| 2 | Family ids, `freedIds` | `IncrementalResolver` | (a) name | Labels only. Decisions are sorted by first member. |
| 3 | Rules version | `IncrementalResolver.rulesVersion` | (a) | The code digest is the compiler's closure of the resolver (`IdentityRulesSources`). The normalizer and its resources are reached, so they are covered. |
| 4 | `LiveCorpus` memos (`heads`, `records`, candidates) | `LiveCorpus` | (a) cache, given the event feed | `LiveCorpusSpec` checks equality with `CorpusContext.of` after every event. Gap: a detail page or record filed WITHOUT its `AnswersChanged` event stays stale until a restart. That is a feed-delivery property, not stored state. |
| 5 | FilmIds, `IdAssigner`, `FilmIdCounters` | projection | (a) name | The film and members come from the decision. `IdAssignerSpec`. |
| 6 | `LiveProjectionIndex.previous`, prior takes | projection | (a) name / log | They feed the id label, slot priors and the change log, never the decision. |
| 7 | Record carry-over (`heldLinks`, `retainedSynopses`, the `tmdbAttempt` stamp) | `ProjectionDrafting` | open, record content only | This is not a take or a grouping, but it is history in the record (IMDb link, synopses). Listed under gaps. |
| 8a | What a take reads afresh each apply (the poster vote's records, the broadcast join's, synopses, IMDb finds) | `AgreementTakes`, the last-apply shortcut | **(b) fixed** | These are not stored, but the quiet-apply shortcut handed back the last result until the version moved. Every TMDB read goes through `Noting`, and the last full apply's records, searches and finds are what `AgreementStage.reads` tells the feed (`FamilyAnswerStore.readBy`, finds included). |
| 8 | Held verdicts: the receipt was incomplete | `AgreementStage.held`, `identity_agreements` | **(b) fixed** | The receipt lists only family answers. A verdict now records everything it read, each answered or not: every family answer (unanswered ones too), the record of the film TMDB leans to, every TMDB lookup its fill makes (searches, records, release days, cast, details), the fill's production searches, and the year. The full check re-reads each one (`AgreementStage.read`). The feed now counts a filing of any TMDB record or search a held verdict read (`FamilyAnswerStore.readBy`, `AgreementStage.reads`). Specs: `AgreementStageSpec` "...the record of the film TMDB leans to is filed..." (the Dolly class), `AnswerFeedSpec` "count a TMDB record or search a kept verdict read". |
| 9 | Year read from the clock | `AgreementStage.resolved` | **(b) fixed** | The year is in the receipt, and a year turning clears `checked` and the last-apply shortcut. Spec: "decide a kept verdict again when the year turns". Cost: once a year, every verdict is read again; the ones whose year read moved are decided again. |
| 10 | Verdict found by cluster key | `verdictOf`, `corrected` (and its budget-spent fallback) | **(c) removed** | A verdict is now found by its listings' digest, under whichever key filed it (`heldFor`, `heldByDigest`). Specs: `AgreementCorrectionSpec` "stand as last decided after a deploy, whether or not it hands the stage its cluster keys", and harness property 3. |
| 11 | Verdicts adopted under a key (by first listing) | `AgreementStage.adopt` | **(c) deleted** | Subsumed by 10: a legacy verdict is found by its digest. `AgreementKeyingSpec`. |
| 12 | Merge carried memories: union of `unreached` and `askedNow` | `keying` | **(c) deleted** | A merge names the survivor and carries nothing. `AgreementKeyingSpec` "a merge ... take on none of the younger's memories". |
| 13 | Settled because it settled before (`unreached.contains`) | `resolved` | **(c) fixed** | A cluster settles when every question it waits on was attempted, which is the question memory's to say. `AgreementKeyingSpec` "decided as a whole resolve decides it". |
| 14 | Cluster keys, counter, key store, restart re-attach | `ClusterKeys`, `AgreementKeys` | (a) name | Nothing is found by a key (10). Harness property 3 restarts with keys kept, lost and renumbered. |
| 15 | `unreached` (questions none answered, asked every pass) | `AgreementStage` | (a) asking only | It no longer decides a take (13). It can be derived from the receipts' unanswered entries; see Simplifications. |
| 16 | `askedNow` (inline ask once) | `AgreementStage` | (a) asking only | The answers it files are inputs. It only stops a second 5-second inline wait. |
| 17 | `checked` (digest, version) | `AgreementStage` | (a) cache validity | Sound once every receipt entry's filing moves the version (8) and the year clears it (9). |
| 18 | `waiting`, `blindAt` | `AgreementStage` | (a) | A waiting cluster takes the model's take, as a whole resolve with the same gaps does. It is resolved again when due. |
| 19 | `candidateFilms` (digest, blindAt) | `AgreementCandidates` | **(b) fixed** | It cached TMDB searches by listing digest, so a re-asked search with a new candidate stayed unread. The cache now keeps its receipt and stands only while none of it was filed since (`standing`). Its reads also join the receipt being made. Spec: `AgreementPosterSpec` "take a candidate a search filed again finds". |
| 20 | Correction verdicts' receipt | `AgreementCorrections` | **(b) fixed** | The receipt held family reads and asked ids, but not the TMDB records or candidates it reads. Every TMDB and venue read now goes into `StoredVerdict.reads` through the one seam (`Noting`, `receipting`), and a filing of one decides the correction again. Spec: `AgreementCorrectionSpec` "be decided again when a TMDB record it read is filed again". |
| 21 | Budget-spent correction fallback (stands as last decided, under any rules) | `corrected` | (a) at quiescence | A deliberate cost bound (`CorrectionsPerApply`). It equals the whole resolve once `backlog` is false (harness property 2). |
| 22 | `correctionWaits` | `AgreementCorrections` | (a) at quiescence | Cleared when a verdict is found under another key (10). It decides only whether a waiting correction is read again. |
| 23 | `rereads` (a record asked once in the stage's life) plus `waitsOn` | `AgreementCandidates` | **(c) fixed, deleted** | Whether a year-only record still waits depended on whether THIS stage instance asked. It now depends on the question memory (the read was attempted; the asker marks an answer as one), and it is in the fill's receipt (`tried|...`). An attempt marked since the last apply reads every receipt again. Spec: `AgreementBroadcastSpec` "...hold no take, in a stage that never asked it too". |
| 24 | `last`, `takenAs`, `digested`, `events`, `keyed` | `AgreementStage` | (a) cache / name | Identity caches of the same decision objects. |
| 25 | Staging events, `Merged` wait start | publisher | (a) display | The screen only. |
| 26 | Question memory (`attempted`) | worker | (b) input | It is read by the whole resolve too (`newAgreementStage`, harness `World.tried`). |
| 27 | `IdentityDriftCheck` (`Adopt` → `forget`) | worker adapters | dead | Not wired since the OOM. Delete it (movies-80; see Simplifications). |

The counts of the 27:

| Verdict | Count | Items |
|---|---|---|
| (a) cannot affect a take | 17 | |
| (b) fixed | 5 | 8, 8a, 9, 19, 20 |
| (c) fixed or deleted | 5 | 10–13, 23 |
| Record carry-over, open | 1 | 7 |
| Dead | 1 | 27 |

## Backfill of prod's stored verdicts

The change edits `AgreementStage.scala`, so `AgreementStage.RulesVersion` moves. On the first apply after the deploy,
no stored verdict stands: `v.rules == decidedUnder` fails for all of them. Every one of them was cached with an
incomplete receipt, and every one is decided again with its full receipt, a budget at a time:

- Agreement verdicts: `BackfillPerApply` (500) an apply, in the resolution's order. Until a later apply's budget
  reaches one, it stands as it was, and the stage reports a backlog so the next apply goes on. Before this change,
  every deploy touching the stage decided every agreement verdict again on its first apply, unbounded: the boot pass
  that OOM-killed worker-us.
- Corrections: `CorrectionsPerApply` (200) an apply, as before. Until then each stands as last decided.

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
| `unreached` | Derive it from the held verdict's receipt (its unanswered entries); the load-time rebuild goes too | −25 |
| Fill blind wait (`Waiting.blindAt` for a fill) | Subsumed by the receipt: an unanswered TMDB read is in it and reopens the verdict. It stays only for the staging screen's `onTmdb` | −20 |
| Family-only `unanswered` list | Deleted in this change: every family read, unanswered too, is in the receipt | −4 (done) |
| `adopt` (legacy verdicts by first listing) | Deleted in this change: found by digest | −20 (done) |
| Merge union | Deleted in this change | −6 (done) |
| `IdentityDriftCheck` (unwired since the OOM) and `AgreementStage.forget` | Delete | ≈ −200 |
| `StoredAsWritten.Keys` and the drift replay's key plumbing (91cfbe7e8) | Keys decide nothing now, so the replay need not keep them | −20 |
| Correction receipt as `Map[String, Long]` like the verdicts' | One receipt type, one validity rule, one "read again when filed" path (items 19, 20 and 23 too) | ≈ −40 |
| `IncrementalResolver` as the whole resolve plus a dependency-keyed cache | NOT recommended now. It already is a digest-keyed cache over slices (item 1); its update algorithm only decides which caches to re-check, and `IncrementalResolverSpec` holds it equal every event | — |
| Staging events derived from waits | Possible once waits derive from receipts plus memory. A later step | — |

## What still blocks retiring the checks

1. Item 7 (record content carried by the projection), and receipts of the question memory's outcomes for a
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
