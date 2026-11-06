# Identity structural fixes: typed unknown, asking apart from deciding, one replay world, rule burn-down (2026-11-05)

The owner (2026-11-05) asked for four structural fixes, done serially in this order: **4, 1, 3, 2**. This note is
the plan and the Identity-Design reference for every commit of them: the rule commits of fix 2 name it in their
`Identity-Design:` trailer. Each fix lands in milestones, each with a test that fails before it, and the behaviour
changing ones with the measurements named below.

## The owner rules every fix serves

| rule | short form |
|---|---|
| R1 | No stickiness: a take is a function of the current inputs, never of the path. |
| R2 | Incremental equals whole, by construction. |
| R3 | A flip is a wrong match: an A -> B -> A served is a defect even when A is right. |
| R4 | Decide from population-independent inputs: a listing's own facts plus TMDB. |
| R5 | Unmatched beats wrong. |
| R6 | Near-ties stay unmatched. |
| R7 | Fix the mechanism, not the case. |

## The diagnosis (2026-11-05)

- **70 % of prod's reversals are a two-step write** (`identity-no-drift.md`, "Predicting flips"; 151 of 215 over
  10-29..11-05). A scrape-triggered projection asks its new cluster's agreement questions inline
  (`AgreementStage.askedInline`, at most `AskNowWithin` = 5 s, the 16-thread `InlineFamilyAsks` pool,
  `IdentityProjectionWiring.scala:127-161`), decides on what arrived, and writes "no film" (`pooled:no-film`); the late
  answer's projection restores the film 5-7 s later. 143 of 166 traced DE+UK listing changes had answers filed
  during the dropping projection, the last 0.1-1.8 s before the drop (`AWAIT-ANSWERS` verification).
- **Projections run one at a time per country** (`IdentityProjection.tick` / `tickChanged` / `project` synchronized,
  `IdentityProjection.scala:210/221/238`; `AgreementStage.apply` synchronized, `AgreementStage.scala:357`; one
  scheduler, `IdentityProjectionWiring.scala:276-278`), so an inline wait stalls every film of the country. Waiting
  longer inside the projection is not an option (measured tails: inline asks > 60 s 6 %, > 300 s 0.7 %).
- **A missing answer reads as "none"** in several places (fix 4's inventory below), so a decision made before its
  answers looks like a decision made on them.
- **Four replay harnesses rebuild the world four ways**, and each grew its own harness bug on one day: the drift
  replay's shared random stream (`IdentityDrift.Share`, fixed by keyed fills 80b59d0da8e1), slices lacking answers
  (Keswick "Hay Fever", Main #2180), verify's dating of kept listings (a57790c723f2), what-if's missing answers
  (e32381c12933).
- **71 of 131 rule ids are population-dependent** (kind iii, `RuleContract`, 4d9ad4ba61de), and every non-deploy take
  change of the 7 days to 11-05 (1,567) was decided by one.

## Fix 4: typed unknown (first)

**Serves** R1, R2, R7: a decision can no longer be made on an answer it does not have without saying so.

**Today.** `Answer[+A] { Known, Unknown }` (`IdentityModel.scala:533`) is the read type, but `Unknown` means both
"not asked / in flight / failing and retrying" and, once `QuestionMemory.attempted` holds, "failed for good"; the
difference lives outside the answer, in the stage's `attempted` / `givenUp` (`AgreementStage.scala:102,107`), and
`toOption` / `isKnown` / `getOrElse(Nil)` turn either into "none" at the reading site.

**The type.** An answer is one of three, and nothing else may stand for the third:

```scala
enum Answer[+A] {
  case Known(value: A) // answered: the source said this (an empty answer included)
  case Failed          // terminal: given up on (QuestionMemory.givenUp), decided without it
  case Unknown         // not yet answered: never asked, in flight, or failed and due again
}
```

`Unknown` keeps its name though it now means only "not yet answered": `Dependency.digest` digests an answer by its
text, so a rename would move every stored receipt's unanswered digest (`Dependency.Unanswered`) and re-decide every
stored verdict on the deploy. The cases carry no question: the read that returned one names it. Below, "awaiting"
means `Unknown`.

- `toOption` and `isKnown` go: a reader matches the three cases, so the compiler (`-Werror`, exhaustive matches)
  lists every site. `Failed` is the only case a decision may read as "no evidence", and it does so through ONE named
  function (`Answer.orNone` / `Answer.evidence`), never `getOrElse`.
- The lookups (`IdentityLookups.detail`, `candidates`, `film`, `releaseDay`, `cast`, `overview`), the family, poster
  and catalogue stores' reads and the TMDB store read through `QuestionMemory`: answered -> `Known`; given up
  (`givenUp`: durably unanswered, or failed `giveUpFailures` times in a row) -> `Failed`; anything else -> `Unknown`.
  A missing map entry is `Unknown`, never `Known(Nil)`.
- The resolver and the stage decide from `Known` and `Failed` only. A cluster with an `Unknown` read returns an
  explicit result naming them: the model's `ResolverDecision.awaits` already is one (`TakeOutcome.Pending`), the
  stage's `None` verdict for a waiting cluster becomes `StageVerdict.Awaiting(questions)` instead of an absent
  `Option` (`AgreementStage.scala:878`, served as the model left it at `:582`).
- The settle rule of 2026-10-24 (decide once every question was ATTEMPTED, `AgreementStage.settled` /
  `settles` / `tried`, `:958-972`) is the one place that deliberately reads a retrying failure as "none". Fix 4 keeps
  its behaviour but makes it explicit: the stage reads `Unknown` with `attempted` as `Failed` through one function,
  `AgreementStage.decideWithout`, and that function is the site fix 1 deletes.
- `Dependency.digest` gives `Failed` its own digest: a receipt that read a terminal failure is told apart from one that
  read a gap. Stored verdicts that read `Unknown` re-read on the deploy through the existing backfill
  (`BackfillPerApply`, `identity-no-drift.md`, "Backfill").

**Sites the compiler must find** (the inventory of 2026-11-05; the design note's list is updated as each is fixed,
with its classification: (a) already typed, a rename; (b) "unknown read as none", behaviour kept and made explicit;
(c) a real bug, named, tested and measured):

| site | reads | first classification |
|---|---|---|
| `CandidateGeneration.scala:63` `answers.getOrElse(query, Answer.Known(Nil))` | a query no answer map holds read as answered empty | (c) candidate |
| `CandidateGeneration.scala:67,115,119,137`, `ResolverRun.scala:115`, `CorpusContext.scala:109-110` | an unanswered search's hits as none | (b): the model counts it in `unanswered` / `awaits` |
| `LiveCorpus.scala:332,350` | `named` / `known` hits `getOrElse(Nil)` | to classify |
| `AgreementStage.scala:971-972` `settled` | attempted `Unknown` as `Known(absent)` | (b) -> `decideWithout` |
| `AgreementStage.scala:875-884` | an unanswered family's verdict dropped (`flatMap(_.toOption)`) | (b) |
| `AgreementStage.scala:894,900`, `:768` | an unknown fill / record as none | to classify |
| `AgreementCandidates.scala:71-99,155-159` | a poster as unread, film hashes `getOrElse(Nil)`, title searches | (b) |
| `AgreementCorrections.scala:132-261` | records, posters, families `toOption.flatten` | to classify |
| `AgreementTakes.scala:20-171` | detail, cast, overview, record `toOption.flatten` | to classify |
| `Agreement.scala:144-148,169,171,247,350-357` | `Known(Nil)` / `Known(None)` fallbacks; a find attempted as naming no film | (b), and `:148`'s catch-all to classify |
| `Catalogue.scala:44-59` | a catalogue page or id attempted as naming nothing | (b) |
| `IdentityLookups.cast` / `overview` defaults `Known(None)` | not asked read as answered none | (c) candidate |

**4a's sites** (the compiler's list, 2026-11-05; each reads `Failed` through `failedAs`): `AgreementTakes.filledTake`
(a find given up on names no TMDB film), `AgreementTakes.taken` (likewise), `AgreementTakes.screened` (a poster given
up on: no distance, no veto), `Broadcast.take` (productions given up on: none), `Catalogue.named` (a page or id given
up on names nothing, still asked), `VenueListings.listed` (a programme given up on names nothing);
`StoredTmdbLookups.releaseDay` passes a `Failed` record on as `Failed`.

**4c's sites** (stickiness audit G5's stage half): the settle rule's collapse is now typed. `AgreementStage.settled`
returns `Failed` (not `Known(absent)`) for a question attempted and not answered, `Agreement.tmdbFound` likewise for a
find; the readers decide on it through `failedAs` (`AgreementCandidates.posterRead`, `Catalogue.takenOn`,
`AgreementTakes.filledTake` / `taken`) or read it as none already (`AgreementCorrections`' posters and its families'
TMDB ids, by `toOption`). `settled` is the one place an answer still inside its retries is decided on as if given up;
fix 1 deletes it. 0 moved: each reader reads `Failed` as it read `Known(absent)`.

**4b's sites** (the model's producer, and every reader of "awaited"): `StoredFirstLookups` answers `Failed` for a gap
the question memory has given up on (`LiveAsks.givenUp`: failed its kind's give-up count in a row, never for one never
attempted), still asking it on its backoff. Readers of "not answered" now read `Answer.isAwaited` (`Unknown` alone),
not `!isKnown`: `ResolverDecisions`' `unanswered` count and `awaits` (a given-up search is no longer Pending: the
owner's 2026-10-31 rule), `Candidate.of`'s `awaitingRecord` (a record given up on: decided on its hits),
`CandidateGeneration.pagePending`, `AgreementCandidates.undatedOf` (a release day given up on is not waited on) and
`AgreementTakes.broadcast` (a record given up on is decided without). Classification of the inventory's two (c)
candidates: `CandidateGeneration.reachOf`'s default `Known(Nil)` is NOT a bug -- `Reach.of` reads `Known(Nil)` and
`Unknown` alike (no hit either way), so it moved nothing; it is `Unknown` now for honesty. `IdentityLookups.cast` /
`overview`'s `Known(None)` defaults are only for lookups with no record source (the venues' pages, test doubles):
TMDB's lookups override both, so they stay.

**What 4b moves, and why the harnesses cannot see it.** Only a question given up on reads otherwise, and none of the
measuring worlds hold one: the local prod mirror does not sync `identity_question_markers` (`debug-mirror-collections`),
and recordings answer a gap as never asked. So what-if, the CI measure and drift replay 4b as 0 moved by construction;
in prod it moves only takes whose TMDB question failed 8 times in a row (~16 h of failures, an outage-only case).

**4d: every remaining collapse named.** `Answer.toOption` and `isKnown` are gone (renamed `answered` / `isAnswered`,
whose doc says a decision may read an unanswered one as none only where it is typed as awaiting it); the compiler listed
every reader, 58 in main sources (15 files). By what makes the collapse sound, or not:

| group | readers | `Unknown` read as none is | verdict |
|---|---|---|---|
| model decisions | `CandidateGeneration` (hits, reach, details), `ResolverRun`, `ResolverDecisions`, `Candidate.of`, `LiveCorpus`, `SuspectTakes` | typed: the decision `awaits` the search, record or page (`TakeOutcome.Pending`) | sound |
| stage verdicts and corrections | `AgreementStage` (verdicts, fill), `AgreementCorrections` (families, posters, programme, ids), `AgreementCandidates` (posters), `Agreement` (records), `Catalogue` | typed: a gap holds the cluster (`waiting`) or the correction (`correctionWaits`); `Failed` enters only through `settled` | sound until fix 1 deletes `settled` |
| stage reads never waited on | the venue page in `AgreementCandidates.measured` / `AgreementCorrections`' stated listings / `AgreementTakes`; TMDB cast and descriptions (`AgreementTakes`' synopsis and people); records read for a title or an edition (`looked.film`) | NOT typed: decided on as none with no wait | OPEN: fix 1 asks these before the cluster is projected (they join its question set) |
| bookkeeping | `ResolverRun` counts, `LiveCorpus.AnswersChanged`, `IdentityProjectionWiring`, `LookupFill`, `ModelLookups`, `SuspectTakeCount` | no decision | n/a |

**Measured by:** no take moves (what-if `--verify` on every country; a CI identity-measure with right->wrong 0 and
0 moved; drift clean) except at a site classified (c), whose moves are listed, judged and tested failing-first.
`IncrementalWholeEquivalenceSpec`, `LateInputEquivalenceSpec` and `ScopedProjectionEquivalenceSpec` stay green.

**Milestones.** 4a the three-case type, nothing producing `Failed` yet, every exhaustive match given its `Failed` arm
(a terminal failure read as no evidence through `Answer.failedAs`, the one function that may) (0 moved: no producer); 4b the model's sites; 4c the stage's sites (`decideWithout` the one collapse);
4d `toOption`/`isKnown` renamed so the compiler lists every reader (table above). STOP after 4 lands: report the list and the numbers.

## Fix 1: asking apart from deciding (second)

**Serves** R1, R2, R3 (the two-step write: 70 % of the backtest's flips, half of DE's on the reconciliation), R4, R7.

**The owner's choice (2026-11-06): the listing readiness gate at the model's input** (option B), over a cluster's write
deferred inside the projection (option A, the first wording above: its question set grows with the population, and the
deferral would live in the projection's closure scope). The 7-day reconciliation (flip-timeline per reversed title, the
backtest's definition): DE 134 reversals, 67 LATE-ANSWER (Geister's 56 among them), UK 110, 18 LATE-ANSWER plus 24
"(IMAX) Sinners" the tool's search misses; so fix 1 stays before fix 2.

**The change.**

1. **A listing's own questions** (`AgreementStage.questionsOf(listing)`): every gap the families' verdicts meet over the
   listing alone -- its titles' and directors' searches, the records their hits name -- none for an event no film
   database holds; plus its venue page while its read was never attempted (`ListingReadiness.ofPage`,
   `PageWait.unattempted`). A function of the listing and the answers filed: the same whatever else is billed (R4).
2. **The gate** (`IdentityModelService.gate`, `ListingReadiness`, `AgreementReadiness`): a listing billed otherwise than
   the model holds it (new, or its facts moved) is taken in only once each of its questions is answered or concluded by
   the question memory (`QuestionMemory.concluded`: a durable non-answer, or failures its kind gives up on; never one
   still inside its retries, nor one never asked). Until then the model keeps the version it holds; the questions are
   asked through the queue claimed ahead of the agreement's own (`AgreementQuestions.enqueue(claimAhead = 0)`), and
   every filing wakes the model, whose next drain promotes each held listing whose questions closed -- no new scrape.
   No clock of ours ends the wait: each ask terminates (2a: a turned-away ask fails, a family with no source is filed).
3. **A listing new to the model** is shown at once on its own decision, no film, Pending on its questions (the main
   session's choice (a); a later match is a gain, never a reversal), never joining another's decision. The one place to
   switch to (b), shown only once taken in: `IdentityModelService.NewWhilePending`.
4. **Restart** (the owner's condition): the held listings are persisted (`identity_pending`, `PendingListingStore`: each
   key, its last ready version, since when), the newest version is the archive's, and whether it is ready is never
   stored but read afresh. A take-up re-reads every row: ready now (answered while the worker was down) -- its newest
   version taken in and the row released; else held again, its questions asked again; a row whose listing is billed no
   more released. Idempotent: a take-up after one that stopped mid-promotion reads the same, neither taking a version
   twice nor losing one (`IdentityModelServiceSpec`, "survive a restart"). Residue: a scrape archived but not yet drained
   when the worker stops is taken up ungated (the model's venue buffer is in memory) -- a window of the drain's settle,
   seconds; its listings are decided Pending on their open questions as before.
5. **Incremental equals whole:** a whole resolve reads the same versions the gate let in; every replay (drift check,
   slices, measure) takes each version as ready (`ListingReadiness.Always`), its recorded answers being there already.

**Not in the gate** (cluster-level, the stage's waits as before): the posters of a cluster's candidates, the find of an
agreed IMDb id, the fill's TMDB answers -- questions only a population leads to. The inline asks (`askNow`) stay until
1c measures what is left of them.

**Measured by:**
- newly appearing listings' latency to their first decided take, before and after (prod logs, the take-change log's
  `decided` lines against the listing's arrival);
- worker throughput (projections a minute, projection wall time p50/p99) and heap (the memory-budget rule: the new
  per-cluster question set is bounded and counted);
- the reversals `scripts/flip-timeline <cc> [--since 7d]` classifies LATE-ANSWER (b242b5bda037: per-listing timelines
  of projections, take changes and asks with their late answers), which fix 1 should drive to near zero, against
  SIBLING/POPULATION, RULE and DEPLOY/RESTART, which it should leave as they were;
- the flip rate from the prod flip monitor (a worker counter of reversals by pattern, built by another agent): its
  value for the 7 days before the deploy against the 7 after is the proof; the R-lost pattern should fall to ~0.

**Retires** the parked inline-ask cancel (agentctl item parked until 2026-11-09): with no inline asks, there is
nothing to cancel, and step 2b ("cancel a cluster's unstarted inline futures at the deadline") is superseded. An
experiment entry records it.

**Milestones.** 1a+1b the gate, restart-safe (behaviour change: what-if, CI measure, drift; prod numbers after the
deploy); 1c the inline asks deleted once the gate's prod numbers show what is left of them, with the parked cancel's
experiment entry. STOP after 1 lands.

## Fix 3: one shared replay core (alongside or third)

**Serves** R2 (the gates that prove it must agree on what the world is) and R7 (four harness bugs, one cause).

One `ReplayWorld`: listings with their arrival schedule, answers (stored, recorded, or fetched on demand: one
`AnswerSource` trait with those three implementations), a clock read from the recorded dates (never the harness's own
fixed clock), and content-keyed randomness (a draw is a function of the seed and what it is drawn for, as
`IdentityDrift.Share.filed` already is). Every harness builds its world through it and only chooses the arrival
schedule:

| harness | arrival schedule it chooses |
|---|---|
| `IdentityDrift` | seeded arrangement: venues, departures, re-scrape halves |
| slice cases (`IdentitySliceCasesSpec`, `IdentityDriftSlicesSpec`) | the slice's listings, 12 seeds |
| verify-recording / next-day | the recording's own days |
| `FamilyConvergence` / `MirrorRecompute` | the family's population lattice / the mirror history at T0, T1 |
| `identity-what-if` | base vs head over the mirror now |

Pieces landing elsewhere are adopted, not duplicated: the what-if extension (live answer source, final take,
`--recording`, bisect: `WHAT-IF-STATE`), the slice-coverage and drift-scenario-digest guards, and the per-family
gate switch (`GATE-SWITCH-STATE`). Each harness migrated is one milestone with its existing specs green and a
capture-replay case in the same commit (the `driftguards` rule).

**Milestone 3.1 (2026-11-06): the mirror's world** (`services.identity.MirrorWorld`, worker tests). The listings, venue
pages and documents of the local prod mirror at a moment, replayed through the identity history, and the clock AT that
moment. `MirrorRecompute`, `FamilyCounterfactual` and `identity-what-if` (`WhatIfResolve.corpusAt`, its settled final
take too) read the mirror through it; each built its own copy before, every one dated by the wall clock it ran at (the
verify-dating bug class of a57790c723f2). A what-if `--at` the default (now) reads alike; a past `--at` is now dated by it.

**Measured by:** each migrated harness gives the same verdicts as before on its own specs and on main's last gate run
(drift: the same flips per seed; slices: the same takes; what-if: `--verify` equal).

## Fix 2: rule burn-down (ongoing, after the per-family check gates)

**Serves** R3, R4, R5, R6. Starts once the per-family convergence check is THE gate (`GATE-SWITCH-STATE`).

Each kind (iii) rule, one commit each, is either rewritten as monotone in the population (a count that only grows as
listings arrive and counts only what the listing's own facts or TMDB do not deny, as `venues.current` was on
2026-11-05), or demoted to a SUPPORTING vote: it may block a new take, never withdraw one unless the listing's own
facts or TMDB contradict it. Order: by flips and served losses caused (the flip monitor's counter by `decided_by`, `scripts/flip-timeline`'s RULE
and SIBLING/POPULATION reversals by rule,
the backlog `identity-flip-backlog.tsv`), first `accept:joined`, `pooled:no-film`, `pooled:unnamed` (685 / 348 / 170
of the week's 1,567 changes). Namesake step 3 and bare-borrow should fall out (`PARKED-REVIEW`).

**Each commit:** `Identity-Design: docs/design/identity-structural-fixes.md`; what-if first; then a CI identity
measure with right->wrong 0; the family check clean; its row struck from the backlog when a flip goes.
A rewrite that costs served right takes stops for the owner (trade rule: ~100 right per wrong cured, each listed).

## Order and budget

4 -> 1 -> 3 (alongside where it unblocks a gate) -> 2. CI: at most 2 identity measure+drift sets in flight across
all agents (`CI-QUEUE.md`); what-if before any CI measure. State: the agent's `STRUCTURAL-FIXES-STATE.md`.
