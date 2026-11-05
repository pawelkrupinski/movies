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

**Measured by:** no take moves (what-if `--verify` on every country; a CI identity-measure with right->wrong 0 and
0 moved; drift clean) except at a site classified (c), whose moves are listed, judged and tested failing-first.
`IncrementalWholeEquivalenceSpec`, `LateInputEquivalenceSpec` and `ScopedProjectionEquivalenceSpec` stay green.

**Milestones.** 4a the three-case type, nothing producing `Failed` yet, every exhaustive match given its `Failed` arm
(a terminal failure read as no evidence through `Answer.failedAs`, the one function that may) (0 moved: no producer); 4b the model's sites; 4c the stage's sites (`decideWithout` the one collapse);
4d the shims deleted. STOP after 4 lands: report the list and the numbers.

## Fix 1: asking apart from deciding (second)

**Serves** R1, R2, R3 (the two-step write is 70 % of the flips), R7.

**The change.**

1. **Ask on arrival.** An enrichment step on a scrape's arrival (`onModelBatch`, `WorkerWiring.scala:218-222`, after
   the model's take-up) asks, through the existing question queue (`AgreementQuestions.enqueue`), every question the
   listing's cluster will need: `AgreementStage.questionsOf(cluster)`, extracted from `verdictsOf`'s gap collection
   (a pure function of the cluster's listings and the answers held). Follow-up questions (a search hit's record, a
   find for an agreed IMDb id) are asked when the answer that names them is filed: the filing's handler computes the
   cluster's questions again. New-arrival asks are claimed ahead of the agreement queue's `Behind` (7 days), as the
   model's live asks are today: the owner's "a new film's questions are asked straight away" (2026-10-23) stands.
2. **Project only what is complete.** A cluster is projected only when every question it needs is `Known` or
   `Failed` (fix 4's type). Every ask terminates (2a, landed d71cc09331ad / 30bf85002bb2: a turned-away ask fails, a
   family with no source is filed), so this is bounded by the answers' own timeouts and retries, not by a clock of
   ours. If a hard cap proves needed, it reads the injected clock and is a named, counted outcome.
3. **The projection is a pure function of stored inputs.** `askedInline`, `askNow`, `askedNow`, `AskNowWithin`,
   `InlineRounds`, `InlineFamilyAsks` and the inline pool go. A cluster with an `Unknown` question is not written:
   its listings keep the take written before, ONLY because nothing new was written yet; there is no `Held` basis and
   nothing is substituted (the parked `experiment/pending-losses-held` stays parked). A newer trigger for the same
   cluster supersedes a waiting one (`EventTrigger` already coalesces).
4. **Incremental equals whole by construction:** the whole resolve and the scoped projection both skip exactly the
   clusters with an `Unknown` read, and decide the rest from the same stored inputs.

**Open, for the owner, before 1 lands (STOP point):** a listing new to the site has no take written yet. Either it
appears at once on its own card as unmatched-pending (a gain, never a reversal, when its film comes), or it is held
off the site until its cluster is complete (p50 ~5 s, p90 ~20-30 s, p99 ~150-240 s from the 2026-11-05 latency
measurements). The default this plan builds: unmatched-pending at once, flagged `provisional`.

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

**Milestones.** 1a `questionsOf` extracted, equal to the gaps `verdictsOf` collects (property spec, 0 moved); 1b
asked on arrival and on filing (behaviour: asks earlier, decides the same); 1c projection skips incomplete clusters,
inline asks deleted (behaviour change: what-if, CI measure, drift; prod numbers after deploy). STOP after 1 lands.

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
