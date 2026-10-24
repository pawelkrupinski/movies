# Identity questions: one model for what the resolver and the agreement ask (2026-10-23)

Identity decisions read answers to questions asked of outside sources. Two systems ask them, with different rules for
asking, storing, remembering a non-answer, waiting and giving up: the model's gaps (`CandidateQuery`, film records,
venue pages) and the agreement stage's questions (a `Set[Question]`, step C1). GapHold, the drift check and
`AnswersChanged` see only the first, yet the agreement's answers move takes too, model takes included.

The plan unifies the plumbing and keeps the meanings: model answers still form candidates through a dependency chain,
agreement answers still vote, veto or correct a take. Steps, each landed on its own with 0 takes moved before step 4:

1. **One question type and policy table** (this note; no behaviour change). `services.identity.Question` names every
   question, `QuestionKind` its kind and the `HttpPath`s that ask it, `QuestionPolicies` (worker) what each kind is held
   to, read from the very constants the code acts on (since C3 the table defines them). `QuestionPoliciesSpec` pins the
   table below.
2. **One "waiting on" view**: model gaps ∪ agreement unknowns per listing and family, counted as unsettled by the drift
   check. Done 2026-10-24: `services.identity.WaitingOn`, read off the stage's kept waits by
   `AgreementStage.waitingOn` (a cluster's family questions, its fill's TMDB answers, its take's correction's questions);
   `IdentityDriftCheck` counts a listing waiting on any of them unsettled, and logs how many waited on the agreement
   alone (`waitingOnAgreement`). The explain page reads the production mirror, which holds no agreement wait: showing it
   there needs the waits stored, left for when step 4 stores them.
3. **One asker with priorities**: new film (inline, small per-tick budget) > agreement > refresh and sweep, spread
   across the day; a non-answer remembered for every kind; an age backstop for what the changes feed misses. It
   builds on the `path` label of `kinowo_worker_http_total` (`tools.HttpPath`) and the paced changes sweep
   (`tmdb-changes-sweep`, in flight 2026-10-23, identity-resolver.md §19.3 once landed).
   Done so far (2026-10-24): the model's live asks are hybrid (`LiveAsks` in `StoredFirstLookups`). A question
   never asked is asked live at once, as before; one asked and left unanswered waits out its backoff, the fill's to
   ask then, instead of being asked live again on every drain. The fill and the live asks share one memory
   (`WorkerWiring.identityQuestionMemory`), and every non-answer marker lives in its own collection,
   `identity_question_markers` (`TmdbKind.Marker`; the answers stay where they are). A marker written to
   `tmdb_queries` before the move is still read until the sweep's 7-day grace deletes it. And the agreement's side
   (the owner's choice): a cluster the live stage meets for the first time since it started, past its first pass, has
   its families asked inline (`AgreementStage.askNow`, `InlineFamilyAsks`) for at most `AgreementStage.AskNowWithin`
   (5 s), then resolved again on what they filed; what is not answered by then is the queue's, as before, and an
   answer that comes later is filed and asks for a projection. Each cluster is asked so once; the drift check's stage
   never asks.
   C1, fewer abstractions (2026-10-24): `Set[Question]` is the one currency for the agreement's questions asked, open
   and waited on. Gone: `AgreementStage.Open`, `AgreementStage.CorrectionWaits`, `Question.ofOpen` / `toOpen`, the
   accessors `wantedFinds` / `wantedPosters` / `wantedCatalogue` / `wantedRecords` and the `Pass`'s five accumulators.
   The stage notes every question in one set (the corrections' posters apart, handed a few at a time), keeps a
   cluster's and a correction's waits as one, hands `ask` (`AgreementQuestions.enqueueOpen`) and `askNow` one, and
   `AgreementStage.wanted` is all of them; a caller after one kind matches it out (`Question.families` for the
   family store's pairs). Task types, payloads and dedup keys are as before (`QuestionPoliciesSpec`). The model's
   `AnswersChanged` stays split: every reader indexes each kind into its own map, so a union would be re-split per tick.
   C2, one task type, one handler, one asking function (2026-10-24): every agreement question has a stable id, the form
   its store files or names it by (`Question.id`, read back by `Question.parse`: `<family>|<question>`, `find|<imdbId>`,
   `tmdb|record|<tmdbId>`, `poster|venue|<url>`, `poster|film|<tmdbId>`, `catalogue|<source>|<id>`,
   `catalogue-page|<url>`). `AgreementQuestions.enqueue` queues them as one task type, `AskQuestion`, its payload the
   ids (one a task, a source's catalogue ids 50 a task as before), its dedup key `agreement|<ids>` (a family
   question's as before), claimed `Behind` and counted under the same `AgreementQuestionMetrics` labels.
   `AgreementAsker.ask` asks every kind, and `InlineFamilyAsks` asks a new cluster's families through it (a late answer
   filed and announced, as on the queue). Gone: `AgreementQuestionHandler`, `AgreementFindHandler`,
   `AgreementPosterHandler`, `AgreementCatalogueHandler`, `AgreementQuestions.enqueueOpen` /
   `AgreementCatalogueQuestions.enqueue`, and the inline asks' own filing. The four old task types and their payload
   decoders (`AgreementQuestions.questionsOf`) stay only for the tasks queued before the deploy, asked by the same
   asker (`AgreementAsker.oldQueued`); remove them after 2026-11-07.
   C3, one memory of non-answers and one policy that decides (2026-10-24): `QuestionMemory` (was `TmdbGapMemory`) is the
   one memory, keyed by the `Question`, for every kind the identity system asks itself: `due(questions)`,
   `unanswered(questions)` (a durable non-answer, its kind's long wait), `failed(questions)` (5 min doubling to 6 h), and
   the TMDB outage/recovery release, kept for the model's questions. The model's markers keep their ids
   (`unanswered|<TmdbStore.questionId>`, `unanswered|film|<id>`) and the fallback read of old ones in `tmdb_queries`; an
   agreement question's is `unanswered|<Question.id>`. The asker remembers a failed ask and the task is done (a host
   whose breaker is open still defers it); `AgreementQuestions.enqueue`, the queue's asker and the inline asks skip a
   question still inside its wait, so the queue's backoff no longer asks it a second time. The poster give-up counts
   the memory's failures in a row (`QuestionPolicies.PosterGiveUpAttempts`), files the poster unread as before and is
   remembered `PosterUnreadAge`. Venue pages stay the pipeline's (their freshness mark). `QuestionPolicies` holds every
   renewal age, non-answer wait and sweep age, and the stores (`FamilyAnswerStore`, `PosterAnswerStore`,
   `CatalogueAnswerStore`), the memory (by the question's kind) and `TmdbStoreSweep` read them from it. `QuestionRenewals`
   (was `TmdbRefreshes` and `TmdbAgeBackstop`) is the one renewal by age, `due(): Seq[Question]`, which the lookup fill
   asks after its gaps: aged questions, then at most 50 aged records and people (`Question.Person`, a new kind) a round,
   the store scanned at most every 6 h. Retry timings of agreement failures moved from the queue's backoff to the
   memory's; nothing else. For step 4 the memory records every agreement ask that comes to something and answers
   `outcome(question)`: none (never attempted; merely queued is not), `Answered`, `Absent` (a durable non-answer, an empty
   answer filed included) or `Failed(attempts, nextRetryAt, host)`, and `attempted(question)`, until the marker is swept
   (7 d). Each host's health is a marker of its own (`roundEnded(…, host)`; TMDB's keeps `unanswered|tmdb-health`), and a
   host's recovery releases the failures marked against it; only the fill reports a host's rounds today (TMDB).
4. **One settle rule** in place of GapHold and `PartialAfter`, and no timer. Done 2026-10-24 (C4): a cluster waiting
   on its families' questions SETTLES once every question it waits on has been ATTEMPTED (`AgreementStage.settles`):
   answered, a durable non-answer (404/410, "nothing there", filed empty) or a failed ask (connection, timeout, 5xx after
   the fetch's on-the-spot retries). It is then decided on the families that answered, the rest left out as no verdict.
   A question merely enqueued, never attempted, holds the cluster; no clock releases it. "Attempted" is the memory's
   record of each ask (`QuestionMemory.attempted`, injected as the stage's `attempted`; an answer held counts by
   itself), and an ask that failed asks for a projection as a filing does, so the cluster settles at once.
   Settling ends the wait, not the asking: the questions none answered stay asked on every pass
   (`AgreementStage.unreached`, retried on the memory's backoff, 5 min doubling to 6 h), and the settled verdict reads
   them as unanswered (`AgreementStage.Unanswered`), so the first answered reads it again, after a restart too. Any new
   data reads a settled cluster again: a late answer (that read), a refreshed answer that moved (its read's digest), a
   re-scrape (its listings' digest); a settled cluster settles again rather than waiting anew. A TMDB record read again
   for its release day is waited on until its read is filed or attempted (`AgreementStage.waitsOn`); `RecordWait`'s
   day is gone. `PartialAfter` is gone; the drift check's whole resolve applies its fresh stage once, reading the same
   memory. OUTSTANDING questions — handed to the queue and never attempted (a task lost, the queue wiped, a restart
   mid-task), or failed and past their retry — are queued again every 10 min (`OutstandingQuestions`,
   `identity-outstanding-questions`). The one rule holds for every agreement question a take reads, not the clusters'
   alone: a find, a poster, a catalogue page or id, a record's day, a family's question read by a correction or the
   broadcast join is waited on until attempted, then read as no evidence (`AgreementStage.settled`: a find as naming no
   TMDB film, a poster as unread, a catalogue question as naming nothing, a correction's family as no verdict), the take
   or correction decided without it, the question asked on every pass and read again once answered (a find or record
   the stage waits on is counted among the answers' changes as the TMDB store files it, C5's D3). So the policy table has no wait column: a model question
   waits on fill rounds, an agreement question until attempted.
   Host recovery (2026-10-24, the owner: "track external services being down and coming back"): the enrichment chain's
   host breakers tell a listener each transition (`HostCircuitBreakerHttpFetch.Transition`: `Opened`, `Closed`,
   `Answered(failures)`). A host back — its breaker closed after an outage, or answering after at least
   `MinOutageReads` failures in a row that never opened it — is `HostRecovery`'s, on a thread of its own: the memory
   marks it recovered (`recovered(host)`, every failure marked against it before due), the stage's open questions that
   failed against it are queued again at once (the stage hands a question over once per answers' version), and the fill
   is asked for a round; one log line each, `<host> back: N questions asked again`. Nothing is sent outside the
   queue's and the fill's own pace. Every failed agreement ask is marked against its host (`AgreementQuestions.hostOf`):
   the one its failure names (a status, an open breaker, an unreadable body), else the last host the ask requested,
   which the host breaker notes on the asking thread (`tools.RequestedHost`) — a timeout or refused connection names none.
   C5, one way answers are cached and one change feed (2026-10-24). The caches stay apart, each for its own reason:
   `CachedTmdbDocuments` (the byte-capped document LRU and its film-hit cache, sized by the 2026-10-07 heap dump),
   `StoredTmdbLookups` (one prefetch's decoded working set, let go after it) and `VenuePageIndex` (the frozen snapshot each
   settle diffs). D1: `TmdbIdentityLookups` no longer memoises a title's IMDb suggestions for 10 min (Unknown included);
   the questions asking while a read runs share it, and a fill round's lookups keep each title's read for the round
   (`oneReadPerTitle`, one ask per question). D2: one `AnswerFeed` (identity, beside `AnswerChanges`;
   `FiledAnswerFeed`, owned by `IdentityTmdbLayer`). The TMDB store publishes a document whose value moved, the family
   store every answer it files (and `noteFiled`'s TMDB record read again), the venue page index a page whose answer moved
   at a settle (before the drain, as before); subscribers run on the publishing thread. The model subscribes (TMDB keys
   to `filed`, venue keys to `observed`); the agreement stage reads `FamilyAnswerStore.changes`, a view counting only the
   keys `Question.parse` names, so a TMDB filing leaves its version, and a quiet tick, as they were (`AnswerFeedSpec`
   holds the old and new versions and `changedSince` sets side by side). D3 (done): the agreement view maps the TMDB
   store's own filings of a find (`tmdb_queries:find|<imdbId>`) and a film record (`tmdb_films:<id>`) to the ids the
   stage reads them by (`find|<imdbId>`, `tmdb|record|<id>`; `FamilyAnswerStore.agreementKeyOf`), but only while the live
   stage waits on that question (`awaitedBy(stage.wanted.contains)`): a take-up files thousands of records, and each
   counted would move the stage's version and re-read every verdict. So the asker's two hand bridges (`noteFiled` after a
   find and after a record read again) are gone, and with them the corrections' special case for a find "filed outside
   the answers' changes": a waiting correction is read again when a find it asked is filed, as for any question. A find
   or record filed unmoved moves nothing; the ask's attempt still reads the takes again (`attemptedSince`). A wanted
   record or find filed by the model's fill now releases the wait too, before the agreement's own ask. Next: D4.

Decided by the owner: a new film's questions are asked **straight away**, inline, with no budget of their own beyond
the rate-limit protections every request already goes through (the 429 gate, breakers, `HostPolicies`) (2026-10-23);
and NO timer (2026-10-24, superseding the 30-minute deadline of 2026-10-23): a take settles once every question it
waits on was attempted, and any answer or new data recalculates it. A new answer can always move a take: settling only
ends the wait, never the take's readiness to change when an answer arrives or changes.

## The questions

| kind | question | source | asked by (`path`) |
|---|---|---|---|
| Title | `CandidateQuery.Title`: a yearless title search | TMDB /search/movie | fill, live, refresh |
| Director | `CandidateQuery.Director`: a credited director's (or writer's) films | TMDB /search/person → movie_credits | fill, live, refresh |
| Imdb | `CandidateQuery.Imdb`: IMDb's films of this very title | IMDb suggestions → TMDB /find | fill, live, refresh |
| ImdbTitled | `CandidateQuery.ImdbTitled`: the same in any language or AKA | IMDb suggestions + titles → TMDB /find | fill, live, refresh |
| TitleCredit | `CandidateQuery.TitleCredit`: the director a title closes on | IMDb suggestions → Director's walk | fill, live, refresh |
| FilmRecord | a film an answer names | TMDB /movie/{id} ×2; fallback ids → IMDb | fill, live, refresh, sweep |
| Person | a person a director search names: the films they directed and wrote | TMDB /person/{id}/movie_credits | fill, live, refresh, sweep |
| VenuePage | a listing's detail page | the venue's site (`VenuePageReader`) | venue_page |
| FamilySearch | `title\|…`, `director\|…` of a family | IMDb, Wikidata, Filmweb, Metacritic, RT (`FamilySources`) | agreement |
| FamilyRecord | `record\|…` of a family | as above | agreement |
| FamilyShowing | `showing\|<venue>`: a venue's Filmweb programme (PL) | Filmweb | agreement |
| Find | an agreed IMDb id | TMDB /find | agreement |
| Record | a TMDB record read again for its release day | TMDB /movie/{id} | agreement |
| Poster | a venue's or a TMDB film's poster | the image | agreement |
| CataloguePage | the catalogue ids a venue's film page links | Letterboxd, Flicks, Kinoteka, Alamo pages | agreement |
| CatalogueId | what a catalogue id maps to | Wikidata P-ids (Webedia, Letterboxd, RT) | agreement |

The agreement's questions are `AskQuestion` tasks claimed `AgreementQuestions.Behind` (7 days) behind every other task; the model's
are asked by the lookup fill (`identity-lookup-refresh`, every 30 min, `IdentityLookupFillRate`, 300/min in prod) and
live at take-up (`StoredFirstLookups`).

## The policy table (`QuestionPolicies.table`): it decides

Every age, non-answer wait and sweep below is defined in `QuestionPolicies` and read from there by the code that acts on it (C3):
the renewals (`QuestionRenewals`), the memory (`QuestionMemory`, by the question's kind), the agreement's stores'
`wanted` and the store sweep. Only the venue page's 6 h is the pipeline's, stated here. A remembered non-answer is an
`unanswered|` marker in `identity_question_markers`, swept 7 d after it was stamped.

| kind | store | asked again | non-answer | swept |
|---|---|---|---|---|
| Title, Director, Imdb, ImdbTitled, TitleCredit | tmdb_queries | 1 d below 0.9 confidence, 7 d settled (`QuestionRenewals`) | remembered (fill and live asks): empty 1 d, failure 5 min doubling to 6 h | unread 30 d |
| FilmRecord | tmdb_films | changes feed; 30 d while the model reads it (`QuestionRenewals`) | as above | unread 30 d |
| Person | tmdb_people | as FilmRecord | its director search's marker | unread 30 d |
| VenuePage | venue_pages | 6 h (`Freshness` DetailEnrich, the pipeline's) | the pipeline's: gone (404/410) filed; failure marked attempted, read again past 6 h | never |
| FamilySearch | identity_family_answers | 90 d | empty filed; failure remembered, 5 min doubling to 6 h | 400 d since fetched |
| FamilyRecord | identity_family_answers | 365 d | as above | 400 d |
| FamilyShowing | identity_family_answers | 20 h | as above | 400 d |
| Find | tmdb_queries | when the stage opens it | none found filed; failure remembered, 5 min doubling to 6 h | unread 30 d |
| Record | tmdb_films | once, when a take waits on its release day | unknown filed empty; failure remembered, 5 min doubling to 6 h | unread 30 d |
| Poster | identity_family_answers | 365 d | failure remembered, 5 min doubling to 6 h; given up after 8 in a row, filed unread and remembered 7 d | 400 d |
| CataloguePage | identity_family_answers | 90 d | empty filed; failure remembered, 5 min doubling to 6 h | 400 d |
| CatalogueId | identity_family_answers | 365 d mapped, 30 d unmapped | as above | 400 d |

A take waits on a model question by the lookup fill's rounds, and on every agreement question until it is attempted
(step 4): no column, since it never varies within a system.

## What the table shows (the holes steps 2-4 close)

- **The drift check sees only the model** (verified): `IdentityDriftCheck` counts a key unsettled when
  `ResolverDecision.unanswered > 0`, the resolver's unanswered candidate queries. An agreement unknown is not counted.
- **Three stop-waiting rules**: fill rounds, GapHold's 2 h (reverted), `PartialAfter`'s 10 min with its
  at-least-one-answer condition. Since step 4 the agreement's is one, with no clock: until attempted.
- **Non-answer memory was partial**: until step 3 only the fill wrote gap markers, and a live miss at take-up left
  none, so it was asked again at every take-up. The model's live asks write them now. Every other kind remembered its
  non-answer its own way (checked 2026-10-24): an empty answer is filed as one (a find naming no film too), a failed
  agreement ask waited out the task queue's backoff, a failed venue page read is marked attempted for the detail
  refresh's 6 h. Since C3 one memory (`QuestionMemory`) holds every failed ask of the identity system's own, the
  agreement's included; an empty answer is still filed, and the venue page still the pipeline's.
- **Records and people refreshed only from the changes feed**; an edit it never showed (the sweep behind longer than
  TMDB's 14 days of history, or an edit read as irrelevant) left them stale for good. Since step 3 the lookup fill
  also fetches again, last in its round and at most `QuestionRenewals.PerRound` (50) a round, every record and person
  the model reads that TMDB last gave over 30 days ago (`QuestionRenewals`), oldest first.
- **Separate askers, separate pacing**: the fill's rate, the task queue's `Behind` and the sweep's own pace. The `path`
  label now splits them on the counter.

## The staging screen (`/debug/identity/staging`, local)

What every country's takes still wait on, and what was decided lately, on one dev-only page of a local dev server
(`scripts/dev-server.sh`; 404 in prod like the rest of `/debug`). The stage's waits live in each worker's memory
(`AgreementStage.waitingOn`), not in Mongo, so each worker publishes them to its own database, and the local server
reads every country's through the prod mirror:

- **Worker** (`IdentityStagingPublisher`, every 20 s, on its own thread, never the projection's): over the takes the
  projection last wrote, a cluster whose `WaitingOn` is not settled STAGES — its listings, take, model gaps, whether the
  fill waits on TMDB, and each open question with its kind, who it is asked of and `QuestionMemory.outcome` (not
  attempted / failed n× with the next retry and host / absent / answered), plus attempted/total, rewritten as it changes
  and deleted once it stops. Every RESOLUTION since the last publish is written once, its own document, which the
  database deletes 7 days later (a TTL index on `expireAt`): a cluster that staged and waits on nothing now (`settled`
  with its take, or `regrouped` when its listings no longer form it); and, from each LISTING's film as the last publish
  saw it, a cluster of new listings (`decided`: most new films, asked inline within one projection) or one whose
  listings' film moved (`gained`, `lost` or `changed`, with the film it had: `was`). By listing, not by cluster key: a
  key moves when a new listing sorts first or clusters merge, which changes no listing's film. The first publish after
  a boot only records. At most 500 staging clusters (the longest waiting) are listed and 500 resolutions written a
  publish; 60 questions each; the per-country beat counts them all. Each publish also exports the takes by basis
  (`kinowo_worker_identity_takes{basis}`, `_take_listings{basis}`) and counts each resolution
  (`kinowo_worker_identity_cluster_changes_total{basis,change}`): the worker dashboard's "Identity takes" panels.
- **Store** (`IdentityStaging`, `identity_staging` in each country's database): staging clusters, resolutions and
  the country's beat. Mirrored locally (`DebugMirror.IdentityStaging`, `scripts/local-mirror/mirror-targets.js`), so
  the page follows prod as the mirror tails it, TTL deletes included.
- **Web** (`IdentityStagingController`, wired in `DebugWiring` over the per-country mirror connections): every
  country's section on the same page, each cluster a `<details>` row opening onto its questions; the page polls
  `/debug/identity/staging/data` every 5 s with each country's cursor, so a poll carries only the clusters written
  since (and those written at the cursor, once more). A beat older than two minutes is shown stale: that worker
  stopped publishing, or the mirror stopped following.
