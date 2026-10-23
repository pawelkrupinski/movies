# Identity questions: one model for what the resolver and the agreement ask (2026-10-23)

Identity decisions read answers to questions asked of outside sources. Two systems ask them, with different rules for
asking, storing, remembering a non-answer, waiting and giving up: the model's gaps (`CandidateQuery`, film records,
venue pages) and the agreement stage's questions (`AgreementStage.Open`). GapHold, the drift check and
`AnswersChanged` see only the first, yet the agreement's answers move takes too, model takes included.

The plan unifies the plumbing and keeps the meanings: model answers still form candidates through a dependency chain,
agreement answers still vote, veto or correct a take. Steps, each landed on its own with 0 takes moved before step 4:

1. **One question type and policy table** (this note; no behaviour change). `services.identity.Question` names every
   question, `QuestionKind` its kind and the `HttpPath`s that ask it, `QuestionPolicies` (worker) what each kind is held
   to today, read from the very constants the code acts on. `QuestionPoliciesSpec` pins the table below.
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
   (`WorkerWiring.identityGapMemory`), and every non-answer marker lives in its own collection,
   `identity_question_markers` (`TmdbKind.Marker`; the answers stay where they are). A marker written to
   `tmdb_queries` before the move is still read until the sweep's 7-day grace deletes it. And the agreement's side
   (the owner's choice): a cluster the live stage meets for the first time since it started, past its first pass, has
   its families asked inline (`AgreementStage.askNow`, `InlineFamilyAsks`) for at most `AgreementStage.AskNowWithin`
   (5 s), then resolved again on what they filed; what is not answered by then is the queue's, as before, and an
   answer that comes later is filed and asks for a projection. Each cluster is asked so once; the drift check's stage
   never asks.
4. **One settle rule** in place of GapHold and `PartialAfter`: a decision on an established take waits only on its
   family's own open questions, until answered or the deadline passes. This is GapHold v4.

Decided by the owner (2026-10-23): one deadline for every kind, **30 minutes**, after which the take settles on what it
has while the questions not yet reached keep being asked; and a new film's questions are asked **straight away**,
inline, with no budget of their own beyond the rate-limit protections every request already goes through (the 429
gate, breakers, `HostPolicies`). A new answer can always move a take: settling only ends the wait, never the
take's readiness to change when an answer arrives or changes, before the deadline or after.

## The questions

| kind | question | source | asked by (`path`) |
|---|---|---|---|
| Title | `CandidateQuery.Title`: a yearless title search | TMDB /search/movie | fill, live, refresh |
| Director | `CandidateQuery.Director`: a credited director's (or writer's) films | TMDB /search/person → movie_credits | fill, live, refresh |
| Imdb | `CandidateQuery.Imdb`: IMDb's films of this very title | IMDb suggestions → TMDB /find | fill, live, refresh |
| ImdbTitled | `CandidateQuery.ImdbTitled`: the same in any language or AKA | IMDb suggestions + titles → TMDB /find | fill, live, refresh |
| TitleCredit | `CandidateQuery.TitleCredit`: the director a title closes on | IMDb suggestions → Director's walk | fill, live, refresh |
| FilmRecord | a film an answer names | TMDB /movie/{id} ×2; fallback ids → IMDb | fill, live, sweep |
| VenuePage | a listing's detail page | the venue's site (`VenuePageReader`) | venue_page |
| FamilySearch | `title\|…`, `director\|…` of a family | IMDb, Wikidata, Filmweb, Metacritic, RT (`FamilySources`) | agreement |
| FamilyRecord | `record\|…` of a family | as above | agreement |
| FamilyShowing | `showing\|<venue>`: a venue's Filmweb programme (PL) | Filmweb | agreement |
| Find | an agreed IMDb id | TMDB /find | agreement |
| Record | a TMDB record read again for its release day | TMDB /movie/{id} | agreement |
| Poster | a venue's or a TMDB film's poster | the image | agreement |
| CataloguePage | the catalogue ids a venue's film page links | Letterboxd, Flicks, Kinoteka, Alamo pages | agreement |
| CatalogueId | what a catalogue id maps to | Wikidata P-ids (Webedia, Letterboxd, RT) | agreement |

The agreement's questions are tasks claimed `AgreementQuestions.Behind` (7 days) behind every other task; the model's
are asked by the lookup fill (`identity-lookup-refresh`, every 30 min, `IdentityLookupFillRate`, 300/min in prod) and
live at take-up (`StoredFirstLookups`).

## The policy table today (`QuestionPolicies.table`)

| kind | store | asked again | non-answer | swept | a take waits |
|---|---|---|---|---|---|
| Title, Director, Imdb, ImdbTitled, TitleCredit | tmdb_queries | 1 d below 0.9 confidence, 7 d settled (`TmdbRefreshes`) | marker (fill and live asks), empty 1 d, failure 5 min doubling to 6 h (`TmdbGapMemory`) | unread 30 d | on fill rounds |
| FilmRecord | tmdb_films | changes feed only | as above | unread 30 d | on fill rounds |
| VenuePage | venue_pages | 6 h (`Freshness` DetailEnrich) | gone (404/410) filed; failure nothing | never | on fill rounds |
| FamilySearch | identity_family_answers | 90 d | empty filed; failure on queue backoff | 400 d since fetched | 10 min* |
| FamilyRecord | identity_family_answers | 365 d | as above | 400 d | 10 min* |
| FamilyShowing | identity_family_answers | 20 h | as above | 400 d | 10 min* |
| Find | tmdb_queries | when the stage opens it | nothing filed, no marker | unread 30 d | 10 min* |
| Record | tmdb_films | when a take waits on its release day | nothing filed, no marker | unread 30 d | 10 min* |
| Poster | identity_family_answers | 365 d | given up after 8 failures, retried after 7 d | 400 d | 10 min* |
| CataloguePage | identity_family_answers | 90 d | empty filed; failure on queue backoff | 400 d | 10 min* |
| CatalogueId | identity_family_answers | 365 d mapped, 30 d unmapped | as above | 400 d | 10 min* |

\* `AgreementStage.PartialAfter`: a waiting cluster resolves once every question is answered, or once 10 min have
passed AND at least one is (`AgreementStage.due`). A cluster none of whose questions is ever answered keeps waiting
until one is.

## What the table shows (the holes steps 2-4 close)

- **The drift check sees only the model** (verified): `IdentityDriftCheck` counts a key unsettled when
  `ResolverDecision.unanswered > 0`, the resolver's unanswered candidate queries. An agreement unknown is not counted.
- **Three stop-waiting rules**: fill rounds, GapHold's 2 h (reverted), `PartialAfter`'s 10 min with its
  at-least-one-answer condition.
- **Non-answer memory is partial**: until step 3 only the fill wrote `TmdbGapMemory` markers. The model's live asks
  write them now; the agreement's TMDB find and record re-read, and a venue page read that fails, still leave none.
- **Records and people refresh only from the changes feed**; a missed window leaves them stale. The paced sweep in
  flight (`tmdb-changes-sweep`) persists its window and cursor so a missed window resumes; an age backstop is still
  step 3's.
- **Separate askers, separate pacing**: the fill's rate, the task queue's `Behind` and the sweep's own pace. The `path`
  label now splits them on the counter.
