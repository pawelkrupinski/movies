# 2026-10-19 — a listing-level "is this a film at all?" signal (branch `experiment-film-ness`, recording 38713274617, base `b79c71a78`)

## Hold off a take whose billing reads as no film (film-ness) — V3f landed 2026-10-19
- **Inventory first:** `NonFilmEvents` (venue-aware, read only by the agreement stage, so the MODEL still takes an
  event billing), `Acceptance.notPerformedBeside` (a performance piece beside the film's piece), `onNoReusedPage`
  (needs a copied director), the scrape filter `NonMovieEventClassifier`. The brief's examples are mostly gone from the
  served takes on this recording: Schöne Aussicht is absent, Wohnzimmerkonzerte and "Dave - Music in Motion" stay
  BelowThreshold, SEVENTEEN's tours are concert films judged right; only "Giorgia Fumo - OUT OF OFFICE" (pooled 39%)
  stands, and it carries no event word at all — no title signal reaches it.
- **Offline (B), every base take × the judgements (4,715 judged, 17 wrong):** event vocabulary separates nothing
  (AUC 0.47; 0 of 275 judged event-worded takes wrong). A grouped-CV logistic over confidence + listing shape (no
  year/director/runtime/poster, one showing, the venue's take share, "Name – Show") drops log-loss 0.0252 → 0.0228
  (Brier 0.00363 → 0.00357), but the gain is from FACTLESSNESS (no director AUC 0.78, no runtime 0.73), the class-2
  bare-title story above, not event-ness. The venue's take share (< 0.5) concentrates the kinoheld gig venues
  (Kaminwerk, Lagerhalle, Casablanca Zwickau) but is a whole-corpus quantity the resolver has no cheap pre-take form of.
- **V1 (hand): `NonFilmEvents` at the model's acceptance rules** (venue-free `NonFilmEvents.Billing`), sparing a
  record itself titled as a performance. 10 DE/UK/US model takes moved: 1 cure by side effect (Diessen "Island") for a
  NEW wrong (Gröben "Island" → a 4-minute short), and right relays/concert films lost (NT Recorded Live ×2, SIX The
  Musical Live ×2, "Where Dragons Live" read as a performer billed live, GIANT - The Play).
- **V2 = V1 + "the record shares only the billing's event words"**: +38 right "BP: Silvesterkonzert 2026" (the billing's
  only other word "BP" is too short to count, so every record "shared nothing").
- **V3f (hand, kept form): V2's test alone** (`FilmNess`): a billing with event words (live, tour, show, band, gala,
  stage, night, …konzert, …festival, …party, comedy, tribute…; none inside brackets nor in a compound naming film), no
  named relay, no screening attached, and words beside them; refuse a record whose own or original title shares none
  of those other words, unless the record is in another script (its alternatives count then) or shares a year the
  billing states. Facts are NOT required: kinoheld's page copies the found record's runtime and year onto the gig.
  Iterations: V3/V3b moved nothing (`isScreenedBroadcast` vetoes any " live"; "Tour 2026" read as a relay's season);
  V3d needed the record's alternatives left out (TMDB 1419814 files "Tanzwut - Live Tour 2026 | Special Guest:
  Sagenbringer" as an alternative title of the Number_i film) and lost ENHYPEN ×16, ELF (Pyjama Party) ×6, Centre
  Stage, a 1972 alt title, Behind The Open Door, Pornfilmfestival's judged-right A Very Good Boy; V3e/V3f spared those.
- **Result (V3f, identity-measure-local, every country):** DE model 9 lost, all event takes judged wrong here
  (Goli "3-D-Show: Süd-Afrika" → VAPE SHOW; Comedy Buster Heidelberg LIVE!! ×2 → Peter Kay Live; Italo Cinema
  Festival 2026 ×2 → The Cure - Maia North Festival; KILMINISTER - A Tribute to Motörhead ×2; One Stage, Five Bands;
  Andreas Kümmert … live 2.0 → Allen Anders) = 9 cures, 0 right lost, 0 new wrong; ES/PL/UK/US and every final stage
  unmoved. Tanzwut stays taken (its page copies a year within one of the record's). Cost: a few regexes per listing,
  two lazy word lists, no state.
- **Why it was first parked:** fitted round by round to one recording's listings (each refinement spared a loss it had just
  caused) and its 9 cures are all one feed (kinoheld) — measure on a newer recording before trusting it; the
  surviving wrong takes of the class (Giorgia Fumo, Auf Augenhöhe, Soul Kitchen Band, Purple Rising) share a real
  word with their records, which no title rule reaches.
- **Learned variant:** not built in the resolver (a listing-level feature in `identity-weights.json` needs the
  refit and a recapture); offline it adds only the factlessness signal above.
- **Re-check (2026-10-19, V3f unchanged, squashed onto main `7261d4558` as branch `film-ness-recheck`):** no
  recording newer than 38713274617 exists locally, and GitHub's 403 blocks fetching one, so the out-of-sample check
  ran on the OLDER full recording 38666576506 (never tuned on, ~8 h earlier, mostly the same listings):
  identity-measure-local, every country — DE model 8 lost, all 8 cures already judged wrong (the same kinoheld set
  minus one Italo Cinema Festival listing), 0 right lost, 0 new wrong, 0 unjudged moves; ES/PL/UK/US and every final
  stage unmoved. So it holds, but this adds little independence: still one feed, no new cure. Separately, V3f breaks
  `ResolverAllocationBudgetSpec` ("a title dated by the year…": 35,177 B vs the 35,000 budget; main passes).
  **Fixed (72400733f):** not the lazy vals (read once per listing) but the rule's `.flatMap(filmLike)` — a function, a
  fresh `Right` and a fold closure per accepted take; a match returning the verdict untouched brings it to 34,968.7 B,
  main's own 34,969 (budget unchanged), and drops the unused `Listing.eventBilled` with its `NonFilmEvents.Billing`
  refactor. Re-measured on main `db31941c7`, recording 38713274617: DE model 9 cures (the six event billings main now
  carries as known-wrong `served,rows` rows), 0 right lost, 0 new wrong; every other country and stage unmoved; the
  identity suite green. - **Landed (2026-10-19, at the user's call):** measured on two LOCAL recordings only (38713274617, tuned on, and
  38666576506), both clean; the six known-wrong rows struck to `open` with the takes rows re-emitted. Still owed: a
  measure on a newer recording (`scripts/identity-measure`) once GitHub is back — a new wrong or right lost there
  sends V3f back here. New external traffic, small: a take V3f refuses lets the resolver fall through to per-name
  searches, so prod asks TMDB/IMDb a few more searches of event-like billings (DE Lagerhalle's "Wohnzimmerkonzerte
  Festival - Toi Et Moi | …": 5 new, filled into the DE capture).
- **Code:** `experiment/film-ness` (V1 `b03ef128a`, V2 `10610e9db`, V3f `a0429af20`).
- **Next angle:** a venue prior — the share of a venue's listings the model leaves unmatched or reads as events,
  computed once per corpus (it singles out the gig venues), as a condition beside V3f's test or Giorgia Fumo's bare
  "Name – Show"; and kinoheld's copied facts (runtime/year equal to the found record's) read as no facts.
