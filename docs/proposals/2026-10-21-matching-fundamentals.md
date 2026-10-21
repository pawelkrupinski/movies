# Matching fundamentals: what to change beyond per-billing fixes

Written 2026-10-21, at main `96fb23c6c`. This is research and a proposal. No production code changed. The numbers
come from the five local prod mirrors (`mongodb://127.0.0.1:28017/kinowo_<cc>_prod_mirror`, PL `kinowo_prod_mirror`),
read the same afternoon. The labels are `identity-judgements.tsv` plus `review_local.review_answers`. To reproduce, run
`scripts/experiments/matching-fundamentals/export.js` (mongosh, once per mirror, writing JSONL), then
`analyse.py <repo> <dir>` (`python3 -I`), both in `scripts/experiments/matching-fundamentals/`.

## 1. Where the error budget is

Each judged listing below is one (venue, billing), and it is read through what prod *serves*: the screening's
`filmId` → `movies.tmdbId`, not the trace.

| cc | listings | wrong film served | event served as a film | judged right, served none | judged right served |
|---|---|---|---|---|---|
| pl | 11,325 | 2 | 0 | 32 | 662 |
| de | 34,212 | 15 | 0 | 70 | 944 |
| uk | 36,356 | 6 | 10 | 174 (161 are CBeebies Panto, a disputed judgement) | 3,155 |
| us | 103,279 | 0 | 0 | 3 | 1,599 |
| es | 6,370 | 0 | 0 | 0 | 187 |

- **Wrong-served: 31 listings.** These are the 21 TSV listings from the brief, plus UK "Toddler Club: Disney Junior
  Cinema Club 2026" ×10, which the review page marks an event. By cause:
  - **Event, stage or programme read as a film (~18).** Pornfilmfestival nights ×5 (cured on main, not yet
    deployed), Disney Junior Cinema Club ×10, Regio Tales, "A Christmas Carol by Patrick Barlow", Into the Shadows,
    Notre-Dame de Paris ×2, SHINJUKUY BOYS // … The Notre-Dame and Into the Shadows rows overlap with the double count
    above, so the class total is approximate.
  - **Namesake or remake against a stated or copied fact (~10).** The Killer ×4, Kohlhiesels Töchter, Ein Neues
    Leben, The Black Hole, Zniknięcie, Island, München Mord.
  - **Factless bare billing (1).** Binti.
  - **Copied catalogue facts.** About 8 of the 31 rest on facts that a feed (kinoheld, Filmstarts, Cineamo) copied
    from the *found* film (Island, Regio Tales, Black Hole, The Killer).
- **Lost-right: ~120 listings, outside the disputed panto.** DE: Der kleine Prinz ×16 (Cineamo overrule), The
  Killer ×7, Dolly ×10, Alpen Film Festival ×12 (a review that looks mistaken), Über Unterbiberger. PL: UFF and
  Kinoteka strands, opera retransmissions, Maraton horrorów. UK: Q&A suffixes, Purbeck, Kino KILN.
- **Unmatched listings by billing kind.** The kind comes from the prototype parse in §3.1. "film" means no event word.

  | cc | film | relay | multi-work | live event | mystery | festival slot | total untaken |
  |---|---|---|---|---|---|---|---|
  | pl | 533 | 166 | 451 | 57 | 0 | 9 | 1,216 |
  | de | 1,688 | 78 | 269 | 184 | 37 | 66 | 2,322 |
  | uk | 1,125 | 103 | 79 | 326 | 437 | 26 | 2,096 |
  | us | 112 | 711 | 3 | 98 | 8 | 1 | 933 |
  | es | 36 | 40 | 0 | 0 | 0 | 0 | 76 |

- **Unmatched real films.** I hand-read a 40-listing random sample of the "film" column per country. About 15–25% of
  it is real films: PL ≈ 90 listings, which matches the 86 films in
  [pl-unmatched-real-films](../investigations/2026-10-20-pl-unmatched-real-films.md); DE ≈ 350–420; UK ≈ 170. The
  rest of that column is events that no title word gives away: comedians, travel talks, local concerts, Peppa and
  Bing sets, El Clásico. UK has ~230 more listings that are double bills of two TMDB shorts ("We're Going on a Bear
  Hunt + The Tiger Who Came to Tea").
- **Relays are the largest recall class.** About 1,100 relay-worded listings are untaken. US "MetOpera: Carmen
  (2009)" alone is 514; Opéra de Paris Fanciulla is 185 (no TMDB record). The US/ES relay agent puts ~750 of these
  within reach: house banner + work + season + encore/live.
- **Events mis-taken as films, unjudged.** The judged count is 10 + ~8. The unjudged pool cannot be sized from title
  words: the lexicon's hits on served takes are mostly real films ("24 Hour Party People", "28 Days Later", "(Pyjama
  Party)" screenings).

**Conclusion of the budget.** Wrong-served is ~31 of ~190k listings and is already in the long tail: each cause is
1–10 listings. The remaining budget is recall, in this order: relays (~750–1,100), multi-work bills (~230 UK + PL
marathon sets), strand-prefixed and edition real films (~600), and unmatched real films that need a better search
(typos, Polish-only titles, not yet on TMDB). Any fundamental change must buy recall without adding wrongs.

## 2. Why the per-billing fixes keep coming

- Since 10-14 there have been 194 commits to `identity/` and `titlerules/`. By subject: 85 are billing/title
  interpretation (prefixes, suffixes, version tags, "and", "//", "+", subtitles, credits), 37 are relay, stage or
  season, 16 are vetoes and dates, and 19 are refactors.
- **About two thirds of the churn is reading the billing string**, and that reading happens in at least 15 places:
  - `TitleRules` / `ExtraTitleRules` (~350 regex rules in tiers)
  - `TitleNormalizer`
  - `NonMovieEventClassifier` (the scrape filter)
  - `NonFilmEvents` (the agreement stage)
  - `FilmNess` (model acceptance)
  - `ListingShape` (relay, house, bills, programme slot)
  - `BilledPiece`, `MultiFilmBill`, `StageRelays`, `StageWorks`, `LeanStageWorks`, `VersionTags`,
    `TitleDecorations`/`DecorationWords`, `TitleCredit`, `BillingReversals`, `AcceptanceTitleRules`
    (`andJoinsTwoWorks` …)

  Each reads the raw string again with its own regexes, so a new billing shape has to be taught to several of them.
  For example, "(Cine)" was a title rule, then a relay word, and then had to stay out of the work.
- The prod traces of the five mirrors fire **404 distinct rule/veto names**.

## 3. Candidates, measured

### 3.1 An explicit event / non-film classifier stage before matching: rejected as a gate, kept as routing

- **Prototype.** A multilingual lexicon parse (`analyse.py: parse`) sorts each billing into one of: mystery,
  multi-work, relay, live event, festival slot, or film. I gave it no tuning beyond a first pass.
- **As a refusal gate (no-record kinds refuse any film):**
  - It cures **0 of the 31 wrong-served**. They read like film titles: "Regio Tales", "Into the Shadows",
    "Notre-Dame de Paris", "Pornfilmfestival: <film>".
  - It refuses **512 judged-right listings**. These are HYBE CHEERING PARTY ×461 (a concert-film record), Splat!FilmFest
    titles, "Live - Vortrag:" travel films with records, "+ Q&A" screenings, and Bowie talk screenings.
- This matches V3f film-ness (event vocabulary AUC 0.47) and the venue-kinds experiment (event share runs backwards)
  in `identity-experiments.md`. **Title words do not separate events from films.** What does is context: one
  performance, no runtime, an arts-centre venue, an "interval", and copied facts.
- **Keep the kind as a routing field instead.** A relay goes to the relay matcher (§4.1). A multi-work bill goes to
  piece matching (each piece its own work). A festival slot goes to "no work". Mystery goes to "no work". A film goes
  to the normal search. This is part of change 4.2, not a stage of its own.

### 3.2 Runtime and date-window plausibility as HARD constraints: rejected

Each test runs on served takes. Each cell is listings violating / listings checkable.

| constraint | judged right violating | judged wrong violating | unjudged violating |
|---|---|---|---|
| stated year vs film year, Δ≥3 | DE 34/298, UK 2/55, ES 4/146 | 0/7 | 355 |
| runtime Δ >20 min and >30% | UK 42/1,708, DE 16/151, US 8, ES 9 | 0/9 | 176 |
| film first released >90 d after the last showtime | 4 / ~6,500 | 0/23 | 118 |
| film year > Cineamo upper bound +1 | DE 9/308 | 1/1 (Ein Neues Leben) | 19 |

- The judged-right takes that violate are anniversary re-releases ("Der mit dem Wolf tanzt (35-jähriges Jubiläum)",
  dated by the re-release), "with Wine" and Q&A screenings (the venue's runtime includes the event), festival
  premieres before the TMDB date (UFF "Demony" 2027), and season relay records.
- The wrongs **do not** violate these constraints. They are factless, or their facts were copied from the found film.
  The model already holds these signals as soft features. Making them hard costs rights and cures nothing.

### 3.3 Cross-venue / cross-source agreement as a first-class signal: mostly already there; one by-product worth keeping

- **Same billing at ≥2 venues, half taken:** DE 8 keys (32 untaken listings, 9 judged lost), UK 11 (12), PL 1. A "my
  siblings took it" fill is worth ≤45 listings. Families already pool within a country, and the cross-country
  version was measured negative on 10-19.
- **Wrong-served are not caught by agreement.** DE 9 of 15 are single-venue billings. The multi-venue ones (The
  Killer, Notre-Dame) are wrong *everywhere alike*, because they share the same copied facts.
- **Catalogue-id fill:** an untaken listing whose Cineamo/Webedia/Flicks id other listings take as one film gives
  **8 listings** in DE and 0 elsewhere. The ids already work.
- **Catalogue-id disagreement as a detector is useful.** One Webedia id taken as two films: DE "Emil und die
  Detektive" ×134 (1931 vs 2026), ES "Sentido y sensibilidad" ×83 (1995 vs 2026, with a judged-wrong DE sibling), and
  "La invitación" ×14. That is 236 listings where one catalogue id names two films, a cheap findings-report section.
  UK Spektrix collisions are an artefact: Spektrix ids are per venue.

### 3.4 Learning from the user's judgements more directly: yes, as a queue and a loop, not a new model

- **Ranking the unjudged uncertain listings** (untaken BelowThreshold/Vetoed film-kind, plus PooledMatch takes) by
  billing:
  - PL 226 listings / 97 billings; the top 50 cover 79%.
  - DE 1,016 / 650; the top 50 cover 39%.
  - UK 862 / 419; the top 50 cover 57%.
  - US and ES: the top 14 and the top 6 billings cover everything.
- **About 170 review answers settle ~1,300 listings**, roughly 7× the review page's whole history (270 answers).
- The 10-06 ML experiment showed that a learned model trained on today's takes only reproduces them: the 126
  hand-labelled clusters were the only independent test. More labels on the uncertain margin is what would let the
  refit and the cannot-link cuts move.
- **The export gap is real.** Several 10-18 answers never reached the TSV (wrong-matches inventory, "Reviews to
  re-check").

### 3.5 Structured billing parsing: prototyped as kinds only; see 4.1

### 3.6 Simplifying the rule zoo: a consequence of 4.1, plus an ablation pass (4.5)

## 4. Proposal, ranked

| # | change | expected impact | effort | risk |
|---|---|---|---|---|
| 1 | Relay matcher keyed on (house, work, season/date, encore) | +750 to 1,100 listings (US Met Carmen 514, UK/PL/ES relays) | M (1 wk) | low: a wrong season is the worst case, and a relay never takes a feature |
| 2 | One structured billing parse (`Billing`), consumed everywhere | +~600 strand/edition/bill listings over time; ends the ~85-commits-a-week title churn; deletes ~10 readers | L (2–3 wk, staged) | medium, held by an equivalence phase |
| 3 | Review queue + closed loop (rank by listings × uncertainty, auto-export, every answer a slice case) | measurement on ~1,300 listings now unjudged; feeds every other change | S (2–3 d) | low |
| 4 | Fact provenance: copied facts read as no facts; catalogue-id disagreement as a finding and a deny signal | ~8 of 31 wrong-served; 236 id-collision listings triaged | M | medium (kinoheld copies facts onto real films too) |
| 5 | Rule ablation and deletion budget | fewer rules, faster resolves; no take change by construction | S–M | low |

Not proposed: an event gate on title words (§3.1: 0 cures, 512 rights lost), hard runtime/date constraints (§3.2: 0
cures, ~110 rights lost), and cross-country pooling (measured 10-19).

### 4.1 Relay matcher (rank 1)

- **What.** `Billing.kind = Relay(house, work, season, encore, live)` routes the listing to a matcher that searches
  for and scores *season records*: the house's production of the work, from the season the screening dates fall in,
  or the encore's original date. These are scored on house, work and season equality. Learned runtime-gap and
  title-overlap vetoes are off for the class, because Met records carry placeholder runtimes (240 vs 166 for Carmen).
  When a relay has no record (Opéra de Paris Fanciulla, Hamlet, Bayadère), it shows as an *event* rather than "film
  not found". The composer banner ("Bizet: Carmen") reads as the work's credit, and "MetOpera", "Met:", "(MET)" and
  "OPERA-" read as one house.
- **Subsumes.**
  - `StageRelays`' house heuristics and `xtra-canonical-royal-ballet-opera` / `-met-opera` / `-nt-live`.
  - "(Cine)" and "BTS … LIVE VIEWING" title rules, `204a9ddf0` (Met season search), the panto year rule
    (`ffa09f907`), and `another-house-s-season-production`.
  - The stage-house namesake veto, `vetoed-not-released-in-gb-as-namesake` for relays, and `seasonsApart`.
- **Measure.**
  - `identity-measure` over 528, 38713274617 and the newest uk/us/es recording, with the same bar (0 new-wrong, 0
    right→wrong).
  - A per-kind recall row: relay listings taken / judged right.
  - The ~750 listings the US/ES agent named.
  - The UK `expected-hard-clusters`.
- **Plan.**
  1. Build the relay grammar (house aliases, season, encore) as a pure function with a spec per billing shape seen
     in the 5 mirrors (~200 distinct relay billings).
  2. Shadow it: log where the matcher's take differs from the resolver's.
  3. Switch relays over, country by country.

### 4.2 One structured billing parse (rank 2)

- **What.** `Billing(raw) → {banner/strand, works: [Work(title, year?, credit?, pieceRole)], joiner: single|bill|marathon|segment, version (extended, restored, anniversary n, re-release), presentation (format, language, certificate, accessibility, "with Wine", Q&A, pyjama party), kind (film|relay|bill|programme|mystery|live), venueTag}`.
  It is computed once per listing in intake, stored on the listing, and versioned like the title rules. The search
  builder, `IdentityMeasures` (title feature against `works`, not the raw string), acceptance (`billsSeveralWorks`,
  `andJoinsTwoWorks`), the agreement stage and `NonFilmEvents` read fields, never the raw string. Grammar data
  (strand lists, version words, joiners per language) lives in one table per country. New shapes are added there with
  a corpus fixture, not as a new regex in a new reader.
- **Subsumes.**
  - `TitleRules`/`ExtraTitleRules`' search tier, the strip parts of `TitleNormalizer`, `VersionTags`,
    `TitleDecorations`/`DecorationWords`, and `BilledPiece`/`MultiFilmBill`.
  - The bill predicates in `ListingShape`, `TitleCredit`, and `AcceptanceTitleRules.andJoinsTwoWorks`.
  - `FilmNess`' word lists and `NonMovieEventClassifier`'s title half.
  - Today's commits: numbered subtitles, version tags, prefixes, Peppa suffix, "and" bills, the studio tag, and the
    "(Cine)" reading.
- **Measure.**
  - **Phase A (equivalence).** Each reader is switched to the parse's field one at a time. The diff must be
    byte-identical over the five captures, as the 10-06 consolidation proved for `ListingShape`.
  - **Phase B.** Each grammar improvement is a normal resolver change against the bar. The scorecard gains a
    per-kind table (§1) that should shrink "film, untaken".
- **Plan.**
  1. Snapshot every distinct billing in the five mirrors (~120k) with today's readers' outputs as a golden file.
  2. Write `Billing` to reproduce them.
  3. Move readers over.
  4. Delete the old readers.

### 4.3 Review queue and closed loop (rank 3)

- **What.**
  - The review page's default queue ranks unjudged billings by (listings × 1 − confidence), across countries, with
    the relay and bill kinds shown.
  - Every answer is exported to the TSV on save, which removes the never-exported gap.
  - Every answer emits an `identity-slice --emit-case`, so a later change that breaks it fails a gate.
  - The weekly refit reads the new answers as hand labels with weight.
- **Subsumes.** Manual inventory passes like 2026-10-20's, and agents judging in bulk ("judged 10-1x" rows), which
  the inventory found disagreeing with the user's own reviews.
- **Measure.**
  - The fraction of served listings under a judgement, which is ~4% today.
  - Unjudged uncertain listings: 2,216 today.
  - The time from answer to gate.
- **Plan.**
  1. Rank by listings in the existing `ReviewController` queue.
  2. Make `LabelsExport` run on write.
  3. Add a CI lint that every `review_answers` row has a TSV row.

### 4.4 Fact provenance (rank 4)

- **What.**
  - A fact is credited only when its source is independent of the candidate. The year, runtime or director on a
    kinoheld, Filmstarts or Cineamo page *equal to the found record's* counts as copied (no evidence), unless the
    venue's own page states it.
  - A catalogue id that names two films across listings in a week becomes a finding and denies the minority film
    where the billing states nothing else.
- **Subsumes.** Several `factsFromCatalogue` special cases, the copied-original-title vetoes, and the
  Island/Regio Tales known-wrong rows.
- **Measure.**
  - The bar, plus the 31 wrong-served: about 8 should go to none or right.
  - Watch kinoheld's real-film takes for losses. The V3f history says kinoheld copies facts onto real films as well.
- **Plan.** Offline first, joining copied-equals-found per feed against the judgements, as in §3.3. Then the resolver.

### 4.5 Rule ablation and deletion budget (rank 5)

- **What.**
  - Run a leave-one-out ablation of every acceptance rule, veto and title rule over the recordings, as 10-05 did for
    the agreement. Delete those whose removal moves no take. After 4.1/4.2, re-ablate the ones those changes subsume.
  - Make a standing rule: a new rule must cure ≥N listings or be expressed as grammar data.
- **Measure.** By construction, 0 moves per deleted rule (`identity-measure`), and trace rule-name count (404 today).
- **Plan.** One scripted ablation run per rule family under the heavy-build lock, overnight.

## 5. Order

3 first (days, and it sharpens every later measurement), then 1 (largest recall, contained risk), then 2 in its
equivalence phase with 5 alongside, and 4 last.
