# Grouping coherence: a detector for wrong groupings (2026-11-01)

The owner, 2026-11-01: build a detector for WRONG GROUPINGS -- clusters that merge listings of different films -- on two
signals only, report-only. It changes no take and no resolver decision.

## The two signals (`services.identity.GroupingCoherence`)

1. **The members disagree on facts.** Within a cluster, the members' OWN stated facts conflict. They are read through the
   listing-listing measures (`IdentityMeasures.listingListing`), each listing's stated year (`statedYear`: its year, else
   its title-year marker) compared:
   - `year.delta` more than 1. A dated re-release the record carries (`Film.closestYear`) explains a year. So does an
     old film (10+ years) that one venue dates to the screening year (an undated re-release). An anniversary's year is
     no statement (the measure leaves it out). The weight is 1.0 when 5+ years apart, else 0.6.
   - `director` `different` / `different_script`. Two of the record's own credits are not a conflict (a co-direction
     each venue credits half of). Neither is a relay's or a stage work's credit. The weight is 1.0.
   - `runtime.delta` of at least max(30, a quarter of the longer). A short's programme (40 min or less), a relay, an
     event-worded billing and a slot around the record's cut (the shorter runs the record's cut, the longer is the
     film plus wine, a Q&A or adverts) are not conflicts. The weight is 0.5.
   A chain billing one title alike counts as one statement.
2. **A member would resolve differently on its own.** The resolver already scores each node alone in
   `FamilyScope.of(node)` / `takenAlone(node)`: its own evidence, no pooling, no cluster vote. Its trace now keeps two
   more things. `DecisionTrace.Alone` is the film the node's own rules take alone, with its confidence and rule.
   `DecisionTrace.Denied` is its own evidence's denial of the film taken. Both are filled in
   `ResolverDecisions.traceOf` and read only by this reader. No decision changes.
   - **prefers**: the member alone takes another film at 60% or more, and at least 20 points above its own score of
     the cluster's film (or the cluster's film is not in its pool). The weight is min(1, 0.4 + 0.7 x confidence), so
     it is flagged on its own from 86%.
   - **denies**: its own evidence denies the cluster's film. The weight is 1.0, or 0.4 for a denial by the
     probability cut alone.
   - A pinned cluster is never questioned.

   `SuspectTakes` asks whether a take fits its own listing. This asks whether the members fit each other and the
   film, so the two are complementary and nothing is duplicated. The corpus input is the same: `SuspectTakes.corpus`
   takes, each listing as its venue page states it.

**Score** = the heaviest finding of each kind, summed. **Flagged** at score >= 1.0: one strong finding, or two weaker
ones together.

## Where it runs

- **The identity takes check** (`scripts/ci/identity-takes-check.sh`, the convergence run's `takes` rows, i.e. the
  whole-corpus resolve). `integration.IdentityResolveDump` computes the coherence and writes it on each suspect
  cluster's first member line in `decisions-<cc>.jsonl`. `scripts/ci/grouping_suspects.py` then does four things:
  - ranks the clusters into `grouping-suspects-<cc>.json` and `.md` (the `identity-takes-<cc>` artifact);
  - appends the top 20 flagged to the step summary;
  - counts the members the hand judgements call right or wrong;
  - appends one `identity_suspect_groupings` metrics line, which `ci_trends` exports as
    `kind=identity.suspect_groupings` (fields in the twenty-runners schema table).

  It is report-only and never fails the check. A reused head resolve carries the field too, because it is in the
  stored dump. A row that resolves nothing (no identity code moved) reports nothing.
- **`/debug/review`** lists the flagged suspects of the country asked for above the queue, most suspect first. It
  reads them from `grouping-suspects-<cc>.json` in `KINOWO_GROUPING_SUSPECTS_DIR`, else `target/grouping-suspects`.
  The mirror's stored decisions keep no trace of a match (`DecisionTrace.kept`), so the page shows what the last whole
  resolve ranked.

## Calibration: the known groupings (`GroupingCoherenceCalibrationSpec`)

Each known-bad grouping is rebuilt as it stood: its listings as the investigations and the fix's specs hold them,
forced together under the film it took with a curator pin (`PinClaim.IsFilm`), then read as an own match. The
members' traces are therefore what the current resolver's lone-node scoring says of each member against the
historical film.

| known grouping | rebuilt from | flagged | by |
|---|---|---|---|
| UK Prima Facie: Chichester + NT Live + bare, under 963568 (NT recording) | relay-apart-from-film.md, RelayOwnFactsSpec | yes | alone: Chichester's own evidence (Susanna White in its text) prefers 1127282 / denies the recording |
| DE Eternity as prod grouped it: 4 Cineamo-linked bare [2025] + kept Caligari Freyne, under 1259102 | kept-overruled-apart.md, IdentityResolverCasesSpec | yes | denies: each linked listing's own evidence denies Freyne's film, its Cineamo link overruled by the sources backing Meures' |
| DE Eternity with a Meures listing in it (the "Freyne vs Meures" shape) | same | yes | director (Freyne / Meures) + denies |
| UK The Ladykillers: Nerve Centre + Keighley's 1955 under the 2004 remake | drift slice nerve-centre-...-039701b7 | yes | Keighley's 1955 / Mackendrick facts deny the remake |
| UK The Ladykillers as the drift flip stood: Nerve Centre ALONE under 5516 by its pooled match | same | **no (a miss by design)** | one listing is no grouping; its take is SuspectTakes' question |
| DE Peppa | the current corpus (below) | see below | |

Known good, none flagged in the spec: the same shapes resolved right. Prima Facie's bare billings and Chichester go
under the 2026 film. Eternity's Meures listings sit with the linked billings whose link they overrule. DE Karla has
a feed's copied 99' beside the 2025 film's stated facts. The Ladykillers at Nerve Centre and Keighley go under 1955.
Over the corpus, every flagged cluster whose judged members the hand judgements call right is a false positive. The
report counts them (`judged_right`).

## The first run over the current corpus

Measured with `scripts/identity-measure --all-countries --recording prod-latest`. The current corpus is prod-latest
41265286306.

- **Run 41346696928 (first thresholds).** Verdict LANDABLE: no take moved. Flagged: pl 7, uk 7, de 7, us 3, es 0.
  Most flags were false positives, and each now has a case in `GroupingCoherenceSpec`:
  - a Cyrillic credit and a co-director's typo read as disjoint directors;
  - a venue dating its local release or re-release to 2026 beside the record's year;
  - a title-year marker over a feed's year;
  - an edition's year ("Extended Edition" 2011);
  - two RBO relays' season years;
  - a slot or a slip in a runtime;
  - a venue crediting producers (UK "Forgotten Island").
- **Run 41348097395 (calibrated thresholds, commit ff2665231e2).** Judged below. Its ranked reports are in the
  `identity-takes-<cc>` artifacts (`head/grouping-suspects-<cc>.json`/`.md`).

Likely true positives among the first run's flags, for the owner to judge:

| country | cluster | why |
|---|---|---|
| de | tmdb:42236 Fellinis Casanova | Filmmuseum München's two pages: "il-casanova-di-federico-fellini", and "federico-fellini-werbefilmeintervista" (another programme, crediting Jeunet), grouped by title |
| de | tmdb:10835 The Killer (1989) | Bambi's kinoheld "the-killer-" [2024] beside Apollo Aachen's "Killer, The (1989)": Woo's 2024 remake or a wrong feed year |
| us | tmdb:26039 Point Blank (1967) | Drexel Columbus "Point Blank" [2010] (Cavayé's À bout portant?) beside Alamo's 31 "Point Blank (1967)" |
| pl | tmdb:1933 Inni (2001) | KINOMUZEUM "Inni" [2021] beside Cinema City's "Inni 25-lecie" [2001]: probably a wrong venue year |

Precision on the first run's flags, by inspection of their members and pages: about 4 of 24 are plausibly wrong
groupings. The calibration above removes about 15 of the 20 false positives. The calibrated run's precision (1 of 11 flagged) is judged below.

DE Peppa: the corpus splits "Festtagsspaß mit Peppa" across two TMDB records (1650896 ×344, 1778515 ×33). That is a
split, not a merge, and no member of either cluster prefers the other alone. The known wrong take, Cinefactory's
"Fun_with_Peppa", shows only as a weak cut-only denial (0.4) and is not flagged.

## Judged: run 41348097395

Judged against TMDB (web) and the `decisions-<cc>.jsonl` explanations (venue year, director, runtime per listing).
"Flagged" is score >= 1.0; "top" is the whole ranked list (at most 20; every country had fewer clusters).

| country | clusters | flagged | flagged really wrong | precision (flagged) | top: really wrong |
|---|---|---|---|---|---|
| pl | 10 | 1 | 0 | 0/1 | 0 |
| de | 18 | 4 | 1 (+1 unclear) | 1/4 | 1 (+ a few unclear) |
| us | 1 | 1 | 0 | 0/1 | 0 |
| uk | 9 | 5 | 0 | 0/5 | 0 |
| es | 3 | 0 | 0 | n/a | 0 |
| total | 41 | 11 | 1 | 1/11 | 1 |

Only one grouping is confirmed wrong, so the year signal is the noisy one: every false positive but one is a year
(or a credit) that a venue states differently from the record. The detector's value is the rare true positive, not
its flag precision.

**Really wrong**
- de tmdb:42236 Fellinis Casanova: Filmmuseum München lists two programmes under the one title; the second credits
  Jeunet and is not Fellini's Casanova (1976). Grouped by title.

**Unclear**
- de tmdb:1151422 Royal Opera House: Carmen (Marias Kino): TMDB holds the broadcast twice; the denial is about a
  duplicate record, not a wrong film.
- de tmdb:1419383 Die andere Seite (credits Minoguchi vs Mertens), tmdb:1167307 David (credits differ, Hirak vs
  Cunningham/Dawes), tmdb:1624471 Finding Connection (2023 vs 2026), tmdb:1648927 Frost ohne Schnee und Eis (2022 vs
  2026), tmdb:1755291 NAZA (PL, 2023 vs 2026), tmdb:1154730 La Gradiva (PL, 2023 vs 2026): a differing year or credit
  with nothing on the pages to settle it.
- de tmdb:1778515 Festtagsspaß mit Peppa (Cinefactory "Fun_with_Peppa", cut-only denial 0.4): the known wrong take.

**Not wrong** (the listing's year, credit or runtime is the venue's error or a different cut of the same film)
- pl Inni (1933, The Others 2001): KINOMUZEUM's [2021] listing has director Amenabar and matches runtime; a wrong
  venue year. Skarpetek 4, Hokus, The Room: A Tribute, Pasazer Andrzej Munk, Przepis na Swieta: dubbing or
  commentator credits, or runtimes. Everytime, The Social Reckoning: Kino Muzeum states 2023/2024 for several 2026
  films; its year is unreliable.
- us Point Blank (26039): Drexel's flicks.us listing [2010] credits John Boorman, the 1967 director, with a matching
  runtime. A wrong feed year (country mismatch), not the French 2010 film.
- de The Killer (10835): Bambi's kinoheld [2024] and Cinecitta against 5 venues with Woo and the 1989 runtime. The
  2024 is a wrong feed year (Woo's remake is not in cinemas); worth one look at the Bambi page if it returns.
  B-Movie Lust & Sound: the 1989 is parsed from the title's "1979-1989" range. Coyote vs. ACME (made 2023, released
  2026), Krankheit wie ein Gedicht, Amore und Basta, Mad Dog of Europe, Triegel trifft Cranach, Metropolis, Es,
  Bonhoeffer, Mockingjay: production vs release years and cuts.
- uk all 5 flagged: Doctor Who: The Satan Pit (2006 episode, 2026 re-release), NT Live: Golden Boy, The Polar Express
  (2004), The Dark Knight (2008), Pirates of the Caribbean (2003): a venue stating the re-release year (or the
  original year) against the record's. Also Forgotten Island (producer credits), Tampopo (1985 vs 1988), the Andre
  Rieu and Disney Junior runtimes.
- es: nothing flagged; the three clusters are a release-year gap and two runtime spreads.

**Follow-ups.** (1) Fix Fellinis Casanova: split the same-venue programmes that credit different directors.
(2) The year signal flags re-release years: a venue year equal to the current year, or to the record's year plus a
re-release, should weigh less (UK produced all 5 false flags). (3) Kino Muzeum, kinoheld (Bambi) and flicks.us
(Drexel) feed years are unreliable.

## CI cost

Per takes row, from the run's `timings.tsv` and the dump log:
- the in-JVM `GroupingCoherence.corpus` takes 0.1 s (es) to 3.4 s (us);
- `grouping_suspects.py` takes 0-2 s.

That is under 6 s per row, against a 13-108 s head resolve.
