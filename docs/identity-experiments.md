# Identity experiments: tried, measured, not landed

A record of resolver changes that were built and measured but did not land, so the next attempt starts from what was
learned instead of from scratch. Not landing is not a verdict on the idea: most of these failed on one cost a slightly
different approach could avoid, and each entry says what that cost was and which angle looked worth trying next.

Each entry: what it tried, how it was measured, the result, why it stopped, and where the code is. Code that left a
branch is kept as a local annotated tag `experiment/<name>` (`git show experiment/<name>`, `git log main..experiment/<name>`).
Add an entry whenever an experiment is dropped or parked; edit an entry in place when one is retried.
Whenever a fix touches the same listings, class, rule or stage as an entry here, go back through that entry first and
consider a new angle: a narrower scope, another signal, another stage, or what has changed on main since that could
avoid the cost that sank it. Try it, or say why not.

The trade rule they were judged by: zero new wrong takes; a right take may be lost only as a deliberate trade, up to
~100 per wrong one cured, each listed with evidence in `test/resources/fixtures/identity-judgements.tsv`.

## Measuring-tool caveat (read before trusting a "moved nothing")

`scripts/identity-measure-local` (and CI's measure) replays the final stage over the checked-in capture only, which
holds the clusters the model left unmatched when it was recorded. A take the head newly refuses at the model stage never
reaches that replay: a final-stage 0 there means not seen, not refilled. Variants B–D below were first misread as
"moving nothing served" for this reason. Count model-stage losses as candidate served losses.

<<<<<<< HEAD
## 2026-10-19 — margin over the runner-up as a REFUSAL (branch `experiment-margin-signal`, tag `experiment/margin-signal`, recording 38713274617, base `82de60cf0`)

The "next angle" of `experiment/margin-take` (its entry lives on that tag): refuse a take whose runner-up is close.

- **Tried:** `Acceptance.clearOfItsRunnerUp`, a filter in `takenBy`, so it covers the alone, pooled and season paths.
  It refuses a take when the best other eligible candidate is within R× its probability. In the exempt variants, a take
  `EvidenceWeights.favours` over that rival stands. The commits are points a–f: R 1.5/2/3, without and with the
  exemption.
- **Separation on current code first.** Source: the prod mirror's `identity_traces`, all five countries, 2026-10-19.
  The taken film's probability is not stored, so it was rebuilt as prior + evidence terms through the isotonic map. The
  traces were joined to the judgements with `label_of` (`target/margin/q1/sep.py`).
  - Unit: (country, title, film). 12,228 takes; 580 judged, 546 right and 34 wrong.
  - Ratio AUC 0.59, probability AUC 0.51. With 34 wrong, 5-fold cross-validation stays below 0.5 for both models. The
    training set's 0.69 → 0.75 does NOT hold.
  - Ratio < 2 covers 491 takes: 65 right, 4 wrong, 422 unjudged. By rule: favoured-calibrated 164, family 137,
    season-record 53, imdb-suggested 51.
- **Measured** with `scripts/identity-measure-local` (all five countries, base `82de60cf0`):

  | point | cured | right lost | unjudged lost | new wrong / right→wrong |
  |---|---|---|---|---|
  | e: R 2, exempt when favoured | 0 | 361 (model 354 + final 7) | 2,257 | 0 / 0 |
  | b: R 2, no exemption | 16 listings (ES "La maleta" ×14, UK "Notre-Dame de Paris" ×2) | 443 | 4,060 | 1 / 2 (DE "Live - Vortrag: Südafrika") |

  Both are NOT LANDABLE. Points a, c, d and f were not measured: R 2 already shows the shape of the result.
- **Where the refused takes came from** (e's lost listings, by prod's trace rule): favoured-calibrated 3,939,
  season-record 191, sole-result 105, family 32, imdb-suggested 27, pooled 12.
  - favoured-calibrated still loses under the exemption. The exemption weighs the rival with the highest probability,
    while the rule weighs the second-ranked one.
  - No weak rule concentrates the wrong takes. Scoping the refusal to the weak rules would cure nothing.
- **Examples** (verdicts by `scripts/label-lookup`). `identity-judge-cards` cannot card these: it cards only the
  capture's unmatched takes, and these are matched model takes.
  - UK "RBO Live - Tosca" → RBO 2026/27 Tosca: RIGHT, lost. The runner-up is another season's record of the same work.
  - UK "Dune: Part Three - See It First Preview": RIGHT, lost.
  - US/UK/ES "BTS WORLD TOUR 'ARIRANG' IN SÃO PAULO" ×2,089: unjudged and plainly right, lost. The runner-up is the
    Buenos Aires live viewing at the same probability. The city in the title decides, not the facts.
  - DE "Lalka (The Doll)" ×30 → 2026: unjudged and right (PL judges it so), lost.
- **Why it stopped:** a close runner-up is mostly a namesake from the same series (other cities, other seasons,
  remakes) that the title's words already tell apart. Probability does not see that, so the margin carries no new
  information on the takes the stack makes. Each wrong take cured costs ~30 right and ~250 unjudged.
- **Not tried:** margin as a weighted unified signal. On the 2026-10-08 training rows (`target/margin/q1/derive.py`),
  margin ≥ 2× fires on 26 wrong vs 12 right untaken contenders. The pinned
  `model.lean&count.leaning&title.namesIt&film.current` fires on 2 training clusters. Swapping `model.lean` for
  margin ≥ 2× or ≥ 5× fires on 2–3, all right: too thin to replace it or refit `LeanMargin`.
  `scripts/identity-refit` tunes neither the unified rules nor `LeanMargin`.
- **Code:** `experiment/margin-signal`, points a–f as commits; the tip is f, R 3 exempt.
=======
## 2026-10-19 — veto-only signals as logistic weights: release countries, certificate, poster, synopsis (branch `experiment-weighted-guards`, tag `experiment/weighted-guards`, recording 38713274617)

**Tried.** Promote four signals that today only veto or pin a take to weighted levels of the per-candidate model,
with every guard left as it is:

- `release.country`: released / unreleased in the venue's country ([[ReleaseVeto]]'s record).
- `certificate`: match / near / below / above, on the venue country's scale ([[Certificates.below]]'s ladder).
- Poster pHash: match ≤8 bits / near / far / contradicts (another candidate matches).
- Synopsis proper names against the TMDB overview: match ≥2 / near 1 / none / contradicts.

Release and certificate became `ListingFilmMeasures` slots (`IdentityMeasures.GuardMeasures`). They sit outside
`FactMeasures` and the own-evidence sums, so they weigh in the probability only, and the venue's certificate is
carried into `Evidence`. With no table, the control commit moved 0 takes over all five corpora. Each signal's table
was fitted the calibration's way (log-likelihood ratio, α 0.5, missing neutral) on every candidate of the five
recorded corpora: weak labels from today's takes, judgements overriding (`integration.IdentityGuardUnits`,
`scripts/ml-identity/weighted_guards.py`).

**Poster and synopsis cannot be per-candidate measures.**

- *Poster:* hashes exist only for the posters the agreement stage asks about. A node averages ~20 candidates × 8 TMDB
  posters, so hashing for every candidate means millions of image fetches. A hash costs 8 bytes to hold.
- *Synopsis:* overviews come from the whole TMDB document, one Mongo read per candidate. The recordings answer none (0%).
- Both were measured for confidence (B) on the captures only.

**Coverage.**

- Captures (3,547 hand-labelled units, 189 right): release 100%, certificate 5.2% (UK only), poster 17.7% (UK, PL),
  synopsis 8.1% (UK, PL).
- Recordings (403,854 candidate rows): release 100%, certificate 7.8% (match 2,917/11,741, below 0/584).

**(B) Confidence**, grouped 5-fold cross-validation over today's log-odds. The joint row is the levels refit by a
logistic beside today's score; the additive row is the calibration's own fit added to it.

| units | variant | AUC | log-loss |
|---|---|---|---|
| captures | today | 0.928 | 0.1341 |
| captures | + poster (joint) | 0.938 | 0.1223 |
| captures | + synopsis (joint) | 0.932 | 0.1255 |
| captures | + all four (joint) | 0.942 | 0.1148 |
| captures | + release / + certificate (joint) | 0.928 / 0.926 | 0.1340 / 0.1349 |
| recordings | today | 0.9975 | 0.01939 |
| recordings | + release, additive | 0.9968 | 0.02074 |
| recordings | + both, joint | 0.9976 | 0.01906 |

- *Recordings:* release added the calibration's way hurts, and pushes down 347 of 759 judged-right takes by more than
  0.3 log-odds. The joint refit gains next to nothing.
- *Captures:* poster and synopsis separate right from wrong where known. Poster contradicts 0/155 right, match 24/0;
  synopsis contradicts 0/126, match 25/4.

**(A) Takes against base** (identity-measure-local; model stage, final 0 throughout).

| variant | gained right | gained unjudged | right lost | new wrong | other |
|---|---|---|---|---|---|
| release (base b79c71a78) | 12 | 9 | 16 | 2 | 3 cures, 65 unjudged moves |
| certificate (base b79c71a78) | 10 (5 judged in 916c9835d) | 0 | 1 | 1 | 3 unjudged lost, 1 switch |
| both (base b79c71a78) | 17 | 15 | 17 | 1 | 3 cures |
| certificate-agrees: match +1.79, below −3.88 only (base 0c1a0ae3b) | 11 | 1 (Almost Famous, right) | 0 | 1 | 1 switch not accepted |

- *Release losses:* new and festival films TMDB dates no release of in the venue's country: UK "ODEON Pride Nights -
  We Will Find Happiness" ×14, DE "Rodychi", PL "Lalka" 2026, UK "Short Cuts (35mm)". Its gains are DE "The Killer" ×3
  (1989) and Notre-Dame ×2 cured.
- *Certificate-agrees gains:* Moana 2026, E.T., Doctor Zhivago, LOTR, Caravaggio, The Enforcer 1951.
- *Certificate-agrees new wrong:* Hyde Park's double bill "The Tiger Who Came for Tea and We're Going on a Bear Hunt" →
  The Tiger Who Came to Tea, 36.6% → 49%.
- *Certificate-agrees switch:* Ciné Lumière's "De Gaulle 1: Resistance" moves from 2026 to Resistance (2020).
- *Full certificate table only:* "near" and "above" cost "Home Alone with Wine" (18 at the venue against PG) and
  "Kissing Jessica Stein (25th Anniversary)".

**Why it stopped.**

- Release as a weight trades right for right, and double-counts what the veto and the country measure already read.
- Certificate agreement is the one real gain, 11 right with nothing lost. It stays blocked by one double-bill take
  and one switch.

**Retried with the bill guards (base 75a28ea41).** A certificate of a listing that `MultiFilmBill.billsTwoWorks` (the
`notADoubleBill` gate) or `MultiFilmBill.marker` names now reads `missing:bill`, pinned by `CertificatesSpec`. The
measure did not move:

- 11 right gained, 0 right lost.
- 1 unjudged gain: Kino KILN's "Almost Famous", which looks right.
- The same new wrong and the same switch.

None of the existing bill reads catches Hyde Park's title, which joins its two works with "and". title-trace shows
marker none and billsSeveral no, so the guard cannot reach that take. The switch comes from a learned listing-listing
cannot-link: it now keeps "De Gaulle 1: Resistance" apart from "De Gaulle Part 1: Résistance" [2026] {Baudry}, and the
listing pools to Resistance (2020). That reads as wrong, and it is not judged.

**Next angle:** recognise a bill that joins two works with "and" or "&" as a bill, a heuristic the existing guards lack.
Hold the certificate weight out of listing-listing cannot-links. Then re-measure the certificate-agrees variant; its
11 gains stand. Poster
and synopsis as WEIGHTS in the unified stage (`UnifiedEvidence`), where their answers already exist, rather than per
candidate.
>>>>>>> d24b32d1b (docs: record the weighted-guards experiment (release, certificate, poster, synopsis as weights))

## 2026-10-19 — a venue's KIND as a prior: its share of relays and events (branch `experiment-venue-kinds`, tag `experiment/venue-kinds`, base `619e02d91`)

### Hold an ordinary feature for an event- or relay-shaped billing at a venue of relays and events — not landed: dropped on offline evidence
- **Idea:** the film-ness entry's next angle — a per-venue profile computed once per corpus, to single out the gig and
  relay houses where a bare billing is rarely a film. Per venue, the share of its OTHER titles (leave-one-out by
  normalised title) that the existing classifiers read as a RELAY (`NonMovieEventClassifier` screened broadcast,
  `ListingShape.billsAHouse`) or a non-film EVENT (non-screening, live event); optionally V3f's event words
  (konzert, comedy, lesung, party, tour, …). The hold: at a venue of ≥10 other titles whose share is ≥ S, an ordinary
  feature (≥60 min, not a relay record) with no fact of the billing's own (year, runtime, director) is denied when
  the billing's cue fires — **A** its own kind is relay/event, **C** it names the record only by a piece (shares a
  word with the record's titles and says ≥2 more: "Fallen Angels by Noël Coward") or names a stage work.
- **Tried (offline only, no JVM measure):** `scripts/title-trace --batch` over every billing for the classifiers'
  reading (PL, UK, DE complete; US 67,928 of 92,267; ES not reached — the resumed batch stopped on a compile failure
  and was paused for memory), joined to per-country trace / fact / TMDB-record exports; labels
  `identity-judgements.tsv` via `label_of`. Code (on the tag): `scripts/ml-identity/venue_kinds_experiment.py` (`python3 -I … <repo>`
  from the data dir; `EVENTWORDS=1` adds V3f's event words). CBeebies Panto 2026 excluded from the counts below:
  main's `479d8ace6` fixes it.
- **Coverage:** venues (≥10 titles) whose other titles are ≥20 % relays: PL 20, UK 512, DE 160, US 135, ES 0;
  ≥20 % events: PL 3, UK 1, DE 5 — the event classifiers barely fire, so the profile is in effect a relay profile.
  The gig venues it was meant to find score low: Kaminwerk 6 %, Lagerhalle 9–10 %, Casablanca Zwickau 8 % (26 / 22 /
  18 % with V3f's event words, still below the festival houses: Moviemento 64 %, Kino Central 69 %, both near 0
  without them).
- **Separation:** on 3,831 judged current takes the venue's relay share ranks wrong over right with AUC 0.64 (4,001
  takes / 0.65 against main's judgements at `619e02d91`); the event share alone runs backwards (0.45).
- **The hold, simulated** (current takes, cure = judged wrong vetoed; right lost = judged right vetoed):

  | cue | S | cured | right lost | unjudged vetoed |
  |---|---|---|---|---|
  | C piece/stage alone (any venue) | 0 | 5 | 99 | 1,653 |
  | C | ≥0.10 | 2 | 29 | 672 |
  | C | ≥0.20 | 2 | 14 | 327 |
  | A the billing's own kind | ≥0.20 | 0 | 3 | 22 |

  - C ≥0.20 cures UK Theatr Gwaun «Romeo and Juliet» → 1968 and DE Trifthof «Gegen das Vergessen – Weilheim…» → 2015;
    it loses «Fallen Angels by Noël Coward» → the 2026 capture ×4 (Shrewsbury, Phoenix, Pershore, Wetherby), «Über
    Unterbiberger – Vom Woid in die Welt» ×3, Hilltop «Film Society:» prefixes ×2, POC-DOCS, a Welsh «Noson Film
    Glasurol: Carrie (1976)», Wolfhagen «Wir sind Juden aus Breslau», Kelkheim «Live - Vortrag:» travelogues ×2.
  - A ≥0.20 loses Haverhill «Hadestown: The Musical» and the two Kelkheim travelogues, cures nothing. With V3f's event
    words A ≥0.20 cures 4 (DE «Pornfilmfestival:» nights at Moviemento / Kino Central) for 5 right lost — a festival
    rule's job, not a venue prior's.
- **Why stopped:** the piece cue does the work and the venue share only thins it, never to the trade rule: at best 2
  cures for 14 rights, the rights being exactly the relays and programme prefixes a relay-heavy venue bills correctly.
  The event side, which was the point (gig venues), is invisible to the classifiers that exist.
- **Code:** `experiment/venue-kinds` — `VenueKinds` (profile + cues A and C), a `CorpusContext.venueKinds` hook and a
  `FamilyScope.deniedBeside` hold at `MinShare` 0.20. **Uncompiled and unmeasured**: written before the offline
  numbers sank it, never built nor run through identity-measure.
- **Next angle:** a venue prior needs an event classifier that sees gigs first (Kaminwerk, Lagerhalle); the piece
  cue alone is better aimed at a canonical tier ("X by <playwright>" names a play) than at a venue share.

## 2026-10-19 — agreement ACROSS countries (branch `experiment-cross-country`, tag `experiment/cross-country`, prod mirror of 2026-10-19 17:42Z)

### A bare listing adopts, or is denied by, another country's fact-backed take of the same title in the same week — not built (offline: almost nothing to cure)
- **Tried (offline only, no resolver change, no JVM):** captures and resolution are per country today; every pooling
  (listing clustering, `venues.corroborating`, ClusterVoting) stops at the border. Exported once per country from the
  local prod mirror (read-only, all five fresh): `identity_traces` (take, basis, rules, evidence, candidates),
  `identity_listings` (the billing's own originalTitle, releaseYear, runtime, director, showtime window) and
  `tmdb_films` (title, year, directors) — 175k listings. Labels: `identity-judgements.tsv` via `label_of`.
  Definitions: **bare** = the billing states no year (field or "(1926)" in the title) and no director (runtime ignored:
  UK/DE/ES/US state it on most billings); a take is **fact-backed** when a director the billing states is among the
  record's or its stated year is within ±1; **same week** = showtime windows overlapping within 3 days; the title key
  folds case, accents, brackets, format tokens and a leading article. Code:
  `scripts/ml-identity/cross_country_export.js` (mongosh) + `cross_country_experiment.py` (`python3 -I … <repo> <data>`).
- **Coverage:** 5,654 films taken, 1,249 in ≥2 countries (461 in ≥3; UK–US 681, DE–US 569, DE–UK 524 pairs).
  13,704 taken listings are bare; for 9,013 of them (1,017 country-films) the same film is fact-backed in another
  country. Judged among those: 578 right, **10 wrong** — so pooling by FILM (corroboration) would reinforce wrongs:
  DE «Fallen Angels by Noël Coward» ×5 on the 1995 Wong Kar-wai film (UK repertory houses back it by director),
  DE «Donnerstagskneipe» ×2 on «X» 2022 (PL), PL «Zniknięcie» on The Vanishing 1993 (US), UK «Romeo and Juliet» 1968
  (PL), DE «The Black Hole» 1979 (UK). Any pooling must key on the TITLE billed, never on the film taken.
- **Disagreement:** 707 title keys are billed in ≥2 countries in the same week; on 53 the countries take different
  films. Judged per listing pair in its own country: 3,620 both unjudged, 51 right|unjudged, 36 unjudged|right,
  6 wrong|unjudged, **5 wrong|right — all one title** (DE Fallen Angels by Noël Coward vs UK, the Coward relay).
- **Ceiling:** 272 judged-wrong current takes (42 distinct country-titles). 266 have NO fact-backed take of their
  title in another country that week: they are local events and bills (CBeebies Panto ×214, Royal Ballet: La
  Bayadère ×5, DE Kneipe/jazz/porn-festival nights, Kurzfilmprogramm). Only 6 have one: the 5 Fallen Angels and DE
  Bambi «The Killer» — and the DE «The Killer» class (4 venues) is not bare: its billings state 2024 + John Woo
  (copied feed facts), so a bare-only rule cannot reach it. Translated titles barely link: PL/DE billings carry an
  originalTitle on 14 % / 56 % of listings, and the PL ones that do are often a bill's piece (below).
- **The rule, simulated** (fired = bare listings whose take differs from the one donor film; listings whose donors
  disagree among themselves skipped, ~95–99):

  | variant | fired | cure (wrong→right/none) | gained right (none→right) | right lost | new wrong (judged) | unjudged moved |
  |---|---|---|---|---|---|---|
  | ADOPT (year or director donor, raw then original-title key) | 844 | 5 | 36 | 1 | 107 | 695 |
  | ADOPT-CAND (only a non-denied own candidate, else none) | 844 | 5 | 28 | 1 | 106 | 69 |
  | DENY (drop the take only) | 56 | 5 | – | 1 | 0 | 50 |
  | ADOPT-DIR (director-backed donors only) | 177 | 5 | 25 | 1 | 105 | 41 |
  | ADOPT-DIR-RAW (…and raw-title key only) | 72 | 5 | 25 | 1 | 1 | 40 |
  | DENY-DIR-RAW | 21 | 5 | – | 1 | 0 | 15 |

  - cure: DE Cineplex Leipzig/Dresden/Titania/Köln/Mannheim «Fallen Angels by Noël Coward» 1995 → the 2026 relay
    (donor UK, director Scott Ellis) — RIGHT, but it is ONE title.
  - gained right: PL Multikino ×16 «Fallen Angels by Noël Coward» none → the relay; UK «Resident Evil» ×4 (PL donor);
    UK «Hocus Pocus» → 1993 (US donor); DE «The Killer» ×2 → 1989 (US donor «The Killer (1989)»).
  - right lost: DE Metropolis «GIRLFRIENDS» 1978 (right) → the 2026 film (UK GFT «Girlfriends (2026)») — the
    remake/same-title class the idea was meant to cure, now caused by it.
  - new wrong: PL Helios ×104 «Zestaw 1/2 Maraton Halloween 26» → Clayface / Other Mommy (a marathon bill whose
    originalTitle names one piece; gone with the raw-title key); UK Quad «Obsession (PG)» → DE's 2025 «Obsession»;
    PL «Lalka (Dolly)» → DE's year-backed «Lalka» (gone with director-only).
  - unjudged moved (eyeballed, ADOPT-DIR): plausible — UK «Home Alone (Pyjama Party)» ×12 → 1990, «The Grinch
    (Pyjama Party)» ×6 2000 → 2018, DE «LA HAINE (30TH ANNIVERSARY)» → 1995, UK «Jak Zyc…» ×4; doubtful — UK «Fame
    (15)» 1980 → 2009 (the 15 certificate is the 1980 film's), DE «The Heat» 2013 → Heat 1995, UK «The Sacrifice»
    1986 → Sacrifice 2026, UK «Passenger» → The Passenger, UK «Dracula» ×3 → 1931, PL «Caravaggio. Arcydzieła…»
    (an exhibition doc) → Jarman's 1986. ADOPT's 645 fills are Peppa's festive special (UK/US ×605, DE donor by
    year) — probably right, but each country's resolver leaves it untaken today for its own reasons.
- **Why stopped:** the cure pool is one title (Fallen Angels, 5 DE listings; the 16 PL ones untaken). The wrong
  classes still served are local events and copied facts, which another country never bills or bills alike. The
  safest variant (DENY-DIR-RAW) is 5 cures for 1 judged right lost plus 15 unjudged drops; the adopting ones trade
  ~25 gained rights for remake swaps of the very kind the idea targets (Girlfriends, Fame, The Heat, The Sacrifice):
  a bare title in another country is the same bare-title problem, and a donor's director or year proves only ITS film.
- **Cost (if built anyway):** resolution, captures, recordings, the convergence legs, identity-slice/measure and the
  ratchet are all per country, each worker its own pod and Mongo database. A cross-country signal needs a shared
  store of fact-backed takes (title key × week → film, ~18k entries / 8.5k (country, key, film), ~2 MB — inside the
  memory budget) written by every worker and read by every other, or a second pass reading the other countries'
  takes. Either makes a country's take depend on others' state and timing (order effects, a take flipping when
  another country's scrape lands), and every hermetic leg and slice would need the foreign evidence filed into its
  recording/capture to stay reproducible. Not worth a resolver prototype on this evidence.
- **Next angle:** none for the take itself. Two by-products worth keeping: (1) the 5 DE Fallen Angels wrongs are
  better cured in-country (the billing "by Noël Coward" names a play — the relay rule's own signal); (2) as a
  REPORT, not a rule, the 53 disagreeing title keys per week are a cheap triage list for the Identity findings issue
  (a country disagreeing with another's director-backed take of the same billed title).

## 2026-10-19 — the boot's broadcast flip (UK "RBO Cinema Season 2026-27: Macbeth"), base `1a2e33add`

### Never wait on a year-only record no screening day's relay can be — parked
- **Seen:** the first projection after deploy 8b75b70 (2026-10-15 23:49) lost the Met's Macbeth (tmdb:1703622, judged
  right) at 90 UK venues, basis Vetoed; the next projection 40 s later took it back as Broadcast. Mechanism (read from the
  code): `AgreementStage.rereads` is the process's, so at every boot `undatedOf` makes `Broadcast.take` wait (`Left`) on
  each candidate record whose stored `local.release_date` is a bare year, until the re-read is filed
  (`noteFiled(recordReadId)`); the re-read often files no day (mirror: 29,430 year-only records, 10,568 re-fetched and
  still year-only, e.g. 1702769 local "2027", english "2027-02-03"), so the wait recurs at every boot.
- **Tried:** skip the wait for a record whose year no screening day's `RelayRunDays` reaches back to.
- **Result:** the pinned case (a 2018 "Royal Opera House: Macbeth" year-only) passed before the change too: that record
  does not fit the credited billing, so it never held the take. Which year-only record held prod's take is not known:
  the recording's model takes the Met's record itself (OwnMatch), so the slice cannot replay prod's Vetoed + Broadcast.
- **Next angle:** find why `readRecordAgain` re-files the localized partial without its day, or keep the re-read's
  outcome across boots (a `StoredVerdict`-like record per re-read film), or let a broadcast take stand as last decided while
  its re-read is outstanding (as `d006d2c3e` did for corrections). `IdentityBillingTakeOscillating` names it when it recurs.
- **Code:** `experiment/broadcast-airable-year`.

## 2026-10-19 — a venue's cast field naming none of a film's cast, as a NEGATIVE signal — LANDED (tag `experiment/cast-mismatch`, recording 38713274617, base `experiment/cast-tiebreak` 1009a8c83)

- **Rule** (`CastEvidence.mismatch`, a model-stage node denial in `FamilyScope.denialByNode`; never a weight): a candidate is
  denied when every listing of the node whose venue lists a cast FIELD of its own (`Listing.listedCast`: no synopsis, no
  kinoheld/kinoprogramm/Webedia/Cineamo copy; two names of two words or more, no one-word entry — a mononymous star, a
  puppet, a role in brackets) names none of its Latin-script top-billed TMDB cast, matched order aside ("Li Gong" is
  "Gong Li"); and no stated year (±1) or director agrees with it. Not read beside a stage work, relay, performance or
  bill of several works (`castIsTheFilms`), for an animated film or documentary (TMDB genres 16/99: voice casts,
  contributors), nor for a film of another original language than the venue country's (dubs, transliterations).
  Variant "veto alone" (no own-fact exemption) not measured: offline it costs the four takes below the exemption spares.
- **Offline, current takes** (1009a8c83's model + final takes joined to the recording's cast fields and TMDB credits):
  1,663 listings / 51 billings with zero overlap before guards, judged 0 wrong / 10 right (US Spirited Away). Almost all
  are right takes the guards above exist for: Ghibli English dubs (~1,400 listings), Korean/Chinese name order, Greek and
  Japanese-script TMDB casts, a doc's contributors (UK "The Sixty-Year Mission"), the Muppets' characters, mononyms
  (Jailer, Mercury). After the guards: 7 billings, 36 listings — ES "La maleta" ×14 → Objetos (2022) (wrong), US Met
  "Tosca (2026 Encore)" ×17 (relay, guarded), and singles all spared by their own director (Mercury, Jailer, "The Short
  Films of David Lynch", Muppets Take Manhattan) but UK "Mayhem 2026: The Whisper" (unjudged, no TMDB record of its cast).
- **Offline, every judged row** (judgements × recording listings, zero overlap, guards in): 523 wrong / 2 right listings
  (the right two ROH Carmen relays, guarded) — NT "Hay Fever" ×222 and other relays, "Fallen Angels by Noël Coward" →
  Coward 2026 ×187, PL "Lalka" → Has 1968 ×72, "It (1990)" → And the Sea Will Tell, Escape Room 2, Gremlins → Innerspace.
  None of these is a current take: the signal is mostly a guard against their return.
- **Measure** (identity-measure-local 1009a8c83 → b62db4ce2, all countries): ES model 14 cured (Ocine ×14 "La maleta"
  Objetos → Therón's 2026 film), 9 gained (Artesiete ×8 + Bahia Mar "LA MALETA" → 2026 film, judged right); 0 right lost,
  0 new wrong; DE, PL, UK, US and every final stage unmoved. LANDABLE locally.
- **LANDED** (rebased on `edb40f880`, genre ids on the record since 4c3f3d604): the animation/documentary guard reads
  `Film.hasGenre(16 | 99)`, and a record whose genres are NOT known (`genreBits` 0) is never vetoed. Re-measured
  edb40f880 → the landed commit: ES model 14 cured, 9 gained right, 0 lost, 0 new wrong, all else unmoved. The
  known-wrong "La maleta" → Objetos row it cures is a plain wrong judgement again. In prod the rule is inert until
  stored records carry genres and a cast (the TMDB record backfill: a record without `castPairs` or `genreBits` vetoes
  nothing). `Listing.listedCast` is inside listing equality (the model reads it): a venue editing its cast re-resolves
  the listing.

## 2026-10-19 — cast as a positive tie-break among namesakes (branch `experiment-cast-tiebreak`, tag `experiment/cast-tiebreak`, recording 38713274617, base `f48ab8001`)

Follows `experiment/cast-overlap` (coverage, not the rule, was the limit).

### Coverage first — LANDED in f48ab8001 (languages only)
- **Where cast was missing.** `TmdbNormalizer.minimal` has kept the top 10 cast since f1b4f51c6 (2026-10-06); prod's records
  filed before it hold none (lacking `credits.cast`: PL 17,483, UK 31,783, DE 42,064, ES 7,577, US 44,413 of the Local
  partials, mirror 10-19). The captures held a cast for 5–7 % of the films they asked because they were taken
  (10-13) through a `MemoLookups` that passed no cast on (fixed 5faaac6c8): every cast asked was noted unknown.
- `UnmatchedClustersFill --recording` now answers every cast a capture asked (and any its replay newly asks) and each
  record's `original_language` (now kept by `minimal` and on `IdentityMeasures.Film.originalLanguage`) off the
  recording's TMDB tree: PL 1,299, UK 3,478, ES 91, US 296 casts; DE asks none (its venue casts are kinoheld copies).
- **The refilled casts alone, under today's `cast.venue`** (`7e20550f2`, measured fc2288144 → 7e20550f2): UK final
  +6 — 5 gained unjudged (all right, judged below) and **1 NEW WRONG**: Electric Palace Hastings' "Straight Talk: A
  Tribute to Dolly Parton" → the 1992 film (the venue page credits Barnet Kellman and the film's runtime/year: a feed copy
  of the film the tribute is named after). So only the languages landed; the casts stay on this branch. The next
  re-capture (casts now pass through `MemoLookups`) and the prod cast backfill will bring that wrong take back unless
  the cast rule changes first.

### The tie-break — LANDED 2026-10-19 (with the refilled casts)
- `CastEvidence.tieBreak`: of two or more candidates (contenders and denied ones) the billing names alike
  (`Agreement.namesIt`), the one whose top-billed cast the venue's OWN text (`SourceReliability` Names, as before) names
  two whole names of, no other doing so; never a lone candidate; a rival named once or not at all is no evidence against
  it; nothing where a rival's cast is unknown, where any listing bills a director, or bills several works
  (`ListingShape.billsSeveral`). Still the pinned `cast.venue` fill rule (agreement stage only), never a fitted or pooled
  signal; the fill's guards (`bill.piece`, `stage.work`, …) apply as before.
- **Measure** (identity-measure-local, f48ab8001's tree fc2288144 → 1009a8c83, every country): model stage unmoved
  everywhere; UK final +6 gained, 0 lost, 0 right lost, 0 new wrong; DE, ES, PL, US unmoved. The Dolly Parton tribute is
  not taken (a director is billed). Gained, all judged right 10-19: Chapter Cardiff "Out of the Woods" ×2 (2025, Sal
  Bardo: Fisher, Kennedy, Davison named), Phoenix Leicester "Amazing Grace + Q&A" (Walters, Lewis) and "Premature +
  Writer/Director Q&A" (Rottger, Watson), Riverside Studios "Man Baby (… Preview + Q&A + Props Table)" (Ladwa,
  Fitzgerald), Riverside Theatre Woodbridge "Dracula + Q&A" → Hammer's 1958 film (Lee, Cushing; new against the old
  rule, which a remake's one shared name blocked).
- **(B) confidence/separation for listings billing no director:** none at the model stage — cast never enters the model;
  in the agreement stage it decides 6 no-director ties right, 0 wrong. Too few to refit anything on.
- **Not tried:** cast for a lone candidate (no tie); prod's served takes (needs the backfill); cast in the model's
  logistic (experiment A of `cast-overlap`: −22 right).
- **Next angle:** land the tie-break before (or with) the prod cast backfill and the next re-capture, which otherwise
  surface the Dolly Parton wrong take under today's rule.

## 2026-10-19 — cast overlap, beyond the pinned `cast.venue` (branch `experiment-cast-overlap`, tag `experiment/cast-overlap`, recording 38713274617, base `a07939cf6`)

### Coverage: why `cast.venue` gains 0
- **Venue side** (recording, the venue's OWN text — kinoheld / kinoprogramm pages and Webedia / Cineamo ids left out, as
  `namesAreVenues` does): a cast field on PL 23 %, UK 83 %, DE ~0 % (all 2,324 DE cast fields are kinoheld copies of
  kinoheld's own match, as often of a WRONG film as a right one: 41 wrong / 32 right judged), ES 72 %, US 96 % of listings;
  a synopsis on PL 41 %, UK 66 %, ES 97 %, US 1 %.
- **Film side:** the recording's TMDB bodies carry credits for ~97 % of films, but prod's store (`tmdb_films`,
  `local.credits.cast`) holds a cast for only 24 % (PL), 30 % (UK), 44 % (DE), 19 % (US) of records — filed before records
  kept it — and the captures for 5 % (PL 68/1,379), 7 % (UK 254/3,753), 0 (DE, ES, US).
- So `CastEvidence` ("any candidate's cast unknown → nothing") almost never fires where the unified rules were counted
  (the captures), nor in prod. Not covered by other rules — gated out by unknown casts.

### Separation on the judged set (`identity-judgements.tsv`, venue's own text, film cast non-empty)
| level | right | wrong |
|---|---|---|
| none | 66 | 72 |
| one | 31 | 6 |
| two+ | 83 | 12 |
The 12 wrong two+ are multi-film bills (Bear Hunt + Tiger Who Came to Tea, Gruffalo, LOTR triple), a tribute (Dolly
Parton) and a recut (Manhunter: The Final Cut): cast confirms the film is IN the bill, not that it is the bill.
Prose naming no cast is no evidence (prod takes, silver: log-LR −0.15); a cast FIELD naming none is (−3.29).
Joined to prod's traces (takes prod still makes; 102 units with text and cast, only 6 wrong): CV AUC of today's logit
0.705 → 0.781 with cast, log-loss 0.218 → 0.199 — too few wrong takes to trust. Silver candidate-level units (2,699 prod
takes corroborated by director + exact year, 2,701 rivals; cast field only): two+ 2,438 / 307 (+2.07), one 77 / 461
(−1.78: rivals share one actor — sequels, remakes), none 62 / 1,679 (−3.29). Nearly every such take is already decided
by its director: of the judged takes prod makes whose text names two+ of the cast, 1,053 of 1,147 listings also have
`director=same_person`; cast is new information mainly for the 849 prod takes billing no director (two+ 168, one 82,
none 269, unknown 330).

### A: a `cast` listing-film measure in the calibrated logistic — NOT LANDABLE
- `IdentityMeasures.castRelation`: `two_plus` / `one` / `none` beside a cast field, `two_plus` or `missing:prose` beside
  prose only, `missing:film` where TMDB credits nobody or the cast was not fetched; node listing side = union of its
  listings' `VenueNames` (a marker hash records "had a cast field"). Weights the silver log-LRs above, NOT refit by
  `scripts/identity-refit`: its units come from the captures, which hold casts for <7 % of candidates (0 in DE/ES/US),
  so a relearn has nothing to fit. Memory: a `FilmCast` per candidate record (~10 int arrays, ~300 B), nothing per listing.
- `identity-measure-local` (model stage; final stage unmoved, DE and PL unmoved): gained right 6 (UK 5, ES 1), plus
  unjudged ES "La maleta" ×23 (14 off Objetos 2022 onto Therón's 2026 film — reads as a cure) and UK Crescendo
  (L'Objet du délit), Stormy Waters (Remorques 1941), Out of the Woods ×2; LOST right 22 (UK 21: NT-style "Fallen Angels
  by Noël Coward" ×13, NAZA + Q&A ×8 via family moves; US Reel Rock 19 by `cast=none`), unjudged lost 25 (US Met
  "Tosca (2026 Encore)" ×17 by the 2018 record's cast); RIGHT→WRONG 3 (Fallen Angels → Lukas Dhont's "Coward" 2026).
- **Failure modes seen:** stage relays and concerts whose record lists another season's or no cast; a dub's voice cast
  (Spirited Away, Cars) naming none; pooled/family names spreading a rival's two+ across a cluster.

### B: relax `CastEvidence` (unknown-cast candidates left out) — moves nothing
Every country and stage 0 lost / 0 gained: the captures the final stage replays know almost no casts.

- **Next angle:** backfill `credits.cast` into prod's stored records and the captures first (coverage, not the rule, is
  the limit); then try cast only as a POSITIVE tie-break among same-title rivals with no director billed, never `none`
  as a penalty, and keep it out of pooled/family names.

## 2026-10-19 — take stability across scrapes: hysteresis and flip history (branch `experiment-take-stability`, base `b79c71a78`)

### Hold a long-held take against a switch (hysteresis), or flag a flipped take low-confidence — not landed
- **Tried:** (a) a listing that has held film X for ≥ H hours keeps it unless the switch comes with a resolver-closure
  change (reset), H ∈ {6, 24, 48} h, holding "no film" too or not; (b) a take whose listing flipped in the last 8 days
  (A→B→A reversal, ≥2 changes, or one change outside a deploy) as a low-confidence/suspect signal.
- **Measured:** not by `identity-measure`: its corpus is ONE recording resolved fresh, with no per-listing take
  history, so a prior on past takes has nothing to read there. Replayed instead over prod's own history: 8 days of
  `identity take change` lines (VictoriaLogs, 5,791 changes) and the mirror's current `identity_traces` (173,632
  listings), every take judged by `identity-judgements.tsv` (`label_of`). A change within 30 min of a build's first
  projection in its country counts as closure-driven ("deploy"), the rest steady-state. Scripts:
  `scripts/experiments/take-stability/` (`fetch.sh`, `analyse.py`, `signal.py`, `hyst.py`).
- **How often takes flip:** 4,383 listings changed in 8 days (DE 2.8 %, UK 4.0 %, US 1.8 %, PL 0.8 %, ES 0.5 %); 1,083
  changed ≥2 times, 971 reversed (A→B→A: DE 318, UK 621). 48 % of changes are steady-state (no deploy). Reversals are
  a few event billings at many venues (UK "CBeebies Panto 2026: Treasure Island" 211 changes, BTS "Arirang" live viewing
  166, Starlight Express 112, RBO Macbeth 52, LoL Worlds finals 63), mostly in changed-scope projections, median 10 h
  between flips; basis OwnMatch↔BelowThreshold/Vetoed.
- **Which side is right:** steady-state changes judged: right→wrong 209 vs wrong→right 181; gained wrong 288 vs gained
  right 59; lost right 88 vs lost wrong 86. Deploy changes are mostly cures (lost wrong 631, gained right 334,
  wrong→right 161, vs right lost 167, right→wrong 5). Steady churn is roughly a coin toss; closure changes cure.
- **(a) Hysteresis, billings whose switch it would block** (harm blocked = a right take kept or a wrong one kept out;
  cure blocked = a wrong take frozen or a right one kept out):

  | H | reset on closure change | hold "none" | harm blocked | cure blocked | unjudged |
  |---|---|---|---|---|---|
  | 6 h | no | no | 63 | 191 | 152 |
  | 24 h | no | no | 37 | 109 | 98 |
  | 24 h | yes | no | 17 | 5 | 84 |
  | 24 h | yes | yes | 21 | 15 | 158 |
  | 48 h | yes | no | 16 | 2 | 65 |

  Without the reset it freezes ~3 cures per harm prevented, every served known-wrong take too (9 billings served now,
  all held 8 days+). With the reset it still blocks data-driven cures (DE "The Killer" losing tmdb:970347 at 4 venues,
  "Das Boot - Director's Cut"), and the 84 unjudged billings it would freeze are unaudited.
- **(b) Flip history as confidence, current matched takes:** per LISTING it looks decisive — reversed 221 wrong / 169
  right (57 %) vs stable 46 / 2,992 (1.5 %) — but per BILLING (country, title, film) it is three billings (CBeebies
  Panto, DE "The Killer", PL "Zniknięcie" tmdb:1644): reversed 3 wrong / 18 right (14 %), any steady-state flip 7 / 30
  (19 %) vs stable 35 / 432 (7.5 %). ~2.5× lift on n=37, with the judgements themselves biased toward listings found by
  take-change hunts. It flags 0 of the 9 served known-wrong billings (all stable).
- **Why stopped:** the hysteresis prior trades cures for harm (no reset) or blocks little that the oscillation fixes
  per billing would not (with reset), adds per-listing state the hermetic gates cannot measure (the corpus has no take
  history, so CI would never see what it froze), and reverses the repo's stance that a resolve is a function of the
  current corpus. The flip flag is a weak, small-n signal.
- **Next angle:** treat a steady-state REVERSAL as a bug report, not a prior: alert on `TakeReversals` per billing
  (the gauge exists) and fix the two rules undoing each other (as with "The Ties That Bind Us"); 3 billings explain
  ~60 % of reversals. If a confidence feature is wanted, count reversals per billing across venues, not per listing.
- **Code:** `experiment/take-stability` (the replay scripts; no resolver change).

## 2026-10-19 — margin over the runner-up (recording 38713274617, base `a07939cf6`)

### Take a candidate below the rating cut that dominates its rivals ("dominant") — dropped

- **Tried:** a last alone rule, `dominant`: the best eligible candidate (priors lending) at probability ≥ floor F and ≥ R×
  its runner-up's (or with none), no closer record — below `ratingCut` (37.5 %). Constants, no flag.
- **Opportunity** (`scripts.MarginOpportunity`, the five captures, nodes still unmatched after the agreement stage):
  1,173 nodes have a best candidate under the cut. F ≥ 0.5 is above the cut, so the grid is F ∈ {0.2, 0.25, 0.3}:
  F 0.3 → 22 nodes at R 5 (16 at R 10) + 7 with no rival; F 0.25 → 75 + 15; F 0.2 → 80 (R 5) / 99 (R 2) + 16. The
  probabilities cluster on class plateaus (0.2966, 0.2942, 0.3674 …) and runner-ups sit at ~0.03, so R barely
  discriminates: almost every best candidate below the cut is ≥ 5× its runner-up (the `rivals` measure is already in
  the probability).
- **Measured** (`scripts/identity-measure-local`, all five countries; model + final stage; LOST = model lost + final
  right-lost):

  | point | gained | right | unjudged | NEW WRONG | right→wrong | lost |
  |---|---|---|---|---|---|---|
  | A F 0.30, R 5 | 183 model + 16 final | 29 | 45 | 125 (117 = UK "CBeebies Panto 2026: Treasure Island") | 3 | 6 |
  | B F 0.25, R 5 | 239 model + 17 final | 54 | 64 | 138 | 3 | 7 |
  | D = A + the billing is only the film's title | 31 model + 7 final | 18 | 17 | 3 | 2 | 1 |

  Every point NOT LANDABLE. The wrong ones are the predicted kinds: a broadcast panto taking the 1950 or 2027 namesake,
  MET relays taking a namesake film ("MET: Silent Night" → 2023), double bills ("The Tiger Who Came for Tea and …"),
  events ("Boarding on Insanity + Director's Q&A" → a 9-minute short), a stage work ("Manon" → 2007), and bare
  namesakes ("Unrest" → 2006, "Dernier été" → 2026, "Obsession (PG)"). Worse, the rule pre-empts better takes: an
  alone take keeps a node apart from its cluster's pooled or agreed film — DE "CONFIDENTE" 2025 → 2016 and "WHEN PIGS FLY"
  2026 → 1993 (both judged WRONG), UK "Andre Rieu: Let it Snow" → the 2019 film. The bare-billing guard (D) removes the
  event/relay/bill class but not the bare namesakes or the pre-emption.
- **Separation (does the margin help at all?)** On the 274 clusters of the unified training set
  (`identity-unified/training.tsv.gz`, 2026-10-08) whose top-logit candidate `identity-judgements.tsv` judges (168 right,
  106 wrong): AUC of the probability alone 0.72, of log(p1/p2) alone 0.76; 5-fold CV logistic AUC 0.69 → 0.75, log-loss
  0.624 → 0.582, Brier 0.214 → 0.196 with the ratio added. Ratio quantiles p10/p25/p50: right 1.6/8.3/24, wrong
  1.0/1.6/3.0. Below the cut the ratio does NOT separate (AUC 0.58, p1 0.48; 37 right vs 59 wrong), which is why the take
  rule fails. Among takes the stack makes (147 judged, 17 wrong) it does: ratio < 2 holds 5 of 17 wrong for 13 of 130
  right; over all 6,463 taken clusters ratio < 2 is 142 takes. The judged set is biased toward suspects; the training
  rows predate several resolver changes.
- **Why it stopped:** zero new wrong is unreachable — the wrong kinds below the cut look exactly like the right ones
  on probability and margin.
- **Next angle:** margin as a REFUSAL, not a take: refuse (or send to the agreement stage) a take whose runner-up is
  within 2× (`LeanMargin`-like) unless a fact of the listing's own favours it — 142 takes to weigh against ~5 wrong
  cured, within the trade rule if measured so on current code. It could also replace the pinned `leaning` conjunction
  in `identity-unified-rules.json` only after that measure.
- **Code:** `experiment/margin-take` (points A, B, C = F 0.2/R 2 unmeasured, D; `scripts.MarginOpportunity`).

## 2026-10-19 — a listing-level "is this a film at all?" signal (branch `experiment-film-ness`, recording 38713274617, base `b79c71a78`)

### Hold off a take whose billing reads as no film (film-ness) — not landed
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
- **Why not landed:** fitted round by round to one recording's listings (each refinement spared a loss it had just
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
  `ResolverAllocationBudgetSpec` ("a title dated by the year…": 35,177 B vs the 35,000 budget; main passes) — its
  three new lazy vals on `IdentityMeasures.Listing`. Re-run the measure on the next recording once GitHub is back.
- **Code:** `experiment/film-ness` (V1 `b03ef128a`, V2 `10610e9db`, V3f `a0429af20`).
- **Next angle:** a venue prior — the share of a venue's listings the model leaves unmatched or reads as events,
  computed once per corpus (it singles out the gig venues), as a condition beside V3f's test or Giorgia Fumo's bare
  "Name – Show"; and kinoheld's copied facts (runtime/year equal to the found record's) read as no facts.

## 2026-10-19 — a venue's history as a prior (branch `experiment-venue-history`, prod mirror of 2026-10-19)

### Score a candidate's film age against the venue's other takes ("Sparrows" 1926 at a silent house) — not landed
- **Tried (offline, no resolver change):** per venue, the age buckets (0–1, 2–4, 5–14, 15–34, 35+ years) of the films
  prod takes for its OTHER billings (leave-one-out by normalised raw title; Dirichlet-smoothed, α = 10, toward the
  country), as a log-likelihood ratio `log P(bucket | venue) / P(bucket | country)` beside the candidate's calibrated
  logit. Data: the local prod mirror's `identity_traces` (173k listings, 165k takes, 7,565 venues, every candidate with
  its probability and year) and `tmdb_films`; labels `identity-judgements.tsv` via `label_of`: 10,540 judged
  candidates of 5,748 listings (1,016 right), 452 judged current takes (348 right, 104 wrong). The documentary share
  was not built: `tmdb_films.record` holds no genres. "Earlier dates only" was not built: the mirror holds current
  screenings only. Code: `scripts/ml-identity/venue_profile_experiment.py`.
- **Result (B, confidence):** the prior alone barely ranks (candidate AUC 0.571); on current takes it runs BACKWARDS
  (AUC 0.376: the judged right takes are often old films at ordinary venues, the wrong ones current releases at
  ordinary venues). Logistic beside the logit, 5-fold grouped by title, country dummies: log-loss 0.2770 → 0.2705,
  Brier 0.0778 → 0.0763 — UK and DE better, PL and US worse. A fixed weight lowers the candidate ranking in every
  country (same-title listings AUC 0.891 → 0.851). LLR < −1 flags 6 wrong takes (The Killer 2024 ×4, The Black Hole,
  The Stranger 2025) and 3 right (It (2017) at US luxury chains, Pan Tadeusz 1999 at Bali-Kino).
- **Result (A, takes, proxy):** the argmax over non-denied candidates of the judged listings, logit + w·LLR: w = 0.1
  4 wrong→right / 2 right→wrong; 0.25: 7 / 5; 0.5: 9 / 10; 1.49 (the fitted ratio): 20 / 16. The losses are relays and
  plays whose current record is right at a venue of older films (Fallen Angels by Noël Coward 2026 → the 1995 film ×5,
  Met Opera Macbeth 2026 → 2014, Black Water 2025 → 2018, Norymberga 2025 → 2006). Every move but The Killer at
  KommKino is between candidates at ~3%, which no rule takes. Not built into the resolver or run through
  identity-measure: the proxy already moves right takes at every weight (zero new wrong is the bar), and a profile from
  the venue's own takes needs a second resolve pass or stored takes as input.
- **Same venue, same raw title, earlier take:** no use as a prior. Within one snapshot only 212 (venue, title) pairs
  bill twice (206 agree, none judged wrong); across time it is what `StoredFilmSlots`/`IdSeeding` already inherit, and
  a wrong take persists as itself (`expected-matches.tsv` only holds takes the ratchet called right, so it leaks the
  label).
- **Why stopped:** film age is the wrong axis. What a venue screens tells repertory from multiplex, but the hard cases
  are relays, plays and remakes where the CURRENT record is right at a repertory house (or vice versa), and
  `ReleaseVeto` and the copied-fact work already took The Killer's class.
- **Next angle:** a venue prior over KINDS, not ages (share of relays / events / non-film billings, the "Auf
  Augenhöhe" entry's next angle), used only to veto, never to choose; or the venue's catalogue-id coverage as a
  reliability prior.

## 2026-10-19 — language from format tokens, and genre (branch `experiment-language-genre`, recording 38713274617)

### Weigh the listing's format tokens against TMDB `original_language`, and its genres against TMDB genres — not built (offline: no gain)
- **Tried (offline only, no resolver code):** language constraints from the showtimes' `format` tokens and the title
  (PL NAP/DUB/LEK/ORG/napisy/dubbing → not `pl`; DE OmU/OV/OmdU → not `de`, OmeU → not `en`; UK/US Subtitled/SUB/DUB
  → not `en`, HINDI/TAMIL/… → that language; ES VOSE/VO/DOB → not `es`), levels match / contradicts / unknown against
  the candidate's TMDB `original_language`; listing genres mapped onto TMDB ids (documentary 99, animation 16, plus
  music, horror, comedy, family, sci-fi), levels agree / contradicts (venue says doc/anim, film neither) /
  contradicts:film (film doc/anim, venue's genres not) / neutral / unknown. Code: `scripts/ml-identity/language-genre/`
  (`run.sh`).
- **Data:** neither field is in the resolver's data. `TmdbNormalizer.minimal` keeps neither `original_language` nor
  `genres`, search hits keep id/title/original title/year/popularity only, the captures hold neither, and the prod
  mirror's `tmdb_films.record` holds no genres. The recordings' raw `/movie` bodies (and search results:
  `original_language`, `genre_ids`) do, so adding them needs NO new lookup: keep two fields in `minimal` (~8 bytes a
  film: a 2-letter code and a genre bitmask), refill the captures from a recording, and let the stored records gain them
  as they are re-fetched (or re-fetch). The listing side is free: formats and genres are already scraped (a byte and a
  bitmask per listing).
- **Coverage (listings carrying it):** language PL 5,645/10,593, DE 4,479/33,489, UK 3,359/36,748 (791 a named
  language), US 246/100,152, ES 4,319/6,705; genres PL 1,647, DE 6,043, UK 25,855, US 97,063, ES 6,210 (doc/anim:
  377 / 1,380 / 3,490 / 10,218 / 1,408). On the failure classes it is nearly absent: of the ratchet's (38420977752)
  31 WRONG takes, 2 carry a language contradiction (DE "CLOSE YOUR EYES" OmeU vs `en`, "The Tea Alchemist" OmeU), 0
  carry listing genres; the remake ("Sparrows" `xx`, "Hocus Pocus" `sk`) and translated-title ("BARWY OCHRONNE /
  CAMOUFLAGE" OmeU vs `uk`: a *match*) classes bill no token that tells the namesakes apart.
- **(B) confidence** (`identity-unified/training.tsv.gz`, 24,267 labelled contender rows, judgements override, grouped
  5-fold logistic, hand ×5): base AUC 0.9987 / hand AUC 0.9643, LL 0.1837, Brier 0.0355; +language 0.9987 / 0.9643,
  LL 0.1839, Brier 0.0354; +genre 0.9987 / 0.9640, LL 0.1849, Brier 0.0357; +both 0.9987 / 0.9645, LL 0.1846. Nothing.
  The calibration's own counts make language perverse: contenders at `contradicts` are 31 right : 64 wrong, ABOVE the
  prior (27 %), so a refit weighs a contradiction ~+0.3. Every one of today's 31 contradicted takes is right: German
  films billed OmU/OmeU (dialect, Swiss, English-subtitled screenings: "Mittagsstunde", "Der Himmel über Berlin"), Polish
  films with NAP/DUB (accessibility subtitles: "Ojczyzna", "Przytul mnie"), UK relays with SUB/Subtitled (NT Live, ROH).
  Genre contradictions on today's takes: 220, all right (venues list animated films under "Family", music biopics under
  "music").
- **(A) takes, simulated at the model stage** (best contender, accept at logit ≥ 0.27 ≈ the showRatings cut):
  language moves 1–2 clusters (right↔right); genre with its in-logit fit loses 78 right for 15 gained, 2 cured, 2 NEW
  WRONG; with calibration-style weights gains 50 (weak-labelled, mostly what later stages take already) for 4 right
  lost and 9 NEW WRONG (DE "München Mord: …" → "Der beste Papa der Welt", ES "Carmen (OPERA LIVE)" → Carmen 1921). Not
  landable; identity-measure not run, since the data is not plumbed and the offline fit leaves nothing to measure.
- **Why stopped:** neither signal separates right from wrong beyond what the model already reads; format tokens mean
  "subtitled screening", not "foreign original", too often; genres are too coarse and inconsistently listed. Memory
  would be cheap; the value is not there.
- **Next angle:** only OmeU-vs-`en` held up on the judged set (13 wrong : 3 right contradicted) — as a narrow agreement
  veto it would cure 2 WRONG + 2 UNJUDGED ("Pornfilmfestival: The more the merrier") for 1 right lost ("Shoshana"),
  but a listing's showtimes mix OmU and OmeU and English-language documentaries screen OmeU: measure it per showtime,
  and only after `original_language` is in `minimal`.

## 2026-10-19 — OmeU listing vetoes an English-original film (branch `experiment-omeu-veto`, tag `experiment/omeu-veto`, base `b6e058316`)

### Deny a candidate whose TMDB `original_language` is `en` when the listing's showtimes are (all / mostly) OmeU — not built (offline: loses far more than it cures)
- **Idea:** the `experiment/language-genre` study's one surviving form — "OmeU" (original, ENGLISH subtitles) implies
  the original is not English (13 wrong : 3 right contradicted among judgement rows) — as an agreement/unified veto,
  measured per showtime as that entry asked. Code: `scripts/ml-identity/omeu-veto/` (`run.sh`): prod's served DE takes
  from the local prod mirror (`screenings` → `movies.tmdbId`, 627 listings with an OmeU showtime: 570 all-OmeU, 12
  majority, 45 minority), each film's `original_language` from recording 38713274617's TMDB bodies, verdicts from
  `identity-judgements.tsv`.
- **All-OmeU listings taking an `en` film: 72** — judged 6 wrong (5 "Pornfilmfestival: …" at Moviemento / Kino Central,
  "The Killer" 2024 at Savoy), 2 right ("Shoshana", "LOVE & WAR"), 64 unjudged. By inspection the unjudged are almost
  all right: BTS "ARIRANG" concert relays (26 listings), "NAZA" (Palestinian doc, TMDB `en`, 9), "Bridget Jones's
  Diary", "Pride", "The Red Shoes" ×2, "Der Teufel trägt Prada 2", "Sonnenaufgang" (1927), "Mama", "Mārama", "Lady",
  "Black Burns Fast", jazz docs (Monk, Ornette, Milford Graves); only 2 wrong (AFRIKAMERA / LT KINO shorts → tmdb 25132
  "Shorts" 2009). German venues bill OmeU on English films (English subtitles for an international audience, or just
  "OV"). **Majority variant:** +5 listings, all NAZA (right). The sibling's cures ("CLOSE YOUR EYES", "The Tea
  Alchemist") are not among today's takes.
- **Guard tried:** exempt documentary/music films (TMDB 99 / 10402): 27 listings left — 5 judged wrong + 2 unjudged
  wrong vs Shoshana + ~19 unjudged right (Bridget Jones, Pride, Prada 2, Red Shoes, Mama, Mārama…). Still a net loss.
  TMDB's own `original_language` is also wrong on small films ("Barrio Triste" and "Narciso, film esperienza" read `en`
  though spoken es / it), so the signal is noisy on exactly the art-house titles OmeU marks.
- **Why stopped:** right losses outnumber cures ~8:1 (guarded ~3:1); no resolver code written, identity-measure-local
  not run. The cured wrongs have better-aimed fixes: "Pornfilmfestival:" is a festival programme prefix (a title rule,
  then the bare title's own match), "The Killer" a remake-vs-original choice (year/director evidence).

## 2026-10-19 — film ids in booking and detail URLs (investigation only; recording 38713274617 + prod mirror 10-19)

- **Asked:** do `Showtime.bookingUrl` / `filmUrl` carry film ids (IMDb/TMDB/Filmweb, or a chain's own) the resolver
  does not read yet, and would they settle takes — as `externalIds` (external) or as a listing-listing pooling key
  (chain-internal)?
- **Coverage (recording):** a booking URL on 96–99 % of listings everywhere (PL 96, UK 96, DE 98, ES 98, US 99); a
  detail URL PL 89, UK 96, DE 10, ES 37, US 100. Id-shaped URL pieces were classed per host and position: a *film* id is
  constant across a listing's showtimes and distinct between a venue's listings; most hosts' pieces are session ids.
- **External ids:** IMDb in URLs only at US Majestic Theater Wayne (3–10 listings, all already taken right); none of
  TMDB/Filmweb/EIDR anywhere. Detail pages linking IMDb: Kinoteka (read already), 2 Drafthouse, 1 Iluzjon Filmweb.
  **Settles 0 takes.**
- **Chain ids already captured** as `externalIds` (and must-linked by `ConstraintEdges.catalogueKey`): Multikino, Cinema
  City, Vue, Odeon, Cineworld/Everyman/Showcase box office, Flicks, Taquilla, Ocine, Artesiete, Alamo, Landmark.
  **Not captured:** Helios film id (PL, 1.8–2.1k listings, 52 venues), CinemaxX Vista HO code (DE ~1.4k, 31 venues),
  kinoheld film slug (DE ~3k, 103 venues), Marcus FeatureCode, Cinemark `m=`, Harkins HO (US ~0.3–11k), biletyna
  slug (PL). All stable: 99.4–100 % the same id for a (venue, title) between recordings 38666576506 and 38713274617.
- **Value as a pooling key — nil:** the listings sharing an uncaptured chain id bill the same title in all but a
  handful of groups (Helios 0 of 90 shared ids retitled, Marcus/Cinemark/Harkins 0, kinoheld 2 of 250), so title
  pooling already joins them. Against prod's takes (mirror identity_traces) the groups where the id disagrees with
  the takes are: CinemaxX 8 retitled (must-link would FILL "Panem-Mockingjay 1+2" and "Lord Of The Rings Triple"
  — double bill / marathon, judged WRONG for its 2025 take — with the single film: 12 listings of NEW WRONG; BTS
  "in Busan" under the Buenos Aires code at 3 venues: ambiguous, other chains bill Busan too), kinoheld "Emil und die
  Detektive" (one slug, 1931 and 2026 films, BOTH judged right: a must-link loses a right take). As a cannot-link
  ("different id → different film") it would split today's right pools: one TMDB film under 2+ ids in Helios 26,
  kinoheld 37, Cinemark 13 titles (format/event variants). Cures found: 0 standing (the CinemaxX "Sinn und
  Sinnlichkeit" 4584/1503762 split seen in the recording has since healed in prod).
- **Why stopped:** no prototype built — the estimated payoff is 0 cures against ≥12 new wrong (must-link) or dozens
  of split pools (cannot-link). Not worth an identity-measure run.
- **Code:** the probes only, `experiment/booking-ids` (`scripts/experiments/booking-ids/`: `slots.py` over a
  recording's corpus, `mirror.py` against a mirror export of cinema_scrapes + identity_traces, `stable.py` across two
  recordings, `detail.py` for one slot's disagreeing groups).
- **Next angle:** if CinemaxX ever matters, scope its HO code to CinemaxX-named venues (the disagreements come from
  partner venues booking through cinemaxx.de, e.g. Kino am Raschplatz) and title-gate it as a feed id
  (`SourceReliability.linkDecides` = false); the mirror's lean `cinema_scrapes` keeps few showtimes, so measure
  coverage on a recording, not the mirror.

## 2026-10-15/17 — wrong-match classes, round 2 (recording 38713274617, base `d0d3ffb1c` unless noted)

### Class 2 strict: a take on a bare title needs a fact of the listing's own (variant H)
- **Tried:** a take needs one fact the listing states itself — year, director, original title, a runtime that tells
  namesakes apart, feed id, season, stage house, or "current release" (screened within a year of a cinema release TMDB
  records, no namesake as recent); otherwise unmatched. H applies it in the model AND the database-agreement stage
  (a consensus on a bare title needs such a fact too).
- **Result:** 24 wrong cured (model 17: PL 3, DE 14; final 7: PL 1, DE 3, UK 3) for ~191 judged-right lost plus ~625
  unjudged takes lost — mostly right-looking bare titles ("NASZE KRÓLESTWO", an Asterix re-release, "Kicia Kocia"
  children's screenings).
- **Why stopped:** inside 100:1 on judged takes, but the 625 unjudged losses could not be evidenced; the user chose not
  to land it.
- **Variants:** B, C, D (model stage only) — misread as moving nothing served (see the caveat). E (family verdicts
  strict) and F (in a family's verdict a TMDB feed link vouches; family verdicts refuse uncorroborated takes): F first
  looked like 7 served cures for 56 right lost, but that measure had the blind spot above; it was dropped with H. G: an
  intermediate step to H.
- **Code:** `experiment/identity-class2-corroboration` (`780d92ec0`, variants G and H on `d0d3ffb1c`),
  `experiment/identity-class2-variant-f` (`b392bb63c`).
- **Next angle:** the cost is children's / re-release bare titles. A "current release" fact that also accepts a
  re-release or a children's matinee strand, or exempting titles with no namesake recent enough to compete, may keep
  most of the 625. A narrow form landed instead (refuse a bare take only against a better-evidenced namesake).

### Agreement short of the quorum needs a fact of the listing's own
- **Tried:** only the database-agreement stage: a consensus below the quorum on a bare title needs a listing fact.
- **Result:** 4 cured, 45 right lost. Short-quorum bare agreements are right ~9 times in 10, and the cures look exactly
  like the losses in this signal space.
- **Code:** `experiment/identity-agreement-quorum` (`7c3a33657`).
- **Next angle:** the 4 cures each had their own signal — Halstead's bare "Romeo and Juliet" is a relay on the RBO
  broadcast day (class 4); "Archiv Negativ: Turksib" is agreed on a record not titled "Turksib" (a title-guard gap).

### Count the venue's poster, cast and certificate in the agreement quorum (2026-10-19, recording 38713274617, base `b79c71a78`)
- **Tried:** `Agreement.weighed` reads the venue's own evidence on each film the families took (`AgreementTakes.venueEvidence`,
  not waited on, like the cast and synopsis): (1) a venue poster within `VoteBits` of the film's TMDB poster, no poster
  vetoing it, is one quorum vote; (2) the venue's text naming two whole names of its cast, no other candidate's, is one;
  (3) both; (4) a poster veto, another candidate's cast named, or the venue's certificate `StepsBelow` under TMDB's is one
  dissenting vote. Independence by provenance as the stage already draws it: a feed's poster shows nothing
  (`PosterEvidence.shows`), a feed catalogue's text names nobody (`venueNames`). Variant 5 prints every weighed film's
  support, quorum and venue evidence and applies nothing.
- **Result:** every variant moved NOTHING (`identity-measure-local`, 5 countries: model and final 0 lost / 0 changed / 0
  gained). Variant 5 over the capture's 702 weighed films: 133 sit one vote short, only 2 of those carry a venue poster
  (PL Goplana "Johnny", already taken as `Poster`; Garden Cinema "Last Train Home" 2009, still refused by a title guard)
  and none a cast vote; the poster/cast votes otherwise land on 12 agreements already at quorum (Candyman ×9,
  Caravaggio, Pianista, The Cremator: all judged RIGHT). Dissent fires on 9 films, 8 of them already short of the quorum
  (PL "Lalka" 1968 at three venues, UK "Resident Evil" 2002, "Fatherland" 1994 ×2, "Golden Boy" 2025 — each the losing
  namesake of a right take) and the ninth (PL Fenomen "Dyrygent" → The Conductor 1980) already poster-vetoed after the
  quorum. The certificate never fired.
- **Why stopped:** the stage after the quorum already spends this evidence: the poster vote and the fill take the
  one-short clusters, and the poster veto refuses what a poster dissent would. The one capture WRONG agreement (UK
  Beehive "Into The Shadows", an event) shows no poster or cast. The "pooled vote" wrongs (Notre-Dame de Paris ×2,
  Pornfilmfestival "The Dating Game") are model takes the quorum never weighs.
- **Confidence (B):** venue votes 12/12 right, dissent 9/9 on films not right — consistent, but every one where the
  outcome is already decided; no separation gained on the judged set.
- **Code:** `experiment/quorum-votes` (variants are `Agreement.QuorumVariant` commits 1–5 on the branch).
- **Next angle:** none here. Venue evidence would matter only for a model take's correction, where the quorum is not read.

### Refuse an uncorroborated take below a confidence threshold
- **Tried (offline, over H's classified moves):** wrong uncorroborated takes sat at 3–62%; refuse below a threshold.
- **Result:** 6 cured, all near 39%, with dozens of right takes in the same band. No separating threshold.
- **Code:** none (analysis only).

### Class 3: copied feed facts
- **Tried:** only facts their own source decides back a namesake past the release veto; a pin vouches and a house
  production corroborates; a current film headed in the billing stands against a current release.
- **Result:** cured no served wrong take, lost one right take. Cannot reach "The Killer": the feed's copied year and
  director agree with the film the feed picked, and nothing the venue states independently contradicts it.
- **Code:** `experiment/identity-wrong-classes` (`8c0c9d17e`; its first commits are the class-2 groundwork F builds on).
- **Retried 2026-10-18, LANDED for "The Killer"** (`ReleaseVeto.ofCurrent`): not the copied facts but the release
  TYPES TMDB records. Woo's 1989 film has a German cinema re-release dated 2026-10-29, the week the venues screen it; the
  2024 remake only a digital German release (Peacock). A namesake with no cinema release in the venue's country, not
  itself current, beside one released in its cinemas in the screening's season, is vetoed unless the venue's OWN facts
  back it. Local measure (recording 38713274617, base `7ae897d65`, DE): 2 cured (City-Kinos, KommKino → the 1989 film),
  4 bare Cineamo venues gained the 1989 film, and Filmladen Kassel's "Space Cadet" moved off Cineamo's 2024 Prime Video
  film to none — the venue screens Kid Koala's film ("Mission Sternenhimmel"), so a second cure. 0 right lost. A first
  all-country measure lost ES Serantes' "De repente" {Lewis Allen} [1954]: TMDB records old films' releases sparsely, so the
  vetoed namesake must date from the last five seasons (`StreamingEraYears`); after that, every country 0 right lost.
- **Next angle:** (The Stranger was cured 10-18 another way: see "Hold apart a join made only by a copied original title".)
  A runtime gap outweighing a matching copied year/director ("The Stranger" bills 95 min vs 122) — only
  "The Stranger" in the corpus then; Alpha (51 min off) was cured another way. Meets `FamilyScope`'s release-veto check,
  as does the separate `wmo-d-origin-once` work.

### Billed-director identity denial (landed, then replaced)
- **Tried:** deny a film whose director differs from one the title bills ("Alpha (dir. Ducournau)").
- **Result:** landed in `06432928e`; first measure tripped on "mit Regie-Besuch!" read as a credit and on
  different-script names (宮崎駿, Smaragdis); narrowed to "different" only.
- **Why replaced:** the user preferred reading the credit into the listing's director at intake (`TitleFacts`,
  `f7dbf688b`), where the ordinary director measure keeps Alpha right with no special rule.

### A dead AKA-rung change ("Ecrire la vie")
- **Tried:** an alternative-title rung change aimed at "Ecrire la vie".
- **Result:** moved nothing on the whole corpus; dropped uncommitted.

## 2026-10-15/18 — copied facts, origin once, short refusal, id clashes (recording 38713274617, base `f2f27813d` unless noted)

Measures by `identity-measure` (DE unless noted; the other countries moved nothing or only unjudged takes). Reports and
slice outputs: `~/.cache/kinowo-identity/handoff/2026-10-15-wrong-match-round2/item-{d-origin-once,e-copied-facts}/`.

### Refuse a short whose every fact is a feed's copy ("Island")
- **Tried:** DE "Island" (Augustinum, kinoheld) takes a 3-min short on kinoheld's copied director, runtime and year,
  and no feature titled "Island" is in the pool for the short rule to prefer. Refuse a short when every fact backing it
  is a feed's copy; a second commit also refused shorts named by a piece of a longer billing.
- **Result (run 38730798889):** Island cured; 57 unjudged takes lost, several clearly right (IMAX Speyer's 40-min
  documentaries, "Donald und der Vogel Strauß", "Dr. Dolittle und seine Tiere").
- **Why stopped:** a refusal rule is too blunt; led to weighing a copied fact less (next entry).
- **Code:** `experiment/identity-short-copied-facts` (`c04b18ac7`, its last two commits).
- **Next angle:** the losses are all shorts with no rival at all; refuse only where the billing's runtime (or the
  screening's slot length) is a feature's, so a short has to explain a 90-min slot.

### A copied fact weighs a share of its own (`ScopeModel.copied`, signal `<signal>@copied`)
- **Tried:** each copied year, director and runtime scores at a share of its weight; the refit gained a step for the
  share. Variants: both directions at 0.5 (A) and 0.25 (B); support only at 0.5 (C5) and 0.25 (C25).
- **Result:**

  | Variant | Run | Cured | Right lost | Right→wrong | Unjudged moves |
  |---|---|---|---|---|---|
  | A, 0.5 | 38807878048 | 0 | 2 | 5 | 18 |
  | B, 0.25 | 38807898963 | 2 | 3 | 5 | 36 |
  | C5, support 0.5 | 38812134524 | 1 | 6 | 5 | 21 |
  | C25, support 0.25 | 38812145695 | 4 (Alpha, local heroes ×2, ATMOS: Michael) | 9 | 5 | 36 |

  The refit kept nothing; its copied-share step moved 1 unjudged take (it measures only the unmatched-cluster captures,
  which hold no own-match take, so it never sees these cures or losses).
- **Why stopped:** every variant flips "Fallen Angels by Noël Coward" ×5 (Cineplex) to Wong Kar-wai's 1995 film:
  Cineamo's copied facts were the only thing picking the 2026 stage capture over the famous namesake. And no target
  was cured: Island rides an exact title at search rank 1 with no rival; The Killer has 6 corroborating venues and
  rank 1 on both sides; Black Hole did not move.
- **Code:** `experiment/identity-copied-share-a` (`17929b7aa`), `-b` (`b1070e660`), `-c5` (`437819a1e`), `-c25`
  (`a2e03d3c5`); combined with origin once: `-ad` (`6ada52193`), `-c25d` (`ed289fe5b`).
- **Next angle:** discount a copied fact only when a namesake competes and nothing the venue states itself agrees; or
  keep its weight but count copies from one feed once (next entry). Fallen Angels needs its "by Noël Coward" credit
  read as a stage-play fact at intake (`TitleFacts` now does such peels) before any discount is safe. Leads it
  surfaced, still unjudged: "Teenie Fasching"→Fasching (1939), "Dark New Wave Night"→New Wave, "Three for Silver"→Tea
  For Three, "VOODOO LOUNGE"→VooDoo (2017), "Kurzfilmprogramm"→Endlich Tacheles ×2.

### Count each origin of a listing's facts once
- **Tried:** a feed entry backs a film once however many venues it is copied onto (corroborating venues counted by
  origin, not by venue).
- **Result:** on base `1d6fba3ea`: 0 cured, DE 2 + US 2 right lost, 28 unjudged moves. Re-measured on main as run
  38807915210 with the 28 judged: ~22 right lost (Der kleine Prinz ×15, Do You Love Me ×3, Mittagsstunde — the venues'
  own Cineamo ids name those films) for 7 cures (local heroes „Preisverleihung" ×2; Royal Ballet: La Bayadère ×5, the
  2027 relay, not the 2018 recording; both since known-wrong `served`). Its final stage showed 0 moves only because of
  the caveat above; slices show those takes going to none. With copied shares: A+D (run 38808011724) loses 4 right,
  C25+D (run 38812172262) 41, both keeping the 5 right→wrong. Two refits (`item-d-origin-once/refit{1,2}`) kept nothing.
  Re-measured locally 10-17 on base `36b83b72d` (identity-measure-local, hermetic): DE 2 + UK 5 cured, DE 23 + US 2
  right lost — Der kleine Prinz (1972) ×23, Volcanoes 3D ×2.
- **Why stopped:** ~3 right lost per cure, and its targets (The Killer, Black Hole) not cured.
- **Code:** `experiment/identity-origin-once` (`d9249b8dd`, ported onto main; first built as `52af5ce06`).
- **Next angle:** the losses are venues whose own feed id names the film; let an own id stand outside the origin
  count, and count origins once only among venues with no id of their own. A full re-calibration on a prod snapshot
  was never run.
- **Retried 2026-10-18, LANDED** (`origin-pages`): only the venues reading one listings site's ENTRY PAGE (Flicks',
  a `p:` claim) count once; a feed's catalogue id stays each venue's own. Local measure on recording 38713274617,
  base `807e7f287`: UK 5 cured (Royal Ballet: La Bayadère → the 2018 recording, known-wrong `served`, struck), US 2
  right lost (Volcanoes 3D at two IMAX venues reading one Flicks page, its copied director the only fact: recorded
  `accepted-right-loss`), DE Der kleine Prinz untouched. The Killer and NT Live Hamlet were not among the moves.

### Hold apart a join made only by a copied original title whose link names another film ("The Black Hole")
- **Tried (10-18, branch `kw-copied-facts`):** a cannot-link (`copied-original-apart`) between two nodes that meet
  only through feed-copied original titles (no billed title or search form alike) when one node's TMDB-keyed link
  names a film the other does not hold: either another film the other already holds, or one the other's own facts
  deny.
- **Result (identity-measure-local, base `807e7f287`, recording 38713274617):** DE model 2 cured (CITY 46 "THE BLACK
  HOLE", Filmmuseum München "The Stranger"), 2 right lost (DFF "CONFIDENTE", Traum-Kino "TOGETHER APART") and 3
  unjudged lost that look right (Andreasstadel "Departures" → Nokan, Filmhauskino "ENOUGH IS ENOUGH" → Trop c'est trop,
  Kurbelkiste "Felix" → Felix - Ein Hase auf Weltreise). Every other country moved nothing.
- **Why stopped:** each loss is the "holds another film" half: a venue bills a film's English title and its operator
  linked a namesake record. Black Hole is the same pattern; only the recording's gap (508578m is absent) sets it apart.
  The "denies" half landed alone (`ConstraintEdges.copiedOriginalJoinsAnotherFilm`): measured on base `f1571ea7e`,
  all five countries, it cures The Stranger and moves nothing else (LANDABLE).
- **Code:** `experiment/identity-copied-original-apart-wide`.
- **Next angle:** Black Hole needs a signal that its link is right, such as a festival banner or the 2024 record being
  in the recording, not a rule against the copy.

### Hold two family records apart when they name another id in one database ("Romeo and Juliet") — narrowed form LANDED 10-18
- **Tried:** UK Empire Theatre Halstead "Romeo and Juliet" (Royal Ballet relay): Wikidata's undated record of the play
  (IMDb tt2428892) joins IMDb's 1968 Zeffirelli film by title alone. A record naming another id in the same database
  is another entry; the vote join reads the same `Agreement.joins`. A follow-up reads only IMDb and TMDB ids so, as
  Wikidata spells some sites' ids its own way.
- **Result (slices and the ratchet; no whole-corpus measure):** Romeo and Juliet cured; lost DE "Der Film 444 …"
  (accepted) and UK "Thanks for coming FFFL", and asked a UK capture fill.
- **Why stopped:** the UK right loss with no evidenced trade; the take stays known-wrong.
- **Code:** `experiment/identity-id-clash` (`4e406da84`; the unlanded commits are `a11b0633d` and `4e406da84`).
- **Retried 10-18 (branch kw-id-clash):** the clash in `apart` only where one record is bare (undated, uncredited):
  identity-measure-local on 38713274617 (base `807e7f287`) still lost UK FFFL and newly UK "NFTS Collective Visions of
  Pleasure: I.K.U". Traced: FFFL stood on the PICK join (Wikidata's bare tt36481185 beside IMDb's and RT's dated 2026
  Cavalier), I.K.U on the VOTE join (the model's bare vote for TMDB's Brazilian "IKÚ" beside Wikidata's dated 2001
  film), Romeo on the vote join the other way round (the bare TAKER, Wikidata's play, beside a dated vote). The record's
  Wikidata type is not in the capture (families are filed parsed records), so the type angle needs a WikiFamily change
  and a recapture. LANDED instead: `Agreement.votesAnotherEntry` — the model's vote does not join takers that ALL date
  and credit nothing when it names another IMDb or TMDB id. Measure (base `4d43b4341`, every country): 1 cure, 0 lost,
  0 new wrong, nothing else moved.
- **Next angle (if the pick-join form is wanted):** carry Wikidata's non-film classes (play, literary work) into its
  record so a pick can be held apart by type, not by ids every right-by-luck agreement like FFFL's also clashes on.

## 2026-10-13/18 — shorts, relays, certificate search (other sessions' branches, traced 10-18 from their transcripts)

### Strict short denial (class 5, strict form)
- **Tried:** a stricter refusal of shorts (`ShortFilms`, `FamilyScope`, `FeedLink`, `SuspectTakes`, title words).
- **Measured:** CI convergence run 38690448859 (measure/3696d942d), red after 36 min, read then as "not landable";
  recording, base and per-country numbers were not recorded.
- **Why stopped:** superseded by the narrower class-5 shorts rule that landed 10-15 (`d2f919535`, `d2557b009`,
  "take no short a billing opens on and goes on past with words of its own").
- **Code:** `experiment/identity-class-5-strict` (`3696d942d`).
- **Next angle:** measure it locally (identity-measure-local) on top of the landed rule to see what it adds beyond it;
  the original red never said which takes it cost.

### Relay pre-emption of the pooled rules: stage-production and the full form
- **Tried:** let a relay's dated record (stage-production) or its broadcast-season record pre-empt the pooled rules,
  as each already does for a listing alone (`e8844fd1c`, first as review-fixes `f4796e608`, 10-13); variants with
  stage-production only (`e52c49045`) and broadcast-season only.
- **Result:** the full form gained 134 right takes but lost Savoy Worksop's "MET:Manon": pooled stage-production read
  one member's stated year as the whole cluster's, split the Manon cluster off the Met's record, and gave Aldeburgh's
  Nutcracker the 2025/26 record. Broadcast-season only: UK 2 cured, 148 right gained, 0 lost on recording 38713274617 —
  that one LANDED 10-18 (`d20d61970`, `2882dab86`, `a4f9ec5c8`). No separate numbers for stage-production only.
- **Code:** `experiment/identity-relay-preempt-pooled-full` (`e8844fd1c`), `experiment/identity-relay-preempt-stage-only`
  (`e52c49045`).
- **Next angle:** pooled stage-production with the year read per member rather than per cluster (or only when every
  member that states a year agrees) would keep the Manon split from happening.

### Search a title without the certificate the format peel reads ("item 8")
- **Tried:** `SearchTitles.uncertified` strips the certificate by `FormatTags.CertificateNote` instead of its own
  "(15)"-only shape (first as review-fixes `a8c0bf422` / `1c9a52e07`, 10-13).
- **Result:** identity-measure-local on recording 38713274617, base `b36500129`: loses 8 right takes, Lighthouse Poole's
  "Family Cinema: <film> - Cert X" (Paddington, WALL·E, …); a re-run with the new queries answered live lost the same
  8, so it is no recording gap. Cures not recorded.
- **Why stopped:** the same-film join to the sibling listings breaks: at the base Poole's "Family Cinema: Paddington -
  Cert PG" pooled with another listing (2 listings, 96.3%), at the head it stands alone at 31.3%. Not the recording:
  the live-gaps run (`--live-gaps`, base `65a204e2b`, 10-18) lost the same 8 and gained 1 right (Bridport's André Rieu
  "Let It Snow"). The BBFC-lists half landed (`c5a960c69`, `d7fa5e95b`). Open item in
  `docs/design/identity-known-wrongs-and-todos.md`.
- **Code:** `experiment/identity-search-uncertified` (`dbf421550`; rebased as `item8-on-main`, `4a194b22d`).
- **Retried 2026-10-18, LANDED** (`14047976a`, beneath it `29abd98da`): the Poole losses were never the search.
  `ListingShape.billedBeside` read "Cert PG" as a piece billed beside the film, and only a recurring-banner exemption
  (the raw "… - Cert PG" spelling's "Cert PG" shape, which the uncertified search no longer made) had kept the take.
  `billedBeside` now peels the certificate first; local measure on recording 38713274617: 0 moves, the UK capture
  filled for the new queries (Snowman double bill). Lesson: a loss that survives `--live-gaps` is code, not data —
  read the head's refusal line (`identity-slice`) before blaming a recording.
- **Next angle:** since 10-18 the intake peel (TitleFacts) takes the certificate into the listing's age rating before
  search; re-check whether the family join still breaks once both sides of the join are peeled the same way.

### Search a house's relay with its screenings' season ("(ROH)" series)
- **Tried:** search a stage house's relay with its screenings' season beside a credit or running time (UK Picture House
  Uckfield's "(ROH)" series), 10-14.
- **Result:** never measured: UnmatchedClustersRatchetSpec red, because the new "<work> <season>" lookups are absent
  from the capture. Needs a recapture and re-record first.
- **Code:** `experiment/identity-relay-season-search` (`d6b06b981`, on three other unmerged commits of that session).
- **Next angle:** the year form of the same idea (search a relay's work with the year its title or banner dates it,
  branch `relay-search-year`, `2b1b1bab3`) measured LANDABLE with 0 moves on 10-18 and is being landed by its session;
  the season form is worth measuring once that is in and a recording holds the season queries.
- **Extended 10-18, LANDED** (`kw-romeo-followups`): a title that is the stage work ALONE, stating no year and no credit
  (a running time at most), is searched with its screenings' season too, and the broadcast join takes it on the record's
  air day. UK Empire Theatre Halstead's "Romeo and Juliet" [170′] (25 May 2027) now takes the RBO 2026/27 relay, and its
  "Tosca", "Swan Lake" and Riverside Woodbridge's two, all on their broadcast days. A first, wider form (any title
  billing a work) asked "Lord of the Rings 2026" of a Fellowship of the Ring screening; whole-title only, it does not.
  identity-measure-local `--live-gaps`, base `0a6079eb3`, all countries: UK final 5 gained right, PL model 2 gained right,
  0 lost, 0 new wrong; captures filled for the new queries.

## 2026-10-18 — a certificate awaiting classification as a new-release mark (recording 38713274617)

### Refuse a decades-older record for a billing whose certificate is pending ("18 CTBA", "Cert TBC")
- **Planned:** UK Watershed Bristol's "The Store (18 CTBA)" (agreed from wiki/rt/tmdb on tmdb:110777, Wiseman's 1983
  documentary) was known-wrong on the premise that a certificate "to be announced" marks a release still awaiting BBFC
  classification. The plan was an "awaiting classification" fact surfaced from `FormatTags.Peeled` into
  `TitleFacts.Facts` and the identity `Listing`, and a guard refusing a record decades older than the screenings.
- **Not built. The data refutes the premise.** "Awaiting classification" marks a title the BBFC never rated, and that
  includes old films no one released here. Watershed bills "18 CTBA" on every unclassified title. Its "Bristol Bad Film
  Club presents: Dial Code Santa Claus (18 CTBA)" takes the 1990 film rightly (prod mirror, OwnMatch). The Store itself
  is RIGHT: Watershed's "Wiseman of the Month" season has Titicut Follies (15), Juvenile Court (15) and La Danse (PG), each
  with the synopsis "The Four Wisemen: get 20% off ... 4 or more tickets". The Store is the fourth, screening once on
  7 Dec, and Wiseman's film is set in Neiman Marcus at Christmas. Re-judged right; the known-wrong row is struck. Of the
  99 UK prod traces billing TBC/CTBA, the model takes a pre-2024 record for only two. One is Dial Code Santa Claus
  (right). The other is Lanternhouse's "Andre Rieu: Let it Snow (TBC)" on Netflix's 2019 "Let It Snow" (tmdb:295151,
  PooledMatch, wrong, not yet judged for UK). A gap small enough to catch the Rieu take (7 years) would also refuse Dial
  Code Santa Claus and The Store.
- **Next angle:** none for a pending certificate on its own. The peel that reads "(18 CTBA)" off the title stands on its
  own merits. The Rieu take is better approached through the concert billing ("André Rieu", "Christmas Concert") than
  through the certificate.

## 2026-10-18 — facts in a venue page's prose (recording 38713274617, base `807e7f287`)

### Read a page's prose as facts at intake (Binti, Into The Shadows, Unrest)
- **Tried:** the TitleFacts angle on the synopsis: what a venue's description states in a structured-looking way
  ("Running Time: 2 Hours (Including Interval)", a director credited) read into the listing's own fields. Checked
  against the three known-wrong rows with `scripts/venue-replay` (the recording holds each synopsis) and the TMDB records.
- **Result:** no intake fact separates any of them. PL Janosik "edukacja MH: Binti": its bilety24 synopsis states no
  year, runtime, credit or two-word name — only plot ("Belgii", "Konga", "Eliasa"). UK Beehive "Into The Shadows":
  "2 Hours (Including Interval)" holds the 2009 documentary's 90 minutes (a 120′ slot with an interval), and a longer
  venue time vetoes nothing (`Agreement.runsShort`) by design. An interval as an event marker was scanned, not built:
  152 UK synopses bill one — nearly all relays taking their stage-production records rightly, and right film takes
  among the rest (Dementia Friendly screenings of "Tea For Two", "Holiday Inn", "Lullaby Of Broadway"; "The Odyssey
  (2026) [35mm]"; "It's Nice Up North + Q+A"; the Candyman double bill); carrying it to the agreement would need a new
  `Listing` field the checked-in captures do not hold (a recapture, GitHub suspended). Unrest's synopsis credits no
  director ("Harvard PhD student Jennifer Brea is struck down…"), so no intake credit reads her either.
- **What landed instead (Unrest):** not intake but the agreement's `billsAnothersDirector`: the venue's own synopsis
  naming WHOLE (`VenueNames.names`) the director of a namesake a family weighed, and no director of the agreed film,
  refuses the agreement as a billed surname already did. identity-measure-local: UK final 2 cured (Southsea "Unrest";
  Kiln "Somali Film Festival: Lifeline", judged right 10-13 but the synopsis is Ilhan Abdullahi's Canadian-Somali
  film, tmdb 1545459, not Alfuqaha's 2025 thriller — re-judged wrong), 0 right lost, every other stage and country 0.
- **Next angle:** Into The Shadows needs an event marker reaching the agreement (an interval with no relay, no film
  word, no double bill — measure it after a recapture holds the field); Binti needs the synopsis compared with each
  namesake's overview, which no signal reads.

## 2026-10-18 — a venue's synopsis against its namesakes' TMDB overviews (branch `kw-synopsis-signal`, recording 38713274617)

### Refuse an agreed or filled film whose namesake's description the synopsis names over its own — LANDED
- **Data:** the recordings hold every TMDB `/movie` body raw (both overviews: the localized one is often empty, the
  English one rarely); the store's minimal record and the captures held none, so the store now keeps an overview's
  capitalised-word stems (`overview_words`, hashes, never text), the captures an `overviews` section and each
  listing's `words`, filled from the recordings (`UnmatchedClustersFill --recording [--listing-words]`).
- **Signal:** `ProseWords` — the words a text capitalises, each as its first four letters folded and hashed, a venue's
  sentence-first word left out: names and places survive translation and inflection ("Belgii" is "Belgium", "Eliasa"
  "Elias"), so a Polish synopsis reads against an English overview; no stopword list, no language rule.
  `SynopsisEvidence.veto`: a namesake (the cluster's TMDB candidates, else its answered title searches) whose
  overview shares 2+ words with the synopsis that the taken film's overview, cast and directors do not, the taken film
  sharing none of its own; titles' words out. Read at take time like the poster veto, never waited on.
- **Offline first** (Python over the ratchet's 1,386 agreement takes): 105 have a synopsis and an overview; the veto
  fired on 3 — Binti (cure), NADZIEJA at Gołdap (judged right; its page is Sødahl's 2019 "Hope", tmdb 616561, word for
  word: re-judged wrong — a second cure) and CK Lublin's "Lalka" (right; Prus named only by the 2010 musical's
  overview) — which the taken film's cast in its own words spares (the capture's casts filled from the recording).
- **Measure** (identity-measure-local, every country): PL final 2 cured (Binti, NADZIEJA), 0 right lost, 0 new
  wrong; DE, ES, UK, US and every model stage unmoved (DE/ES/US captures got no overview: their agreement takes hold
  almost no venue synopsis, 2 DE and 1 US of 1,386). Memory: a listing holds ~44 bytes of words; intake +600 B per
  listing with a synopsis; a veto reads two overviews' hashes off the store, ~3 KB, nothing held.
- **Not cured, Into The Shadows (UK Beehive):** the Shadows tribute's synopsis shares nothing with the 2009 doc's
  overview, but no namesake's names it either. The absolute form ("names 5+ words, none of the taken film's") also
  refuses Wallace & Gromit (Nerve Centre), George Michael (The Arzner) and Rialto's SYMFONIA ZMYSŁÓW: 1 cure, 3 right
  lost on a threshold fitted to one row — not built.
- **Next angle:** the same veto on a MODEL take (`Correction`): Binti and NADZIEJA are model takes in the re-emitted rows
  (known-wrong `rows`); an event's synopsis with no film database word for Into The Shadows.

## 2026-10-18 — copied facts, finish round (branch `kw-copied-facts-2`, recording 38713274617, base `4713c2ea0`)

Whole-corpus local measure of the three landed changes together, every country: DE model 3 cured (The Killer ×2,
Filmladen Kassel "Space Cadet") and 4 right gained (The Killer at four bare Cineamo venues); DE final 2 cured (ATMOS:
Michael, Ganes); 0 right lost, 0 new wrong; ES, PL, UK, US nothing moved.

### A feed-linked fill against a current first hit its copied year denied ("ATMOS: Michael") — LANDED
- **Tried:** `family.feedLink` (a database family's take + the TMDB-keyed feed's link) does not fire where every listing's
  year is a feed's copy and that year denied the CURRENT film TMDB ranked first for a title naming it, for an older film:
  the family took the copy's film by the copied year, no second source.
- **Result:** first form (any first hit) also lost DE Gröben-Lichtspiele's "Camouflage OmU" (Cineamo and Filmweb on the
  2026 Ukrainian film, the 2016 namesake ranked first); narrowed to a current first hit and an older linked film, it
  cures ATMOS alone. The format-banner angle (Atmos beside a silent film) was not needed.

### A near-spelling copied original names no film for the agreement ("Ganes") — LANDED
- **Tried:** the agreement reads a listing whose original title is a feed's copy one edit from its billing
  (`Agreement.copiesItsSearchHit`) without that title when it asks whether the billing names the families' film. The
  brief's "more than one edit" angle could not reach Ganes ("Ganes"→"Ganges" is one edit); `IdentityMeasures.Listing.
  originalStandsApart` already reads such a copy as the feed's search hit at the model stage, the agreement did not.
- **Result:** Ganes cured, nothing else moved (no typo-rescued agreement in the corpus).

### A venue's own subtitle holds its billing off a film titled without it ("Auf Augenhöhe") — not landed
- **Tried:** two forms. (1) No title-segment must-link from a factless "Work - Subtitle" billing (3+ words beyond the
  work, no event or edition word, in none of the film's titles nor its tagline) to the cluster of the film titled "Work":
  the node still joined by the title family's vote (`same-film`). (2) The same test as a node-level denial in
  `FamilyScope` — cures Ostpassagen's "Auf Augenhöhe - Jugend, Macht und Kinderschutz".
- **Result (form 2, base `9a28edbd2`, every country):** DE 41 + PL 14 + UK 6 judged right lost and ~100 unjudged:
  real subtitles TMDB lacks ("Akte Grüninger - Die Geschichte eines stillen Helden", "Hurtigruten und Norwegen - Eine
  epische Reise") and venue decorations the event words miss ("LIEBE BRAUCHT KEINE FERIEN (Best of Cinema)", PL
  "Vincent. Legenda Oceanu", "/ napisy - Nasze Kino", UK "Fallen Angels by Noël Coward", "GIANT - The Play (15)").
- **Why stopped:** far past 100:1. The known-wrong row's reason is confirmed by measure.
- **Code:** `experiment/identity-own-subtitle-apart` (form 2 on top of form 1).
- **Next angle:** only the venue's other programme can tell the circus show apart (Ostpassagen bills theatre); a venue
  whose other listings are mostly non-film events, read as a prior, is the signal — no listing field carries it today.

## 2026-10-14/18 — withdrawn-vote rule (recording 38713274617)

### Withdraw a pooled vote that a billing of the title, taking another film, denies — superseded
- **Tried:** in `ResolverDecisions`, a film only the cluster's pooled vote reaches (no member's own match, no pin) is no
  take when a listing kept apart from the cluster by a `denies-film` edge takes another film itself, is billed as a
  whole as a piece of a member's billing, and its family denies the film (IL KINO "Ukrainian Film Festival Berlin:
  CAMOUFLAGE" pooling to the 2001 film).
- **Measured:** scoped DE selectors: cured 1, lost 0. Landed as 26e48897d; convergence 38656624268 lost PL "Lalka (ale
  to horror)" ×9 and UK "Sense and Sensibility" ×3 / "Home Alone (Pyjama Party)" ×6. Reverted in 22361d8bb.
- **Why it stopped:** whole-corpus losses the scoped selectors missed; then e40e9a4af (`Corroboration.refused`)
  cured Camouflage another way (IL KINO now takes 1735950 via Filled), and the slice case f772906e9 pinned turned real.
  Re-applied on main 10-18, the rule moves none of the title's 12 listings: nothing left for it to cure.
- **Next angle, if a pooled-vote namesake comes back:** gate the withdrawal on the listing carrying a fact of its own
  against the take, and never across a decorated programme edition ("(ale to horror)", "(Pyjama Party)") — the losses'
  shape. Measure the whole corpus first (`scripts/identity-measure-local`, full).
- **Code:** `experiment/withdrawn-vote` (8c666e431).

## 2026-10-18 — a bare title reached only by an alternative title (branch `kw-livegaps`, recording 38713274617, base `dd84b11f3`)

### Refuse an IMDb-suggested film a bare billing names only by an alternative title ("Patagonien") — narrowed form LANDED
- **Tried:** DE Kinopolis Freiberg's "Patagonien" (a travel lecture) took Bozzelli's 2023 "Patagonia" by `imdb-suggested`,
  `title=alternative` (TMDB files the film under "Patagonien" beside its own title), with no fact of the listing's own.
  Not class 2's "a take needs a fact of its own" (variant H, ~191 right + ~625 unjudged lost): only the IMDb rule, only a
  bare billing, only a film named by an alternative title. Three forms, `identity-measure-local`:

  | Variant | Refuses | Cured | Right lost | Unjudged lost |
  |---|---|---|---|---|
  | A | `suggestedOnly` (IMDb alone reached it, title not the film's own or original) | 0 | 0 | 8 (DE: Salò, A Soul Haunted By Painting, _UA_Stop-Zemlia…) |
  | B | `title=alternative`, TMDB's search does not rank the film | 1 | 8 | 8 (DE: Eine Seite des Wahnsinns, Tempel zur goldenen Halle, Wir, das Orchester…) |
  | C | B, and TMDB's search of the billing finds OTHER films | 1 | 0 | 0 (every country) |

  A missed Patagonien: the billing carries the film's alternative title, so it is not `suggestedOnly`. B's losses are
  arthouse German titles of foreign films, the rule's very purpose; each had a TMDB search finding nothing at all,
  where "Patagonien" ranks "Abenteuer Patagonien" and a dozen more.
- **Landed:** C, `Acceptance.alternativeAgainstTmdbsSearch` (AcceptanceSpec). Code of A and B:
  `experiment/identity-alternative-title-suggestion`.
- **Next angle:** the final stage was not seen (caveat above): a capture re-emitted after it shows whether the agreement
  refills "Patagonien" with the 2023 film.

## Remaining wrong takes when round 2 stopped (37 on `d0d3ffb1c`)

Copied feed facts 10 · title piece or banner taken as the title 7 · bare exact title, top hit 6 · pooled family vote 5 ·
fill on a current release 3 · database agreement on a bare title 3 · relay, double bill, other 3. A long tail: each
looked like it needed case-level evidence rather than a class rule.

## 2026-09-29/30 — resolve and seed CPU (performance, not takes)

Measured through the CI measure (`scripts/identity-measure`) with JFR, each against a same-batch base (runner noise is
±25%); decisions had to stay byte-identical. The speedups that landed are in git history.

### Cross-resolve, content-keyed LRU of node scores ("score cache")
- **Tried:** an LRU of node scores kept across resolves, keyed by content.
- **Result:** UK heap 53 → 319 MB, US 106 → 559 MB at ~6–7k entries, ~25% hit rate; UK got slower.
- **Why stopped:** venue churn moves family pools, so the keys change between resolves. Caching Films across regions
  is the same trap: cheapen the derivation instead. (The Lalka family is one work by design; don't split its keys to
  shrink it.) Do not retry as is.
- **Code:** branch `wip/score-memo-rejected` (`a8fdf8dfd`) — neither the branch nor the commit is in this clone, and
  there is no `experiment/*` tag.

### Parallel region take-up ("parallel-regions")
- **Tried:** taking up regions on two threads — redone safely 2026-09-30 with its own `ownPrefetch()` and a
  `RegionPool` of at most 2 in flight, admitted by `tools.HeapHeadroom.tenuredBelow(0.6)`. An earlier form
  (`cc3c66dae`) had been reverted (`709c3bdcc`) after a `held.clear` NPE and US OOM kills.
- **Result:** US replay decisions byte-identical; seed 12.4 s vs 12.7 s — no gain. JFR: both region threads on-CPU
  throughout, 14 monitor events, so not a lock; each region costs ~2× CPU when paired on the 4-hardware-thread runner.
- **Why stopped:** prod take-up (~250 s) runs while `k3s-worker-1` is at 7.9/8 cores with five workers booting, so a
  second thread only competes. Don't re-push without a measured wall-clock gain.
- **Code:** branch `perf/parallel-regions-scoped`, never pushed and not in this clone; no `experiment/*` tag.
- **Next angle:** the levers are boot CPU contention (stagger rollouts) and GC (prod UK was GC-bound: SerialGC full
  GCs 28/min, 0.72 of wall time — the live set during a full take-up), not scoring CPU.

### Memoising `searchGroups` / `Qualifiers.split`
- **Tried:** a memo for `searchGroups` / `Qualifiers.split`, which showed 9.4% / 6.8% of JFR samples.
- **Result:** PL flat, DE +30%; the hash-map overhead ≈ the work saved.
- **Why stopped:** a JFR share that does not turn into a measured delta is noise.
- **Code:** not kept; no `experiment/*` tag.

## 2026-10-06 — learning the matching rules: logistic regression and boosted trees on the pinned corpus

(Merged from the former `docs/design/identity-ml-experiment.md`; code kept in `scripts/ml-identity/`.)

An OFFLINE experiment. No production code changed. The question: can matching rules be derived by linear regression or
machine learning from the whole pinned corpus, and how good, fast and accurate would that be at the bar that matters,
ZERO wrong matches?

Code: `scripts/ml-identity/identity_ml_experiment.py` (the fits and the measurements) and
`worker/src/test/scala/scripts/IdentityMlScorerBench.scala` (an in-JVM scorer for the boosted trees, with
`IdentityMlScorerBenchSpec`). To rerun:

    python3 -m venv <env> && <env>/bin/pip install -r scripts/ml-identity/requirements.txt
    timeout 1800 <env>/bin/python scripts/ml-identity/identity_ml_experiment.py        # ~6 min; target/identity-ml/
    sbt "worker/Test/runMain scripts.IdentityMlScorerBench target/identity-ml/gbm-d2.json target/identity-ml/scores-d2.txt"

### Dataset

The dataset is the unified evidence model's checked-in contender rows,
`test/resources/fixtures/identity-unified/training.tsv.gz`, written by `integration.IdentityUnifiedDataset`
(identity-resolver.md §20.2). They reuse the resolver's own feature extraction (`UnifiedEvidence.contenders`), so the
features match what the shipped fit reads:

- 25,301 (cluster, contender film) rows, 6,763 clusters across all five recorded full corpora (PL, UK, DE, ES, US), and 53 features.
- **Resolver features (14):** `model.logit` (the calibrated, monotone-refitted logistic over the `IdentityMeasures`
  tables: title, original title, year/title-year/season deltas, director, runtime, country, search rank, rivals,
  popularity, venues corroborating), `model.unscored`, `model.deniedBySome` (the learned cannot-links), the acceptance
  rules that fired (`rule.*`), the model's lean and best candidate, and the season/house productions.
- **Agreement signals (39):** family takes and leans (IMDb, Wikidata, Filmweb, Metacritic, RT), turned down, dissent, the
  listing's facts, runtime and contradictions, `venues.current`, the title guards, bills, stage work, the poster
  match/near/veto/vote, quorum, broadcast, catalogue, and the counts and conjunctions behind the quorum.
- **Labels:** 1,606 HAND rows (from `labels.tsv`, judged per listing; every other contender of a cluster whose right film
  a label names is wrong) and 22,643 WEAK rows (today's model take on a matched cluster that no label judges, with its
  other contenders as wrong). 1,052 rows are unlabelled and kept out of training. 6,540 clusters have a labelled-right
  contender, 126 of them hand-labelled.
- `expected-matches.tsv` is the ratchet's list of today's takes, not an independent label, and the 08-06-2026 e2e
  corpus's tmdbIds are the pipeline's own output. Neither adds ground truth, so neither is used as a label.

**The caveat that shapes every number below:** weak labels ARE today's takes. On 6,414 of the 6,540 positive clusters a
model can only reproduce today's stack, never beat it, and every feature that encodes the model's own rules
(`rule.calibrated`, `model.logit`) leaks the weak label. The 126 hand-labelled clusters are the only independent test.

### Method

- **Decision:** per cluster, take the best-scoring contender when its score clears a cut. A take is right or wrong by the
  contender's label.
- **Hold-outs:**
  - Grouped 5-fold cross-validation. Groups are the connected components of clusters joined by any film they hold a
    positive label for, so neither a cluster nor its right film sits on both sides (5,045 groups, the largest 398 rows).
  - Leave-one-country-out.
- **Cuts:**
  - *Oracle*: the cut is picked on the hold-out's own scores. This is an upper bound.
  - *Honest*: an inner grouped 4-fold cross-validation on the training part picks the cut (just above its highest
    counted-wrong take), and that cut is applied to the hold-out. Every take is counted in the hold-out fold only.
- **Zero-wrong bars:**
  - `strict`: any wrong take.
  - `new`: a wrong take that today's stack does not already make. This is the shipped fit's own bar.
  - `fill`: only clusters today's stack takes nothing in. Augmenting, never overriding: 260 clusters, 51 with a right film.
- **Training:** hand rows weighted ×5. Logistic: sklearn L2 (C=1, standardised). GBM: LightGBM, 300 rounds, 15
  leaves, learning rate 0.05. Monotone GBM: each signal held to `UnifiedEvidence.Signals`' direction.

### Results (grouped cross-validation, every take counted in the hold-out)

Today's stack on the same clusters: 6,489 / 6,540 right (99.2%) with **4 wrong**. These are hand-labelled model takes:
DE "Die Unbeugsamen", US "NT Live: All My Sons", PL "Ktoś całkiem obcy" and PL "Teksańska masakra piłą mechaniczną".
On the hand-labelled clusters it takes 75 of 126.

| model | strict recall@0 (oracle / honest) | new-wrong recall@0, honest (gained / lost vs today) | strict recall@1 / @3, honest | fill @0, honest (of 51) | AUC all / hand | train (fit) | score / cluster |
|---|---|---|---|---|---|---|---|
| (a) today's resolver + agreement | — (4 wrong) | 99.2% | — | — | — | — | — |
| (b) logistic, resolver features | 46.6% / 40.6% (1 wrong) | **98.2%**, 0 new wrong (+2 / −69) | 65.4% / 98.1% | 2 right, 0 wrong | 0.9988 / 0.909 | 0.06 s | 0.25 µs |
| (c) GBM, resolver features | 0.2% / 18.4% (1 wrong) | 96.7%, 0 new wrong (+0 / −164) | 79.0% / 95.9% | 0 | 0.9986 / 0.907 | 0.9 s | 7.8 µs |
| (d1) logistic, + agreement | 2.9% / 2.6% (1 wrong) | 3.5%, 0 new wrong | 7.2% / 29.9% | 1 right, 1 wrong | 0.9986 / 0.942 | 0.16 s | 0.6 µs |
| (d2) GBM, + agreement | 0.1% / 19.5% (2 wrong) | 95.9%, **2 new wrong** (+4 / −221) | 95.9% / 98.1% | 4 right, 2 wrong | **0.9996 / 0.978** | 2.2 s | 8.4 µs (JVM 18 µs) |
| (e) monotone GBM, + agreement | 1.0% / 18.7% (1 wrong) | 97.7%, 1 new wrong (+2 / −101) | 90.1% / 98.4% | 2 right, 1 wrong | 0.9996 / 0.982 | 0.5 s | 5.5 µs (JVM 20 µs) |

On the hand-labelled clusters at the honest new-wrong@0 cut, against today's 75 / 126 right:

| model | right / 126 | new wrong |
|---|---|---|
| (b) | 12 | 0 |
| (d1) | 28 | 0 |
| (d2) | 50 | 2 |
| (e) | 59 | 1 |

On every hold-out the learned models take fewer right films than today's stack and lose more than they gain.

**Leave-one-country-out** (honest cut from the other four countries):

- The strict zero-wrong cut does not transfer. Both GBMs take 0–37 of 873 in PL and 0–3 of 1,474 in UK; logistic (b)
  takes 19–61%.
- At the new-wrong bar (b) keeps 93–99% per country, but still adds 2 new wrong in PL and 2 in UK.

#### Why strict zero-wrong collapses

Every model scores today's four wrong takes at ≥ 0.99, which is as high as clean right takes. Each of them is a take
the model's own rules fired on, with the same feature pattern as thousands of right ones. A cut above all four leaves
almost nothing. The features cannot separate those cases, so no learner can. That is a missing signal, not a
learnable rule. They were caught by a poster and by hand: `model-take-audit.tsv`.

#### Calibration (reliability, held-out labelled rows)

| model | predicted | observed |
|---|---|---|
| (b) | 0.9–0.99 bucket, mean 0.985 | 0.995 |
| (b) | 0.5–0.7 bucket, mean 0.63 | 0.50 |
| (c) | 0.1–0.3 bucket, mean 0.17 | 0.07 |
| (d2) | ≥ 0.999 bucket, mean 1.000 (5,573 rows) | 1.000 |
| (e) | ≥ 0.999 bucket, mean 1.000 (3,354 rows) | 1.000 |
| (e) | 0.3–0.5 bucket, mean 0.36 | 0.62 (13 rows) |

- Logistic (b) is well calibrated at the top and over-confident in the middle.
- GBM (c) is over-confident at the low end.
- The middle buckets hold 10–60 rows each. With so few rows a probability there means little, and the zero-wrong cut
  never sits there.
- The full tables are in `results.json`.

#### The wrong takes just below the zero-wrong cut, by name

- **(d2), (e):**
  - PL "Lalka" → Lalka (1968)
  - PL "Akademia Polskiego Filmu: W kręgu kina…" → Strachy (1938), a programme banner
  - PL "Obcy w domu" → 1989, another film's own title
  - PL "Filmowe popołudnie dla dzieci: Jesień" → Sonbahar (2008)
  - PL "Zamki na piasku" → Sandcastles (1972)
  - PL "LALKA / DOLLY" → Lalka (2026)
  - PL "Manon" → 1949, a stage work
  - DE "André Rieus Weihnachtskonzert 2026" → Tage wie diese
- **(b):**
  - UK "Royal Ballet and Opera: Tosca" → the 2025/26 record
  - PL "Silent Night" → Wśród nocnej ciszy
  - UK "Royal Opera House: Carmen", "The Royal Ballet: The Nutcracker"
  - UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea", a double bill
- **(d1):** PL "Fallen Angels by Noël Coward" → Tchórz (2026).

Each of these is a case one of the hand-written guards or thresholds refuses today: `stage.work`, `bill.several`,
`title.anothersOwn`, the quorum and its rise per dissenting family, and the programme banners. They are the same takes
§20.4 to §20.6 found for the unified logistic model.

#### What it would gain

- The right takes today misses, at a zero-new-wrong cut:
  - DE "Die Nibelungen - Teil 1: Siegfried" (b)
  - PL "HER Docs - To nie jest świat dla ludzi z…" → Mysteriet om Menopausen (d2)
  - PL "Baranek Shaun i kudłata bestia" rodzinne (d2)
- At most 2–4 of the 51 fillable clusters, and only (b) adds them with 0 wrong held out.
- The rest of the 51 are the classes §20.14 already measured as unfixable at zero wrong.

### Speed and memory

- **Training:** logistic fits in 0.06–0.16 s and the GBMs in 0.5–2.2 s (25k rows, 4 threads). The whole nested
  cross-validation for five models takes about 6 minutes.
- **Scoring is negligible next to computing the features.**
  - Python takes 0.25 µs per cluster for logistic and 5–8 µs for a GBM (2–57 ms for every contender of all five corpora).
  - In the JVM, `IdentityMlScorerBench` (plain Scala, no native library, no new dependency) reproduces LightGBM's
    probabilities to 5e-11 on every row. It scores the whole corpus in 122 ms (d2: 300 trees, 4,199 splits) and 137 ms
    (e), 18–20 µs per cluster, allocating **0 bytes** (`ThreadAllocation`, median of 20 passes).
  - The features need the full resolver and agreement stage: today's ~3.1 GB-allocation replay of the five corpora.
    A learned scorer would sit on top of that and replace none of it.
- **Model size:**
  - Logistic: 14–53 weights (under 1.3 KB). It ports trivially; `LogisticFit` and `UnifiedWeights` already do it.
  - GBM: 445–540 KB of LightGBM text (1.4–1.7 MB as a JSON dump). It runs in-JVM with the 60-line evaluator in
    `IdentityMlScorerBench`, which handles numeric `<=` splits and NaN direction. LightGBM's JNI build (lightgbm4j) or
    an exported PMML/ONNX evaluator are the alternatives.

### Interpretability

- **Logistic, resolver features (b):** standardised weights `rule.calibrated` 3.1, `model.logit` 1.6,
  `model.deniedBySome` −0.94, `rule.title` 0.73, `rule.imdb` 0.61, `production.house` 0.48, `production.season` 0.39,
  `rule.director` 0.27.
- **Logistic, + agreement (d1):** adds `poster.otherMatches` −1.17, `listing.facts` 0.71,
  `count.takersLessDissent` 0.68, `broadcast.take` 0.53, `agreement.quorum` 0.43. It also gives `and.takersNoDissent`
  −0.46 and `count.takers2` −0.43, which are the wrong sign: collinearity among the count features, not evidence.
- **GBM importance (gain share):** `model.logit` and `rule.calibrated` take 84–93% of it. Next come
  `count.takersLessDissent` (the quorum with its rise per dissenting family), `count.takers`, `rule.title`,
  `broadcast.take`, `rule.imdb`, `poster.match`, `rule.stage` and `poster.otherMatches`.

So the learners mostly reproduce today's rules: the model's own acceptance, then the agreement's quorum with its
dissent rise and the poster veto. They found no conjunction that the hand-written guards miss and that also holds up
held out. Where a learner departs from the guards, it makes the wrong takes listed above.

### Verdict

**Replacing the current fit:** no. On every hold-out each learned model is below today's stack:

- 95.9–98.2% of right clusters, against 99.2%.
- It loses 69–221 of today's right clusters and gains 0–4.
- Only the plain logistic over the resolver's own features stays at zero new wrong held out.

**Augmenting it:** at best 2 of 51 fillable clusters at zero wrong held out. That is noise-level and comes with the
risk below.

**The GBMs rank best on the independent labels** (hand AUC 0.98 against 0.91), but that does not turn into zero-wrong
recall:

- A strict zero-wrong cut sits above today's own four wrong takes, which no feature separates.
- At a cut tuned on held-out folds, the GBMs already make 1–2 new wrong takes.

**Risk.**

- Only 126 hand-labelled positive clusters, and the cut is set by the handful of high-scoring hand-wrong takes named above. One relabel moves it.
- Weak labels make every learner learn "do what today does".
- The leave-one-country-out cuts do not transfer.

**Recommendation.**

- Keep the calibrated logistic plus hand-written guards. ML adds nothing measurable at zero wrong.
- Spend the effort on new evidence for the classes that defeat every learner here: programme banners, stage works,
  another film's own title, and model takes with no counter-signal (posters, venue pages).
- Spend it on more hand labels too, and rerun this script when the labels grow, especially the
  hand-labelled-wrong takes.

## 2026-10-19 — CinemaxX's Vista film code as a title-gated catalogue id (recording 38713274617, base `82de60cf0`)

- **Tried:** the next angle of the `experiment/booking-ids` entry. Read the one `HO\d{8}` code off a listing's
  cinemaxx.de booking URLs (`CatalogueSources.CinemaxX`). Only at CinemaxX's own venues ("CinemaxX …"), never off a
  `MultiFilmBill` marker or "+" bill. Added as `CatalogueId("cinemaxx", code)` and title-gated as a feed's id
  (`CatalogueSources.TitleGated` → `ConstraintEdges.catalogueKey`; kept out of `TitleLinks`' block keys).
- **Coverage:** 1,333 listings at 29 CinemaxX-owned venues. 1,332 carry exactly one code, 87 codes in all.
- **The narrowing's premise is false:** CinemaxX's own venues reuse a code for another billing too. Examples:
  "Edmund und Luzie" / "Pettersson und Findus", "Lord Of The Rings Triple" / "Die Gefährten", "Panem-Mockingjay 1+2" /
  "Mockingjay Teil 1" (the bill guards miss "1+2"), and the Busan listings under the Buenos Aires code. HO00001648
  names BOTH Ang Lee's "Sinn und Sinnlichkeit" (1995, tmdb 4584, 7 venues) and Oakley's 2026 film (tmdb 1503762,
  CinemaxX Essen, billed 2026 / Georgia Oakley: its take is RIGHT). Only the title gate keeps these apart.
- **Result (`identity-measure-local`, DE):** model stage 33,703 listings: lost 0, changed 0, gained 0. Final stage:
  0 / 0 / 0. 0 new wrong, 0 unjudged. `identity-slice` confirms the head reads `ids cinemaxx:HO00001648` on Essen, and it
  stays apart from the 1995 film (year delta, denies-film). Moves nothing.
- **Why stopped:** payoff 0. Beside an agreeing title the title's own must-link already joins these listings, so the
  code adds nothing; without the gate it would join different films. Landing would also need the scraper to capture
  the code into `externalIds`: prod's lean listing read (`LeanListing.leanFilm`) drops showtimes, so prod never sees
  the booking URLs. The recording's full corpus does.
- **Code:** `experiment/cinemaxx-code` (code, plus `CinemaxXCodeSpec`).
