# 2026-10-19 — agreement ACROSS countries (branch `experiment-cross-country`, tag `experiment/cross-country`, prod mirror of 2026-10-19 17:42Z)

## A bare listing adopts, or is denied by, another country's fact-backed take of the same title in the same week — not built (offline: almost nothing to cure)
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
  **Built (2026-10-20)** as the findings report's "Countries disagree" section (`scripts/ci/countries_disagree.py`,
  the shared reading in `scripts/ci/cross_country.py`): of the 53 keys only those with a LONE take — a film no other
  country takes, its billing backing it by no year or director — and not all judged right or all relays/events;
  9 titles on the 2026-10-20 mirror, Fallen Angels the one `check`.
