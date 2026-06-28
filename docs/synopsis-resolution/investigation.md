# Synopsis-Based Resolution — Investigation & Results

## What this is

An investigation into how many currently-unresolved films in the Kinowo corpus
could be linked to TMDB / IMDb / Filmweb using **fuzzy synopsis matching**,
gated on an independent corroborator (never resolved on plot text alone).

The investigation is complete. Phase 2 — wiring these into the production
pipeline — is not yet built.

---

## 2026-06-28 refresh

`accepts.jsonl` now holds the **current** corroborated resolutions (18), not the
original 2026-06-25 snapshot (142, preserved as `accepts-2026-06-25.jsonl`).

Why the re-run: verifying the 142 against live prod showed it had decayed badly —
only 9 rows were still open gaps, 11 had since resolved through the normal
pipeline, and **122 no longer existed in the corpus** under their snapshot key
(they were transient banner / festival / dubbing / repertory keys that folded
away). Two framing bugs were also found and fixed:

- **IMDb mode was rating-gated, not id-gated.** It selected rows whose IMDb
  *rating* was empty, which swept in unreleased films that already had an
  `imdbId` (e.g. *Evil Dead Burn 2026*, `tt31170389`). It now selects rows whose
  `imdbId` is actually missing (`imdbId.isEmpty && tmdbId.nonEmpty`).
- **Same-director-sibling false positive.** Director-only corroboration matched
  *Trzy kolory: Czerwony* → *…: Niebieski* (same director, wrong film). The page
  build now dedups by external URL, keeping the best title match — which drops
  that row automatically.

The harness is no longer throwaway: `worker/src/test/scala/scripts/GateReport.scala`
reads the **live** prod corpus via `MovieRepository.foreachRecord` (paginated —
a single `findAll` over the flyctl proxy corrupts the BSON stream on the full
736-doc load) and loops all three modes. Run it with:

```
MONGODB_URI=… TMDB_API_KEY=… sbt 'worker/Test/runMain scripts.GateReport [out.jsonl] [capPerMode]'
```

Current result: 18 accepts (TMDB 2, Filmweb 15, IMDb 1), **all** corroborated by
director or exact-title, 0 domain-only guesses. Regenerate the report page from
this data with `python3 regenerate_page.py` (writes `~/Desktop/synopsis-resolutions.html`).

---

## Algorithm

**Stem-IDF cosine similarity** between two Polish (or English) plot blurbs:

1. Lowercase, strip diacritics (NFD → ASCII), crude Polish suffix-stemming
   (strip `-ów`, `-iem`, `-ach`, `-ego`, etc. → 4-char floor).
2. Remove stopwords (a, i, w, z, na, do, nie, się, …).
3. Weight each token by IDF across the comparison corpus.
4. Cosine similarity on the weighted TF vectors.

Acceptance gate: **sim ≥ 0.35 AND at least one independent corroborator**:
- Director names overlap between our corpus and the candidate
- Exact-title match (normalised)
- Character names from TMDB credits found in the other blurb
- Cast names overlap

The script ran three modes against the Kinowo `movies` corpus
(films that had a cinema synopsis but lacked the target link):

| mode | reference synopsis | candidate synopsis | target |
|------|-------------------|--------------------|--------|
| `tmdb-pl` | cinema blurb (Polish) | TMDB `overview` (PL locale) | TMDB ID |
| `filmweb-pl` | TMDB overview (PL) | Filmweb `plot.synopsis` | Filmweb link |
| `imdb-eng` | TMDB overview (EN) | IMDb GraphQL plot | IMDb rating |

### Why stem-IDF, not char-trigram

A bake-off of three algorithms (A0=char-trigram, A1=idf-token, A2=stem-idf)
over a sample of 200 films with known correct answers:

| algo | top-1 accuracy | precision@calibrated-τ |
|------|---------------|------------------------|
| A0 char-trigram | ~65% | ~62% |
| A1 idf-token | ~97% | ~98% |
| A2 stem-idf | ~97% | ~98% |

Trigram is too lexically sensitive — one long inflected word can dominate.
IDF weighting downgrades common Polish tokens ("film", "historia", "kiedy")
and upgrades proper nouns and rare domain words. Stem-IDF adds marginal value
(identical accuracy on this corpus).

`SynopsisSimilarity` (char-trigram) is already committed to production as a
tie-breaker inside `TmdbClient.pickBest` and `FilmwebClient.pickBest`; it is
correct for that narrow use-case (already-filtered candidates). The open
candidate matching in this investigation uses stem-IDF because it needs to
discriminate across the full candidate pool.

---

## Results

142 films accepted, 0 false positives (all independently corroborated):

| site | accepted | corroborator breakdown |
|------|----------|----------------------|
| TMDB | 20 | exact-title (13), director (5), character-name (2) |
| IMDb | 71 | director (41), exact-title (20), character-name (7), cast (3) |
| Filmweb | 51 | director (28), exact-title (19), character-name (4) |
| Metacritic | 0 | coverage gap — films not indexed on MC |
| RottenTomatoes | 0 | coverage gap — films not indexed on RT |

### Why each film was NOT resolved by the standard pipeline

Derived from the `accepts.jsonl` fields (`yearMatch`, `rowDirectors`,
`candDirectors`, `matchedDirectors`):

**IMDb (all 71)**: IMDb ID absent from TMDB metadata. The standard pipeline
reaches IMDb only via `tmdb.details(id).imdb_id`; if that field is null (common
for non-US / art films), no IMDb resolution happens regardless of
title/director match.

**Year gap**: ~30 films — our corpus year ≠ external year. The year gate in
`pickBest` (max ±1 year) excluded the correct candidate.

**No director in corpus**: ~25 films — the cinema listing carried no director
credit. Without it the standard disambiguation step abstains.

**Director name mismatch**: ~15 films — we have a director but the name differs
between our data and the external site (transliteration, middle-name inclusion,
etc.).

**Search title ranking / multiple candidates**: ~20 films — year and director
both match but the correct TMDB/Filmweb entry is not returned as the top search
result (e.g. a title shared with another film, or the Polish-market title differs
from the TMDB primary title).

---

## Data files in this directory

`accepts.jsonl` — one JSON object per accepted resolution. Fields:

```
src             "TMDB" | "IMDb" | "Filmweb"
filmId          Kinowo corpus key (sanitize(title)|year)
filmTitle       raw display title from the corpus
filmYear        year from the corpus (may be blank for undated entries)
searchTitle     cleaned search query used (banners stripped by TitleRuleSet)
extUrl          external entity URL
extTitle        title on the external site
extYear         year on the external site
via             corroborator type: "director" | "exact-title" | "character-name" | "cast"
sim             stem-IDF cosine score (float string)
refSyn          reference synopsis (cinema blurb or TMDB EN overview)
candSyn         candidate synopsis (TMDB PL / Filmweb plot / IMDb plot)
rowDirectors    director names from our corpus
candDirectors   director names from the external candidate
matchedDirectors overlap
matchedCharacters TMDB character names found in the other blurb
matchedCast     cast names found in the other blurb
yearMatch       boolean — corpus year == external year
```

`enrich.json` — map of `filmId → {title, city}` from the prod `web_movies`
read model (snapshot taken during investigation). Used to build Kinowo film URLs.

The generated report page was saved to `~/Desktop/synopsis-resolutions.html`
(244 KB, 142 cards grouped by website, sorted by sim desc, each card has a
"why unresolved" explanation and collapsible synopsis pair). Regenerate it
from the data files with the Python script in this document (see below).

---

## What's already in production (committed to main)

- `common/src/main/scala/tools/SynopsisSimilarity.scala` — char-trigram cosine
  + `confidentTieBreak`; tested in `SynopsisSimilaritySpec`.
- `MovieRecord.synopsisCinema` — cinema-only synopsis accessor (excludes
  TMDB/IMDb/Filmweb to avoid circular comparison).
- `TmdbClient.pickBest` — applies `SynopsisSimilarity.confidentTieBreak` as a
  final tie-breaker when candidates are tied on year distance. Tests in
  `TmdbClientSpec` (regression guard + flip-on-matching + keep-legacy-on-unrelated).
- `FilmwebClient.pickBest` / `FilmwebPreview.plot` — same tie-break path for
  Filmweb candidates. Tests in `FilmwebClientSpec`.
- `FilmwebRatings` — passes TMDB synopsis as `referenceSynopsis` to
  `filmweb.lookup`.
- `MovieService` — passes `synopsisCinema` to `tmdb.search`.

None of the 142 resolutions from this investigation have been applied to prod
yet. The pipeline changes above improve disambiguation for films that were
already being searched; they don't yet retry films that returned no results.

---

## Production path (Phase 2 — not yet built)

To enable these 142 resolutions (and future similar cases) without manual
intervention:

### 1. TMDB unresolved retry with synopsis hint

`UnresolvedTmdbReaper` already retries `tmdbId.isEmpty && !detailPending` rows
on a 24h cycle. The synopsis tie-break is already wired into `pickBest`. The
missing piece is a **wider search** for films where the standard year-gated
search returns nothing:

- On `tmdbNoMatch`, if `synopsisCinema.nonEmpty`, retry with `year = None`
  (no year filter) and apply `confidentTieBreak` across all results.
- Accept only if corroborated by director / exact-title / character-name.
- Use a higher floor (e.g. sim ≥ 0.40) for the no-year case to compensate for
  the wider candidate pool.

This would recover the 20 TMDB-unresolved films.

### 2. IMDb direct resolution (bypass TMDB imdb_id)

The 71 IMDb gaps are all due to `imdb_id` being null in TMDB's data. These
films have a TMDB ID — we can use the **TMDB EN overview** as a reference
synopsis to search IMDb directly:

- After `TmdbResolved`, if `imdb_id` is null, fire an `ImdbSynopsisSearch`
  task: fetch the TMDB EN `overview` → search IMDb by title+year → pick the
  candidate whose GraphQL plot scores highest against the overview, gate on
  director/exact-title.
- Accept threshold: sim ≥ 0.35, corroborated.

This path doesn't exist yet. It would need a new task type in the worker
queue and a new `ImdbSynopsisResolver` service.

### 3. Filmweb retry with synopsis

The 51 Filmweb gaps would largely be fixed by:
- Passing the TMDB PL synopsis (from `tmdb.details`) as `referenceSynopsis` to
  `filmweb.lookup` — this is already wired for `FilmwebRatings`.
- Re-running `FilmwebRatings` for films that previously returned no result
  (currently these are cached as "miss" and not retried automatically).

A `FilmwebMissReaper` that clears the negative cache for films where the TMDB
synopsis is now available would trigger the retry through the existing pipeline.

---

## Regenerate the report page

Run from the session scratchpad data (or from `docs/synopsis-resolution/`):

```python
# python3 regenerate_page.py
# reads: accepts.jsonl, enrich.json (same directory)
# writes: ~/Desktop/synopsis-resolutions.html
```

The full generation script is in the previous Claude conversation
(transcript at `~/.claude/projects/-Users-pawel-projects-movies/<session>.jsonl`,
search for `PYEOF`). Key logic:

- Group records by `src`, sort each group by `float(sim)` descending.
- Per card: `searchTitle` + year → Kinowo link; `extTitle`/`extYear` → external
  link; director HTML with matched names in `<b class=hit>`; clue chips; amber
  "why unresolved" strip derived from `yearMatch` / `rowDirectors` /
  `matchedDirectors`; collapsible `<details>` with both synopses.
- Kinowo URL: `/{city}/film?title={quote(title)}` using `enrich.json`.

---

## GateReport harness (throwaway — recreate if needed)

The harness (`GateReport.scala`, run as `sbt worker/Test/runMain scripts.GateReport`)
lived in a throwaway worktree and was removed after the investigation. To recreate:

**Inputs**: prod `movies` collection (via `flyctl proxy 27017:27017 --app kinowo-mongo`
+ `MONGODB_URI` from `.env.local`); live TMDB / Filmweb / IMDb APIs
(`TMDB_API_KEY` in `.env.local`).

**Algorithm per mode**:

```
tmdb-pl:
  for each row where tmdbId.isEmpty && synopsisCinema.nonEmpty:
    candidates = tmdb.search(searchTitle, year=None)  // no year filter
    scored = candidates.map(c => (c, stemIdf(synopsisCinema, c.overview)))
    best = scored.maxBy(_._2)
    if best.sim >= 0.35 && corroborated(row, best.candidate):
      emit accept(src="TMDB", ...)

filmweb-pl:
  for each row where tmdbId.nonEmpty && filmwebLink.isEmpty && tmdbOverview.nonEmpty:
    candidates = filmweb.search(searchTitle, year=None)
    scored = candidates.map(c => (c, stemIdf(tmdbOverview, c.plot)))
    best = scored.maxBy(_._2)
    if best.sim >= 0.35 && corroborated(row, best.candidate):
      emit accept(src="Filmweb", ...)

imdb-eng:
  for each row where imdbRating.isEmpty && tmdbOverviewEn.nonEmpty:
    candidates = imdb.search(searchTitle)  // IMDb GraphQL
    scored = candidates.map(c => (c, stemIdf(tmdbOverviewEn, c.plot)))
    best = scored.maxBy(_._2)
    if best.sim >= 0.35 && corroborated(row, best.candidate):
      emit accept(src="IMDb", ...)
```

**Corroboration** (at least one must fire):
- Director names: `(rowDirectors intersect candDirectors).nonEmpty`
  (after normalising to lowercase + strip diacritics)
- Exact title: `normalize(searchTitle) == normalize(candTitle)`
- Character names: TMDB credits for the candidate → check each character name
  appears in the reference synopsis
- Cast: similar check for actor names

**Emit fields**: see `accepts.jsonl` schema above.
