# Would a Filmweb id as a backup identity improve matching?

Investigated 2026-11-01 (data at about 19:40 UTC), read-only. No code was changed. The owner asked whether
introducing Filmweb ids as a backup identity, in Poland and elsewhere, would improve matching. Another agent is
diagnosing PL "Killhouse" (2026), so it is not analysed here.

**Verdict: no-go on a new Filmweb identity.** The mechanism it would need already exists, Filmweb is already wired
into it, and the gap it would fill is small. Most unmatched listings are events, not films. Most unmatched real films
are TMDB films that the resolver found and refused, or missed through a title problem. A plain Filmweb title search is
wrong about as often as it is right. The cheap, safe options are listed under "What to do instead" at the end.

## What already exists

The question assumes a new design, but most of it is on main already.

- **A backup identity mechanism.** `FallbackIds.Source` has one case, `Imdb`. A film that no TMDB record holds takes
  a negative candidate id, and the decision carries it as `ResolverDecision.fallback`. The rule that accepts one is
  `AcceptanceFactRules.fallback`, and it requires all of these:
  - the listing's title is the record's own title;
  - the listing publishes its director or its exact year;
  - no listing fact contradicts the record;
  - the listing bills one work;
  - no rival with the same title fits as well.

  The site serves such a film with the venue's title, poster, runtime and director, and links the IMDb page and
  rating. The read model shows this, for example DE "Wer weiß schon, was ein Leben ist?" (tt43711476).
- **Filmweb is already a fallback identity, in Poland only.** The agreement stage
  (`AgreementStage`, `IdentityProjectionWiring.identities`) lets a film stand on Wikidata's id everywhere, and on
  Filmweb's id in Poland only, through `ResolverDecision.Fallback("filmweb", id, …)`. It is used only when at least
  three film-database families agree on the film. `ProjectionDrafting` then links that Filmweb page and rating.
  - It was measured on 2026-10-05 and gained PL "Płazy. Pionierzy życia na lądzie" with nothing wrong.
- **Filmweb is already evidence.**
  - `VoterFamily.Filmweb` is one agreement voter in PL, DE and ES.
  - Its programmes of each Polish venue (`VenueListings`) are read against the model's own takes (`Correction`).
- **Filmweb was tried and removed as a single-source fill.** `family.filmweb.took&!model.unscored` filled a no-match
  with Filmweb's take of the title. It was dropped on 2026-10-14 because **13 of its 26 takes were wrong**
  (identity-resolver.md §20.19). The 2026-10-04 signal experiment found the same thing: every family alone added
  wrong matches, at 15–45% of its gains.
- **The venue programmes were tried as a fill and rejected** (§20.11). They gained +1 film, which did not pay for the
  250–520 Filmweb requests a day they cost.

So "Filmweb as a second `FallbackIds.Source`" would be a third route for the same id. The question becomes whether a
looser rule than the existing ones would gain right matches without wrong ones.

## 1. Size of the gap

The data is the local prod mirror, fresh to the second for all five countries. It counts films with a live
screening on or after 2026-11-01. A listing is a distinct `listingKey`. "Unmatched" means `movies.tmdbId` is null.

| Country | Films | Unmatched films | Listings | Unmatched listings | Unmatched films with an IMDb fallback | Listings that publish a year or director |
|---|---:|---:|---:|---:|---:|---:|
| PL | 1,146 | 366 (32%) | 10,203 | 939 (9.2%) | 0 | 5 of 938 |
| DE | 4,430 | 1,586 (36%) | 30,335 | 2,192 (7.2%) | 8 | 48 of 2,192 |
| UK | 2,434 | 856 (35%) | 32,255 | 1,934 (6.0%) | 1 | 36 of 1,933 |
| US | 1,952 | 41 (2%) | 94,292 | 478 (0.5%) | 0 | 0 of 478 |
| ES | 291 | 14 (5%) | 6,111 | 51 (0.8%) | 0 | 4 of 51 |

The last column explains why the IMDb fallback almost never fires: its rule needs a published year or director, and
in Poland 5 of 938 unmatched listings publish one. Filmweb added as a second source under the same rule would fire on
almost nothing either.

### Sample, classified by hand

The sample is random (seed 20261101): 50 PL films, 20 each from DE, UK and US, and all 14 from ES. Each was judged
from its trace (basis, candidates and refusals) and the venue. Each was then checked by a clean-title search on
Filmweb, IMDb's suggestions and the TMDB cache.

| Country | Not a film (event, relay, bill, short block) | Real film, found but refused | Real film on TMDB, not found (title issue) | Real film, not on TMDB | Unclear |
|---|---:|---:|---:|---:|---:|
| PL (50) | 33 | 7 | 4 | 4 | 2 |
| DE (20) | 12 | 4 | 0 | 2 | 2 |
| UK (20) | 13 | 5 | 1 | 0 | 1 |
| US (20) | 13 | 5 | 0 | 2 | 0 |
| ES (14) | 13 | 0 | 0 | 1 | 0 |

PL details:

- **Found but refused:** Opętanie 1981, Ucieczka z Kabulu, DAMA 2025, Znachor 1937, Kościuszko pod Racławicami,
  Alim 1926, Amator 1979.
- **On TMDB, title issue:** Atlantyda (TMDB 618245), "Lalka. DZIEŃ KINA POLSKIEGO", "Violetta villas matka, diva,
  legenda.", "KINO SENSORYCZNE:SKARPETKI 4".
- **Not on TMDB:** Elf. Świąteczne życzenie, Cena buntu, (Okładka) Sztuka podróżowania, Watch Docs "Alice".
- **Not a film:** HAUSER and André Rieu concerts, Royal Ballet and Paris Opera relays, UFF, AfryKamera and Palestine
  Cinema Days short blocks, APF double bills, TERRA travel talks, workshops, debates and PCKul stage plays.

Other countries:

- **DE:** not on TMDB are "Der Weg des Dalai Lama" and "Hotel Polan und seine Gäste". Refused are Catching Fire,
  Imagine Peace, I Pastafari and Asterix in Nubien.
- **UK:** none not on TMDB. Refused are Wicker, Instruktorius, Roger Corman, Cyber City Oedo 808 and Blue Note. The
  Holdovers was missed behind "Chocolate Tasting Kit -".
- **US:** not on TMDB are The Family Recipe (IMDb tt33760366) and Voices of the Shenandoah River.
- **ES:** El gigante mudo is not on TMDB.

**Only the "not on TMDB" column is the gap a backup identity can fill:**

- PL: 8% of unmatched films, roughly 30 films (the interval is wide, about 10–60).
- DE: 10%, but these are German documentaries Filmweb does not know.
- UK: 0. US: 2 films. ES: 1 film.

The other real films are TMDB films. They need a fix to a rule or a prefix, not a new identity, as in
2026-10-20-pl-unmatched-real-films.md.

## 2. Coverage

**Filmweb, PL.** Clean-title searches of the 15 real films in the PL sample:

- Filmweb finds 11 of them: Atlantyda, Lalka, Violetta Villas, Opętanie, Znachor 1937, Kościuszko, Alim, Amator,
  Elf, and "Skarpetki 4" (record 10118632, though the bare "…4" search misses it).
- Of the 4 not on TMDB, Filmweb has 1 (Elf. Świąteczne życzenie, 10113302) and IMDb has 0.

**Filmweb against IMDb, whole PL corpus.** The worker has already asked both families about 348 of the 366 unmatched
PL films (`identity_family_answers`). The exact-title hits:

- Filmweb: 33 films.
- IMDb: 28 films.
- Both: 27 films.
- **Filmweb only: 6 films.** These are Fale, Kos, Mali detektywi i tajemniczy prezent, Ostatni Cygan w Oświęcimiu,
  Upadek and Wilki.

Filmweb's edge over IMDb is Polish titles and Polish new releases, and on this corpus it amounts to a handful of films.

**DE.** Filmweb has exact hits for 73 of 1,586 films and IMDb for 108. Filmweb finds 5 that IMDb does not; IMDb
finds 40 that Filmweb does not. The local equivalents are below:

- OFDb links IMDb ids, so it adds nothing beyond IMDb.
- kino.de forbids automated access.
- Filmstarts blocks its search in robots.txt.

**UK and US.** IMDb is already the fallback. Letterboxd's catalogue is TMDB's (its pages link both ids), so it can
add no film TMDB lacks.

**ES.** FilmAffinity returns a Cloudflare 403. The one ES film not on TMDB is on none of them.

**Venue links**, which would be far stronger evidence than a title search:

- **Filmweb's venue programmes.** 347 of the 366 unmatched PL films screen at a venue Filmweb lists, and 71 have a
  programme entry on the listing's days. **0 of them name the listing's film by its title.** Filmweb lists the
  mainstream films, which are matched already, not the events and festival slots that are unmatched.
- **Listings from Filmweb.** Only 34 live listings come from Filmweb (`FilmwebShowtimesClient`, `externalIds.filmweb`),
  and none of them is unmatched.
- **Venues' own pages.** These carry IMDb ids at two venues and Filmweb ids at one (Iluzjon). This is from
  2026-10-19-film-ids-in-booking-and-detail-urls; that study found they settle 0 takes.

## 3. How it would work

**As an identity.** Add `FallbackIds.Source.Filmweb(code 2)`: number bits hold Filmweb ids (largest about 10.14 M,
well under 2^27). Its candidates come from Filmweb's title search, with records from `/api/v1/title/<id>/info`,
which gives title, original title, year and poster. Other endpoints give the rest; the family records already carry
runtime, directors and countries.

- **Merge with TMDB.** When TMDB gains the film, the same cluster takes the TMDB id on the next projection. A
  fallback takes only when no TMDB film is taken, so nothing sticks and no migration is needed. The film key does
  not change, because keys only name.
- **Filmweb has no IMDb or TMDB id** in its API or pages, so the record can't be checked against IMDb or TMDB directly.
- **Wikidata is the bridge.** P5032 (Filmweb film ID) is on 60,261 items. 59,809 of those carry an IMDb id and 55,417
  a TMDB id. It is CC0.

**As evidence.** Filmweb already votes in the agreement stage (PL, DE, ES), and its PL programmes already correct
model takes. A Filmweb id becomes a TMDB vote through Wikidata or through a record's IMDb id. This role is built.

## 4. Access and risk

| Source | API | robots.txt | ToS and legal | Anti-bot | Links IMDb or TMDB |
|---|---|---|---|---|---|
| Filmweb.pl | No public API. The undocumented `/api/v1` works (we use it already) | `User-agent: *` is allowed everything | ToS not retrieved. A commercial site, so treat the API as unlicensed: a takedown can come any time | None seen. 200 with a browser user agent | No |
| OFDb (DE) | No | Allowed, except `/suchergebnis` (search) | Not retrieved | None seen | IMDb |
| kino.de (DE) | No | `/api/` disallowed | **Explicitly prohibits automated access and reserves text and data mining (§44b UrhG)** | – | No |
| Filmstarts.de, SensaCine | No | Search disallowed | Not retrieved | – | No |
| FilmAffinity (ES) | No | Unreadable | – | **Cloudflare 403** | – |
| IMDb | Paid (AWS Data Exchange). Free datasets are non-commercial only | Search disallowed | **Conditions forbid scraping without written consent.** We already call suggestion.imdb | – | Is IMDb |
| Letterboxd | Application-only | Many AI bots disallowed | – | – | IMDb and TMDB |
| Wikidata | SPARQL and dumps | Fine | **CC0** | Polite user agent | Filmweb, IMDb, TMDB |

The legal exposure is already there: the worker scrapes Filmweb's programmes, ratings and search today. A new
identity would make the site depend more on an unlicensed API. Filmweb pages are stable (id in the URL,
`/film/<slug>-<year>-<id>`), but there is no JSON-LD and no contract.

## 5. Estimate and prototype

**Prototype.** The 12 PL unmatched films with exactly one Filmweb exact-title hit, judged by venue and context:

| Listing (venue) | Filmweb take | Verdict |
|---|---|---|
| Mali detektywi i tajemniczy prezent (Światowid) | 10120394 (2026) | **Right; not on TMDB, so a real gain** |
| Kos PL (Fenomen) | Kos 2023 | Right, but TMDB has it (1047917): a search fix |
| Frida: Viva la Vida (Rialto) | 2019 | Right; TMDB 649244 was refused |
| Michał Anioł. Miłość i śmierć (Rialto) | 2017 | Right; TMDB 463901 was refused |
| baranek shaun i kudłata bestia + warsztaty (Muza) | 2026 | Right; TMDB 1477104 was refused as a bill |
| Milczące kwiaty, świadkowie + … + Sen | 2024 | **Wrong**: a bill of three shorts |
| Zaczarowany flet (opera relay) | the 1972 film | **Wrong** |
| Wiedeńska krew (PCKul, stage) | the 1942 film | **Wrong** |
| Mangrove (Muza) | the 2011 short | **Wrong**: probably McQueen's 2020 film |
| Lepszy model (Paradox) | the 2014 Disney TV film | Unsure, probably wrong |
| Ostatni Cygan w Oświęcimiu (Cytadela) | a 1994 record | Unsure; TMDB 550591 was refused |
| Polski | 2016 | Unsure |

That is 5 right and 4 wrong, with 3 unsure: a wrong rate of about 44% of those judged. It matches the 13/26 measured
on 2026-10-14. Another 21 films have several Filmweb films under the exact title (Upadek ×5, Wilki ×6, Proces ×6,
Carmen ×8), and Filmweb alone can't choose among them.

**Expected gain**, in films moved from unmatched to a correct match:

| Country | Under the existing fallback rule (needs a year or director) | Under a looser exact-title rule |
|---|---|---|
| PL | ≈0 | ≈5–15 films, about 10–20 listings (1–2% of PL's unmatched listings, 0.1–0.2% of all PL listings). It brings a wrong match for roughly every right one unless it is guarded far more tightly |
| DE | ≈0 (Filmweb's DE records are not an identity) | – |
| UK, US, ES | 0 | – |

The owner's rule is that unmatched beats wrong, and §20.19 prices a wrong match at up to ~100 missed ones. The looser
rule fails that.

## Recommendation

**No-go** on adding `FallbackIds.Source.Filmweb` or any Filmweb-alone take.

What to do instead, in order of payoff:

1. **Fix rules and titles for TMDB films.** In the PL sample, 11 of the 15 real films are on TMDB and were refused or
   missed through a title problem: festival prefixes, "| UFF", strand suffixes, below-threshold classics.
2. **Narrow optional experiment.** In the existing agreement stage, a Filmweb fallback for a **current release**
   (record year ≥ last year, unique exact title, the listing billing one work, not `NonFilmEvents`). On today's data
   it would take Mali detektywi, with 0 wrong. Measure it with `scripts/identity-measure` before deciding.
3. **Wikidata P5032 as a crosswalk** (CC0) when a Filmweb id must become a TMDB or IMDb id. Don't scrape Filmweb more.

Scratch data (the TSVs, JSON dumps and probe output) is in this session's scratchpad. The queries are reproducible
against `kinowo*_prod_mirror` on port 28017.
