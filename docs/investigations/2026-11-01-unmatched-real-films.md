# Recovering real films the resolver leaves unmatched

Started 2026-11-01 (data at about 20:00 UTC), following
[2026-11-01-filmweb-backup-identity.md](2026-11-01-filmweb-backup-identity.md), which found that most unmatched real
films are TMDB films the resolver refused or missed. This notebook holds the whole effort: the census, the causes, and
one section per fix, kept up to date as each one lands. Each fix's commits name it in an `Identity-Design:` trailer.

## 1. Census

**Source.** The local prod mirror (`kinowo*_prod_mirror` on :28017, fresh to the second), read-only. A listing is a
`listingKey` of a screening on or after 2026-11-01 whose film has no `tmdbId`. Its `identity_traces` row gives the
blocker, the searches run and the candidates with their scores.

**Events first.** A broad title pattern (concerts, relays, talks, workshops, marathons, secret screenings, shorts
blocks, festival passes, every Kino PCKul title) set aside the obvious events. That pattern is wider than
`NonFilmEvents` (which is tuned for precision). Every other cluster (one film id) was judged by hand against TMDB's
API, with the trace's candidates beside it: event, a film on TMDB (with its id), a film not on TMDB, or unclear. 1,980
clusters were judged in 29 batches. An "unclear" verdict was used wherever namesakes could not be told apart.

| Country | Unmatched listings | Events (pattern) | Events (judged) | Refused (films) | Title miss (films) | Not on TMDB | Unclear |
|---|---:|---:|---:|---:|---:|---:|---:|
| PL | 931 | 549 | 279 | 26 (24) | 23 (22) | 9 | 45 |
| DE | 2,157 | 631 | 1,098 | 84 (72) | 19 (17) | 66 | 259 |
| UK | 1,920 | 1,241 | 551 | 50 (46) | 23 (14) | 5 | 50 |
| US | 476 | 441 | 23 | 4 (4) | 5 (1) | 2 | 1 |
| ES | 51 | 12 | 38 | 0 | 0 | 1 | 0 |

"Refused" means the right TMDB film was among the candidates and a rule or cut turned it down. A "title miss" means
no search found it. In all, **about 230 listings (4% of the unmatched) are recoverable TMDB films**; the rest are events
(about 85%), films TMDB lacks, or namesakes nothing tells apart. The pattern's event set was spot-checked afterwards:
it had swallowed the UK "<film> + Q&A" listings, which section 3 counts separately.

## 2. Causes, by listings

| Cause | PL | DE | UK | US | All | Examples |
|---|---:|---:|---:|---:|---:|---|
| Refused below the rating cut, the facts weighing against (runtime gap, one venue, low popularity) | 13 | 43 | 23 | 3 | 82 | DE "Asterix in Nubien", UK "Here Lived" (venue 135′ against an 81′ film) |
| Banner or strand text left in the search (each its own banner) | 17 | 10 | 13 | 5 | 45 | PL "Alim \| UFF", "Wielki żar \| Pięć Smaków", US "TOUCH OF EVIL: THE RESTORED CUT" |
| **A companion event joined to the film ("+ Q&A", "+ panel", "+ Interval")** | 0 | 3 | 30+ | 0 | 33+ | UK "The Assassin + Q&A", "The Odyssey + Interval" |
| Refused by a learned cannot-link veto | 3 | 8 | 6 | 1 | 18 | DE "Kult-Special: Blade Runner - The Final Cut" |
| Found only by IMDb (the venue spells the title otherwise) | 1 | 13 | 3 | 0 | 17 | DE "Mein Sommer mit Nonna", UK "The Hunger Games Sunrise of the Reaping" |
| **Spelling: "&" for "and", "SS" for "ß"** | 0 | 7 | 2 | 0 | 9 | DE "Sinn & Sinnlichkeit" ×5, "DER RHEIN FLIESST INS MITTELMEER" |
| A namesake runner-up as close | 4 | 4 | 0 | 0 | 8 | DE "ZUSAMMEN" |
| Venue typo | 1 | 3 | 4 | 0 | 8 | UK "Jak Zyc Zeby Nie Zwarjowac" |
| Local title TMDB lacks | 3 | 1 | 0 | 0 | 4 | PL "Kusama. Nieskończoność" |
| Festival catalogue-namesake veto | 0 | 3 | 0 | 0 | 3 | DE "Jüdische Filmwoche: Das siebte Kreuz" |
| Instalment numbered in words ("Część 3" against "Część trzecia") | 1 | 0 | 0 | 0 | 1 | PL "DIUNA: CZĘŚĆ 3 - 2D DUB" |
| Other | 7 | 6 | 1 | 0 | 14 | |

Only the two bold rows are single mechanisms. The rating-cut refusals are the calibration doing its job: in every
sampled case a published fact (a runtime gap, one venue billing it, no year) weighs against the film, and the owner's
rule keeps those unmatched. The banner row is 40 banners, each a title-rule row of its own.

## 3. Fix A: a companion event is no film of its own

**Cause.** A billing joining an event to its film ("The Assassin + Q&A") was split on the "+", and the event piece
was searched and read as a shape like any other. "Q&A" found Q & A (1990), which then:

- stood as the runner-up the film's own facts did not outweigh ("its own facts do not favour it over the runner-up":
  UK "The Assassin + Q&A", "My Daughter Is a Zombie + Q&A", "Oxide Ghosts: The Brass Eye Tapes + Q&A");
- or ranked first, at 46.1%, for documentaries ("Inside Gaza + Q&A", "Nina Roza + Q&A");
- and "Panel", "Interval", "reception", "Gäste" and "Filmgespräch" were missing from the side-event words, so "The
  Odyssey + Interval" and "Was haben wir gelacht + Gäste" were read as bills of two works.

**Change.**

- A shape that names nothing but an event (`IdentityMeasures.namesOnlyAnEvent`: an event or side-event word, every
  other word one billed around a film) is dropped from the listing's shapes and from the bill's searched works. The
  listing's own titles are always shapes, so a film called "Q&A" is still billed by its title.
- `title-words.tsv` side-event rows: panel, interval, reception, ScreenTalk, Gäste, Filmgespräch.
- A guard for the near-tie the Q&A candidate used to hide: a bare billing joined to an event, with no year or
  director, names the newer of two namesakes no fact tells apart (`ListingShape.eventsOlderNamesake`, the festival
  rule's twin). Without it UK "Premature + Writer/Director Q&A" took Dan Beers's 2014 comedy (judged wrong). With it,
  the take is Kevin Walls's 2026 film, whose writer-director is at the screening. UK "The Assassin + Q&A" (Hou
  Hsiao-hsien's, a cinematographer's Q&A) stays unmatched under the guard.

**Slice (prod-latest recording, prod mirror).** 16 listings recovered: UK It's Nice Up North ×4, Premature, The Fall
of Otrar, Chess of the Wind, Inside Gaza, Who Are You Polly Maggoo, My Daughter Is a Zombie, The Odyssey ×2, Nina Roza;
DE Nur eine Frau, Was haben wir gelacht ×2. The remaining 68 companion listings stay unmatched: double bills, shorts
blocks, TV episodes, and documentaries TMDB lacks.

**Measure.** _pending_

## 4. Fix B: a billing's "&" and a record's "ß"

**Cause.** A title relation compares keys of letters and digits. "Sinn & Sinnlichkeit" keys to "sinnsinnlichkeit" and
TMDB's "Sinn und Sinnlichkeit" to "sinnundsinnlichkeit": only an `overlap`, which the learned cannot-link cut refused.
An upper-case billing writes "FLIESST" for "fließt", and `deburr` leaves "ß" alone.

**Change.** `TitleRelationMeasures.titleRelation` names a record exactly when:

- the LISTING writes an "&" and its key, with "&" and each language's "and" (and, und, y, i, et) folded alike, is the
  record's; or
- the RECORD writes an "ß" and the keys agree once it reads "ss".

The first measure (41347266388) folded conjunctions on the "ß" path too, and was NOT LANDABLE: Hufnagel's
"Lieblingsmenschen - Die außergewöhnliche Freundschaft von Agnes & Amir" has an "ß", so "… Agnes und Amir" ×56 named it
exactly and was kept apart from the bare "Lieblingsmenschen" ×30, which then lost the film their feed link gave them.
The "ß" key now folds nothing else, and a billing spelling out a record's "&" ("Renoir and Love") is read as before.

**Slice.** DE "Sinn & Sinnlichkeit" ×5 (Georgia Oakley's 2026 film), "DER RHEIN FLIESST INS MITTELMEER", UK "Music &
Lyrics"; the first measure added UK "Anna & The Apocalypse".

**Measure** (e1111902242, run 41350255694, all five countries, pinned recording 573). LANDABLE:

- 8 gained right: DE ×6 (Sinn & Sinnlichkeit ×5, Der Rhein fließt ins Mittelmeer), UK ×2 (Music & Lyrics, Anna &
  The Apocalypse);
- 0 lost, 0 right→wrong, 0 new wrong.

Drift rows (run 41350289811, seeds 41 and 42, all five countries): nine clean. UK seed 42 shows one flip, "Scottish
Ballet: These Violent Delights" at Keswick Alhambra. Fix A's unrelated branch shows the same flip (run 41348680527),
so it is main's, not this change's (control run 41355445466 on main).

Cross-recording (recording 528, run 41355500669): the same 8 gains in DE and UK, nothing lost.
