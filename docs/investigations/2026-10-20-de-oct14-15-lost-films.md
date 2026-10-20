# Germany: the 98 TMDB films lost in the 2026-10-14/15 resolver tightening

Investigated 2026-10-20, read-only (VictoriaLogs take-change log, the local prod mirror at 13:2x UTC, and
`scripts/identity-slice` replays). All times are UTC. This follows up cause 3 of
[the DE corpus decline](2026-10-20-de-corpus-decline-oct12-19.md).

## Summary

Between 10-14 12:00 and 10-15 16:00, 46 worker-de builds (ending at `8b75b70`) logged `identity take change: lost` for
listings of **136** distinct TMDB films. **98** of them were not gained by any listing between 10-14 12:00 and 10-19 03:00.
That is the source doc's definition, reproduced exactly. They are listed in full below
(214 listings; 63 of them are one esports relay, 28 one festival).

| Class | Films | Meaning |
|---|---:|---|
| Correct refusal | **82** | the old take was wrong: an event, concert, reading, theatre staging, festival or esports billing, a namesake, a short taken for a feature, or a kinoheld page copied from another film |
| Correct refusal, now cured | **4** | the old take was a wrong namesake; the same listing now takes the right film (Lieblingsmenschen → 1185542, Chasing Hope → 1786543, Pink Power → 1499152, On the Road – En el camino → 1517123) |
| **Wrong loss** | **9** | a real film that should still match (2 regained on 10-20, 7 still lost) |
| Undecided | 3 | Le Journal d'une femme de chambre (Kamino; Renoir 1946 or Buñuel 1964), Drishyam 3 (Hindi or Malayalam; listing gone), Susanne Siegert (judged right, reads as a talk) |

**Verdict: the class is overwhelmingly correct.** About 88% of the 98 films are refusals of wrong takes, and the
replays confirm them. On the 10-15 recording, `8b75b70` vs `89ced660a` classifies the batch as 179 cured, 15 lost, 0 new-wrong.
The wrong losses are small: 9 films on about 12 listings. Four root causes account for them, ranked by films recovered:

1. **The page-feed copied-title denial** (`b88faf7ea`) in `2900ea1`: 4 films.
   - Hin und Weg was regained by today's code.
   - Mein Opa der Bankräuber and Zirkus Fumeusement are already filed as `accepted-right-loss`.
   - Das Auge des Bösen was bisected to `b88faf7ea`, and its judgement is itself wrong.
2. **Cluster cannot-link vetoes** (learned listing-listing / listing-film below the cut): 2 films, Die Blutgräfin and
   Catching Fire (WA). Neither is reproduced by any code in the range.
3. **Piece-beside** (`8e5d2d06d`, `83a8abd68`) reading a film's own subtitle as a second piece: 2 films (Mission to Marsh –
   Auftrag: Moor!, Frauen bildet Banden – Eine Spurensuche…). Neither is reproduced on the 10-15 recording. Frauen bildet Banden was regained at 10-20 10:27.
4. **A feed-linked film not honoured** (BelowThreshold while the venue's cineamo id names the film): 1 film (Life Support,
   Babylon Mitte, cineamo 1643143m). It is not reproduced, and none is taken at both refs.

Met Opera "La Fanciulla del West" (1703629), ENHYPEN "Blood Saga" (1785968) and Lieblingsmenschen were suspected in the
source doc, but they are **not wrong losses of this class**:

- The Met Opera and ENHYPEN listings were lost under `8b75b70` on 10-16 / 10-20, not on 10-14/15. Each was re-filled within
  seconds to hours (Broadcast / Agreed / OwnMatch). They flicker as pairs of lost and gained lines, and none of them stays lost.
- Lieblingsmenschen's old take 788385 is a 65-minute 2021 film. The CineStar listings now take 1185542, Helena Hufnagel's 2026
  release. That is a cure.

## Method

1. **The list.** VictoriaLogs (`ssh monitoring-1 … /select/logsql/query`), `_time:[2026-10-13T12:00Z, 2026-10-20T23:59Z]
   country:de kind:*`, gave 1,967 lines. The lost lines in [10-14 12:00, 10-15 16:00) name 136 TMDB films (`tmdb_before`).
   Removing every film with a `tmdb_after` gain between 10-14 12:00 and 10-19 03:00 leaves 98. This matches the source doc.
   The loss build and reason come from each film's earliest lost line (`build`, `basis`, `reason_first`/`reason_last`).
2. **Prod now.** The local mirror `kinowo_de_prod_mirror` (`scripts/local-mirror/freshness de`: synced 11:23) supplied three things:
   - `tmdb_films` for the old film's title, year, runtime and director;
   - `identity_traces` for each lost listing's take now and for every listing still holding the film;
   - `cinema_scrapes` for the venue's own facts (feed ids, original title, director, synopsis).
3. **Replay.** Each run below replays all 214 listing keys on recording 38713274617 (10-15). The recording holds 217 listings
   across 99 slices; it lacks 1 of the 98 films (UNDER|DOX Peacemaker).
   - `scripts/identity-slice de --batch <214 listing keys> --ref 89ced660a --ref 8b75b704b --recording 38713274617`
     covers the whole 10-14/15 range.
   - `--ref 8b75b704b --ref a744eff38` covers today's releases.
   - `--bisect 89ced660a..8b75b704b` was run on Das Auge des Bösen, a wrong loss whose log reason names no rule. It lands on `b88faf7ea`.
4. **Judging.** Each film was judged against `identity-judgements.tsv` where it has a row, and otherwise against the TMDB
   record vs the billing and the venue's page. Many of these takes were judged on 10-14/15 while the rules were written.

## Root causes, by rule and commit

"Code?" in the table says whether the slice moves the listing between `89ced660a` and `8b75b704b` on the 10-15 recording:

- **yes**: the code moves it.
- **same**: both refs take the same thing. The prod loss came from the boot's whole-corpus re-resolve or from changed input,
  not from a rule.
- **—**: the recording does not hold the listing.

| # | Rule (reason in the log) | Commit(s), build | Films | Correct (+cured) | Wrong | Undecided | Code? |
|---|---|---|---:|---:|---:|---:|---|
| R1 | page-feed copied title: "denied: named only by the title its feed (p:kinoheld…) copied" | `b88faf7ea` (Deny a namesake only a page feed's copied original title names…), in `2900ea1` 10-15 11:23 | 38 | 33 | 4 | 1 | yes, all |
| R2 | piece-beside: "no film: one piece of the billing names X — Y is billed beside" | `8e5d2d06d`, `83a8abd68` (and `b5e156555` for agreement takes, `138b55d52` for stagings), in `83a8abd` 10-14 17:33 | 31 | 28 + 1 | 2 | 0 | yes, except the 2 wrong (same) and Peacemaker (—) |
| R3 | festival billed by its own name; esports event | `dcc3eab6d` in `7db2cf5` (Alpen Film Festival, Freeride ×28); `NonFilmEvents` esports (older), surfaced at the `0690fe7` boot (League of Legends Worlds 26 ×63) | 3 | 3 | 0 | 0 | yes |
| R4 | a short under a feature's title: "untaken: a N-minute short, its title naming a feature-length film too" | `d2f919535` in `2900ea1`; `d2557b009` in `0690fe7` | 5 | 5 | 0 | 0 | yes |
| R5 | uncorroborated namesake | `e40e9a4af`, `b253a14e7`, `a9dc7a2d7`, in `0690fe7` 10-15 15:01 | 1 | 0 + 1 | 0 | 0 | yes |
| R6 | a cineamo feed's copied title or short pull | `a88f373c6`, `8c9d857bf`, `c1ef3a56c`, `b951ffa46`, in `ec1e55a` 10-14 20:07 (Ray Wilson, Imagine Peace at later boots) | 5 | 5 | 0 | 0 | yes |
| R7 | silent billing, sound-era film | `94a47eaba` in `7db2cf5` | 1 | 1 | 0 | 0 | yes |
| R8 | cluster cannot-link / join veto: "kept apart from … denies-film / Learned(…below the cannot-link cut)", "joined by same-title / same-catalogue-id / title-segment" | no rule commit for the wrong ones: losses at boots of `5cb1a27` (no identity commit since `44af4e28a`), `0d7c789` (agreement refactor), `6d6e108`, `ec1e55a`, `2900ea1` | 10 | 5 + 2 | 2 | 1 | mixed: Unterbiberger, Chasing Hope, Bin ich das Arschloch?, Christl Sittenauer, Pink Power yes; Blutgräfin, Catching Fire, Blade Runner, BERGERS, Drishyam same |
| R9 | below threshold / no candidate (other) | `1b59ae8` boot (Life Support), `26e4889` (Kamino; `26e48897d` was reverted in `22361d8bb`), `0690fe7` (Friday the 13th: Legacy), `0d89de4` (Lynyrds Frynds) | 4 | 2 | 1 | 1 | same, except Friday the 13th (yes) |
| | **total** | | **98** | **82 + 4** | **9** | **3** | |

### The wrong losses

| Film | Listing (venue) | Rule | Code? | Now (mirror 13:2x) | Fix |
|---|---|---|---|---|---|
| 285025 Hin und Weg (2014) | HIN UND WEG D, ab 12 / ab 6 mit Begleitung (KiTT Tettnang) | R1: the kinoheld slug is the billing's own title | yes | **regained** 10-20 07:06 by `aa334f31d`; the slice moves it back between `8b75b704b` and `a744eff38` too | done |
| 67499 Mijn Opa de Bankrover (2011) | Mein Opa der Bankräuber (Filmmuseum München) | R1: the copied original shares only "Opa" with the German title | yes | lost; judged right, `accepted-right-loss` | R1 should count the record's own German title (absent from TMDB's `local`) or the director + runtime agreement as the billing's own evidence |
| 28978 Der Zirkus (1928) | ZIRKUS FUMEUSEMENT (Casablanca Zwickau, Filmmuseum München) | R1 | yes | lost; judged right, `accepted-right-loss` | accept (the title shares nothing with the film) |
| 3051 Eye of the Devil (1966) | Das Auge des Bösen (Filmfabrik KommKino, Filmhauskino) | R1: `b88faf7ea` (bisected over `89ced660a..8b75b704b`), shown as `joined by same-title`, cluster vetoed | yes | lost | **fix the judgement first**: `identity-judgements.tsv` has 3051 **wrong** for both venues, but the KommKino page bills original title "Eye of the Devil", J. Lee Thompson, Kerr/Niven/Tate, which is 3051 exactly. Flip to right, then the takes check will flag the loss |
| 1251608 Die Blutgräfin (2026) | Die Blutgräfin - The Blood Countess (Moviemento) | R8: learned listing-listing cannot-link to the venue's own "Blind Date Cinema: Die Blutgräfin" billing (cineamo 1251608m) | same (none at both refs) | lost (36 other DE listings take 1251608) | the learned cannot-link should not separate two billings at one venue when one carries a feed id naming the film the other's title names exactly |
| 101299 Catching Fire (2013) | The Tribute von Panem - Catching Fire (WA: 2026) (Scala Tuttlingen) | R8: learned cannot-link plus "event, not a film: live event" | same (none at both refs) | lost (89 other DE listings take 101299) | read "(WA: 2026)" (Wiederaufführung) as a re-release marker, not a live event or a year; `6d6e10830` added "Fassung"/"restaurierte" as edition words, so "WA" belongs there too |
| 1551429 Mission to Marsh (2025) | Mission to Marsh - Auftrag: Moor! (Capitol-Kino) | R2 | same (`8b75b70` takes it) | listing gone | piece-beside should keep a take when the "beside" piece is one of the record's own titles or alternative titles |
| 627671 Frauen bildet Banden (2019) | Frauen bildet Banden - Eine Spurensuche zur Geschichte der Roten Zora (Museum-Lichtspiele Tübingen) | R2 | same | **regained** 10-20 10:27 by `aa334f31d` | as above |
| 1643143 Life Support (2026) | Life Support (Babylon Mitte) | R9: BelowThreshold, best 1552707 at 35%, while the feed links cineamo 1643143m | same (none at both refs) | lost | honour a cineamo feed id that names a TMDB film whose title is the billing's exact title. The same gap is behind Das Versteck and Vorspiel in the release-losses doc |

The 2 R2 losses, the Blutgräfin, Catching Fire and Life Support are **not reproduced** by the code range on the 10-15
recording: `8b75b70` takes Mission to Marsh and Frauen bildet Banden, and both refs take none for the other three. They are
the same boot / whole-re-resolve or input effect seen for LOTR in
[the release-losses doc](2026-10-20-de-release-match-losses.md). They are not a rule landed on 10-14/15.

## Since 10-19, today's releases, and the TMDB backfill

- **Regained after 10-19 03:00**: 4 of the 98 films.
  - Hin und Weg and Frauen bildet Banden: right, regained by `aa334f31d`.
  - Tagebuch einer Kammerzofe (Kamino): undecided.
  - Nosferatu (426063) at Kulturbühne Versmold: regained 10-19 15:52 under `8b75b70`, but that is a **wrong** take. It is
    Graf von Bothmer's live score to the 1922 film, not Eggers' 2024 film.
- **`a744eff38` (~10:50)** vs `8b75b704b` on the same 214 keys moves exactly one listing: Hin und Weg, none → 285025, RIGHT.
  It cures 0, loses 0 and makes 0 new-wrong. `aa334f31d` and `a744eff38` restore nothing else of the 98.
- **TMDB backfill (genres / original_language / cast)**: it changes nothing here. None of the 98 losses was refused on cast,
  language or genre (no reason names them). The wrong ones are cluster, feed and piece decisions that read none of those fields.
  The backfill switches on the cast-mismatch guard (`784d6da76`), which can only refuse more.

## Recommended fixes, ranked by films recovered

1. **Fix the R1 copied-title denial where the copy is a literal translation or the record's own title (2 films still lost: Das Auge des Bösen, Mein Opa der Bankräuber; Hin und Weg already back).** The billing "Das Auge des Bösen" translates the copied original "Eye of the Devil", whose German TMDB title is "Die schwarze 13". "Mein Opa der Bankräuber" translates "Mijn Opa de Bankrover". In both, the director and runtime agree. Let a director + runtime (or year) agreement count as the billing's own evidence against the denial. Then re-measure with `scripts/identity-measure-local`, because the rule's 33 correct refusals (concerts, comedians, parties) must stay refused, and most of those carry a director copied from the wrong kinoheld page.
2. **Fix the R8 cluster vetoes between a venue's own billings (2 films: Blutgräfin, Catching Fire WA; recurring).** Both
   are a learned listing-listing cannot-link splitting two billings of one film, and Catching Fire's is also read as a live
   event. Reproduce on a recording newer than 10-20 (none is replayable yet; see the release-losses doc's limits) with
   `--orders 12`. Then pin each with `identity-slice --emit-case`. Add "WA"/"Wiederaufführung" to the edition words.
3. **Correct the judgements, so the gates stop hiding a wrong loss and stop calling a correct refusal LOST.**
   - Flip `de Filmfabrik KommKino / Filmhauskino "Das Auge des Bösen" tmdb:3051` from wrong to **right**.
   - Flip `de * "ALPEN FILM FESTIVAL 2026" / "ALPENFILMFESTIVAL…" tmdb:1625266` (a 13-minute short "How I Started A Film
     Festival") from right to **wrong**. The festival refusal is correct, but the slice counts it as LOST.
   - Review `KAP-Torgau "Susanne Siegert" → 182771` (Ich bin Susanne, 1933).
4. **R2 piece-beside: keep the take when the beside piece is the record's own subtitle or alternative title (2 films).**
   Frauen bildet Banden is already back. Mission to Marsh will matter when it is next billed. This is the same fix the release-losses doc asks
   for Über Unterbiberger and Ben'Imana, so it recovers more than these 2.
5. **Honour a feed id that names a film of the billing's exact title (1 film: Life Support).** It shares a cause with Das
   Versteck and Vorspiel (release-losses doc).
6. **Unpick the wrong regain**: Nosferatu at Kulturbühne Versmold (426063, 2024) should be the 1922 film or none.

No change is recommended to R3–R7. Every film they refused is a correct refusal.

## All 98 films

Columns:

- **Old take**: the TMDB record the listing lost.
- **n**: the lost listings.
- **Lost**: the first lost line and its build.
- **Code?**: as above.
- **Class**: C correct, K correct and now cured, W wrong loss, U undecided.
- **Now**: the lost listing's take in the mirror, or the regain.

| TMDB | Old take | Listing @ venue | n | Lost | Rule | Commit | Code? | Class | Now / note |
|---|---|---|---:|---|---|---|---|---|---|
| 1068427 | Lapua 1976 (2023, 114m) | (1976) @ Metropol Düsseldorf | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | listing gone |
| 515576 | Oni žive (2018, 29m) | 11FREUNDE live @ UCI Kaiserslautern | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | listing gone |
| 38142 | 5 Centimeters per Second (2007, 62m) | Abdul Kader Chahin @ Kino in der Lagerhalle +1 | 2 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 16026 | Sällskapsresan eller Finns det svenskt kaffe på grisfesten (1980, 107m) | Andalucia Tour Flamenco @ Kurbelkiste | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1184012 | opium_latte raw studio sessions vol. 1 (2023, 6m) | Barn Bop Session Vol. I @ Kino im Druckluft | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 423739 | Ad-vientu (2016, 20m) | Bergstadt-Advent @ KNUP – Soziokulturelles Zentrum Oerlinghausen | 1 | 10-15 15:01 `0690fe7` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1425829 | Cléo se Fait des Films (2025, 17m) | BODYROCK Festival 2027 - Loud and Unleashed @ Kino in der Lagerhalle | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1405954 | Bastard Toadflax (2025, 12m) | Bucking Fastard @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 988336 | Cœur de slush (2023, 116m) | COEUR DE PATATE @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 3051 | Die schwarze 13 (1966, 96m) | Das Auge des Bösen @ Filmfabrik KommKino +1 | 2 | 10-15 11:23 `2900ea1` | R1 page-feed copied title (via same-title join; bisected) | b88faf7ea | yes | W | Vetoed; judged wrong, but the venue page bills Eye of the Devil, J. Lee Thompson, Kerr/Niven/Tate: the label is wrong |
| 264535 | در غربت (1975, 91m) | Der Fremde klub Kurbelkiste @ Kurbelkiste | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1523917 | Üçleme (2025, 10m) | Die Hobbit Triologie Extended! @ Filmwelt Grünstadt | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 25869 | Salaam-E-Ishq (2007, 201m) | Dirty Deeds - A Tribute To AC/DC @ Kino in der Lagerhalle | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 48579 | La pretora (1976, 98m) | Doppelkonzert: My Sister Grenadine & Mai Ki @ Raststätte | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 414692 | Fuk It (2016, 7m) | FUK @ KNUP – Soziokulturelles Zentrum Oerlinghausen | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 305291 | Сад (2008, 96m) | Garten-Tag @ Kino im Druckluft | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1291458 | Abá e Sua Banda (2025, 84m) | Gitte Haenning & Band @ Casablanca Zwickau +1 | 2 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 728816 | Gameboys: The Movie (2021, 107m) | Glueboys @ Kaminwerk | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1654043 | Heilig Schuim (2026, 60m) | Heilige Cnaipe @ KNUP – Soziokulturelles Zentrum Oerlinghausen | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 285025 | Hin und Weg (2014, 95m) | HIN UND WEG D, ab 12 / ab 6 mit Begleitung @ KiTT - Kino und Kleinkunst Tettnang e.V. | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | W | regained 10-20 07:06 aa334f3; title exact, runtime and director agree; the kinoheld slug is the billing's own |
| 1749361 | Magnaci i Czarodzieje (2021, ?m) | Hiss und Dr. Will & The Wizards @ Filmforum im Stadttheater Landsberg | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 463791 | Catastrophe (2017, 2m) | Katastrophe! Katastrophe! @ Filmforum im Stadttheater Landsberg | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 431564 | Alligatoah - Livemusik ist keine Lösung (2016, 126m) | Kleine KUURHUUS Musik @ Inselkino Spiekeroog | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 748248 | 80.000 Schnitzel (2020, 105m) | Kürbisse schnitzen @ Das Haus | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1329147 | Vive e Deixa Andar (2024, 107m) | La fjella leve \| Let Our Mountains Live @ Kulturhaus Caserne Kino Studio 17 | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1375213 | LA. LA. End (2024, ?m) | LA MANÉKINE @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1368438 | Die Macht der Mikroben (2024, 95m) | LIVE Les Amoureuses @ CLUB PASSAGE | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 724024 | Ma Petite Sophie (2018, 99m) | MA P’TITE DAME @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 67499 | Mijn Opa de Bankrover (2011, 90m) | Mein Opa der Bankräuber @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | W | Vetoed; judged right, accepted-right-loss: the Dutch film's German title, director and runtime billed |
| 494407 | Pokémon 21: Die Macht in uns (2018, 97m) | Power to The Popel Philipp Weber @ Kurbelkiste | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1113975 | Die Satelliten-Schlacht (2023, 53m) | Rymden @ Filmforum im Stadttheater Landsberg | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 14415 | Séraphine (2008, 125m) | SERAPHINA KALZE @ KAP-Torgau e.V. | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 267 | Live Flesh - Mit Haut und Haar (1997, 97m) | Silvesterparty - mit Live Band & Disco @ Kino in der Lagerhalle | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | listing gone |
| 182771 | Ich bin Susanne (1933, 98m) | Susanne Siegert @ KAP-Torgau e.V. | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | U | Vetoed; judgements say right; the billing reads as a person (reading/talk), so the label is doubtful |
| 936744 | Carga animal (2023, 19m) | Transport: DEPARTURE @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 1747965 | Кто поедет в Улан-Батор (2026, 70m) | Ulan & Bator: Überraschungszwei @ Kino in der Lagerhalle +1 | 2 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 28978 | Der Zirkus (1928, 72m) | ZIRKUS FUMEUSEMENT @ Casablanca Zwickau +1 | 2 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | W | listing gone; judged right, accepted-right-loss: Chaplin's The Circus as a live-score evening |
| 1189860 | Zum See (2023, ?m) | Zum Dorfkrug @ Kino in der Scheune Dresden | 1 | 10-15 11:23 `2900ea1` | R1 page-feed copied title | b88faf7ea | yes | C | Vetoed |
| 13189 | Charles Dickens' Weihnachtsgeschichte (1984, 100m) | Charles Dickens Weihnachtsgeschichte - präsentiert von Theat @ Kino in der Lagerhalle | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 4998 | Import/Export (2007, 135m) | Christian Schulte-Loh - Import Export @ Kino in der Lagerhalle | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 62001 | Explosion in Kuba (1979, 122m) | Cuba - Karibikträume und Sozialismus @ Cineplex Baden-Baden | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 238511 | Die Liebe ist ein seltsames Spiel (1961, 101m) | Die Liebe ist ein seltsames Spiel - eine Schlagerreise mit R @ Forum 2 - Kulturverein Olympiadorf e.V. | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | listing gone |
| 7305 | Überleben! (1993, 121m) | Dio - Alive - eine Hommage an Ronnie James Dio @ Kino in der Lagerhalle | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 760104 | X (2022, 105m) | DONNERSTAGSKNEIPE + X @ AJZ Kino | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 466081 | The Informer (2019, 109m) | Film+Musikfest: Arthur Robison: THE INFORMER @ Lichtwerk im Ravensberger Park | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | Vetoed |
| 627671 | Frauen bildet Banden (2019, 77m) | Frauen bildet Banden - Eine Spurensuche zur Geschichte der R @ Museum-Lichtspiele Tübingen | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | same | W | regained 10-20 10:27 aa334f3; the subtitle is the film's own |
| 426063 | Nosferatu - Der Untote (2024, 133m) | GRAF VON BOTHMER: NOSFERATU - mit Chor @ Kulturbühne Versmold | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | regained 10-19 15:52 8b75b70 |
| 9524 | Comedian Harmonists (1997, 124m) | Jüdische Filmwoche: Comedian Harmonists - Sechs Lebensläufe  @ Toni und Tonino | 2 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 1126705 | Kleine Eheverbrechen (2023, 90m) | Kleine Eheverbrechen - Thiele Neumann Theater @ Kino in der Lagerhalle | 1 | 10-15 15:01 `0690fe7` | R2 piece-beside (staging) | 138b55d52 | yes | C | BelowThreshold |
| 1644329 | Weites Meer (2000, ?m) | Korsika! Wilde Berge - Weites Meer @ Movie im Luitpoldhaus | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 1551429 | Mission to Marsh (2025, 78m) | Mission to Marsh - Auftrag: Moor! @ Capitol-Kino | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | same | W | listing gone; German release title of the 2025 Kornelsen doc is 'Mission to Marsh – Auftrag: Moor!' |
| 964 | Das Phantom der Oper (1925, 93m) | MUSICAL MITTWOCH - Das Phantom der Oper @ Xinedome | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 9023 | Spirit - Der Wilde Mustang (2002, 80m) | Mustang - Ride the Forbidden Kingdom @ Prestige | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 83770 | On the Road - Unterwegs (2012, 124m) | On the Road - En el Camino @ Cinema &amp; Kurbelkiste Münster | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | K | takes 1517123; now 1517123 En el camino (2026), right |
| 1010170 | Patagonia (2023, 110m) | Patagonien – Durch die Wildnis bis nach Feuerland @ Magazin | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 1265827 | Retro (2025, 150m) | Retro - An Undiscovered World, Running High, Carrying Dreams @ Apollo Aachen | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 447273 | Schneewittchen (2025, 109m) | Schneewittchen - Frei nach den Brüdern Grimm @ Kino in der Lagerhalle | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 5825 | Schöne Bescherung (1989, 97m) | SCHÖNE BESCHERUNG - KAMINERS WEIHNACHTEN @ Kammerspiele Kleinmachnow | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 527625 | So oder so ist das Leben (1976, 100m) | SO ODER SO IST DAS LEBEN – EINE HOMMAGE AN HILDEGARD KNEF @ Lichtspieltheater Willich | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 1246049 | Dracula - Die Auferstehung (2025, 129m) | STEFAN JÜRGENS live im Beluga Kino: DRACULA - Eine Literatur @ Beluga Kino | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 206656 | Richard Wagner (1913, 102m) | Tannhäuser - Richard Wagner @ Stadtkino Trostberg | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 51423 | Bessere Zeiten (2010, 99m) | ULLA MEINECKE - BESSERE ZEITEN! @ Beluga Kino | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 71456 | Under Pressure (1997, 84m) | Under Pressure - Ein tiefer Riss durch die USA @ Metro-Kino im Schloßhof +1 | 2 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 6623 | Projekt: Peacemaker (1997, 124m) | UNDER\|DOX: PEACEMAKER – MIROTVORAC @ Werkstattkino e.V. | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | — | C | listing gone |
| 969572 | Im Fluss (2008, 6m) | Urban Priol - „Im Fluss – täglich quellfrisch, immer aktuell @ Kino im Waldhorn | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 316859 | Geht nicht gibt's nicht (2002, 90m) | Verrückt nach dieser Welt - Geht nicht gibt's nicht @ Eden Palast Aachen | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 76706 | Weihnachtspost (2010, 90m) | WEIHNACHTSPOST - SUZANNE VON BORSODY @ Saalbau-Lichtspiele Weilmünster | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 639563 | Wie immer (2010, 7m) | WIE IMMER – NUR ANDERS! Ein Experiment an einem ganz normale @ Eschborn K | 1 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | BelowThreshold |
| 1585701 | La vie de Nicolas (2025, ?m) | „MIT ALLES! EXTRASCHARF! – WILLICH EDITION” - THOMAS NICOLAI @ KAP-Torgau e.V. +1 | 2 | 10-14 17:33 `83a8abd` | R2 piece-beside | 8e5d2d06d/83a8abd68 | yes | C | NoCandidate |
| 1625266 | How I Started A Film Festival (2026, 13m) | Alpen Film Festival 2026 @ Alpenfilmtheater Füssen +9 | 10 | 10-15 01:53 `7db2cf5` | R3 festival billed by name | dcc3eab6d | yes | C | Vetoed; judgements say right (*), but a festival programme is not the 13-min short: flip the label |
| 1686112 | IA, au cœur du cinéma (2026, 52m) | FREERIDE FILMFESTIVAL 2026 CINEMA EDITION @ Atelier Kino im Savoy-Theater +27 | 28 | 10-15 01:53 `7db2cf5` | R3 festival billed by name | dcc3eab6d | yes | C | Vetoed |
| 1552059 | League of Legends Worlds25 - Finals in Cinema (2025, 420m) | League of Legends Worlds 26 - Finals in Cinema @ Adria Filmtheater Steglitz +62 | 63 | 10-15 15:01 `0690fe7` | R3 event: esports | NonFilmEvents (pre-range), at 0690fe7 boot | yes | C | BelowThreshold |
| 42067 | 1001 Nights (1998, 23m) | 1001 Nights - A Thousand & One Nights @ Colosseum-Center | 1 | 10-15 11:23 `2900ea1` | R4 short under a feature title | d2f919535 | yes | C | listing gone |
| 1193753 | Gina (2022, 7m) | GINA - mit der Domschule Würzburg @ Central im Bürgerbräu | 1 | 10-15 11:23 `2900ea1` | R4 short under a feature title | d2f919535 | yes | C | BelowThreshold |
| 865342 | Nuisance Bear (2021, 15m) | Nuisance Bear @ Filmmuseum München | 1 | 10-15 11:23 `2900ea1` | R4 short under a feature title | d2f919535 | yes | C | BelowThreshold |
| 1029804 | The Community (2022, 4m) | THE COMMUNITY @ Kommunales Kino - Im Alten Wiehrebahnhof | 1 | 10-15 11:23 `2900ea1` | R4 short under a feature title | d2f919535 | yes | C | listing gone |
| 1103660 | A Menos que bailemos (2023, 14m) | UNLESS WE DANCE - QUEERE FILME ZUR LÄNGSTEN NACHT @ Caligari FilmBühne Wiesbaden | 1 | 10-15 15:01 `0690fe7` | R4 short under a feature title | d2557b009 | yes | C | BelowThreshold |
| 788385 | Lieblingsmenschen (2021, 65m) | Lieblingsmenschen @ CineStar Berlin - Kino in der Kulturbrauerei +2 | 3 | 10-15 15:01 `0690fe7` | R5 uncorroborated namesake | e40e9a4af/b253a14e7 | yes | K | takes 1185542; now 1185542 (Hufnagel 2026), right |
| 1094789 | Imagine Peace (2010, 40m) | Imagine Peace (GLOBALE) @ Kinocenter Giessen | 1 | 10-15 11:23 `2900ea1` | R6 short as feed pull | c1ef3a56c/b951ffa46 | yes | C | Vetoed |
| 9785 | Unsichtbar - Zwischen zwei Welten (2007, 106m) | Invisibles @ Kommunales Kino im Künstlerhaus | 1 | 10-14 20:07 `ec1e55a` | R6 cineamo copied title | a88f373c6/8c9d857bf | yes | C | Vetoed |
| 346681 | Wilson - Der Weltverbesserer (2017, 95m) | RAY WILSON @ Centraltheater Brake | 1 | 10-15 15:01 `0690fe7` | R6 cineamo copied title | a88f373c6/8c9d857bf | yes | C | Vetoed |
| 10946 | Unsere Erde (2007, 95m) | Terra @ Magazin | 1 | 10-14 20:07 `ec1e55a` | R6 cineamo copied title | a88f373c6/8c9d857bf | yes | C | Vetoed |
| 370963 | Silent Rebellion (1982, 90m) | À BRAS-LE-CORPS (Stille Rebellion) – Schweizer Filmwoche @ Theatiner Film | 1 | 10-14 20:07 `ec1e55a` | R6 cineamo copied title | a88f373c6/8c9d857bf | yes | C | Vetoed |
| 1152327 | Charlie Chaplin (1999, ?m) | STUMMFILM MIT TOBIAS RANK:CHARLIE CHAPLIN @ Union Filmtheater Friedrichshagen | 1 | 10-15 01:53 `7db2cf5` | R7 silent billing, sound film | 94a47eaba | yes | C | Vetoed |
| 1692284 | Shepherds (?, 5m) | BERGERS (Hirten) @ Theatiner Film | 1 | 10-14 20:07 `ec1e55a` | R8 cluster cannot-link / join veto | ec1e55a boot | same | C | listing gone |
| 77691 | Das Kaninchen bin ich (1965, 110m) | Bin ich das Arschloch? @ Kino in der Scheune Dresden | 1 | 10-15 11:23 `2900ea1` | R8 cluster cannot-link / join veto | 2900ea1 boot | yes | C | Vetoed |
| 19333 | Die Brut des Teufels (1975, 83m) | Chasing Hope Der andere Blick @ Metropol Düsseldorf | 1 | 10-15 11:23 `2900ea1` | R8 cluster cannot-link / join veto | 2900ea1 boot | yes | K | takes 1786543; now 1786543 Chasing Hope (2026), right |
| 615 | Die Passion Christi (2004, 127m) | Christl Sittenauer @ Filmstudio Glückauf +1 | 2 | 10-15 11:23 `2900ea1` | R8 cluster cannot-link / join veto | 2900ea1 boot | yes | C | Vetoed |
| 1251608 | Die Blutgräfin (2026, 119m) | Die Blutgräfin - The Blood Countess @ Moviemento | 1 | 10-14 14:19 `5cb1a27` | R8 cluster cannot-link / join veto | 5cb1a27 boot | same | W | Vetoed; real film: the venue's own Blind Date billing links cineamo 1251608m; 36 other DE listings take it |
| 1269325 | Drishyam: The Conclusion (2026, 170m) | Drishyam 3 @ CinemaxX Essen +1 | 2 | 10-15 12:53 `2900ea1` | R8 cluster cannot-link / join veto | 2900ea1 boot | same | U | listing gone; two 2026 Drishyam 3s (Hindi, Malayalam); listing gone |
| 335984 | Blade Runner 2049 (2017, 164m) | Kult-Special: Blade Runner - The Final Cut @ Hall of Fame Kamp-Lintfort | 1 | 10-14 14:57 `6d6e108` | R8 cluster cannot-link / join veto | 6d6e108 boot | same | C | Vetoed |
| 1214558 | Power (2024, 88m) | Pink Power @ Cine K Oldenburg +5 | 6 | 10-14 21:33 `ec1e55a` | R8 cluster cannot-link / join veto | ec1e55a boot | yes | K | takes 1499152; now 1499152 Pink Power (2025), right |
| 101299 | Die Tribute von Panem – Catching Fire (2013, 146m) | The Tribute von Panem - Catching Fire (WA: 2026) @ Scala Tuttlingen | 1 | 10-14 16:58 `0d7c789` | R8 cannot-link (learned) + live-event | none in range (0d7c789 refactor boot) | same | W | Vetoed; re-release (WA) of Catching Fire; 89 other DE listings take 101299 |
| 697 | Oktober 1917 – Zehn Tage, die die Welt erschütterten (1928, 94m) | Über Unterbiberger - Vom Woid in die Welt @ Cinecitta | 1 | 10-15 11:23 `2900ea1` | R8 cluster cannot-link / join veto | 2900ea1 boot | yes | C | Vetoed |
| 1556121 | Legacy (?, ?m) | Friday the 13th: Legacy @ Casablanca Berlin | 1 | 10-15 15:01 `0690fe7` | R9 below threshold | 0690fe7 | yes | C | listing gone |
| 45212 | Tagebuch einer Kammerzofe (1946, 91m) | Le Journal d'une femme de chambre - Tagebuch einer Kammerzof @ Kamino | 1 | 10-15 00:33 `26e4889` | R9 below threshold | 26e4889 | same | U | regained 10-20 10:27 aa334f3; French title is also Buñuel's 1964 film; venue gives no year/director |
| 1643143 | Life Support (2026, 90m) | Life Support @ Babylon (Mitte) +1 | 2 | 10-15 07:01 `1b59ae8` | R9 below threshold | 1b59ae8 | same | W | BelowThreshold; feed links cineamo 1643143m |
| 615097 | Lynyrd Skynyrd Helfest 2019 (2019, ?m) | LYNYRDS FRYNDS @ KAP-Torgau e.V. | 1 | 10-15 05:14 `0d89de4` | R9 no candidate | 0d89de4 | same | C | listing gone |
