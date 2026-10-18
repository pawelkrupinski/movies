# Identity: known wrong matches and open work

Status: snapshot of 2026-10-15 ~06:30 CEST. Wrong matches are what prod served at that time (local prod mirror,
all five countries fresh, lag 2–40 s) judged against `test/resources/fixtures/identity-judgements.tsv` at main
409391b8e. Fixes that landed or were queued after the snapshot are noted under "Open work". To refresh: re-run
the mirror × judgements tally (served = `identity_traces` keyed by a `screenings` listing with a future showtime;
judgement key = country, venue or `*`, normalised raw title, film; venue rows override `*`).

## 1. State

| cc | served | matched | unmatched | judged right | wrong, known | wrong, unlisted | unjudged | right film missing |
|---|---|---|---|---|---|---|---|---|
| PL | 10,541 | 9,295 | 1,246 | 579 | 0 | 0 | 8,716 | 72 |
| UK | 36,751 | 33,443 | 3,308 | 1,692 | 6 | 0 | 31,745 | 620 |
| DE | 33,248 | 30,850 | 2,398 | 540 | 33 | 28 | 30,249 | 131 |
| US | 96,681 | 95,450 | 1,231 | 867 | 2 | 0 | 94,581 | 399 |
| ES | 6,337 | 6,250 | 87 | 24 | 1 | 0 | 6,225 | 24 |

About 95% of served listings have never been judged, so the wrong counts are a floor. Wrong rows ever vs still
served: PL 270/0, UK 93/6, DE 211/36, US 79/2, ES 6/1 (105 PL wrong rows use filmweb/rt/cinemeta/wikidata ids
and can't be checked against a take).

## 2. Known wrong matches served

Classes:
1. Wrapper/banner/event text taken as the title.
2. Weak evidence: a bare title takes a namesake.
3. A copied fact, or one source counted many times.
4. A stage relay or live capture matched to the wrong production.
5. A short, or a runtime mismatch.
6. Other.

"Status" is the row's state/scope in identity-judgements.tsv. UNLISTED means prod serves a take labelled wrong that
no known-wrong/accepted row covers.

| cc | class | listings | billing | served (wrong) | right film | status | why it is wrong |
|---|---|---|---|---|---|---|---|
| DE | 1 | 2 | Pornfilmfestival: The more the merrier | Immer mehr, immer fröhlicher (1943) `tmdb:80616` | — | cured 10-18 (`ListingShape.festivalsCatalogueNamesake`, 45dce4d74; rows open, take none) | the festival's shorts programme presented by Paulita Pappel, not Stevens' 1943 comedy |
| DE | 1 | 1 | Mystery-Vorfilm als Warm-up zu „Miles Davis – Birth of The Cool“ | Miles Davis: Birth of the Cool (2019) `tmdb:566232` | — | known-wrong / served | a mystery warm-up short screened before "Miles Davis – Birth of the Cool", not the documentary itself |
| DE | 1 | 1 | DONNERSTAGS-KNEIPE | X (2022) `tmdb:760104` | — | known-wrong / served | a Thursday pub night ("Donnerstags-Kneipe") billing no film, not Ti West's "X" |
| DE | 1 | 1 | Tim Brownlow - Nosferatu: The Journal of a Broken Man | Nosferatu, eine Symphonie des Grauens (1922) `tmdb:653` | — | re-judged RIGHT 10-15 | a live score played in sync with Murnau's 1922 film (the venue's page: "Neuvertonung von F.W. Murnaus Stummfilmklassiker"), so the take is right |
| DE | 1 | 1 | UNLESS WE DANCE - QUEERE FILME ZUR LÄNGSTEN NACHT | A Menos que bailemos (2023) `tmdb:1103660` | — | known-wrong / served | a programme of queer films named after the 14-minute short "Unless We Dance", not that short |
| DE | 1 | 1 | SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE | 片袖の魚 (2021) `tmdb:797140` | — | known-wrong / served | a double bill (Shinjuku Boys + The Fish with One Sleeve): matches neither film |
| DE | 1 | 1 | GINA - mit der Domschule Würzburg | Gina (2022) `tmdb:1193753` | — | known-wrong / served | a school screening of "Gina", not the 7-minute Indonesian short of that name |
| DE | 2 | 3 | The Killer | The Killer (2024) `tmdb:970347` | — | no longer served 10-18, known-wrong / rows only (`ReleaseVeto.ofCurrent`: TMDB dates the 1989 film's German re-release 2026-10-29, the remake only a digital German release) | the 4K restoration of John Woo's 1989 The Killer (tmdb 10835, which 8 venues take); the 2024 remake was streaming-only |
| DE | 2 | 3 | Lieblingsmenschen | Lieblingsmenschen (2021) `tmdb:788385` | — | known-wrong / served | at CineStar it is Hufnagel's Lieblingsmenschen – Agnes & Amir (tmdb 1185542, 23 venues), not the 65m 2021 indie |
| DE | 2 | 1 | München Mord: Leben und Sterben in Schwabing | Der beste Papa der Welt (2019) `tmdb:655654` | — | known-wrong / served,rows | the "München Mord" TV episode "Leben und Sterben in Schwabing" (the venue's synopsis), not Sascha Bigler's "Der beste Papa der Welt" |
| DE | 2 | 1 | Auf Augenhöhe - Jugend, Macht und Kinderschutz | Auf Augenhöhe (2016) `tmdb:375111` | — | known-wrong / served | the venue's own programme lists a circus show (ost-passage-theater.de/spielplan, 18.10.2026), not the 2016 film |
| DE | 2 | 1 | Patagonien | Patagonia (2023) `tmdb:1010170` | — | known-wrong / served | a Patagonia travel-lecture show, not Simone Bozzelli's 2023 drama "Patagonia" |
| DE | 2 | 1 | Gegen das Vergessen - Weilheim in der Zeit des Nationalsozialismus | Gegen das Vergessen (2015) `tmdb:599503` | — | known-wrong / served | a local documentary on Weilheim 1921–1945 (Heribert Riesenhuber, the "Gegen das Vergessen" initiative), not Luigi Toscano's 2015 portraits |
| DE | 2 | 1 | Alpha (dir. Ducournau) | Alpha (2026) `tmdb:1122030` | — | known-wrong / served | the billing names Ducournau: her Alpha (tmdb 1284460, which Cineamo links), not Shiv Rawail's Indian Alpha |
| DE | 2 | 1 | THE BLACK HOLE | Das schwarze Loch (1979) `tmdb:9570` | — | known-wrong / served | CITY 46 bills Moonika Siimets' 2024 Estonian film The Black Hole (Globale Literaturfestival), not Disney's 1979 film |
| DE | 2 | 1 | RAY WILSON | Wilson - Der Weltverbesserer (2017) `tmdb:346681` | — | known-wrong / served | a concert by ex-Genesis singer Ray Wilson, not Craig Johnson's Wilson (2017) |
| DE | 2 | 1 | Friday the 13th: Legacy | Legacy (None) `tmdb:1556121` | — | known-wrong / served | a 96m Friday the 13th fan film; nothing to do with David Slade's undated Legacy record |
| DE | 2 | 1 | ES (ULRICH SCHAMONI) | Es (2017) `tmdb:346364` | — | known-wrong / served | Ulrich Schamoni's 1966 "Es", not Andy Muschietti's 2017 "It" |
| DE | 3 | 1 | Imagine Peace (GLOBALE) | Imagine Peace (2010) `tmdb:1094789` | — | known-wrong / served | a GLOBALE documentary series slot: Chiquet's 2026 doc Imagine Peace (tmdb 1604464, which Cineamo links), not the 2010 40-minute short |
| DE | 3 | 1 | Über Unterbiberger - Vom Woid in die Welt | Oktober 1917 – Zehn Tage, die die Welt erschütterten (1928) `tmdb:697` | tmdb:1559083 | known-wrong / served | Ditscherlein's 2025 doc on the Unterbiberger Hofmusik, not Eisenstein's October (1928), whose facts the feed copied |
| DE | 3 | 1 | Ecrire la vie - Writing Life | Giv os en chance (2025) `tmdb:1579323` | tmdb:1518673 | known-wrong / served | Écrire la vie (as its other billings, labelled wrong), not the Danish Giv os en chance |
| DE | 4 | 23 | League of Legends Worlds 26 - Finals in Cinema | League of Legends Worlds25 - Finals in Cinema (2025) `tmdb:1552059` | — | cured on main and in prod (8b75b70 takes none, 10-18; `numeralRelation` reads the billing's own "26" over a feed's copied "Worlds25", 88a82446a) | the 2026 League of Legends Worlds final, not the 2025 one's record (as PL's and UK's labels say) |
| DE | 4 | 5 | Fallen Angels by Noël Coward | Gefallene Engel (1995) `tmdb:11220` | tmdb:1702350 | **UNLISTED** | a stage capture of the Roundabout Broadway revival of Coward's play (Trafalgar, in cinemas 22 Oct 2026), not Wong Kar-wai's 1995 film — one line for every venue billin… |
| DE | 4 | 2 | Carmen (RB+O 2026/27) | Carmen: Salzburger Festspiele 2026 (2026) `tmdb:1740965` | — | known-wrong / served | "RB+O 2026/27" bills the Royal Ballet & Opera Carmen (tmdb 1702759, which Cineamo links), not the Salzburg Festival 2026 recording |
| DE | 5 | 1 | Leica | Leica (2026) `tmdb:1737142` | — | known-wrong / served | Holzemer's feature doc Leica (tmdb 1689091, which prod takes), not a 15-minute Israeli short |
| DE | 5 | 1 | THE COMMUNITY | The Community (2022) `tmdb:1029804` | — | known-wrong / served | Cineamo links tmdb 1450062, The Community (2025, 64m doc); the take is a 4-minute short |
| DE | 5 | 1 | The Stranger | Der Fremde (2025) `tmdb:1429348` | — | known-wrong / served | Filmmuseum München's 95m The Stranger is Welles' 1946 film (tmdb 20246), not Ozon's 122m L'Étranger |
| DE | 5 | 1 | Island | Island (2023) `tmdb:1192466` | — | known-wrong / served | an evening feature slot (Gröbenzell's own catalogue id cineamo 63585 is the 2011 feature "Island"), not the 3-minute short "Island" |
| DE | 6 | 1 | Kohlhiesels Töchter | Kohlhiesels Töchter (1920) `tmdb:175204` | tmdb:79252 | known-wrong / served | wrong by the right film the labels name at * |
| UK | 1 | 1 | SISTER ACT - NORTHALLERTON MUSICAL THEATRE COMPANY | Sister Act (1992) `tmdb:2005` | — | known-wrong / served | Northallerton Musical Theatre Company's stage production of the musical, not the 1992 film |
| UK | 1 | 1 | Gremlins Special Event | Innerspace (1987) `tmdb:2614` | — | known-wrong / served | a Gremlins screening event; Innerspace was taken by its director walk and the event's 120m with title=none, the title naming another Dante film |
| UK | 2 | 1 | Social Reckoning | The Trial of the Chicago 7 (2020) `tmdb:556984` | — | known-wrong / served | Aaron Sorkin's "The Social Reckoning" (the venue's synopsis: Frances Haugen), not "The Trial of the Chicago 7" |
| UK | 2 | 1 | About Time: Once Upon a Time (15) | Once Upon a Time… All About My Mother (2012) `tmdb:640160` | — | known-wrong / served | John Berger's 1985 series About Time, episode Once Upon a Time (Watershed's Berger at 100 season), not the 2012 Almodóvar doc |
| UK | 4 | 1 | Ballet - Notre-Dame de Paris | La Scala Ballet: Notre-Dame de Paris (2013) `tmdb:790330` | — | known-wrong / served | the Paris Opera's Roland Petit Notre-Dame de Paris (Pathé Live, recorded Dec 2025), not the 2013 La Scala production |
| UK | 4 | 1 | Opéra de Paris: Notre-Dame De Paris | Notre-Dame de Paris (Opéra Bastille) (2021) `tmdb:973305` | — | known-wrong / served | the Paris Opera's Dec 2025 recording (in cinemas 14 Mar 2027), not the 2021 Opéra Bastille record |
| US | 3 | 2 | NT Live: Hamlet | National Theatre Live: Hamlet (2015) `tmdb:396227` | tmdb:1507580 | known-wrong / served | both venues billing it (Burns Court Sarasota, Film Streams Omaha) read one Flicks page (id 3058) still carrying Cumberbatch's 2015 year and runtime, screening 24 Nov-2… |
| ES | 2 | 1 | ESCAPE ROOM 2 | Escape Room 2: Mueres por salir (2021) `tmdb:585216` | — | known-wrong / served | at a Spanish multiplex in October 2026 it is Joel Joan's Catalan Escape Room 2 (tmdb 1573952, 61 venues), not the 2021 US sequel |

## 3. Open work (TODO)

### Landing queue (landed 2026-10-15)
- [x] **Arsenal Berlin client** — landed a2e50ec85..f58b71add (measure 38708378594: 0 moves, as the recording held only
  Cineamo's Arsenal rows; recording 38713274617 green). Kazahana is known-wrong (served) until a recording reads Arsenal's
  own calendar. Its cures (Kaidan, Ototo, Ningen no joken II/III, Kazahana) show only in a recording taken after prod
  deploys the client — check then and strike the Kazahana row. Prod (mirror, 10-15 15:00) reads the calendar: Kazahana
  now takes none (Kinoshita's 1959 169401 at 55%, below the cut), Mozu 593128 and West Indies 271858 are taken, unjudged.
- [x] **a083-fix** — landed 9f9b41682..6abe17876 (measure 38737196525, LANDABLE: PL 1 cure, UK 2 cures + 1 gained right,
  DE 4 cures + 6 gained right, 0 right lost, 0 new wrong; convergence 38749834547 green). Its factless-short rule was
  dropped in the rebase (main's ShortFilms.untaken already cures GINA and THE COMMUNITY). It does NOT move DE Fallen
  Angels: the current recording already takes 1702350 (right).
- [ ] Then re-run the state tally (§1) on the mirror once prod has deployed.

### Live wrong matches with no fix yet
- [x] **DE "League of Legends Worlds 26 - Finals in Cinema" ×23** → the 2025 final record. Class 4/event: a yearly
  event must match its own year's record, or none. Cured by 88a82446a (a feed's copied original "Worlds25" never numbers
  the billing's "26" its way; `numeralRelation`), pinned by the identity-slices case
  `de-adria-filmtheater-steglitz-league-of-legends-worlds-26-final-*`; prod (8b75b70) takes none at all 63 venues (10-18).
- [ ] **DE "Fallen Angels by Noël Coward" ×5** → Wong Kar-wai 1995. Unlisted. The current recording already takes the
  right 2026 record (1702350); check prod's ×5 after the next deploy (`scripts/listing-history de --title 'Fallen Angels'`).
- [ ] **NT Live: Hamlet ×2 (US)** — the 2026 record never enters the candidates; Flicks' 2015 year is counted once per
  venue reading the same Flicks page.
- [x] **ESCAPE ROOM 2 (ES)** — bare title, 2021 film at 57% vs the 2026 one backed by 63 venues. Cured by the evidenced-namesake refusal (`Corroboration.refused`); struck 10-18.
- [ ] **Lieblingsmenschen ×3, Black Hole, Stranger, Island, Unterbiberger, Alpha (dir. Ducournau), ES (ULRICH SCHAMONI)**
  (cured 10-18: Gegen das Vergessen — `BilledPiece` keeps no piece for a title in the venue's own language or one naming only
  its town; Ecrire la vie — `IdentityMeasures.billsWorksOfTwoTitles`; Friday the 13th: Legacy, earlier) — namesakes or copied
  facts; no safe rule yet.
- [x] **Pornfilmfestival "The more the merrier"** — the billing states no facts. Cured 10-18 by 45dce4d74
  (`ListingShape.festivalsCatalogueNamesake`: under a film festival's banner a billing stating no year, director or
  runtime names no film ten years or more older than its screenings, unless it names its director). Slice on
  38713274617: takes none (vetoed 80616); La Boum (Apollo's birthday banner, no festival) keeps tmdb:166 and
  "Hommage Rosa von Praunheim 2" keeps his 1983 film.
- [ ] **Notre-Dame de Paris (Opéra de Paris)** — TMDB has no record of the 2027 relay; the served takes are
  known-wrong.

### Structural fixes (one commit per class, each measured over the whole corpus)
Order by the live tally: 4 (relays, 33 of 70 live wrong listings), then 2, 3, 1, 5.
- [ ] **4. Relays as (work, house, season)**, read from the billing and the screening dates; another house's or
  another season's record is denied. Covers Penrith "RB&O 26/27" ×3 (accepted loss from 0cef7a16a; gap documented
  in `IdentityMeasures.stageWorkQueries`), NT Live Hamlet, Notre-Dame, League of Legends.
- [ ] **2. A take needs one independent corroborating fact** (year, director/cast, runtime band, feed id, or a strong
  same-title cluster); otherwise unmatched. Expect bare-title right losses; measure and present the trade.
- [ ] **3. Count each fact/vote once per origin** (page/feed/upstream venue), in SourceReliability.
  Partly done 2026-10-18 (`origin-pages`): a listings site's entry page counts once among corroborating venues; a
  feed's catalogue id and upstream venues still count per venue (the whole form lost 25 right takes: docs/identity-experiments.md).
- [ ] **1. Learned banners**: a segment one venue/feed bills beside many different works is a banner, peeled before
  search; existing ExtraTitleRules/title-words entries become fallbacks or go.
- [ ] **5. Runtime as strong negative evidence**: a short can't take a feature-length slot unless the feed links it
  or the billing says shorts/Kurzfilm.

### Scraper and title-parsing review (2026-10-15)
- [x] **`SearchTitles.uncertified` keeps its own `(15)`-only regex.** (done 2026-10-18: it reads
  `FormatTags.uncertified`; the Poole losses were `ListingShape.billedBeside` reading "Cert PG" as a piece, which only a
  recurring-banner exemption had hidden — it now peels the certificate first; local measure 0 moves) A raw spelling a listing keeps ("Digger - Cert
  15", "EOS - Monet (Cert. TBC)", "Hay Fever (12A) 160 mins") is searched with "Cert 15" as a candidate;
  `FormatTags.CertificateNote` already holds the shape. Fix: `uncertified` = `CertificateNote.replaceFirstIn(t, "$1").trim`
  (test: `SearchTitles.candidates("Digger - Cert 15", None) shouldBe Seq("Digger")`, fails before). Tried 2026-10-15:
  the new queries leave `UnmatchedClustersRatchetSpec` asking answers the recording lacks (uk "Double Bill: The
  Snowman (1982) / The Snowman and the Snowdog (2012)"), so it needs `integration.UnmatchedClustersCapture`
  re-captured and `scripts/identity-measure` LANDABLE first. `IdentityMeasures.searchQueries` (identity module) calls it too.
- [x] **Five UK BBFC value lists besides the `CERT` placeholder** (done: `services.movies.BbfcCertificate` owns them; the
  placeholder, Ticketsolve, Spektrix, BFI Southbank and the Gatsby parser read it; `SearchTitles` follows with the fix above): `TicketsolveClient.TitleCertificate`,
  `SpektrixClient.Bbfc`, `BfiSouthbankClient.BbfcBadge` (adds R18) and `SearchTitles`. One `BbfcCertificate` object
  should own the values and the placeholder derive from it.
- [x] **The same ingest note shapes are spelled twice** (done 2026-10-18: `TitleRulePlaceholders`' `ASTERISK_NOTE`,
  `RUNTIME_MINS`, `CERT_NOTE`, read by `FormatTags` and the rules alike; no `xtra-rerelease-bracket` exists, so
  `ReReleaseMark` has no copy to share with): `FormatTags` `AsteriskNote`, `BracketedRunningTime`,
  `ReReleaseMark` against `ExtraTitleRules` `xtra-asterisk-note-suffix`, `xtra-runtime-suffix`,
  `xtra-rerelease-bracket` (different widths: the rule takes `*[^*]+*` and a bare running time). Share them as
  `TitleRulePlaceholders` like `CERT`, then re-measure.
- [x] **`replaceAll("\\s+", " ").trim` is written 18 times across clients** (done: `tools.Whitespace.collapsedSpaces`,
  16 copies; NBSP still folded by its two callers alone — folding it for every client moves titles, a change of its own), two of them after an NBSP replace
  (`KinoZamekClient.collapse`, `VenueCredits`). One `TitleText.collapseSpaces` would also decide NBSP for the rest
  (Jsoup `.text` leaves U+00A0, which Java `\s` skips); that moves titles, so snapshot + measure.
- [x] **Hand-written bodies in client specs** (done 2026-10-16: Spektrix replays a Leeds Heritage recording, the one client
  billing `attribute_Website`; BFI three real `section#shows` pages — a booked film, a sold-out one, a Reuben Library talk;
  Ticketsolve Mwldan's recorded "Sonic Sounds (18+)" show, its category blanked: no feed bills an uncategorised one yet)

### Parked resolver work
- [x] **review-fixes leftovers** (branch `review-fixes`, 4 days old; six commits ported 2026-10-18, local measure 0 moves):
  - [x] `f4796e608`, landed 2026-10-18 as the broadcast-season record alone pre-empting the pooled rules (local measure
    on recording 38713274617: UK 2 cures, 148 gained right, 0 lost — the Romeo and Juliet relays on their air day).
    Pooled stage-production was the harm: one member's stated year read as the cluster's broadcast split Savoy Worksop's
    "MET:Manon" off the Met's record and took Aldeburgh's Nutcracker as the 2025/26 one.
  - [x] `c5aa11025`, searching a relay's work with its banner's year ("Tosca 2027"): landed 2026-10-18 as `0256ddd92`,
    the captures filled offline with the new queries answered live (`5b96b2673`); local measure with `--live-gaps`: 0 moves.
  - [x] `d929894c0`, landed 2026-10-18: the house table, its derived initials and what bills a house moved to
    `services.movies.StageHouse` (common), read by the slot fold (local measure: 0 moves, snapshots unchanged).
- [x] **Item 8's certificate search** (done 2026-10-18, see the `SearchTitles.uncertified` entry) (branch `item8-search-fill`, dbf421550): the UK capture is filled, but the local
  measure loses Lighthouse Poole's eight "Family Cinema: <film> - Cert <X>" right takes (Paddington, WALL·E…): their new
  spellings ask family questions no recording or filed answer holds, so the same-film join to their siblings goes. Needs a
  recording (and prod's family answers) taken with the new queries.
- [x] **Withdrawn-vote rule** — not needed (10-18): e40e9a4af (`Corroboration.refused`, a bare take refused where
  a namesake its family evidences stands against it) cured IL KINO's Camouflage, which now takes Gibalenko's 1735950
  (right) via Filled; its slice case is a real `not:tmdb:56085` case. Re-applied on main the rule moves none of the
  title's 12 listings. Code kept as `experiment/withdrawn-vote` (8c666e431); entry in docs/identity-experiments.md.

### Review of the 10-12..15 identity changes (2026-10-15)
Each a production change: measure with `scripts/identity-measure`, land one at a time.
- [x] **billedBeside keeps differ by stage.** (done: `BilledPiece.of`, read by both; the agreement stage reads its
  `Keeps.Facts` only — the billing's stated facts, other venues stating them: the record keeps (current, untranslated) and
  the venue-word keeps took the families' namesake of the piece in the capture: CBeebies Panto's 2027 Treasure Island,
  Femme, Dave, Giorgia Fumo, 40 Jahre Lumière) The model's `IdentityResolver.billedAsPiece` keeps a take on six signals
  (≥5 venues, a stated fact, a feed link, a current release, an untranslated title, a recurring banner/tag); the
  agreement's `AgreementStage.pieceOf` calls `ListingShape.billedBeside` with none, so it denies agreed films the model
  keeps. One keep predicate, read by both.
- [x] **screenedSeason's earlier-year bypass is unbounded** (done 2026-10-18: a stated year more than a season before the
  screened one reads the season record only when a screening falls within 3 days of its release, `Listing.airDays` and
  `StageRelays.airsOnItsDay`; local measure on recording 38713274617: 0 moves) (8545f8cb7: `IdentityModel.screenedSeason`'s
  `year.forall(_ < season)`, `StageRelays.screenedSeasonRecord`): an encore of a 2019 capture screening in 2027 clears
  its year against the 2026/27 record. Bound it (a year or two), or ask for a screening on the record's air day.
  Tried 2026-10-16, a one-season bound (`year > season - 1` falls back to the stated year): NOT LANDABLE (measure
  38812504435) — DE "MET Opera: Macbeth (Verdi)" ×3 states 2014, its production's premiere twelve years before the
  2026/27 relay it rightly takes (right lost ×3, one new wrong), UK Aldeburgh's "The Royal Ballet: Alice's Adventures in
  Wonderland" [2024] turned to the 2024/25 record. A bound in years cannot tell an old capture's encore from a revival
  relayed live; the air-day test can: carry the listing's screening days into `IdentityMeasures.Listing` and keep the
  bypass only when a screening falls on the season record's release day. The same test would place UK Empire Theatre
  Halstead's bare "Romeo and Juliet" [170′] (screening on the RBO 2026/27 broadcast day) in the relay 1702789, not the
  families' Zeffirelli 6003 (movies-76, recording 38713274617). (The families' 6003 is cured since 10-18 by
  `Agreement.votesAnotherEntry`: the model's vote no longer joins Wikidata's bare record of the play; the listing now
  stays unmatched, the relay take still wants the air-day test.)
- [x] **FeedLink.deniesShort vs FamilyScope.namedOnlyByCopiedTitle** (done: folded into `namedOnlyByCopiedTitle` as its
  first case, unchanged: the same-year guard must not cover it — Kino Gelnhausen's "FILMKUNST: Ein einfacher Unfall",
  Panahi's 2025 film beside a 2025 short, IdentitySliceCasesSpec): the second covers most of the first's cases and
  has a same-year guard the first lacks; `deniesShort` still denies a short the venue's own billed title names.
- [x] **House initials are one house's list** (done: `StageHouses.initials` derives them from every house name of three
  initialled words; `ListingShape.billsAHouse` reads them through `StageRelays.billsAStageHouse`): `StageRelays.HouseInitials = Set("rbo","roh")` plus a regex spelling them
  again; `ListingShape.HouseWords` takes the bare tokens (and royal/live/met/hd/season), so "Jin-Roh" bills a house.
  Tag initials in the StageHouse word table and read them in one place.
- [x] **Search-tier copies of the ingest peels** (done 2026-10-18: one spelling each, FormatTags' width — the asterisk
  note letters-only, the running time taking "mins.", the certificate "(Certificate 15)" / "( 15 )"; the rule keeps a
  bare running time, FormatTags peels one only behind a certificate. `xtra-subtitle-paren-tag` stays its own: a
  bracket of nothing but napisy/dubbing/lektor and one word, where `FormatParenTag` takes any bracket holding a format
  word — not the same shape. FilmScheduleEndToEndSpec's corpus moved nowhere): `ExtraTitleRules`' `xtra-asterisk-note-suffix`, `xtra-runtime-suffix`,
  `xtra-certificate-suffix`, `xtra-certificate-year-suffix` (a3d3ece23) and `xtra-subtitle-paren-tag` repeat
  `FormatTags`' `AsteriskNote`/`BracketedRunningTime`/`CertificateNote`/`FormatParenTag` (1573686c3, a9b51f710) and
  disagree with them (any-character asterisks, a bare "100 mins"). Keep one.
  Tried 2026-10-16, deleting the five copies: they are load-bearing where another strip EXPOSES a note — `apiQuery`,
  the identity model's search title (`IdentityModel`'s `searchTitle`), runs no format re-peel, so "BARANEK SHAUN I
  KUDŁATA BESTIA (DUBBING) (PREMIERA)" kept "(DUBBING)" once "(PREMIERA)" went (SearchOnlyAffixesSpec). Share
  FormatTags' shapes with the rules as placeholders (as `CERT` is), and measure the widths they disagree on.
- [x] **TitleSeason reads any year range from 1800 as a season** (done: a listing's season must fall from 20 seasons before
  its first screening's (the relay era: encores bill 2017/18) to the next, `TitleSeason.of(titles, screenedIn)`; a
  record's stays any. Current-or-next only lost 5 US right takes and made 15 UK ones wrong: RBO 2024-25 encores): "Warschau 1944/45" gets a season, `SeasonsApart` then
  denies every film not dated 1944–45 and the listing reads as a relay. Bound it near `LatestTitleYear`.
- [x] **Venue-named rules**: `NonFilmEvents`' `dismember the alamo` (one chain's series), `xtra-helios-replay-suffix`
  (`w Helios RePlay`) — deleted: Helios's per-cinema rule and the learned `w helios replay` decoration cover the strand,
  `CandidateScoring.namesOnlyItsVenue` and the edition year the Alamo series.
- [x] **More venue-named global strips** in `ExtraTitleRules`: `xtra-kobiece-strands` (`TOMI dla Kobiet`, `Kobiece
  Wieczory w Kino Cafe`) and others naming one venue's strand. Each a learned decoration or a per-cinema rule; measure.
  Done 2026-10-18 for the four a venue name only decorates (Klasyka w kinie Atlantic, Wielka Sztuka w Kinoteatrze
  Rialto, LATO w LUNIE, Kobiece Wieczory w Kino Cafe: any venue's name now; local measure 0 moves). Left, the strand
  only existing under the name: xtra-pp-dzien-dziecka-apollo, xtra-filmoterapia-w-lunie, xtra-klasyka-cycles' w
  Kulturze / w NCKF, xtra-kinoteka-dla-rodzica, xtra-pipe-cycle-banner's Kinoteka, the three Kino Kadr rules,
  xtra-canonical-helios-na-scenie, xtra-wymiary-kina-plac-wilsona-prefix: each wants a per-cinema rule. (done
  2026-10-18: the search tier is venue-blind — keyed on a film's title, not a listing — and a PerCinema rule rewrites
  the slot's display and key, folding a decorated edition into the base film, so the strands keep their tier with the
  venue's name generalised to any venue's word: xtra-pp-dzien-dziecka-w-kinie, xtra-filmoterapia-w-kinie,
  xtra-klasyka-cycles' `w <venue>`, xtra-pipe-dla-rodzica, xtra-pp-venue-junior, xtra-venue-non-fiction-suffix,
  xtra-venue-colon-strand-suffix, xtra-wymiary-kina-na-prefix. Helios na Scenie went to the chain's per-cinema key
  (`xtra-helios-na-scenie`, any case; the seed `helios-suffix-3` already took the exact spelling, so the global
  Canonical fold never fired on a Helios listing). Left: xtra-pipe-cycle-banner's `Kinoteka` — a strand that only exists
  under the venue's name, which a per-cinema rule would fold into the base film, and `KinotekaClient` distrusts a
  decorated heading's year through `apiQuery` (the "Kolor purpury | Kinoteka jest Kobietą" 1985-vs-2026 case).)
- [x] **Two copies of one list**: ~~`FormatTags.FormatVersionWords` repeats `FormatToken`'s keys (and "lek" has no LEK
  badge: "Lalka 2D LEK" shows none)~~ (done: derived from `FormatToken`, "lek" badged LEK); ~~`TitleText.FiledArticles` (ein/eine, no gli) vs `IdentityMeasures.LeadingArticles`
  (gli, no ein/eine); `TitleText.stripPunct`'s own NFD strip drops "ł" where `TextNormalization.deburr` folds it~~ (done:
  `TitleText.Articles`, read by both; `stripPunct` folds through `deburr`).
- [x] **The silent-era denial reads two ways** (done: `ListingShape.silentBillingDenies`; the node and the listing it is
  asked of stay each stage's): `FamilyScope.denialByNode` if any listing of the node bills silent,
  `Agreement` per listing. One predicate.
- [x] `SourceReliability.tmdbLinked` is read without a37454e82's `numbersFilmsByTmdbId` gate in
  `IdentityResolver.billedAsPiece` and `SuspectTakes`; "the feed links one film" is defined twice (`UnifiedEvidence`
  counts the linked listings only, `FeedLink.linked` asks every listing). Check, then one definition. (done: both
  read TMDB's numbers only — a family's resolve decides no piece, `SuspectTakes` reads shipped takes — but
  `Corroboration.facts`' "feed id" ran ungated in a family's resolve, vouching for its film of the link's number; it now
  reads `FamilyScope.feedLinked`. One `FeedLink.linked(listings)`: the linked listings' one film, an unlinked listing
  silent, IdentityResolverCasesSpec.)
- [x] Denials are told apart by their text (`ReleaseVeto.vetoes`' `startsWith`): a typed denial (done: `Denial` with a
  `Kind` — the release veto carries the namesake's id, the cut and another house's season their own kinds).
- [x] `SuspectTakes.billsAnOldFilm` reads the film's own title words ("Silent Night" bills old); `billsAnEdition`
  excludes them. (done 2026-10-18: an old-film word of the film's own titles bills nothing; local measure 0 moves, 0 new suspects)
- [x] Test-only members still in main (closed 2026-10-18: the five deleted and `heldBytes` given a production reader
  are done; `atRest` stays, a domain predicate of the projection's public result, not private state; `keptIndex`, the
  `IncrementalResolver` three and `pooledOrder` stay as white-box reads of invariants no public behaviour exposes — a
  seam made only for them would be test code shaped into production) (the fakes `InMemoryIdentityTraceStore`, `InMemoryPinStore` moved to testkit 10-16; the
  members below read their class's private state, so moving them means a test seam of their own): `CachedTmdbDocuments.holdsAnswer`, `IdentityProjection.keptIndex`/`atRest`,
  `StoredTmdbLookups.heldDocuments`, `LiveCorpus.candidateCount`, `IncrementalResolver.familyKeys`/`familyNodeTexts`/
  `nodeTextOf`, `Acceptance.aloneOrder`/`pooledOrder`, `SourceReliability.FeedIdSources` (done: `holdsAnswer`,
  `heldDocuments`, `candidateCount`, `aloneOrder`, `FeedIdSources` deleted, their specs asserting through public reads —
  a read through asking the backend, the store read again, `candidate(id)` of every film an answer named,
  `refusals(Nil)`'s rule order, `feedStated`; kept: `keptIndex` (the kept index's equality with a fresh build, no public
  read), `atRest` (a pure predicate of public fields, no private state, read by worker test/it and e2e), the
  `IncrementalResolver` three (which key/text OBJECT the families hold, no public read), `pooledOrder` (no public read
  names the pooled rules); also test-only: `CachedTmdbDocuments.heldBytes` (done 10-18: replaced by `occupancies`, published
  as `kinowo_worker_cache_*{cache=tmdb_answers|tmdb_film_hits}` and charted, its spec asserting through it)); the fakes
  `InMemoryIdentityTraceStore`, `InMemoryPinStore`. The `Option(d.get(f)).filter(_.isArray)…` BSON read is written
  out 8 times beside `BsonFields` (done: `BsonFields.elementsOf` / `documentsOf`; the web review stores keep theirs, outside the package) (FamilyAnswerStore, AgreementCatalogue, AgreementPosters, Posters, AgreementVerdicts,
  IdentityTraceStore): a `BsonFields.documentsOf`. Removal needs identity-measure's 0 moves.
- [x] Judgements: ~50 venue rows repeat their `*` row (done: 51 dropped, the lint's `repeated` rule flags a new one;
  INNA MAMUSIA judged right as its other spelling; `tour`/`gira` stay in both concepts — a touring edition and a side event) (same film, verdict, state; e.g. Camp der Verlorenen, PL Lalka
  `imdb:tt0064570` ×3) — check that no venue-scoped reader needs them, then let `identity_judgements.py` flag and drop
  them. `INNA MAMUSIA - MARATON` (undecided) vs `Inna Mamusia - maraton` (right), one film, keys a case apart.
  title-words.tsv holds `en tour`/`es gira` in both festival-edition and side-event (and `tour` twice in festival-edition).

### Review of a083 and the Arsenal client (2026-10-15)
- [x] **The staging piece outweighs every keep but a stated fact** (138b55d52, `IdentityResolver.billedAsPiece`'s
  `staged`): a strand named with a Stage word ("Musical Monday: Grease", "Stage & Screen: …", PL "Scena …") would deny
  the film it bills unless the billing states its year, director or runtime. The 62 prod listings holding such a word
  today move nowhere (slice batches 9f9b41682 vs 6abe17876: UK 1 cure, DE 2 cures, PL/US 0), so it is no regression
  yet; pin a strand case in IdentityResolverCasesSpec before a strand with a Stage word appears. Pinned 2026-10-18
  ("A strand named with a stage word"): a banner a colon closes is no piece beside the film, so "Musical Monday: Grease"
  and "Stage & Screen: Wicked: For Good" take their film today; only a stage word billed after the film denies it.
- [ ] **Arsenal's shared bills are untested**: `ArsenalBerlinClient.parseScreening` drops year, director, country and
  runtime when `movieTitles` holds two or more films, but the 10-14 recording holds no shared bill. Record one
  (`clients.tools.WriteArsenalBerlin`) when the calendar carries it and assert the facts are dropped.

### CI and gates
- [ ] **Decision pending:** pre-push enforces one identity landing at a time (refuse an identity-path push while an
  earlier identity commit's convergence run is pending).
- [ ] Code-gates vs data-findings, the remaining proposal: a pinned, triaged recording for the code gates; whether
  coverage-band drift stays blocking (see the memory note `project_split_code_gates_from_data_findings`).
- [x] Shape-lint base-vs-head diffing only starts with the next shape-path change (the base predates the findings
  files). Closed 2026-10-18 with no change: every base since 8c7e075b0 (10-14) writes the findings files, and the
  shape paths have changed on main since (f345608b8 and others), so `identity-takes-check.sh` diffs base against head
  now; the "base predates the findings files" branch remains only for an old base (a measure of an old commit).
- [x] `build.sbt` sample aliases still include the shape specs, so the auto-bisect runs them (production path).
  Dropped 2026-10-18: the sample aliases replay the sample suite alone (ConvergenceLegWiringSpec pins it).
- [x] 13 deploy specs need the GitOps manifests (moved out of the repo) and fail locally; they are left out of the
  pre-push lint set. Back in 2026-10-18: pre-push's sbt check runs `infra/bin/fetch-gitops` first (GitHub, else the
  local clone ~/projects/movies-gitops) and then runs them (112 tests green locally); neither reachable, it warns and
  skips just those.
- [ ] identity-measure's run on an old commit (b7d2d7ec2) had every takes row fail under main's CI; not investigated.
- [ ] A `ci / test` prometheus FileNotFoundError was reported once on main; not reproduced.
- [ ] Judge a random sample of the ~95% unjudged served listings per country (`scripts/identity-judge-cards`) to
  estimate the unknown wrong rate.
- [x] Tooling review leftovers (CI): fixed 2026-10-18 -- convergence-fill's lane is per country set, the measure
  base-boot key carries the recorded pair (tree and fill assets), the dead `report` job is gone from
  identity-model-convergence (dispatch-only), an unreadable recording-health input now fails the leg unjudged,
  `ci_timings.py`'s gh calls are bounded, the stale "sample rows" comments are corrected, and the deleted `CI timings
  backfill` is no longer named in prometheus.yaml or ci-builds.json.
- [x] Orphaned `measure/*` branches: `scripts/identity-measure` now sweeps origin's (every start, or `--sweep`) whose newest
  run finished over 24 h ago. UNEXERCISED LIVE until GitHub is back: only the stubbed self-test has run it. A branch with no run at
  all is left (origin gives no push time).

### Machine and tooling
- [ ] `core.hooksPath` is rewritten to the absolute root path by Claude Code whenever it creates an
  `isolation: worktree` agent (confirmed 2026-10-15 06:54:35). A product bug; the pre-push hook resets it on each push,
  and `scripts/install-hooks.sh` restores it.
- [x] The JVM reaper now lives in the repo (`scripts/reap-stale-jvms.sh`, spec `scripts/reap-stale-jvms-test.sh`); the
  user switches `~/.local/bin` over with the symlink `scripts/install-hooks.sh` prints.
- [x] Worktrees reviewed 2026-10-15: a8e8055c, ad334852, afc695e6 (their commits were on main under the same subjects),
  the RB+O, Arsenal and a083 worktrees and `movies-wt-convergence-timings` removed. Left: `movies-red2` and the
  kinowo-mobile-release temp worktree (other sessions').
- [x] (fixed in `scripts/reap-stale-jvms.sh`: only `--detach-stdio` servers, `.part` rename is not gone)
  `~/.local/bin/reap-stale-jvms.sh` rule C groups sbt processes by cwd without requiring `--detach-stdio`, so it can
  kill an older LIVE run sharing a cwd (another agent's identity run in one `~/.cache/kinowo-identity/refs/<sha>`, or
  the server a client is attached to); its gone-dir rule also kills JVMs whose ref dir was renamed from `.part`. Only
  `--detach-stdio` servers not named by an `active.json` should be reaped.
- [x] `scripts/local-mirror/prod-tunnel.sh` bound fixed port 27017 (now a free port per caller, spec
  `prod-tunnel-test.sh`).
- [x] sbt-warm took no per-checkout lock when starting a server (now `<state>/<key>.lock`, spec in `sbt-warm-test.sh`).
- [x] (fixed: a checkout whose tool files differ gets `refs/<sha>-t<hash>`; `--stop-served` checks the pid's command
  line; venue-replay's mongosh is `bounded`; identity-capture.sh looks the recording up via identity-tools.sh and hands
  it to the JVM) Every checkout copies its own tool files into the shared `refs/<sha>` tree, so two checkouts with different tool
  versions can compile a mix; `identity-slice --stop-served` kills whatever now holds the saved pid; venue-replay's
  `mongosh` call has no timeout; `identity-capture.sh` still finds its recording in Scala, not identity-tools.sh.
- [x] (specs added: `freshness-test.sh`, `sync-enrichment-cache-test.sh`) No shell self-test for `scripts/local-mirror/freshness` and `sync-enrichment-cache.sh`;
  `mirror-sync-test.sh` needs the live :28017 replica set and stays run-by-hand (`# script-test: manual`).
- [ ] `scripts/run-script-tests --linux`: after 970fbd0f8 (scratch-git's `safe.directory`, the "dubious ownership" root
  cause of pre-push-test.sh's 5-55 Linux failures), 1 run in 13 still failed 42 checks, unreproduced; the Rancher bind
  mount sometimes showed an empty directory to a direct `docker run`. run-script-tests keeps only a failing log's last
  60 lines, so the next one was not captured: keep the whole failing log (and its path) first, then diagnose. (The
  whole log is kept now, `RUN_SCRIPT_TESTS_LOG_DIR`, default ~/.cache/kinowo-script-tests/failed, the host's in --linux:
  read it the next time the run is red.)
- [ ] `agent-procs kill --mine` ended two agents' own tool shells (exit 143) on 10-15; the ancestry is now read from
  the process table (the probable cause, a `ps -p` hop failing, reproduced only with a fake ps). Re-check if it recurs.
