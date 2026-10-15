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
| DE | 1 | 2 | Pornfilmfestival: The more the merrier | Immer mehr, immer fröhlicher (1943) `tmdb:80616` | — | known-wrong / capture,served | the festival's shorts programme presented by Paulita Pappel, not Stevens' 1943 comedy |
| DE | 1 | 1 | Mystery-Vorfilm als Warm-up zu „Miles Davis – Birth of The Cool“ | Miles Davis: Birth of the Cool (2019) `tmdb:566232` | — | known-wrong / served | a mystery warm-up short screened before "Miles Davis – Birth of the Cool", not the documentary itself |
| DE | 1 | 1 | DONNERSTAGS-KNEIPE | X (2022) `tmdb:760104` | — | known-wrong / served | a Thursday pub night ("Donnerstags-Kneipe") billing no film, not Ti West's "X" |
| DE | 1 | 1 | Tim Brownlow - Nosferatu: The Journal of a Broken Man | Nosferatu, eine Symphonie des Grauens (1922) `tmdb:653` | — | known-wrong / served | Tim Brownlow's performance "Nosferatu: The Journal of a Broken Man", not Murnau's 1922 film |
| DE | 1 | 1 | UNLESS WE DANCE - QUEERE FILME ZUR LÄNGSTEN NACHT | A Menos que bailemos (2023) `tmdb:1103660` | — | known-wrong / served | a programme of queer films named after the 14-minute short "Unless We Dance", not that short |
| DE | 1 | 1 | SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE | 片袖の魚 (2021) `tmdb:797140` | — | known-wrong / served | a double bill (Shinjuku Boys + The Fish with One Sleeve): matches neither film |
| DE | 1 | 1 | GINA - mit der Domschule Würzburg | Gina (2022) `tmdb:1193753` | — | known-wrong / served | a school screening of "Gina", not the 7-minute Indonesian short of that name |
| DE | 2 | 3 | The Killer | The Killer (2024) `tmdb:970347` | — | known-wrong / served,rows | the 4K restoration of John Woo's 1989 The Killer (tmdb 10835, which 8 venues take); the 2024 remake was streaming-only |
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
| DE | 4 | 23 | League of Legends Worlds 26 - Finals in Cinema | League of Legends Worlds25 - Finals in Cinema (2025) `tmdb:1552059` | — | **UNLISTED** | the 2026 League of Legends Worlds final, not the 2025 one's record (as PL's and UK's labels say) |
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

### Landing queue (paused: the landing agent was stopped 2026-10-15)
- [ ] **Arsenal Berlin client** — `worktree-agent-ac9de0e136fc1735d` (ported onto the judgements store). Measure
  with `scripts/identity-measure`, land once convergence is green, then dispatch Record scrape fixtures (new
  screening-page lookups). Still unmatched with it: Mozu (TMDB credits the director in kanji), West Indies.
- [ ] **a083-fix** — branch `a083-fix`, tip 474bc600a, LANDABLE (run 38683085033: 12 cures, 0 right lost, 0 new
  wrong). Rebase, re-measure, land. Supersedes `worktree-agent-a083ade05cde02267`. Cures expected: UK SISTER ACT,
  Gremlins, Social Reckoning, About Time; DE GINA, THE COMMUNITY, RAY WILSON, München Mord; PL Okładka "AI";
  Fallen Angels by Noël Coward (UK). Check whether it also cures DE Fallen Angels ×5. Manhunter and the second
  Social Reckoning did not move in the measured corpus.
- [ ] Then re-run the state tally (§1) on the mirror once prod has deployed.

### Live wrong matches with no fix yet
- [ ] **DE "League of Legends Worlds 26 - Finals in Cinema" ×23** → the 2025 final record. Unlisted. Class 4/event:
  a yearly event must match its own year's record, or none.
- [ ] **DE "Fallen Angels by Noël Coward" ×5** → Wong Kar-wai 1995. Unlisted. See a083-fix.
- [ ] **NT Live: Hamlet ×2 (US)** — the 2026 record never enters the candidates; Flicks' 2015 year is counted once per
  venue reading the same Flicks page.
- [ ] **ESCAPE ROOM 2 (ES)** — bare title, 2021 film at 57% vs the 2026 one backed by 63 venues.
- [ ] **Lieblingsmenschen ×3, The Killer ×3, Black Hole, Stranger, Island, Unterbiberger, Gegen das Vergessen,
  Friday the 13th: Legacy, Ecrire la vie, Alpha (dir. Ducournau), ES (ULRICH SCHAMONI)** — namesakes or copied
  facts; no safe rule yet.
- [ ] **Pornfilmfestival "The more the merrier"** — the billing states no facts; the festival + old-film signal
  would also drop right takes (La Boum 4K, the Rosa von Praunheim hommage).
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
- [ ] **1. Learned banners**: a segment one venue/feed bills beside many different works is a banner, peeled before
  search; existing ExtraTitleRules/title-words entries become fallbacks or go.
- [ ] **5. Runtime as strong negative evidence**: a short can't take a feature-length slot unless the feed links it
  or the billing says shorts/Kurzfilm.

### Parked resolver work
- [ ] **Withdrawn-vote rule** — branch `wip-withdrawn-vote` 8c666e431; landed as 26e48897d and reverted
  (22361d8bb): it lost PL "Lalka (ale to horror)" ×9 and UK "Sense and Sensibility" ×3 / "Home Alone (Pyjama
  Party)" ×6. The IL KINO Camouflage case is a `pending:` slice case (f772906e9). Re-measure with
  `scripts/identity-measure` before trying again.

### CI and gates
- [ ] **Decision pending:** pre-push enforces one identity landing at a time (refuse an identity-path push while an
  earlier identity commit's convergence run is pending).
- [ ] Code-gates vs data-findings, the remaining proposal: a pinned, triaged recording for the code gates; whether
  coverage-band drift stays blocking (see the memory note `project_split_code_gates_from_data_findings`).
- [ ] Shape-lint base-vs-head diffing only starts with the next shape-path change (the base predates the findings
  files).
- [ ] `build.sbt` sample aliases still include the shape specs, so the auto-bisect runs them (production path).
- [ ] 13 deploy specs need the GitOps manifests (moved out of the repo) and fail locally; they are left out of the
  pre-push lint set.
- [ ] identity-measure's run on an old commit (b7d2d7ec2) had every takes row fail under main's CI; not investigated.
- [ ] A `ci / test` prometheus FileNotFoundError was reported once on main; not reproduced.
- [ ] Judge a random sample of the ~95% unjudged served listings per country (`scripts/identity-judge-cards`) to
  estimate the unknown wrong rate.

### Machine and tooling
- [ ] `core.hooksPath` is rewritten to the absolute root path by Claude Code whenever it creates an
  `isolation: worktree` agent (confirmed 2026-10-15 06:54:35). A product bug; the pre-push hook resets it on each push,
  and `scripts/install-hooks.sh` restores it.
- [ ] The JVM reaper (`~/.local/bin/reap-stale-jvms.sh`, with the new duplicate-sbt-server rule C) lives outside the
  repo, unversioned.
- [ ] Worktrees to review: `agent-a8e8055c5959ed512`, `agent-ad334852f5d655d25`, `agent-afc695e6b0db42819` (1–2
  unmerged commits each, from before 10-14 12:15; purpose unknown); `worktree-agent-a27df4eb26ac5be7e` (RB+O, landed
  in ported form as 65884e94e, so it can go); `movies-red2`, `movies-wt-convergence-timings`, the
  kinowo-mobile-release temp worktree (`scripts/agent-procs` marks them removable).
