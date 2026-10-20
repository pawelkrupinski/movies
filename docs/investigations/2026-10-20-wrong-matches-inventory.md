# Wrong and lost matches against every judgement, all five countries

Investigated 2026-10-20, about 18:50–19:30 UTC, read-only. This is the inventory the fix agents work from. No identity
code or judgement was changed. Two agents already own German fixes: the LOTR Extended learned veto, and the
piece-beside / De Gaulle / feed-link / copied-title losses. Their clusters are listed here and marked **OWNED**.

## Sources

- **The judgements.** `test/resources/fixtures/identity-judgements.tsv` at `083645525`: 2,247 rows (de 886, pl 588,
  uk 602, us 155, es 16). Every commit to it is authored as Paweł, agents' included, so git authorship cannot tell the
  user's rows apart. The `evidence` column can. Rows whose evidence starts `review page:` or `hand label:` are the
  user's: the review page's export, or older hand labels. Rows starting `judged 10-1x …` are agent judgements. Below,
  **user** means the first kind and **agent** the second.
- **The review page.** `/debug/review*` (web `ReviewController`, `services/review/*`) is dev-only and 404s in prod.
  Its answers go to `review_local.review_answers` on the local mongod (:28017): 259 answers. Of those, 125 are by
  `pawel.krupinski@gmail.com`, 53 by `dev` (the local dev session, also the user) and 81 are `import`. `LabelsExport`
  turns them into TSV rows: `film` becomes `hand label` right plus `must not` wrongs, and `right`/`wrong` become
  `right:`/`wrong:`. Some 10-18 answers were never exported (see "Reviews to re-check").
- **Production has no review collection.** None of the five `kinowo*_prod_mirror` databases holds a review, judgement
  or flag collection.
- **Production's current take** comes from `identity_traces` in each mirror. `scripts/local-mirror/freshness` read
  all five as fresh, synced 18:51–18:52 UTC: pl 11,311 traces, de 34,652, uk 36,795, us 98,551, es 5,975.

### Method

A **listing** is one distinct (venue, raw title, year, take) in `identity_traces`. A listing counts as reviewed when a
judgement row covers its title at its venue or at `*`. The venue's own row overrides `*`, as `label_of` does. For
imdb rows, the film is mapped to TMDB through `tmdb_films.record.imdbNumber`. When a title's only right row is in
another database (imdb/rt), the TMDB film judged right for that title at another venue is used. Classes:

- **a**: prod takes a film that is judged wrong, and no film is judged right. A **wrong match**.
- **b**: prod takes none, but a film is judged right. A **lost match**.
- **c**: prod takes a film other than the one judged right. A **wrong match**.
- **d**: agrees. This includes "none taken, every judged film wrong".
- **u**: prod takes a film nobody judged, while only other films were judged (or only a non-TMDB film is right).
- **x**: no take, and only non-TMDB films are judged. Cannot be compared directly.

The scripts are in the session scratchpad (`wminv/compare.py`, `clusters.py`). They are not committed, because this
change is docs-only. Re-running them takes about 20 s from a mirror export.

## Per-country tables

Cells are **listings / distinct titles (listings whose deciding row is the user's)**.

| cc | a wrong | b lost | c other film | d agrees | u unjudged take | x not comparable | judgement rows not billed now |
|---|---|---|---|---|---|---|---|
| de | 12 / 8 (0) | 105 / 75 (17) | 28 / 5 (0) | 905 / 359 (32) | 108 / 13 (89) | 21 / 5 (0) | 174 |
| es | 0 | 0 | 0 | 194 / 7 (11) | 0 | 4 / 1 (0) | 4 |
| pl | 1 / 1 (0) | 64 / 45 (24) | 3 / 2 (0) | 789 / 149 (462) | 0 | 13 / 12 (12) | 224 |
| uk | 40 / 5 (0) | 662 / 98 (44) | 5 / 4 (0) | 2,661 / 340 (308) | 693 / 7 (689) | 45 / 4 (43) | 61 |
| us | 0 | 4 / 3 (1) | 2 / 1 (0) | 1,952 / 37 (1,210) | 0 | 367 / 2 (367) | 59 |
| **all** | **53 / 14** | **835 / 221** | **38 / 12** | **6,501 / 892** | 801 / 20 | 450 / 24 | 522 |

Wrong matches (a+c): **91 listings / 26 titles**. Lost matches (b): **835 listings / 221 titles**. One title, US
"Festive Fun with Peppa Cinema Experience", is counted under x: its 366 listings are judged right only through an
imdb id. It is the same work as UK's tmdb:1778515, so in effect it is **1,201 lost listings**.

Disagreements on the user's own rows: de b 17; pl b 24; uk b 44 (29 of them are RBO Fanciulla); us b 1 / x 366
(Peppa); **no a or c**. Every wrong take is against an agent judgement. Several of those disagree with an earlier user
review (CBeebies Panto, Maraton Horrorów: see "Reviews to re-check").

The u class is mostly fine: new season records (UK "RBO Cinema Season 2026-27: Manon" → 1702757 at 412 venues, "NT
Live: Hay Fever" → 1791571 at 234, DE André Rieu 2026 → 1707785 at 86). One looks wrong: UK "Boarding on Insanity +
Director's Q&A" → tmdb:1511464 "Paragon Man: A Copykat".

## Root-cause clusters, by listing count

Rule names are as in `identity_traces.rules`. "W=R" means the trace's `weighedFilm` is the film judged right.

| # | cluster | listings | titles | cc | owner |
|---|---|---|---|---|---|
| U1 | event suffix kept ("Cinema Experience") → a wrong film weighed → learned title-overlap veto | **716** | 3 | uk 350, us 366 | unowned |
| U2 | relay / live-event season records below threshold (wrong older record weighed) | **195** | 24 | uk 189, pl 4, de 2 | unowned |
| U4 | right film weighed but below the calibrated cut (strand prefixes, new film vs older namesake, editions) | **133** | 94 | uk 48, de 43, pl 39, us 3 | unowned; DE 23 listings / 12 titles cite a Cineamo link → feed-link agent |
| U3 | relay vetoes: another house's season, stage-house namesake, learned runtime gap, not released in GB | **58** | 23 | uk 52, pl 5, us 1 | unowned |
| U8 | event / festival programme / double bill taken as a film | **44** | 8 | uk 37, de 7 | unowned (36 are CBeebies Panto, a disputed judgement) |
| U5 | wrong rival weighed, right film below the cut | **37** | 37 | de 20, uk 11, pl 6 | unowned |
| U11 | namesake / edition: the old or new film of one title (restorations, remakes) | **37** | 9 | de 32, pl 4, uk 1 | unowned; Der kleine Prinz (16) is a Cineamo-id overrule → feed-link agent |
| U9 | other vetoes: director namesake, not released in DE/PL, programme tag, silent era, learned country | **18** | 16 | uk 10, pl 6, de 2 | unowned |
| R | festival-name veto, against a review that looks mistaken (Alpen Film Festival) | 12 | 4 | de | fix the review, not code |
| U6 | strand prefix hides the film: NoCandidate | **12** | 12 | de 7, pl 4, uk 1 | unowned |
| O3 | piece-beside veto (`billed-as-one-piece`) | 11 | 4 | de | **OWNED** |
| U10 | relay billed bare, taken as the film namesake | **9** | 7 | uk 7, us 2 | unowned |
| O2 | feed-link / copied-title veto (`named-only-by-the-title-its-feed…`) | 6 | 5 | de | **OWNED** |
| O1 | learned cannot-link veto (LOTR Extended) | 3 (+51 unreviewed) | 2 | de | **OWNED** |
| O4 | De Gaulle / Kino KILN | 2 | 2 | uk | **OWNED** (De Gaulle itself now agrees) |

### U1. Event suffix kept: 716 listings, unowned

Prod vetoes "Festive Fun with Peppa Cinema Experience" at 350 UK and 366 US venues with
`veto:learned-title-in-none-overlap-and-year-distance-2`. The film it weighs is never the right one: 749585 "Peppa Pig:
The Golden Boots" (2015), 69484, or 1458437 "Peppa Meets the Baby Cinema Experience" (2025). The suffix "Cinema
Experience" is searched as part of the title, so TMDB's "Festive Fun with Peppa" (1778515) is never weighed. The
learned veto then rightly refuses the wrong film. The billings the rules do strip all take 1778515: "Toddler Club:
Festive Fun with Peppa" at 41 venues (`title:xtra-pp-toddler-club`), "Festive Fun with Peppa" at 9, and "Kids Club:
Festive Fun With Peppa". Judged by the user: `hand label` imdb:tt46666063 and rt (2026-10-08), plus a recall target of
tmdb:1778515.

- Examples: uk ×349 and us ×366 «Festive Fun with Peppa Cinema Experience»; uk «Kids Club: Festive Fun with Peppa
  Cinema Experience»; uk «Festive Fun with Peppa Pig (U)» (BelowThreshold, weighed 1630432).
- Code: `common/src/main/scala/services/titlerules/ExtraTitleRules.scala` (next to `xtra-pp-toddler-club`,
  `xtra-exhibition-on-screen`, `search-plus-event-suffix`).
- Fix: an extra title rule that drops a trailing "Cinema Experience" (and the "Kids Club:" lead) from the search form.
  Only the search form, not the displayed title. The existing "Peppa Meets the Baby Cinema Experience" record carries
  the suffix in its own TMDB title, so the rule must keep that billing's exact-title join (check
  `join:same-title`).
- Test plan: an `ExtraTitleRules` spec case per spelling. `scripts/identity-slice uk --title "Festive Fun with Peppa
  Cinema Experience" --prod-mirror --explain`, then the same for us: expect 1778515 OwnMatch. Then
  `scripts/identity-measure` across all countries. Expect +716 takes and no new wrong takes on titles ending
  "Cinema Experience". Add the US `*` row tmdb:1778515 right, so the gates see the US loss.

### U2. Relay / live-event records below threshold: 195 listings, unowned

The billing is a relay or live viewing. TMDB holds a record for this season's event, but the resolver weighs an
older record of the same house or work, and the trace ends `BelowThreshold`.

- uk ×68 «BTS 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING» and ×68 «… IN SÃO PAULO: LIVE VIEWING» weigh 939984 "BTS:
  Permission to Dance on Stage – Seoul" (2022). Judged right: 1770237 / 1770234 "BTS WORLD TOUR [ARIRANG] in <city>".
- uk ×23 «Royal Ballet: Swan Lake» (+1 «… (2026») weighs 1151419, the 2024 record. Judged right: 1702782, the
  2026/27 record.
- uk ×13 «Royal Opera House: Carmen»: W=R (1151422). The right record is weighed but scored below the cut (user
  review).
- uk «Golden Boy» ×2 (weighed 1143881; right: NT Live 1723978), «Alice's Adventures in Wonderland» ×2, «MET Otello»,
  «Live from the Met: Parsifal», pl «Samson i Dalila» ×2, «OPERA-MANON», uk «EXHIBITION ON SCREEN: Caravaggio» (user,
  weighed the 1986 film).
- Code: `identity/…/StageRelays.scala`, `CandidateScoring.scala` (season-record and stage-production candidates),
  `ExtraTitleRules` canonical forms (`xtra-canonical-royal-ballet-opera`, `-met-opera`, `-nt-live`). There is no
  live-viewing canonical form.
- Fix: (1) a canonical rule for "<artist> '<tour>' IN <city>: LIVE VIEWING", searching "<artist> WORLD TOUR
  [<tour>] in <city>". (2) When a house's season record for 2026/27 exists, prefer it over the same house's
  older record of the work: the `season-record` acceptance should outrank the older record, which carries no season.
  (3) Look at why a W=R season record (ROH Carmen) falls below the cut. The record has no runtime or year, so the
  `missing:` features probably pull it under. Calibrate with `scripts/identity-calibrate` / `identity-refit`, not
  by hand.
- Test plan: `identity-slice uk --title "BTS 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING" --prod-mirror --explain`
  (likewise Swan Lake and ROH Carmen). `MatchedTakesLabelSpec` rows for these titles. `identity-measure` on uk/pl.
  Expect +~190 listings, and no relay taking another season's record.

### U4. Right film weighed but below the calibrated cut: 133 listings, unowned (DE Cineamo subset → feed-link)

**Follow-up (2026-10-20, 22:00 UTC): most of U4 is not lost.** `identity_traces` is the model stage only. Joining
the 128 non-Cineamo U4/U6 listings through `movie_slots` to `movies.tmdbId` in the same mirrors: **102 are served the
judged-right film** (the agreement stage takes them after the model's BelowThreshold: Fallen Angels ×16, Candyman ×8,
Resident Evil ×4, the Korelacje nights, the Purbeck and "+ Q&A" billings …), 1 has no slot, and **25 are really
lost**: de Das Boot - Director s Cut, TACHELES, Tim Brownlow - Nosferatu, Karmele, Tour du cinéma ×2, Der nackte
Wahnsinn, O vecech nadprirozených, JUMP SCARE, ZIRKUS FUMEUSEMENT; pl Cicha powódź, Teatr weteranów, FRIDA KAHLO,
Ice Cream Man ×3 (prod agrees on a Filmweb id only), Audiencja, Lydia, KINO KONESERA, Festiwal Korelacje: „Zemsta”,
Avengers … Dogrywka, André Rieu; uk Pip and Posy's Cinema Show, Spooked: E.T., Purbeck Film Festival - Dr Zhivago.
Re-run the class with served films before sizing a fix by it.

The right film is the weighed one (W=R) but scores under `favoured-calibrated`'s cut. There are three shapes:

1. **Strand / programme prefixes and event suffixes left in the title.** The title feature drops to `title=none` or
   partial. Examples: pl «Festiwal Korelacje: „Zemsta” z autorską narracją…» (5 titles), uk «Purbeck Film Festival -
   Dr Zhivago / Fatherland / Little Amelie», uk «… + Q&A», «Cine-Excess 2026: Wrath Of The Gods», de «Cicle Gaudí:
   LA FURIA», «Baskisches Filmfestival: Karmele», «Der Film 444 und ein Treffen mit dem Protagonisten…».
   [2026-10-20-pl-unmatched-real-films.md](2026-10-20-pl-unmatched-real-films.md) covers the Polish ones.
2. **A new film against an older namesake.** Examples: pl ×16 «Fallen Angels by Noël Coward» (1702350), de ×10
   «Dolly» (1309083, Cineamo-linked), uk ×4 «Resident Evil» (1423191), pl ×3 «Lalka» (user), de ×2 «Newborn»,
   pl ×2 «Ice Cream Man» (user).
3. **Editions and anniversaries.** Examples: de «Das Boot - Director s Cut», «HOBBIT 2/3 (EXTENDED)», uk «The Others
   (12A) - 25th Anniversary Re-release», «Manhunter - 40th Anniversary». Also uk ×7 «Candyman (1992)» and ×1 «(2021)»,
   where `bills-both-works` also fires although the year in the title names one work.

- Code: `identity/…/Acceptance.scala` (favoured-calibrated cut), `UnifiedEvidence.scala`, `TitleShapeMeasures.scala`,
  `common/…/titlerules/TitleRules.scala` + `ExtraTitleRules.scala` (prefix strips). For Candyman:
  `ResolverRun.scala` (`bills both works`).
- Fix: shape 1 is title rules; one rule per strand family recovers several listings each. Shape 3: count
  "Director's Cut", "Extended", "<n>th Anniversary", "Re-release" as edition words (as `6d6e10830` did for
  "Fassung"), and let a parenthesised year in the title satisfy `bills-both-works`. Shape 2 belongs with the DE
  Cineamo subset (23 listings, 12 titles: Dolly, Newborn, Woman, To The Bone, Black Water, Mittagsstunde, …). A venue
  feed id naming the weighed film is the feed-link agent's work. Hand those to it, not a new agent.
- Test plan: `identity-slice <cc> --title … --explain` per example. `identity-measure` all countries, since the
  threshold moves globally. Watch the wrong-take count in `identity-ratchet-report`.

### U3. Relay vetoes: 58 listings, unowned

- uk ×29 «RBO Cinema Season 2026-27: La Fanciulla Del West»: `veto:another-house-s-season-production`. The user
  judged it the Met's 1703629 on the review page (2026-10-06). The UK's "RBO Cinema Season" strand distributes Met
  relays, so the house in the billing is the distributor, not the producer. The same strand bills Macbeth (judged
  right as the Met's 1703622).
- uk ×8 «MetOpera: La Fanciulla del West»: `veto:learned-runtime-gap-6-and-year-distance-2`, W=R. uk «MET:Otello»,
  «Live from the Met: Manon», «Manon (MET)», «Otello (MET)»: learned runtime-gap 36 / 91. The Met records carry
  placeholder or missing runtimes.
- uk «Swan Lake», «Carmen», «Tosca» and «… (2027)»: `veto:vetoed-not-released-in-gb-as-namesake-N-is`, on the RBO
  2026/27 season records.
- pl «OPERA-OTELLO» (accepted-right-loss), «OPERA-SAMSON I DALILA», «OPERA-MAKBET - retransmisja», uk «RBO/MET
  presents MACBETH», «Macbeth (Met Opera)», «Scottish Ballet: These Violent Delights» (user):
  `veto:a-screen-namesake-of-the-work-its-stage-house-s-billing-names`. pl «Samson i dalila | metropolitan opera…»
  (user): `veto:seasonsapart`.
- Code: `identity/…/CandidateScoring.scala` + `Families.scala` (another house's season production; stage-house
  namesake), `LearnedCannotLinks.scala` / `IdentityCalibration.scala` (runtime-gap vetoes), `ReleaseVeto.scala`
  (not released in GB), `StageRelays.scala` / `ConstraintEdges.scala` (seasonsApart).
- Fix: (1) treat "RBO Cinema Season <yyyy-yy>:" as a distributor strand that also carries Met productions, so a Met
  record is no other house's season. (2) Skip learned runtime-gap vetoes when the record's runtime is missing or a
  season placeholder. (3) Exempt season / relay records from the not-released-in-GB namesake veto, since relays
  have no theatrical release. (4) For the stage-house namesake veto, recognise "OPERA-<work>" and "MET" as the Met.
- Test plan: `identity-slice uk --title "RBO Cinema Season 2026-27: La Fanciulla Del West" --prod-mirror --explain`
  and each example. The hard-clusters expectations (`expected-hard-clusters-uk.txt`). `identity-measure uk,pl`.

### U8. Event / programme / double bill taken as a film: 44 listings, unowned (36 disputed)

- uk ×36 «CBeebies Panto 2026: Treasure Island» → 1710881 "Treasure Island" (2027, Ridley Scott's). The agent judged
  it wrong on 10-13. **The user judged it right on the review page on 10-06.** Settle the judgement first. If the
  agent is right, CBeebies panto billings need an event rule (`EVENT_RULES` / film-ness).
- de «Pornfilmfestival: The more the merrier» ×2 (→ 1943 film), «This is Hardcore» (→ 1998 making-of short), «No
  Mercy», «The Dating Game» (known-wrong). These are festival programme segments taken by segment-top-hit or
  favoured-calibrated.
- de «SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE» and uk «The Tiger Who Came for Tea and We're Going on a Bear Hunt»:
  double bills taken as one film. de «Island» (known-wrong).
- Code: `identity/…/FamilyScope.scala` (festival/programme), `BilledPiece.scala` / `ListingShape.scala` (double
  bills: "//" and "and" joining two titles), `Acceptance.scala` (`segment-top-hit`).
- Test plan: slices per example. Add a `//` separator test to the piece splitter. Coordinate with the piece-beside
  agent, since this is the same splitter.

### U5. Wrong rival weighed, right film below the cut: 37 listings / 37 titles, unowned

One listing each, and each needs its own look. Examples: de «To The Bone» (weighed 900665), «Black Water
(GLOBALE)», «KÖNIG DROSSELBART - EIN ADVENTSMÄRCHEN FÜR KINDER» (weighed 13302), «SINGING IN THE RAIN» (→ 1438810,
right 872), uk «Manhunter - 40th Anniversary», «Imamura: Karayuki-san», pl «DKF: Wieloryb» (user). Most of the DE
ones are in the Cineamo subset (feed-link agent). The rest are the U4 levers: title strips and edition words. Test
with `identity-slice --bisect` against `8b75b70`, to separate regressions from old misses.

### U11. Namesake / edition, the old or new film of one title: 37 listings, unowned

- de ×13 «The Killer» → 970347 (Fincher 2024). Judged right: 10835 (Woo 1989, the 4K restoration). Of these, 12 are
  agreed/family takes and 1 is favoured-calibrated. The venue states 1989/Woo at some venues. Fix: a stated year or
  director should split the family (`join:same-title` must not join a listing whose own year disagrees).
- de ×16 «Der kleine Prinz» → 309809 (2015). Judged right: 334199 (1972), named by cineamo 334199m at all 16. This is
  a feed-link overrule → feed-link agent.
- pl ×2 «Ktoś całkiem obcy» (known-wrong), de «Lauras Stern» (2004 vs 2021), de «Sinn und Sinnlichkeit» (2026 vs Ang
  Lee 1995; the venue states 1995/135 min), pl «Binti Edukacja Młode Horyzonty», de «THE BLACK HOLE» (known-wrong),
  pl «Zniknięcie» (→ 1993 remake, not Spoorloos 1988), uk «Premature + Writer/Director Q&A».
- Code: `identity/…/Families.scala` (same-title join), `Acceptance.scala` (`pooled:unrivalled-calibrated`).
- Test plan: slices. `MatchedTakesLabelSpec`. `identity-measure de`.

### U9, U6, U10 (smaller, unowned)

- **U9 other vetoes (18).** uk ×3 «MetOpera: Samson et Dalila» (learned title-overlap). de «ES (ULRICH SCHAMONI)»
  (director namesake: struck as a fix on 10-18 but lost again), «Buddies» (not released in DE), pl «Homo sapiens?»
  (not released in PL; user), «Krew za krew | Splat!FilmFest» (learned country mismatch; user), «Dni Kina Niemego -
  Quo vadis?» (silent-era veto against the 1913 film), pl «Alim / Odblask / Atlantyda | UFF» (programme tag), uk
  «It's Nice Up North + Q&A» ×3 (learned runtime gap 36). The release vetoes are in `ReleaseVeto.scala`. The
  programme tag is in `FamilyScope.scala` / `CorpusContext.scala`: "| UFF" is a festival strand billed beside many
  titles, and should strip rather than veto.
- **U6 NoCandidate (12).** de «Tour du cinéma - Cinéma! <film>» ×4, «THEMENGESPRÄCH. The Broken Circle», «Tim
  Brownlow - Nosferatu: …», «Pornfilmfestival: Un jeune homme de bonne familie», pl 4, uk 1. The strand prefix leaves
  no search. Fix: title rules (`TitleRules.scala`), with a spec case each.
- **U10 relay billed bare (9).** uk «Cosi Fan Tutte» / «Così fan tutte» ×3 → 2006 film (right: RBO 1702775); us «NT
  Live: Hamlet» ×2 → 2015 record (right: 2026's 1507580); uk «Romeo and Juliet» → 1968 (known-wrong); «Notre-Dame de
  Paris» ×2 → 1956 (known-wrong); «A Christmas Carol by Patrick Barlow» → 2009. A venue's relay season context
  (`StageRelays`) should beat a pooled namesake. These are the takes `countries_disagree` flags (`check «cosi fan
  tutte»`).

### Owned clusters (for completeness)

- **O1 LOTR Extended learned veto.** Reviewed: de «DER HERR DER RINGE - DIE RÜCKKEHR DES KÖNIGS - EXTENDED VERSION»
  → 19995 Avatar at Neues Regina (c; Cineamo links Avatar), and de ×2 «Hörspiel: Die drei ??? und die Salztote» (b,
  the same `learned-listing-film-probability-below-the-cannot-link-cut` veto). Unreviewed but plainly lost: «Herr der
  Ringe - Rückkehr des Königs (ext.)» at 43 CineStar venues and «Triple: Der Herr der Ringe - Extended Versions» at
  8. They count as d here only because every row for them is a wrong. Add a right row for tmdb:122 so the gates see
  the loss.
- **O2 feed-link / copied-title (6).** de «Tabaluga und Leo - Die Weihnachtsgeschichte» ×2, «Mein Opa der
  Bankräuber» (accepted-right-loss), «Kochen mit Paul Bednarczyk» (user), «Susanne Siegert» (user; the review looks
  mistaken), one «Über Unterbiberger». Also de «Das Auge des Bösen», where the judgement itself is in question
  (below).
- **O3 piece-beside (11).** de «Über Unterbiberger – Vom Woid in die Welt» ×10 (3 spellings), «ROYA - Eintritt frei
  mit Seebrücke Würzburg e.V.».
- **O4 De Gaulle / Kino KILN.** uk «De Gaulle 1: Resistance» agrees now. «Kino KILN: Almost Famous» and «Kino KILN:
  Portrait of a Lady on Fire» are still lost (BelowThreshold, W=R).

## Unreviewed suspects (no judgement covers them)

These are clearly **unreviewed**. Judge them before fixing anything.

- **Countries disagree** (`scripts/ci/countries_disagree.py --mirror …`, run 19:10 UTC, 15 titles). Highest first:
  - «seventeen world tour new_»: de ×27 → 1662735 "… ENCORE" vs pl/uk/us ×453 → 1797200.
  - «holiday»: de «Die Schwester der Braut» → 16274 (1938), ×1.
  - «obsession»: uk «Obsession (PG)» → 35852 (1949), judged right, vs 1339713 (2026) ×105 elsewhere.
  - «grinch»: uk «The Grinch (Pyjama Party)» ×6 → 2000 live-action, vs 2018 animation ×97.
  - «manon»: es «Manon (BALLET LIVE)» ×7 → 471328, an old record, vs RBO 1702757.
  - «texas chainsaw massacre»: de «Texas Chainsaw Massacre» ×2 → 2022.
  - «heat»: de «The Heat» → 2013 comedy.
  - «carnival of souls»: de → 1998 remake.
  - «dune»: de «DUNE» → 1984.
  - «fame»: uk «Fame (15)» → 1980.
  - «on the road»: de → 2012.
  - «caravaggio»: de → 1986.
  - Also «girlfriends» and «cape fear».
- **Identity findings: pl** (Forgejo issue #4, updated 07:41). The only open findings issue; the others lived on
  GitHub. 18 unjudged suspects. The likeliest wrong: «Okładka „Myślę, więc jestem!”» → 251434 "Thinkin' Big"
  (1986); «24. WJFF: PRZEMYŚL TO» → 1466670 "Si tu penses bien"; «NA WAGĘ ZŁOTA» → 1433583 "The Weight" (2026);
  «LEGENDARNI» → 677561 "Les Légendaires"; «Cinematographers. Spotkanie ze scenograf Magdaleną Dipont oraz Trzy
  kolory: Biały» → 109 (an event with a screening; probably right).
- **Year far from the billing's own year** (unjudged titles, |Δ| ≥ 3; mostly re-releases, so low confidence). Among
  them:
  - de ×81 «Edmund und Luzie - Winterabenteuer» 2026 → 2023 record.
  - es ×99 «Cars: 20 aniversario» → 2006 (right).
  - de ×5 «Monsieur Robert kennt kein Pardon» 2026 → 2023.
  - de «Il sol dell'avvenire» → 925263.
  - uk «Possession» 2026 → 1981.
  - uk «The Women» 2026 → 1939 (`directors-title`).
- **Unjudged takes on reviewed titles (u):** uk «Boarding on Insanity + Director's Q&A» → 1511464 "Paragon Man: A
  Copykat".
- **Take-change oscillations: not measured.** The mirror keeps no take history (`identity_family_answers` holds search
  hits, not takes). `scripts/listing-history` over recordings is the tool for that follow-up.

## Reviews to re-check (they look mistaken)

The first column is the review's verdict. Rows marked **not exported** are answers in `review_local.review_answers`
that never reached the TSV; exporting them would flip a wrong row to right.

| cc | billing | film | review | why it looks wrong | effect now |
|---|---|---|---|---|---|
| de | Das Auge des Bösen (Filmfabrik KommKino, Filmhauskino) | 3051 Eye of the Devil / Die schwarze 13 (1966) | **wrong** (agent 10-15, class 3) | The KommKino page bills original title "Eye of the Devil", J. Lee Thompson, Kerr/Niven/Tate: exactly 3051 ([de-oct14-15-lost-films](2026-10-20-de-oct14-15-lost-films.md)). The row's 78-min / Chabrol argument conflicts with that. | a loss nobody flags |
| de | Alpen Film Festival 2026, ALPENFILMFESTIVAL (+2 spellings) | 1625266 "How I Started A Film Festival" (2026, 13 min) | **right** (review page `film`, `dev` 10-18; overrode the agent's wrong) | A festival programme billing, not a 13-minute short. The ref is probably a mis-pick from the candidate list. | 12 "lost" listings (cluster R) |
| de | Susanne Siegert (KAP-Torgau) | 182771 "Ich bin Susanne" (1933) | **right** (review page `film` 10-18; the card showed "Frankensteins Sohn") | An author's reading. kinoheld copied the slug `i-am-suzanne`. | 1 lost listing (O2) |
| de | Christl Sittenauer / … - Komplizin | 615 The Passion of the Christ | **right**, not exported (review 10-18) | An author/actress evening. The TSV says wrong. | none until exported |
| de | COEUR DE PATATE | 988336 Cœur de slush (2023) | **right**, not exported (10-18) | The kinoheld page is another film's (`billie-blue`). The TSV says wrong. | none until exported |
| de | La fjella leve \| Let Our Mountains Live | 1329147 Vive e Deixa Andar (2024) | **right**, not exported (10-18) | A 2026 Sámi documentary, not the Portuguese film. | none until exported |
| de | NOSFERATU – Eine Symphonie des Grauens – Ballett | 653 Nosferatu (1922) | **right**, not exported (10-18) | A ballet performance of the work. Film-ness says event. | none until exported |
| de | TACHELES – Kurzfilmprogramm (KAP-Torgau) | 703131 Endlich Tacheles (2021) | **right** (review 10-14) | A short-film programme, not the 2021 feature. | 1 lost listing |
| uk | CBeebies Panto 2026: Treasure Island | 1710881 Treasure Island (2027) | user **right** 10-06; agent **wrong** 10-13 (TSV) | The judgements conflict. The agent's evidence (Flicks: "the CBeebies Panto returns") is stronger. | 36 wrong takes, if the agent is right |
| pl | Maraton Horrorów (3 spellings) | 1193501 | user **right** 10-07; agent **wrong** 10-06 (TSV) | A multi-film marathon (three horrors a night). | conflicting rows |
| uk | Royal Ballet: Swan Lake | 1151419 (2024 record) | user **right** 10-08; overruled 10-18 → 1702782 | Check that the overrule is what the user meant. | 23 lost listings (U2) |
| uk | Royal Opera House: Carmen | 1151422 *and* 1702759 both right | two-right exception | Two TMDB records of one broadcast. Fine, but prod takes neither (13 lost). | U2 |
| uk | Obsession (PG) | 35852 Obsession (1949) right; filmweb:140732 via review `film` | review 10-18 | A Polish catalogue id on a UK billing. Every other country takes the 2026 film. | check |

## Proposed order of work

1. Re-check the reviews above (minutes, no code). This removes 12 "lost" listings (Alpen), and possibly reverses
   36 wrong takes (CBeebies).
2. **U1** title rule: +716 listings, one rule, low risk.
3. **U2 + U3 + U10** relay work in one change set (StageRelays / season records / relay vetoes): about 260 listings.
4. **U4/U6** strand-prefix and edition-word title rules, with the PL list in
   [pl-unmatched-real-films](2026-10-20-pl-unmatched-real-films.md); calibrate the cut last.
5. **U11** stated-year family split (The Killer). **U8** the double-bill separator, together with the piece-beside
   agent.
