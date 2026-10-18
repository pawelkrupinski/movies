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
  film to none — the venue screens Kid Koala's film ("Mission Sternenhimmel"), so a second cure. 0 right lost.
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

## Remaining wrong takes when round 2 stopped (37 on `d0d3ffb1c`)

Copied feed facts 10 · title piece or banner taken as the title 7 · bare exact title, top hit 6 · pooled family vote 5 ·
fill on a current release 3 · database agreement on a bare title 3 · relay, double bill, other 3. A long tail: each
looked like it needed case-level evidence rather than a class rule.
