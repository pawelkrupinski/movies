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
- **Next angle:** a runtime gap outweighing a matching copied year/director ("The Stranger" bills 95 min vs 122) — only
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
- **Why stopped:** ~3 right lost per cure, and its targets (The Killer, Black Hole) not cured.
- **Code:** `experiment/identity-origin-once` (`d9249b8dd`, ported onto main; first built as `52af5ce06`).
- **Next angle:** the losses are venues whose own feed id names the film; let an own id stand outside the origin
  count, and count origins once only among venues with no id of their own. A full re-calibration on a prod snapshot
  was never run.

### Hold two family records apart when they name another id in one database ("Romeo and Juliet")
- **Tried:** UK Empire Theatre Halstead "Romeo and Juliet" (Royal Ballet relay): Wikidata's undated record of the play
  (IMDb tt2428892) joins IMDb's 1968 Zeffirelli film by title alone. A record naming another id in the same database
  is another entry; the vote join reads the same `Agreement.joins`. A follow-up reads only IMDb and TMDB ids so, as
  Wikidata spells some sites' ids its own way.
- **Result (slices and the ratchet; no whole-corpus measure):** Romeo and Juliet cured; lost DE "Der Film 444 …"
  (accepted) and UK "Thanks for coming FFFL", and asked a UK capture fill.
- **Why stopped:** the UK right loss with no evidenced trade; the take stays known-wrong.
- **Code:** `experiment/identity-id-clash` (`4e406da84`; the unlanded commits are `a11b0633d` and `4e406da84`).
- **Next angle:** apply the clash only where the record's own type (a play, a stage production) is not a film, or
  only to an undated record; then run the whole-corpus measure.

## Remaining wrong takes when round 2 stopped (37 on `d0d3ffb1c`)

Copied feed facts 10 · title piece or banner taken as the title 7 · bare exact title, top hit 6 · pooled family vote 5 ·
fill on a current release 3 · database agreement on a bare title 3 · relay, double bill, other 3. A long tail: each
looked like it needed case-level evidence rather than a class rule.
