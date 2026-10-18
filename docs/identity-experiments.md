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

## Remaining wrong takes when round 2 stopped (37 on `d0d3ffb1c`)

Copied feed facts 10 · title piece or banner taken as the title 7 · bare exact title, top hit 6 · pooled family vote 5 ·
fill on a current release 3 · database agreement on a bare title 3 · relay, double bill, other 3. A long tail: each
looked like it needed case-level evidence rather than a class rule.
