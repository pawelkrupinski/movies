# 2026-10-15/17 — wrong-match classes, round 2 (recording 38713274617, base `d0d3ffb1c` unless noted)

## Class 2 strict: a take on a bare title needs a fact of the listing's own (variant H)
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

## Agreement short of the quorum needs a fact of the listing's own
- **Tried:** only the database-agreement stage: a consensus below the quorum on a bare title needs a listing fact.
- **Result:** 4 cured, 45 right lost. Short-quorum bare agreements are right ~9 times in 10, and the cures look exactly
  like the losses in this signal space.
- **Code:** `experiment/identity-agreement-quorum` (`7c3a33657`).
- **Next angle:** the 4 cures each had their own signal — Halstead's bare "Romeo and Juliet" is a relay on the RBO
  broadcast day (class 4); "Archiv Negativ: Turksib" is agreed on a record not titled "Turksib" (a title-guard gap).

## Count the venue's poster, cast and certificate in the agreement quorum (2026-10-19, recording 38713274617, base `b79c71a78`)
- **Tried:** `Agreement.weighed` reads the venue's own evidence on each film the families took (`AgreementTakes.venueEvidence`,
  not waited on, like the cast and synopsis): (1) a venue poster within `VoteBits` of the film's TMDB poster, no poster
  vetoing it, is one quorum vote; (2) the venue's text naming two whole names of its cast, no other candidate's, is one;
  (3) both; (4) a poster veto, another candidate's cast named, or the venue's certificate `StepsBelow` under TMDB's is one
  dissenting vote. Independence by provenance as the stage already draws it: a feed's poster shows nothing
  (`PosterEvidence.shows`), a feed catalogue's text names nobody (`venueNames`). Variant 5 prints every weighed film's
  support, quorum and venue evidence and applies nothing.
- **Result:** every variant moved NOTHING (`identity-measure-local`, 5 countries: model and final 0 lost / 0 changed / 0
  gained). Variant 5 over the capture's 702 weighed films: 133 sit one vote short, only 2 of those carry a venue poster
  (PL Goplana "Johnny", already taken as `Poster`; Garden Cinema "Last Train Home" 2009, still refused by a title guard)
  and none a cast vote; the poster/cast votes otherwise land on 12 agreements already at quorum (Candyman ×9,
  Caravaggio, Pianista, The Cremator: all judged RIGHT). Dissent fires on 9 films, 8 of them already short of the quorum
  (PL "Lalka" 1968 at three venues, UK "Resident Evil" 2002, "Fatherland" 1994 ×2, "Golden Boy" 2025 — each the losing
  namesake of a right take) and the ninth (PL Fenomen "Dyrygent" → The Conductor 1980) already poster-vetoed after the
  quorum. The certificate never fired.
- **Why stopped:** the stage after the quorum already spends this evidence: the poster vote and the fill take the
  one-short clusters, and the poster veto refuses what a poster dissent would. The one capture WRONG agreement (UK
  Beehive "Into The Shadows", an event) shows no poster or cast. The "pooled vote" wrongs (Notre-Dame de Paris ×2,
  Pornfilmfestival "The Dating Game") are model takes the quorum never weighs.
- **Confidence (B):** venue votes 12/12 right, dissent 9/9 on films not right — consistent, but every one where the
  outcome is already decided; no separation gained on the judged set.
- **Code:** `experiment/quorum-votes` (variants are `Agreement.QuorumVariant` commits 1–5 on the branch).
- **Next angle:** none here. Venue evidence would matter only for a model take's correction, where the quorum is not read.

## Refuse an uncorroborated take below a confidence threshold
- **Tried (offline, over H's classified moves):** wrong uncorroborated takes sat at 3–62%; refuse below a threshold.
- **Result:** 6 cured, all near 39%, with dozens of right takes in the same band. No separating threshold.
- **Code:** none (analysis only).

## Class 3: copied feed facts
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
  film to none — the venue screens Kid Koala's film ("Mission Sternenhimmel"), so a second cure. 0 right lost. A first
  all-country measure lost ES Serantes' "De repente" {Lewis Allen} [1954]: TMDB records old films' releases sparsely, so the
  vetoed namesake must date from the last five seasons (`StreamingEraYears`); after that, every country 0 right lost.
- **Next angle:** (The Stranger was cured 10-18 another way: see "Hold apart a join made only by a copied original title".)
  A runtime gap outweighing a matching copied year/director ("The Stranger" bills 95 min vs 122) — only
  "The Stranger" in the corpus then; Alpha (51 min off) was cured another way. Meets `FamilyScope`'s release-veto check,
  as does the separate `wmo-d-origin-once` work.

## Billed-director identity denial (landed, then replaced)
- **Tried:** deny a film whose director differs from one the title bills ("Alpha (dir. Ducournau)").
- **Result:** landed in `06432928e`; first measure tripped on "mit Regie-Besuch!" read as a credit and on
  different-script names (宮崎駿, Smaragdis); narrowed to "different" only.
- **Why replaced:** the user preferred reading the credit into the listing's director at intake (`TitleFacts`,
  `f7dbf688b`), where the ordinary director measure keeps Alpha right with no special rule.

## A dead AKA-rung change ("Ecrire la vie")
- **Tried:** an alternative-title rung change aimed at "Ecrire la vie".
- **Result:** moved nothing on the whole corpus; dropped uncommitted.
