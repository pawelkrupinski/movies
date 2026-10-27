# 2026-10-13/18 — shorts, relays, certificate search (other sessions' branches, traced 10-18 from their transcripts)

## Strict short denial (class 5, strict form)
- **Tried:** a stricter refusal of shorts (`ShortFilms`, `FamilyScope`, `FeedLink`, `SuspectTakes`, title words).
- **Measured:** CI convergence run 38690448859 (measure/3696d942d), red after 36 min, read then as "not landable";
  recording, base and per-country numbers were not recorded.
- **Why stopped:** superseded by the narrower class-5 shorts rule that landed 10-15 (`d2f919535`, `d2557b009`,
  "take no short a billing opens on and goes on past with words of its own").
- **Code:** `experiment/identity-class-5-strict` (`3696d942d`).
- **Next angle:** measure it locally (identity-measure-local) on top of the landed rule to see what it adds beyond it;
  the original red never said which takes it cost.

## Relay pre-emption of the pooled rules: stage-production and the full form
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

## Search a title without the certificate the format peel reads ("item 8")
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

## Search a house's relay with its screenings' season ("(ROH)" series)
- **Tried:** search a stage house's relay with its screenings' season beside a credit or running time (UK Picture House
  Uckfield's "(ROH)" series), 10-14.
- **Result:** never measured: UnmatchedClustersRatchetSpec red, because the new "<work> <season>" lookups are absent
  from the capture. Needs a recapture and re-record first.
- **Code:** `experiment/identity-relay-season-search` (`d6b06b981`, on three other unmerged commits of that session).
- **Next angle:** the year form of the same idea (search a relay's work with the year its title or banner dates it,
  branch `relay-search-year`, `2b1b1bab3`) measured LANDABLE with 0 moves on 10-18 and is being landed by its session;
  the season form is worth measuring once that is in and a recording holds the season queries.
- **Extended 10-18, LANDED** (`kw-romeo-followups`): a title that is the stage work ALONE, stating no year and no credit
  (a running time at most), is searched with its screenings' season too, and the broadcast join takes it on the record's
  air day. UK Empire Theatre Halstead's "Romeo and Juliet" [170′] (25 May 2027) now takes the RBO 2026/27 relay, and its
  "Tosca", "Swan Lake" and Riverside Woodbridge's two, all on their broadcast days. A first, wider form (any title
  billing a work) asked "Lord of the Rings 2026" of a Fellowship of the Ring screening; whole-title only, it does not.
  identity-measure-local `--live-gaps`, base `0a6079eb3`, all countries: UK final 5 gained right, PL model 2 gained right,
  0 lost, 0 new wrong; captures filled for the new queries.
