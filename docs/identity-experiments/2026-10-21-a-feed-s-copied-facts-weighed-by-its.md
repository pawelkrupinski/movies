# 2026-10-21 — a feed's copied facts weighed by its measured reliability (fundamentals proposal 4.4, the "Next angle" below) — NOT LANDED (local tags `experiment/copied-share-fit-a`, `-b` in ~/projects/movies, recording 528 DE, base `cf5c9dc0c`, CI runs 943/944)

- **Measured first, per feed** (`integration.CopiedFactReliability`, CI identity-run 824/825/906/942 over 528 and
  38713274617; `scripts/experiments/copied-fact-reliability.py`): each judged (listing, film) pair whose facts a feed
  copied, by whether the copied facts' calibrated contribution backs (+) or weighs against (−) the judged film.

  | feed (38713274617, every judgement) | right + | right − | wrong + | wrong − | P(right \| copy backs) | P(wrong \| copy dissents) |
  |---|---|---|---|---|---|---|
  | webedia | 152 | 1 | 3 | 123 | 0.98 | 0.99 |
  | cineamo | 481 | 111 | 47 | 98 | 0.91 | 0.47 |
  | flicks | 1615 | 578 | 145 | 1849 | 0.92 | 0.76 |
  | kinoheld | 48 | 25 | 97 | 27 | 0.33 | 0.52 |
  | kinoprogramm | 0 | 0 | 0 | 1 | – | – |

  The user's own reviews alone (`review page:`/`hand label:`): cineamo 90/0 backing right/wrong, webedia 6/1, flicks
  504/114, kinoheld 22/17. kinoheld's copy backs a judged-wrong film about as often as the right one: most are events on a
  reused or generic entry (a year and a slot-length runtime that match any recent film: "Ping Pong Session" → Ping Pong
  (2025), "Helmut Schleich" → Helmut (2021)); the resolver already takes few of them (7 judged wrong takes it backs on 528,
  3 cured since by `KinoheldClient.entryRenamed`).
- **Fitted share** (`scripts/experiments/copied-fact-share-fit.py`, over `scored-<cc>.jsonl`: logistic fit of the
  verdict on the resolve's log-odds with each feed's copied part split out, bootstrap by title). kinoheld's backing share
  is NEGATIVE on both recordings (−0.8 on 528, −2.1 pooled; 90% interval below 0); every other feed's interval holds 1
  or is too wide to use (cineamo's dissent 0.25 on 528, 1.9 pooled). The slope of the non-copied log-odds is only 0.1–0.2
  on this sample: the judged pairs are selected suspects, not a sample of takes.
- **Rule:** `ScopeModel.copied` (per feed: support and dissent shares, written by the fit's `--write`): a copied
  fact's signal weighs its own table × share, as `<signal>@copied:<feed>`; probability and evidence sums only, never a
  fact of its own for or against (`EvidenceWeights`). A: cineamo dissent 0.25, kinoheld support 0 and dissent 0.22 (528
  fit, clipped to [0, 1]). B: the dissent shares only.
- **Measured (identity-measure, CI, 528 DE):**

  | variant | run | cure | right lost | right→wrong | new wrong | gained right | unjudged moves |
  |---|---|---|---|---|---|---|---|
  | A | 943 | 0 | 2 + 2 final (Ich bin Dein, The Enforcer; Buddies, Lauras Stern) | 1 (ISLAND 63° 66° N → the 2016 namesake) | 1 (The Stranger → Der Fremde 2025) | 1 (Nuisance Bear) | 53 (Festtagsspaß mit Peppa ×20 lost, BTS Arirang, Texas Chainsaw Massacre ×2, …) |
  | B | 944 | 0 | 1 + 1 final (The Enforcer; Lauras Stern) | 0 | 1 + 1 final (The Stranger; The Piano Tuner → the 2026 film) | 5 (Sinn & Sinnlichkeit ×5) | 19 |

- **Why stopped:** past the bar both ways, and no target cured (Island rides its exact title at rank 1 with no rival:
  its copied facts are not what takes it). Zeroing kinoheld's backing moves only the probability now, so it loses far
  less than the page-copied-no-support rule (~110), but still 40 unjudged takes, "Festtagsspaß mit Peppa" ×20 (plainly
  right) the most. The fit cannot do better: the judgements sample suspects, so kinoheld's measured share is that
  of its suspects, not of its takes (2,444 of 2,542 kinoheld takes on 38713274617 are backed by the copy, nearly all
  unjudged and plainly right).
- **Next angle:** split kinoheld by entry kind before weighing: an event entry (no original title, no credit, a year
  equal to the screening's and a runtime equal to the slot) carries no film facts at all — read it as the venue's name
  only, as `entryRenamed` does for reused entries; a film entry's copy stays in full. Measure that split on these probe
  outputs first (the W+ list above is mostly event entries).
- **Catalogue splits without takes (WS4 item 4):** on 528 DE, of 4,256 feed claims (ids and pages) 1 is taken as two
  films (Cineamo 9373m: "Blutgericht in Texas" 1974 vs the 2003 remake; also Tabaluga 1486412m) and 8 as one film plus
  an untaken side — every untaken side a decorated billing of the same film ("HEIMSUCHUNG - MittagsKINO", "Silent
  Cinema: Edward Scissorhands"), none shares no word with the taken side. Reporting them would be lost takes, not splits,
  so `CatalogueSplits` stays as it is. Unterbiberger and Tyketto were one venue's reuse of an entry, which no
  cross-listing split shows; `KinoheldClient.entryRenamed` (6a3249997) strips them at the scrape. In the DE prod mirror
  every kinoheld film page (1,351) maps to one film.
