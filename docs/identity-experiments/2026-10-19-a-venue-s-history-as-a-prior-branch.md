# 2026-10-19 — a venue's history as a prior (branch `experiment-venue-history`, prod mirror of 2026-10-19)

## Score a candidate's film age against the venue's other takes ("Sparrows" 1926 at a silent house) — not landed
- **Tried (offline, no resolver change):** per venue, the age buckets (0–1, 2–4, 5–14, 15–34, 35+ years) of the films
  prod takes for its OTHER billings (leave-one-out by normalised raw title; Dirichlet-smoothed, α = 10, toward the
  country), as a log-likelihood ratio `log P(bucket | venue) / P(bucket | country)` beside the candidate's calibrated
  logit. Data: the local prod mirror's `identity_traces` (173k listings, 165k takes, 7,565 venues, every candidate with
  its probability and year) and `tmdb_films`; labels `identity-judgements.tsv` via `label_of`: 10,540 judged
  candidates of 5,748 listings (1,016 right), 452 judged current takes (348 right, 104 wrong). The documentary share
  was not built: `tmdb_films.record` holds no genres. "Earlier dates only" was not built: the mirror holds current
  screenings only. Code: `scripts/ml-identity/venue_profile_experiment.py`.
- **Result (B, confidence):** the prior alone barely ranks (candidate AUC 0.571); on current takes it runs BACKWARDS
  (AUC 0.376: the judged right takes are often old films at ordinary venues, the wrong ones current releases at
  ordinary venues). Logistic beside the logit, 5-fold grouped by title, country dummies: log-loss 0.2770 → 0.2705,
  Brier 0.0778 → 0.0763 — UK and DE better, PL and US worse. A fixed weight lowers the candidate ranking in every
  country (same-title listings AUC 0.891 → 0.851). LLR < −1 flags 6 wrong takes (The Killer 2024 ×4, The Black Hole,
  The Stranger 2025) and 3 right (It (2017) at US luxury chains, Pan Tadeusz 1999 at Bali-Kino).
- **Result (A, takes, proxy):** the argmax over non-denied candidates of the judged listings, logit + w·LLR: w = 0.1
  4 wrong→right / 2 right→wrong; 0.25: 7 / 5; 0.5: 9 / 10; 1.49 (the fitted ratio): 20 / 16. The losses are relays and
  plays whose current record is right at a venue of older films (Fallen Angels by Noël Coward 2026 → the 1995 film ×5,
  Met Opera Macbeth 2026 → 2014, Black Water 2025 → 2018, Norymberga 2025 → 2006). Every move but The Killer at
  KommKino is between candidates at ~3%, which no rule takes. Not built into the resolver or run through
  identity-measure: the proxy already moves right takes at every weight (zero new wrong is the bar), and a profile from
  the venue's own takes needs a second resolve pass or stored takes as input.
- **Same venue, same raw title, earlier take:** no use as a prior. Within one snapshot only 212 (venue, title) pairs
  bill twice (206 agree, none judged wrong); across time it is what `StoredFilmSlots`/`IdSeeding` already inherit, and
  a wrong take persists as itself (`expected-matches.tsv` only holds takes the ratchet called right, so it leaks the
  label).
- **Why stopped:** film age is the wrong axis. What a venue screens tells repertory from multiplex, but the hard cases
  are relays, plays and remakes where the CURRENT record is right at a repertory house (or vice versa), and
  `ReleaseVeto` and the copied-fact work already took The Killer's class.
- **Next angle:** a venue prior over KINDS, not ages (share of relays / events / non-film billings, the "Auf
  Augenhöhe" entry's next angle), used only to veto, never to choose; or the venue's catalogue-id coverage as a
  reliability prior.
