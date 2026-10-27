# 2026-10-19 — a venue's KIND as a prior: its share of relays and events (branch `experiment-venue-kinds`, tag `experiment/venue-kinds`, base `619e02d91`)

## Hold an ordinary feature for an event- or relay-shaped billing at a venue of relays and events — not landed: dropped on offline evidence
- **Idea:** the film-ness entry's next angle — a per-venue profile computed once per corpus, to single out the gig and
  relay houses where a bare billing is rarely a film. Per venue, the share of its OTHER titles (leave-one-out by
  normalised title) that the existing classifiers read as a RELAY (`NonMovieEventClassifier` screened broadcast,
  `ListingShape.billsAHouse`) or a non-film EVENT (non-screening, live event); optionally V3f's event words
  (konzert, comedy, lesung, party, tour, …). The hold: at a venue of ≥10 other titles whose share is ≥ S, an ordinary
  feature (≥60 min, not a relay record) with no fact of the billing's own (year, runtime, director) is denied when
  the billing's cue fires — **A** its own kind is relay/event, **C** it names the record only by a piece (shares a
  word with the record's titles and says ≥2 more: "Fallen Angels by Noël Coward") or names a stage work.
- **Tried (offline only, no JVM measure):** `scripts/title-trace --batch` over every billing for the classifiers'
  reading (PL, UK, DE complete; US 67,928 of 92,267; ES not reached — the resumed batch stopped on a compile failure
  and was paused for memory), joined to per-country trace / fact / TMDB-record exports; labels
  `identity-judgements.tsv` via `label_of`. Code (on the tag): `scripts/ml-identity/venue_kinds_experiment.py` (`python3 -I … <repo>`
  from the data dir; `EVENTWORDS=1` adds V3f's event words). CBeebies Panto 2026 excluded from the counts below:
  main's `479d8ace6` fixes it.
- **Coverage:** venues (≥10 titles) whose other titles are ≥20 % relays: PL 20, UK 512, DE 160, US 135, ES 0;
  ≥20 % events: PL 3, UK 1, DE 5 — the event classifiers barely fire, so the profile is in effect a relay profile.
  The gig venues it was meant to find score low: Kaminwerk 6 %, Lagerhalle 9–10 %, Casablanca Zwickau 8 % (26 / 22 /
  18 % with V3f's event words, still below the festival houses: Moviemento 64 %, Kino Central 69 %, both near 0
  without them).
- **Separation:** on 3,831 judged current takes the venue's relay share ranks wrong over right with AUC 0.64 (4,001
  takes / 0.65 against main's judgements at `619e02d91`); the event share alone runs backwards (0.45).
- **The hold, simulated** (current takes, cure = judged wrong vetoed; right lost = judged right vetoed):

  | cue | S | cured | right lost | unjudged vetoed |
  |---|---|---|---|---|
  | C piece/stage alone (any venue) | 0 | 5 | 99 | 1,653 |
  | C | ≥0.10 | 2 | 29 | 672 |
  | C | ≥0.20 | 2 | 14 | 327 |
  | A the billing's own kind | ≥0.20 | 0 | 3 | 22 |

  - C ≥0.20 cures UK Theatr Gwaun «Romeo and Juliet» → 1968 and DE Trifthof «Gegen das Vergessen – Weilheim…» → 2015;
    it loses «Fallen Angels by Noël Coward» → the 2026 capture ×4 (Shrewsbury, Phoenix, Pershore, Wetherby), «Über
    Unterbiberger – Vom Woid in die Welt» ×3, Hilltop «Film Society:» prefixes ×2, POC-DOCS, a Welsh «Noson Film
    Glasurol: Carrie (1976)», Wolfhagen «Wir sind Juden aus Breslau», Kelkheim «Live - Vortrag:» travelogues ×2.
  - A ≥0.20 loses Haverhill «Hadestown: The Musical» and the two Kelkheim travelogues, cures nothing. With V3f's event
    words A ≥0.20 cures 4 (DE «Pornfilmfestival:» nights at Moviemento / Kino Central) for 5 right lost — a festival
    rule's job, not a venue prior's.
- **Why stopped:** the piece cue does the work and the venue share only thins it, never to the trade rule: at best 2
  cures for 14 rights, the rights being exactly the relays and programme prefixes a relay-heavy venue bills correctly.
  The event side, which was the point (gig venues), is invisible to the classifiers that exist.
- **Code:** `experiment/venue-kinds` — `VenueKinds` (profile + cues A and C), a `CorpusContext.venueKinds` hook and a
  `FamilyScope.deniedBeside` hold at `MinShare` 0.20. **Uncompiled and unmeasured**: written before the offline
  numbers sank it, never built nor run through identity-measure.
- **Next angle:** a venue prior needs an event classifier that sees gigs first (Kaminwerk, Lagerhalle); the piece
  cue alone is better aimed at a canonical tier ("X by <playwright>" names a play) than at a venue share.
