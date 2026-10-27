# 2026-10-19 — film ids in booking and detail URLs (investigation only; recording 38713274617 + prod mirror 10-19)

- **Asked:** do `Showtime.bookingUrl` / `filmUrl` carry film ids (IMDb/TMDB/Filmweb, or a chain's own) the resolver
  does not read yet, and would they settle takes — as `externalIds` (external) or as a listing-listing pooling key
  (chain-internal)?
- **Coverage (recording):** a booking URL on 96–99 % of listings everywhere (PL 96, UK 96, DE 98, ES 98, US 99); a
  detail URL PL 89, UK 96, DE 10, ES 37, US 100. Id-shaped URL pieces were classed per host and position: a *film* id is
  constant across a listing's showtimes and distinct between a venue's listings; most hosts' pieces are session ids.
- **External ids:** IMDb in URLs only at US Majestic Theater Wayne (3–10 listings, all already taken right); none of
  TMDB/Filmweb/EIDR anywhere. Detail pages linking IMDb: Kinoteka (read already), 2 Drafthouse, 1 Iluzjon Filmweb.
  **Settles 0 takes.**
- **Chain ids already captured** as `externalIds` (and must-linked by `ConstraintEdges.catalogueKey`): Multikino, Cinema
  City, Vue, Odeon, Cineworld/Everyman/Showcase box office, Flicks, Taquilla, Ocine, Artesiete, Alamo, Landmark.
  **Not captured:** Helios film id (PL, 1.8–2.1k listings, 52 venues), CinemaxX Vista HO code (DE ~1.4k, 31 venues),
  kinoheld film slug (DE ~3k, 103 venues), Marcus FeatureCode, Cinemark `m=`, Harkins HO (US ~0.3–11k), biletyna
  slug (PL). All stable: 99.4–100 % the same id for a (venue, title) between recordings 38666576506 and 38713274617.
- **Value as a pooling key — nil:** the listings sharing an uncaptured chain id bill the same title in all but a
  handful of groups (Helios 0 of 90 shared ids retitled, Marcus/Cinemark/Harkins 0, kinoheld 2 of 250), so title
  pooling already joins them. Against prod's takes (mirror identity_traces) the groups where the id disagrees with
  the takes are: CinemaxX 8 retitled (must-link would FILL "Panem-Mockingjay 1+2" and "Lord Of The Rings Triple"
  — double bill / marathon, judged WRONG for its 2025 take — with the single film: 12 listings of NEW WRONG; BTS
  "in Busan" under the Buenos Aires code at 3 venues: ambiguous, other chains bill Busan too), kinoheld "Emil und die
  Detektive" (one slug, 1931 and 2026 films, BOTH judged right: a must-link loses a right take). As a cannot-link
  ("different id → different film") it would split today's right pools: one TMDB film under 2+ ids in Helios 26,
  kinoheld 37, Cinemark 13 titles (format/event variants). Cures found: 0 standing (the CinemaxX "Sinn und
  Sinnlichkeit" 4584/1503762 split seen in the recording has since healed in prod).
- **Why stopped:** no prototype built — the estimated payoff is 0 cures against ≥12 new wrong (must-link) or dozens
  of split pools (cannot-link). Not worth an identity-measure run.
- **Code:** the probes only, `experiment/booking-ids` (`scripts/experiments/booking-ids/`: `slots.py` over a
  recording's corpus, `mirror.py` against a mirror export of cinema_scrapes + identity_traces, `stable.py` across two
  recordings, `detail.py` for one slot's disagreeing groups).
- **Next angle:** if CinemaxX ever matters, scope its HO code to CinemaxX-named venues (the disagreements come from
  partner venues booking through cinemaxx.de, e.g. Kino am Raschplatz) and title-gate it as a feed id
  (`SourceReliability.linkDecides` = false); the mirror's lean `cinema_scrapes` keeps few showtimes, so measure
  coverage on a recording, not the mirror.
