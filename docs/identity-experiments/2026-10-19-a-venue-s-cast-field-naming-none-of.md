# 2026-10-19 — a venue's cast field naming none of a film's cast, as a NEGATIVE signal — LANDED (tag `experiment/cast-mismatch`, recording 38713274617, base `experiment/cast-tiebreak` 1009a8c83)

- **Rule** (`CastEvidence.mismatch`, a model-stage node denial in `FamilyScope.denialByNode`; never a weight): a candidate is
  denied when every listing of the node whose venue lists a cast FIELD of its own (`Listing.listedCast`: no synopsis, no
  kinoheld/kinoprogramm/Webedia/Cineamo copy; two names of two words or more, no one-word entry — a mononymous star, a
  puppet, a role in brackets) names none of its Latin-script top-billed TMDB cast, matched order aside ("Li Gong" is
  "Gong Li"); and no stated year (±1) or director agrees with it. Not read beside a stage work, relay, performance or
  bill of several works (`castIsTheFilms`), for an animated film or documentary (TMDB genres 16/99: voice casts,
  contributors), nor for a film of another original language than the venue country's (dubs, transliterations).
  Variant "veto alone" (no own-fact exemption) not measured: offline it costs the four takes below the exemption spares.
- **Offline, current takes** (1009a8c83's model + final takes joined to the recording's cast fields and TMDB credits):
  1,663 listings / 51 billings with zero overlap before guards, judged 0 wrong / 10 right (US Spirited Away). Almost all
  are right takes the guards above exist for: Ghibli English dubs (~1,400 listings), Korean/Chinese name order, Greek and
  Japanese-script TMDB casts, a doc's contributors (UK "The Sixty-Year Mission"), the Muppets' characters, mononyms
  (Jailer, Mercury). After the guards: 7 billings, 36 listings — ES "La maleta" ×14 → Objetos (2022) (wrong), US Met
  "Tosca (2026 Encore)" ×17 (relay, guarded), and singles all spared by their own director (Mercury, Jailer, "The Short
  Films of David Lynch", Muppets Take Manhattan) but UK "Mayhem 2026: The Whisper" (unjudged, no TMDB record of its cast).
- **Offline, every judged row** (judgements × recording listings, zero overlap, guards in): 523 wrong / 2 right listings
  (the right two ROH Carmen relays, guarded) — NT "Hay Fever" ×222 and other relays, "Fallen Angels by Noël Coward" →
  Coward 2026 ×187, PL "Lalka" → Has 1968 ×72, "It (1990)" → And the Sea Will Tell, Escape Room 2, Gremlins → Innerspace.
  None of these is a current take: the signal is mostly a guard against their return.
- **Measure** (identity-measure-local 1009a8c83 → b62db4ce2, all countries): ES model 14 cured (Ocine ×14 "La maleta"
  Objetos → Therón's 2026 film), 9 gained (Artesiete ×8 + Bahia Mar "LA MALETA" → 2026 film, judged right); 0 right lost,
  0 new wrong; DE, PL, UK, US and every final stage unmoved. LANDABLE locally.
- **LANDED** (rebased on `edb40f880`, genre ids on the record since 4c3f3d604): the animation/documentary guard reads
  `Film.hasGenre(16 | 99)`, and a record whose genres are NOT known (`genreBits` 0) is never vetoed. Re-measured
  edb40f880 → the landed commit: ES model 14 cured, 9 gained right, 0 lost, 0 new wrong, all else unmoved. The
  known-wrong "La maleta" → Objetos row it cures is a plain wrong judgement again. In prod the rule is inert until
  stored records carry genres and a cast (the TMDB record backfill: a record without `castPairs` or `genreBits` vetoes
  nothing). `Listing.listedCast` is inside listing equality (the model reads it): a venue editing its cast re-resolves
  the listing.
