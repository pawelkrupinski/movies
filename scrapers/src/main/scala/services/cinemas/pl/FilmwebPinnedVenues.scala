package services.cinemas.pl

/** The venues whose Filmweb cinema id [[FilmwebCinemaIdResolver]] takes from this table rather than its fuzzy match:
 *  a registry, one entry a venue. */
object FilmwebPinnedVenues {
  /**
   * Forced resolutions for cinemas the fuzzy matcher gets wrong or can't reach.
   * `Some(id)` pins the verified Filmweb id; `None` explicitly suppresses (the
   * cinema is reported `NO_FILMWEB_ID`, not fuzzy-guessed). Carried over from the
   * old hand-maintained `filmwebCinemaIds` table for the divergent-name cases.
   *
   *   - Multikino Reduta  ← "Multikino Atrium Reduta"  (2119)
   *   - Multikino Wola Park ← "Multikino Wola"          (1380)
   *   - Kino Amondo        ← "Amondo Kino"              (2077)
   *   - Helios Riviera     ← Gdynia "Helios"            (1775)
   *   - Kino Muzeum (Gdańsk) ← "Kino Muzeum"            (2042)
   *   - Multikino Rumia    ← Filmweb lists a bare "Multikino" (no city/district
   *     suffix) for Gdynia/Trójmiasto that fuzzy-matches "Multikino Rumia" at
   *     exactly the 0.5 threshold, discarding the "rumia" token and resolving to
   *     the wrong Multikino. Pin the verified id (1464, see `FilmwebCatchmentCinemas.scala`).
   *   - Kino Apollo: SUPPRESSED — Filmweb lists "Kino Teatr Apollo" (3025) but
   *     its seances API returns empty across the whole window (verified
   *     2026-06), so the old 3025 produced only noise. No usable Filmweb data.
   *   - Kinoteka ← Filmweb "Kinoteka" (55). The fuzzy match is exact, so this
   *     pin is about reliability, not name divergence: kinoteka.pl is down at the
   *     TCP layer (verified globally 2026-06-16), so the venue now depends on the
   *     Filmweb fallback every tick. Pinning the verified id removes that
   *     dependence on the boot-time town-listing GETs succeeding —
   *     a blip there would otherwise leave the venue with no fallback id and a
   *     red /uptime bar while its own site stays dead.
   */
  val overrides: Map[models.Cinema, Option[Int]] = Map(
    models.MultikinoReduta   -> Some(2119),
    models.MultikinoWolaPark -> Some(1380),
    models.KinoAmondo        -> Some(2077),
    models.HeliosRiviera     -> Some(1775),
    models.KinoMuzeumGdansk  -> Some(2042),
    models.MultikinoRumia    -> Some(1464),
    models.Kinoteka          -> Some(55),
    models.KinoApollo        -> None,
  )
}
