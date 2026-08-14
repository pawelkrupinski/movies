package services.movies

import models.{Cinema, CinemaMovie, Helios, Movie, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/**
 * Two DIFFERENT films share one Polish title, and the cinemas screening one of
 * them publish no year to say which. Prod, 2026-08-14, `/poznan`:
 *
 *   - `tylkojednanoc|2026` — tmdbId 1433367, "One Night Only", 102 min.
 *   - `tylkojednanoc|1961` — tmdbId 41050, Antonioni's "La notte", 121 min.
 *
 * The page rendered the SAME film twice under one slug, each card carrying the
 * same Multikino / Cinema City booking links, because the venues' listings were
 * split across the two rows. What decided the split was `concludedKeyFor`: with
 * no year on the listing it ranked the concluded same-title rows by
 * `canonicalRank` and took the smallest — and `canonicalRank` breaks ties on the
 * LOWER YEAR, so the 1961 film won every time.
 *
 * That tie-break is a canonicalisation rule for rows of ONE film (prefer the
 * year-bearing spelling, then the lower year). Applied across two films it means
 * "the older film always wins", which is exactly backwards: what cinemas screen
 * without printing a year is overwhelmingly the new release, not the classic.
 *
 * The archived PL corpus says how common this is: of 33 listings of
 * "Tylko jedna noc" across 31 cinemas, 32 carry NO release year and none carries
 * an `originalTitle`. So the year is not available to disambiguate, and
 * `MixedFilmDetector` — which needs a published original title — cannot speak
 * either. The listing's RUNTIME is what those cinemas do publish, and it is the
 * same evidence `MixedFilmDetector.corroborated` already trusts to tell two films
 * apart — read here as NEARNESS, not as agreement within a tolerance. TMDB says
 * 102 minutes; Cinema City publishes 102 but Multikino publishes 105, so a ±2
 * agreement test answers "neither film" for Multikino and it falls straight back
 * to the wrong guess. 105 is 3 minutes from the romcom and 16 from "La notte" —
 * not a close call.
 */
class SameTitleTwoFilmsSpec extends AnyFlatSpec with Matchers {

  private val Title    = "Tylko jedna noc"
  private val NewFilm  = 1433367   // "One Night Only" (2026), TMDB runtime 102
  private val OldFilm  = 41050     // "La notte"       (1961), TMDB runtime 121
  private val When     = LocalDateTime.of(2026, 8, 14, 20, 5)

  private def cache() = new CaffeineMovieCache(new InMemoryMovieRepository, normalizer = titleNormalizer)

  /** A concluded row for one of the two films, carrying TMDB's own runtime/year. */
  private def resolved(tmdbId: Int, year: Int, runtime: Int, cinemas: Seq[Cinema]): MovieRecord =
    MovieRecord(
      tmdbId = Some(tmdbId),
      data   = (cinemas.map(c => (c: Source) -> SourceData(
                  title = Some(Title), releaseYear = Some(year), runtimeMinutes = Some(runtime),
                  showtimes = Seq(Showtime(When, bookingUrl = None)))) :+
                ((models.Tmdb: Source) -> SourceData(
                  title = Some(Title), releaseYear = Some(year), runtimeMinutes = Some(runtime)))).toMap)

  /** What every one of those 31 cinemas actually publishes: a title, a runtime,
   *  and NO year. */
  private def yearlessScrape(cinema: Cinema, runtime: Int): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = Title, releaseYear = None, runtimeMinutes = Some(runtime)),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(When, bookingUrl = None)))

  private def cinemasOn(cache: MovieCache, year: Int): Set[Cinema] =
    cache.get(cache.keyOf(Title, Some(year))).map(_.cinemaData.keySet).getOrElse(Set.empty)

  "a yearless listing of a title two different films share" should
    "go to the film whose runtime it matches, not to whichever is older" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Seq(Helios)))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))

    // Multikino lists the 2026 romcom at 105 minutes with no year — the prod payload,
    // and 3 minutes off TMDB's 102, so nothing "agrees" here; nearness decides.
    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 105)))

    cinemasOn(c, 2026) should contain(Multikino)
    cinemasOn(c, 1961) should not contain Multikino
  }

  // The exact prod path. Multikino's LISTING ships `runtimeMinutes = 0` — the real
  // figure arrives from its detail page and is recorded on the slot — so the tick
  // that has to choose a film carries no minutes of its own. Prod had Multikino's
  // slot (105 min) sitting on the 1961 row; every tick re-confirmed it there.
  it should "use the runtime already on this venue's slot when the listing ships none" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
    // Multikino's slot, stranded on the 1961 row, carrying the 105 minutes its detail
    // page reported.
    c.putIfPresent(c.keyOf(Title, Some(1961)), r => r.copy(data = r.data + ((Multikino: Source) ->
      SourceData(title = Some(Title), runtimeMinutes = Some(105),
        showtimes = Seq(Showtime(When, bookingUrl = None))))))

    // The listing tick itself knows nothing: no year, no minutes.
    c.recordCinemaScrape(Multikino, Seq(
      yearlessScrape(Multikino, 105).copy(movie = Movie(title = Title, releaseYear = None, runtimeMinutes = Some(0)))))

    cinemasOn(c, 2026) should contain(Multikino)
    cinemasOn(c, 1961) should not contain Multikino
  }

  it should "still reach the older film when the runtime is the OLDER one's" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Seq(Helios)))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))

    // A repertory house screening Antonioni prints 121 minutes and no year.
    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 121)))

    cinemasOn(c, 1961) should contain(Multikino)
    cinemasOn(c, 2026) should not contain Multikino
  }

  // The user-visible symptom, end to end: one venue prints the year, the rest
  // don't. Both listings are the same film and must reach the same row — else the
  // read model projects a card per row and `/poznan` shows the film twice under
  // one slug, each card holding the same booking links.
  "listings of one film that disagree about whether to print a year" should
    "all land on that one film's row, not split across a same-titled older one" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))

    // Kino Spójnia is the one venue in the corpus that publishes the year …
    c.recordCinemaScrape(Helios, Seq(
      yearlessScrape(Helios, 105).copy(movie = Movie(title = Title, releaseYear = Some(2026), runtimeMinutes = Some(105)))))
    // … Multikino, like the other 31, does not.
    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 105)))

    cinemasOn(c, 2026) should contain allOf (Helios, Multikino)
    cinemasOn(c, 1961) shouldBe empty
  }

  // Convergence: whatever order the venues arrive in, and however many settles
  // run, the film ends up on ONE row with ONE card. This is the property the
  // duplicate broke — the corpus never reached a fixpoint, it just kept both.
  "the corpus" should "settle to a single row for the film, whatever the arrival order" in {
    val venues = Seq(Multikino, Helios, models.CinemaCityKinepolis, models.CinemaCityPoznanPlaza)
    venues.permutations.take(6).foreach { order =>
      val c = cache()
      c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
      c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
      order.foreach(v => c.recordCinemaScrape(v, Seq(yearlessScrape(v, 105))))
      c.canonicalizeBySanitize()

      withClue(s"arrival order ${order.map(_.displayName).mkString(", ")}: ") {
        // Every venue on the film they are actually screening …
        cinemasOn(c, 2026) should contain allElementsOf venues
        // … and none of them stranded on the same-titled 1961 film, which is what
        // put a second card under the same slug on the live page.
        cinemasOn(c, 1961) shouldBe empty
      }
    }
  }
}
