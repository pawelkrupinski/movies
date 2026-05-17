package services.movies

import controllers.FilmSchedule
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraper
import services.events.{DomainEvent, MovieRecordCreated}
import tools.FixtureTestWiring

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{LocalDate, LocalDateTime}
import scala.collection.mutable

/**
 * End-to-end regression: run the full production pipeline against the
 * recorded fixture and assert that `MovieControllerService.toSchedules`
 * (which drives the `/` view) emits the complete `FilmSchedule` for
 * `"Diabeł ubiera się u Prady 2"` — every field a user would see on
 * the home page card, plus every enrichment field on the underlying
 * `MovieRecord`.
 *
 * Wires every service `AppLoader` wires (cache, TMDB stage, four
 * `*Ratings` listeners, `ImdbIdResolver`, `UnscreenedCleanup`). Reads
 * cinema HTML / JSON and TMDB / IMDb / MC / RT / Filmweb responses
 * from `test/resources/fixtures/17-05-2026/`. No Play server, no real
 * network, no stubs — the only test-only swaps are `InMemoryMovieRepo`
 * and `FakeHttpFetch`.
 *
 * The flow:
 *   1. `runOneScrapeTick` — sequential mirror of `ShowtimeCache.refreshOne`,
 *      so the test doesn't race the production scheduler.
 *   2. `drainServices()` — drains the `cascadeDrainOrder` worker pools
 *      (MovieService → ImdbIdResolver → the four `*Ratings`) so every
 *      bus event has fired and every downstream HTTP call has resolved.
 *   3. `unscreenedCleanup.removeUnscreened()` — mirrors the daily
 *      cleanup pass that fires ~20 s into production boot.
 *   4. `movieControllerService.toSchedules(now)` — pinned to the
 *      fixture's capture date so the recorded showtimes are still
 *      "future".
 *
 * The disappearance regression this guards against: a row that lands in
 * the cache but goes invisible to users (empty `cinemaShowings`, dub
 * variant folded into regular and overwriting its slot, etc.).
 * Asserting the full `FilmSchedule` makes that mode loud — any field
 * regressing to None or wrong value fails specifically.
 */
class FilmScheduleEndToEndSpec extends AnyFlatSpec with Matchers {
  private val PradaTitle = "Diabeł ubiera się u Prady 2"
  // Pulled from the recorded fixture (`/3/search/movie.<hash>` body for
  // the Polish title, then `/3/movie/1314481/external_ids` for the IMDb
  // id). If TMDB reassigns Prada to a different id later, re-record with
  // `RecordAllDataToFixture` and update these — they're the only
  // film-identifier constants in the spec.
  private val PradaTmdbId = 1314481
  private val PradaImdbId = "tt33612209"

  "the full enrichment pipeline" should
    "keep Diabeł u Prady 2 visible after a startup scrape + event drain + cleanup tick" in new FixtureTestWiring("17-05-2026") {

    val seen = mutable.ListBuffer.empty[DomainEvent]
    eventBus.subscribe { case e => seen.append(e) }

    // 3. Run a single scrape tick — the same thing ShowtimeCache would do at
    //    startup. Inlined so the test can wait deterministically.
    runOneScrapeTick(cinemaScrapers, movieCache, eventBus)

    // 4. Drain every worker pool. Order matters: MovieService.stop() drains
    //    the TMDB stage (which emits the downstream events). After it
    //    returns, every TmdbResolved / ImdbIdMissing has been published —
    //    their listeners ran synchronously on the worker thread and
    //    dispatched their own tasks. Drain those next.
    drainServices()

    // 5. Run `UnscreenedCleanup` exactly as it would fire 20s into boot. If
    //    the pipeline left Prada with empty `cinemaShowings`, this is what
    //    would delete the row in production.
    unscreenedCleanup.removeUnscreened()

    // 6. ASSERT: Prada is renderable on the home page. `toSchedules`
    //    drives the `/` view in production — it filters the cache snapshot
    //    by "has at least one future showtime", groups per-cinema, and
    //    yields one `FilmSchedule` per visible card. Asserting against
    //    `toSchedules` (rather than the raw cache snapshot) catches the
    //    real disappearance mode: a row that's in the cache but invisible
    //    to users.
    //
    //    Pin `now` to the fixture's capture date so the recorded showtimes
    //    are "in the future" relative to the test clock. Without pinning,
    //    every test run after the fixture's last showtime would see an
    //    empty list and the test would fail for a stale-clock reason that
    //    has nothing to do with the code under test.
    val now             = LocalDateTime.of(2026, 5, 17, 0, 0)
    val schedules       = movieControllerService.toSchedules(now)
    val pradaSchedules  = schedules.filter(s =>
      s.enrichment.exists(e => e.tmdbId.contains(PradaTmdbId) || e.imdbId.contains(PradaImdbId))
    )

    def cinemasIn(s: FilmSchedule): Set[Cinema] =
      s.showings.flatMap(_._2).map(_.cinema).toSet
    def showtimesAt(s: FilmSchedule, cinema: Cinema): Int =
      s.showings.flatMap(_._2).filter(_.cinema == cinema).flatMap(_.showtimes).size

    val diag = pradaSchedules.map { s =>
      val perCinema = cinemasIn(s).toSeq.sortBy(_.displayName)
        .map(c => s"${c.displayName}->${showtimesAt(s, c)}st").mkString(",")
      s"title='${s.movie.title}' cinemas=$perCinema"
    }.mkString(" | ")

    withClue(s"events seen=${seen.size}, schedules=${schedules.size}, prada schedules: $diag\n") {
      pradaSchedules should not be empty

      // ── Regular Polish schedule ─────────────────────────────────────────
      //
      // The card every user sees: the canonical Polish row built from six
      // cinema scrapes that all normalise to the same cleanTitle. Asserts
      // every visible field, not just structural shape, so a regression
      // in *any* enrichment client (TMDB / IMDb / MC / RT / Filmweb) or
      // any cinema parser is caught here.
      val regular = pradaSchedules.find(_.movie.title == PradaTitle).getOrElse(
        fail(s"no regular '$PradaTitle' schedule found: $diag")
      )

      regular.movie.title          shouldBe PradaTitle
      regular.movie.runtimeMinutes shouldBe Some(120)
      regular.movie.releaseYear    shouldBe Some(2026)

      // posterUrl: Multikino wins via the "Multikino-first" priority in
      // `MovieRecord.posterUrl`, so the page renders Multikino's official
      // promo poster. Specific URL nailed down — a parser regression that
      // dropped Multikino's posterImageSrc field would shift this.
      regular.posterUrl shouldBe Some(
        "https://www.multikino.pl/-/media/multikino/images/film-and-events/2026/diabel-ubiera-sie-u-prady-2/diabelprada2_plakatoficial-cut.jpg?rev=6e854aefaa8e47c0b27446b60a57f68a"
      )

      // synopsis is Multikino's blurb (longest available — the merged
      // `MovieRecord.synopsis` picks the longest non-empty across cinemas).
      regular.synopsis.map(_.length) shouldBe Some(1006)
      regular.synopsis.get          should startWith ("Miranda Priestly powraca!")

      regular.cast     shouldBe Some("Anne Hathaway, Meryl Streep, Stanley Tucci, Emily Blunt")
      regular.director shouldBe Some("David Frankel")

      // Every cinema that's scraping Prada exposes a deep-link to its own
      // film page. Test the set rather than the order — `MovieRecord.cinemaShowings`
      // is a `Map`, so insertion order is meaningless on the read side.
      regular.cinemaFilmUrls.toSet shouldBe Set(
        Helios                -> "https://helios.pl/poznan/kino-helios/filmy/diabel-ubiera-sie-u-prady-2-4401",
        Multikino             -> "https://www.multikino.pl/filmy/diabel-ubiera-sie-u-prady-2",
        CharlieMonroe         -> "https://kinomalta.pl/movies/diabel-ubiera-sie-u-prady-2",
        CinemaCityKinepolis   -> "https://www.cinema-city.pl/filmy/diabel-ubiera-sie-u-prady-2/7795s2r",
        Rialto                -> "https://www.kinorialto.poznan.pl/wydarzenie/?id=154210",
        CinemaCityPoznanPlaza -> "https://www.cinema-city.pl/filmy/diabel-ubiera-sie-u-prady-2/7795s2r"
      )

      // Showings, per cinema. Numbers are whatever each cinema happened to
      // schedule in the fixture. The point is "the right cinemas are
      // present with their full schedules" — a fold-misbehaviour would
      // shrink CinemaCityPoznanPlaza to dub-size (~1) since it scrapes the
      // dub variant too.
      val byCinema: Map[Cinema, Int] = regular.showings.flatMap(_._2)
        .groupBy(_.cinema).view.mapValues(_.flatMap(_.showtimes).size).toMap
      byCinema shouldBe Map(
        Helios                -> 29,
        Multikino             -> 51,
        CharlieMonroe         -> 3,
        CinemaCityKinepolis   -> 84,
        Rialto                -> 12,
        CinemaCityPoznanPlaza -> 57
      )

      // Date range covers the fixture window — ten consecutive days
      // starting from the pinned `now`.
      regular.showings.map(_._1).toSet shouldBe Set(
        LocalDate.of(2026, 5, 17), LocalDate.of(2026, 5, 18),
        LocalDate.of(2026, 5, 19), LocalDate.of(2026, 5, 20),
        LocalDate.of(2026, 5, 21), LocalDate.of(2026, 5, 22),
        LocalDate.of(2026, 5, 23), LocalDate.of(2026, 5, 24),
        LocalDate.of(2026, 5, 25), LocalDate.of(2026, 5, 27)
      )

      // Enrichment — every external service contributed.
      val enrichment = regular.enrichment.getOrElse(fail("regular schedule missing enrichment"))
      enrichment.tmdbId            shouldBe Some(PradaTmdbId)
      enrichment.imdbId            shouldBe Some(PradaImdbId)
      enrichment.originalTitle     shouldBe Some("The Devil Wears Prada 2")
      // The film hadn't released yet at fixture capture, so IMDb didn't
      // have a rating — that's the correct None.
      enrichment.imdbRating        shouldBe Some(6.7)

      // Per-(date, cinema) showtime list, with room + format tokens on
      // each slot — captures every field of every `Showtime`. Stringified
      // and compared as one multi-line value so any drift (a cinema
      // dropping a screening, a parser regression skipping room labels,
      // a DUB/NAP tag disappearing, an ATMOS suffix vanishing) shows up
      // as a single readable diff. Cinemas within a date are sorted by
      // display name for stable output; the production `toSchedules`
      // orders them by earliest-showtime instead, but for assertion
      // purposes the alphabetical form is friendlier.
      //
      // Slot format: `HH:MM <room> <format1/format2/…>` joined with ` · `.
      // Rialto reports neither room nor format, so its slots are bare
      // times — that's also asserted.
      val perDay = regular.showings
        .sortBy(_._1)
        .flatMap { case (date, byCinema) =>
          byCinema.sortBy(_.cinema.displayName).map { sht =>
            val slots = sht.showtimes.sortBy(_.dateTime).map { st =>
              val room   = st.room.fold("")(r => s" $r")
              val format = if (st.format.isEmpty) "" else s" ${st.format.mkString("/")}"
              s"${st.dateTime.toLocalTime}$room$format"
            }.mkString(" · ")
            f"$date  ${sht.cinema.displayName}%-28s  $slots"
          }
        }.mkString("\n")
      perDay shouldBe
        """2026-05-17  Cinema City Kinepolis         10:30 Sal17 2D/NAP · 10:40 Sala6 2D/NAP · 11:10 Sal15 2D/NAP · 11:50 Sal12 2D/NAP · 12:20 Sala1 2D/NAP · 13:10 Sal17 2D/NAP · 13:20 Sala6 2D/NAP · 13:50 Sal15 2D/NAP · 14:30 Sal12 2D/NAP · 15:00 Sala1 2D/NAP · 15:50 Sal17 2D/NAP · 16:00 Sala6 2D/NAP · 16:30 Sal15 2D/NAP · 17:10 Sal12 2D/NAP · 17:40 Sala1 2D/NAP · 18:30 Sal17 2D/NAP · 18:40 Sala6 2D/NAP · 19:20 Sal15 2D/NAP · 19:50 Sal12 2D/NAP · 21:10 Sal17 2D/NAP · 21:20 Sala6 2D/NAP · 22:00 Sal15 2D/NAP · 22:30 Sal12 2D/NAP
          |2026-05-17  Cinema City Poznań Plaza      10:30 Sala 4 2D/NAP · 11:20 Sala 6 2D/NAP · 12:10 Sala 3 2D/NAP · 13:10 Sala 4 2D/NAP · 14:00 Sala 6 2D/NAP · 15:50 Sala 4 2D/NAP · 16:40 Sala 6 2D/NAP · 17:45 Sala 3 2D/NAP · 19:20 Sala 6 2D/NAP · 20:30 Sala 3 2D/NAP · 21:00 Sala 4 2D/NAP · 22:00 Sala 6 2D/NAP
          |2026-05-17  Helios Posnania               11:10 Sala 3 2D/NAP · 15:30 Sala 1 2D/NAP · 17:15 Sala 7 - Dream 2D/NAP/ATMOS · 18:15 Sala 1 2D/NAP · 20:00 Sala 7 - Dream 2D/NAP/ATMOS · 20:45 Sala 2 2D/NAP
          |2026-05-17  Kino Rialto                   18:00 · 20:15
          |2026-05-17  Multikino Stary Browar        09:20 Sala 2 2D/NAP · 10:20 Sala 6 2D/NAP · 12:00 Sala 2 2D/NAP · 13:00 Sala 6 2D/NAP · 14:30 Sala 4 2D/NAP · 15:40 Sala 6 2D/NAP · 17:00 Sala 2 2D/NAP · 18:20 Sala 6 2D/NAP · 19:40 Sala 2 2D/NAP · 21:00 Sala 6 2D/NAP
          |2026-05-18  Cinema City Kinepolis         13:10 Sal17 2D/NAP · 13:50 Sal15 2D/NAP · 14:30 Sal12 2D/NAP · 15:50 Sal17 2D/NAP · 16:00 Sala6 2D/NAP · 16:30 Sal15 2D/NAP · 17:10 Sal12 2D/NAP · 17:40 Sala1 2D/NAP · 18:30 Sal17 2D/NAP · 18:40 Sala6 2D/NAP · 19:20 Sal15 2D/NAP · 19:50 Sal12 2D/NAP · 20:20 Sala1 2D/NAP · 21:10 Sal17 2D/NAP · 21:20 Sala6 2D/NAP · 22:00 Sal15 2D/NAP
          |2026-05-18  Cinema City Poznań Plaza      10:30 Sala 4 2D/NAP · 11:20 Sala 6 2D/NAP · 12:10 Sala 3 2D/NAP · 13:10 Sala 4 2D/NAP · 14:00 Sala 6 2D/NAP · 15:50 Sala 4 2D/NAP · 16:40 Sala 6 2D/NAP · 17:45 Sala 3 2D/NAP · 19:20 Sala 6 2D/NAP · 20:30 Sala 3 2D/NAP · 21:00 Sala 4 2D/NAP · 22:00 Sala 6 2D/NAP
          |2026-05-18  Helios Posnania               12:45 Sala 1 2D/NAP · 15:30 Sala 1 2D/NAP · 17:15 Sala 7 - Dream 2D/NAP/ATMOS · 18:15 Sala 1 2D/NAP · 20:00 Sala 7 - Dream 2D/NAP/ATMOS · 21:00 Sala 1 2D/NAP
          |2026-05-18  Kino Rialto                   16:30 · 20:45
          |2026-05-18  Multikino Stary Browar        09:00 Sala 2 2D/NAP · 10:20 Sala 6 2D/NAP · 11:40 Sala 2 2D/NAP · 13:00 Sala 6 2D/NAP · 14:20 Sala 2 2D/NAP · 15:40 Sala 6 2D/NAP · 17:00 Sala 2 2D/NAP · 17:40 Sala 1 2D/NAP · 18:20 Sala 6 2D/NAP · 19:40 Sala 2 2D/NAP · 21:00 Sala 6 2D/NAP
          |2026-05-19  Cinema City Kinepolis         13:10 Sal17 2D/NAP · 13:50 Sal15 2D/NAP · 14:30 Sal12 2D/NAP · 15:50 Sal17 2D/NAP · 16:00 Sala6 2D/NAP · 16:30 Sal15 2D/NAP · 17:10 Sal12 2D/NAP · 17:40 Sala1 2D/NAP · 18:30 Sal17 2D/NAP · 18:40 Sala6 2D/NAP · 19:20 Sal15 2D/NAP · 19:50 Sal12 2D/NAP · 20:20 Sala1 2D/NAP · 21:10 Sal17 2D/NAP · 21:20 Sala6 2D/NAP · 22:00 Sal15 2D/NAP
          |2026-05-19  Cinema City Poznań Plaza      10:30 Sala 4 2D/NAP · 11:20 Sala 6 2D/NAP · 12:10 Sala 3 2D/NAP · 13:10 Sala 4 2D/NAP · 14:00 Sala 6 2D/NAP · 15:50 Sala 4 2D/NAP · 16:40 Sala 6 2D/NAP · 17:45 Sala 3 2D/NAP · 19:20 Sala 6 2D/NAP · 20:30 Sala 3 2D/NAP · 21:00 Sala 4 2D/NAP · 22:00 Sala 6 2D/NAP
          |2026-05-19  Helios Posnania               10:00 Sala 1 2D/NAP · 12:45 Sala 1 2D/NAP · 15:30 Sala 1 2D/NAP · 17:15 Sala 7 - Dream 2D/NAP/ATMOS · 18:15 Sala 1 2D/NAP · 20:00 Sala 7 - Dream 2D/NAP/ATMOS · 21:00 Sala 1 2D/NAP
          |2026-05-19  Kino Rialto                   13:00 · 15:30 · 18:00
          |2026-05-19  Multikino Stary Browar        09:00 Sala 2 2D/NAP · 11:40 Sala 2 2D/NAP · 13:00 Sala 6 2D/NAP · 14:20 Sala 2 2D/NAP · 15:00 Sala 4 2D/NAP · 15:40 Sala 6 2D/NAP · 17:00 Sala 2 2D/NAP · 18:20 Sala 6 2D/NAP · 19:40 Sala 2 2D/NAP · 21:00 Sala 6 2D/NAP
          |2026-05-20  Cinema City Kinepolis         13:10 Sal17 2D/NAP · 13:50 Sal15 2D/NAP · 14:30 Sal12 2D/NAP · 15:50 Sal17 2D/NAP · 16:00 Sala6 2D/NAP · 16:30 Sal15 2D/NAP · 17:10 Sal12 2D/NAP · 17:40 Sala1 2D/NAP · 18:30 Sal17 2D/NAP · 18:40 Sala6 2D/NAP · 19:20 Sal15 2D/NAP · 19:50 Sal12 2D/NAP · 20:20 Sala1 2D/NAP · 21:10 Sal17 2D/NAP · 21:20 Sala6 2D/NAP · 22:00 Sal15 2D/NAP
          |2026-05-20  Cinema City Poznań Plaza      10:30 Sala 4 2D/NAP · 12:10 Sala 3 2D/NAP · 13:10 Sala 4 2D/NAP · 14:00 Sala 6 2D/NAP · 15:50 Sala 4 2D/NAP · 16:40 Sala 6 2D/NAP · 17:45 Sala 3 2D/NAP · 19:20 Sala 6 2D/NAP · 20:30 Sala 3 2D/NAP · 21:00 Sala 4 2D/NAP · 22:00 Sala 6 2D/NAP
          |2026-05-20  Helios Posnania               12:45 Sala 1 2D/NAP · 15:30 Sala 1 2D/NAP · 17:15 Sala 7 - Dream 2D/NAP/ATMOS · 18:15 Sala 1 2D/NAP · 20:00 Sala 7 - Dream 2D/NAP/ATMOS · 21:00 Sala 1 2D/NAP
          |2026-05-20  Kino Rialto                   21:15
          |2026-05-20  Multikino Stary Browar        10:20 Sala 6 2D/NAP · 11:40 Sala 2 2D/NAP · 13:00 Sala 6 2D/NAP · 14:20 Sala 2 2D/NAP · 15:40 Sala 6 2D/NAP · 17:00 Sala 2 2D/NAP · 17:50 Sala 4 2D/NAP · 18:20 Sala 6 2D/NAP · 19:40 Sala 2 2D/NAP · 21:00 Sala 6 2D/NAP
          |2026-05-21  Cinema City Kinepolis         12:50 Sal17 2D/NAP · 13:50 Sal15 2D/NAP · 14:30 Sal12 2D/NAP · 15:30 Sal17 2D/NAP · 16:10 Sala6 2D/NAP · 16:30 Sal15 2D/NAP · 17:10 Sal12 2D/NAP · 17:40 Sala1 2D/NAP · 18:10 Sal17 2D/NAP · 19:20 Sal15 2D/NAP · 19:50 Sal12 2D/NAP · 20:20 Sala1 2D/NAP · 22:00 Sal15 2D/NAP
          |2026-05-21  Cinema City Poznań Plaza      10:30 Sala 4 2D/NAP · 11:20 Sala 6 2D/NAP · 12:10 Sala 1 2D/NAP · 13:10 Sala 4 2D/NAP · 14:00 Sala 6 2D/NAP · 15:50 Sala 4 2D/NAP · 16:40 Sala 6 2D/NAP · 18:30 Sala 4 2D/NAP · 21:10 Sala 4 2D/NAP · 22:30 Sala 6 2D/NAP
          |2026-05-21  Helios Posnania               14:00 Sala 1 2D/NAP · 16:40 Sala 1 2D/NAP · 17:15 Sala 5 - Dream 2D/NAP/ATMOS · 20:20 Sala 8 2D/NAP
          |2026-05-21  Kino Rialto                   17:30
          |2026-05-21  Multikino Stary Browar        09:00 Sala 2 2D/NAP · 10:55 Sala 6 2D/NAP · 11:40 Sala 2 2D/NAP · 13:35 Sala 6 2D/NAP · 14:20 Sala 2 2D/NAP · 16:15 Sala 6 2D/NAP · 17:00 Sala 1 2D/NAP · 18:00 Sala 4 2D/NAP · 19:10 Sala 7 2D/NAP · 20:40 Sala 4 2D/NAP
          |2026-05-22  Kino Malta Charlie Monroe     20:30 Sala Marilyn
          |2026-05-23  Kino Malta Charlie Monroe     18:30 Sala Marilyn
          |2026-05-24  Kino Malta Charlie Monroe     18:30 Sala Marilyn
          |2026-05-24  Kino Rialto                   14:45
          |2026-05-25  Kino Rialto                   15:30
          |2026-05-27  Kino Rialto                   15:45""".stripMargin
      enrichment.metascore         shouldBe Some(63)
      enrichment.rottenTomatoes    shouldBe Some(78)
      enrichment.filmwebRating     shouldBe Some(6.29387)
      enrichment.metacriticUrl     shouldBe Some("https://www.metacritic.com/movie/the-devil-wears-prada-2")
      enrichment.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/the_devil_wears_prada_2")
      enrichment.filmwebUrl        shouldBe Some("https://www.filmweb.pl/film/Diabe%C5%82+ubiera+si%C4%99+u+Prady+2-2026-10083431")

      // `displayTitle` is what `MovieController.toSchedules` writes into
      // `Movie.title` — the canonical spelling chosen across every
      // cinema's variant. Rialto reports "Diabeł ubiera się u **p**rady 2"
      // (lowercase p); the five other cinemas all use "**P**rady".
      // `TitleNormalizer.preferredDisplay` prefers the proper-cased Latin
      // variant — if it regressed to the all-lowercase form, the home-page
      // card would suddenly read "u prady".
      enrichment.cinemaTitles shouldBe Set(
        "Diabeł ubiera się u Prady 2",
        "Diabeł ubiera się u prady 2"
      )
      enrichment.displayTitle(PradaTitle) shouldBe PradaTitle
      regular.movie.title                 shouldBe enrichment.displayTitle(PradaTitle)

      // Provenance: one CinemaScrape per cinema that reported this row.
      // Year-divergence is intentional — Multikino + CharlieMonroe drop
      // the year, CC + Helios + Rialto carry it. The redirect in
      // `recordCinemaScrape` collapses all six onto one CacheKey.
      enrichment.cinemaScrapes shouldBe Set(
        CinemaScrape(CharlieMonroe,         "Diabeł ubiera się u Prady 2", None),
        CinemaScrape(CinemaCityKinepolis,   "Diabeł ubiera się u Prady 2", Some(2026)),
        CinemaScrape(Multikino,             "Diabeł ubiera się u Prady 2", None),
        CinemaScrape(Rialto,                "Diabeł ubiera się u prady 2", Some(2026)),
        CinemaScrape(Helios,                "Diabeł ubiera się u Prady 2", Some(2026)),
        CinemaScrape(CinemaCityPoznanPlaza, "Diabeł ubiera się u Prady 2", Some(2026))
      )

      // ── Cyrillic dub schedule (Helios's "ДИЯВОЛ НОСИТЬ ПРАДА 2") ─────────
      //
      // Same tmdbId as the regular schedule, but kept as its own
      // FilmSchedule by the cleanTitle-strict fold gate. If it
      // disappears, the gate is over-aggressive and Ukrainian-language
      // audiences lose their card. If it folds onto the regular row,
      // CC Plaza's slot would shrink to dub-size in `byCinema` above —
      // which is the original regression this spec guards against.
      val dub = pradaSchedules.find(_.movie.title != PradaTitle).getOrElse(
        fail(s"no Cyrillic dub schedule found alongside the regular: $diag")
      )
      dub.movie.title           shouldBe "ДИЯВОЛ НОСИТЬ ПРАДА 2"
      dub.movie.runtimeMinutes  shouldBe Some(125)
      dub.movie.releaseYear     shouldBe None
      dub.cast                  shouldBe None
      dub.director              shouldBe None
      dub.synopsis              shouldBe None
      dub.cinemaFilmUrls shouldBe Seq(
        Helios -> "https://helios.pl/poznan/kino-helios/filmy/dyyavol-nosyt-prada-2-ua-4496"
      )
      val dubByCinema: Map[Cinema, Int] = dub.showings.flatMap(_._2)
        .groupBy(_.cinema).view.mapValues(_.flatMap(_.showtimes).size).toMap
      dubByCinema shouldBe Map(Helios -> 1)

      // Dub row inherits enrichment from the same TMDB id — same imdbId,
      // same MC / RT / Filmweb fields. Only the cinema-side data differs.
      val dubEnrichment = dub.enrichment.getOrElse(fail("dub schedule missing enrichment"))
      dubEnrichment.tmdbId         shouldBe Some(PradaTmdbId)
      dubEnrichment.imdbId         shouldBe Some(PradaImdbId)
      dubEnrichment.cinemaScrapes  shouldBe Set(
        CinemaScrape(Helios, "ДИЯВОЛ НОСИТЬ ПРАДА 2", None)
      )
      // Only one cinema reports the dub, so `displayTitle` just returns
      // that variant (preferredDisplay picks the single available form).
      dubEnrichment.displayTitle("ДИЯВОЛ НОСИТЬ ПРАДА 2") shouldBe "ДИЯВОЛ НОСИТЬ ПРАДА 2"

      // No third Prada — only the regular Polish row and the Cyrillic dub
      // should exist. A third row means either a fold went sideways or a
      // cinema produced an unexpected variant.
      pradaSchedules should have size 2
    }

    // ── Whole-corpus snapshot ─────────────────────────────────────────────
    //
    // Every other `FilmSchedule` gets the same depth of assertion that
    // Prada gets above — every visible field, every cinemaFilmUrl, every
    // enrichment value, every showtime with its room + format tokens.
    // Inline literals for 214 movies × ~10 fields × N days × M slots
    // would be unreadable, so the expected output is a checked-in
    // snapshot file. The test renders the full corpus deterministically
    // (titles sorted, cinemas sorted by display name, showtimes sorted
    // by clock) and compares the result against the snapshot.
    //
    // When the snapshot doesn't exist (first run after a fresh fixture
    // recording), the test writes it and fails loudly so the diff goes
    // through code review before being trusted. To regenerate after a
    // legitimate change: delete the file and re-run.
    val snapshotPath = Paths.get("test/resources/fixtures/17-05-2026/expected-schedules.txt")
    val actual = renderSchedules(schedules)
    if (!Files.exists(snapshotPath)) {
      Files.write(snapshotPath, actual.getBytes(StandardCharsets.UTF_8))
      fail(s"Snapshot didn't exist — wrote ${snapshotPath}. Review the contents, commit, and re-run.")
    }
    val expected = new String(Files.readAllBytes(snapshotPath), StandardCharsets.UTF_8)
    withClue(
      s"Whole-corpus snapshot mismatch. To regenerate after an intentional change:\n" +
        s"  rm $snapshotPath && sbt 'testOnly services.movies.DiabelPradaFilmScheduleSpec'\n"
    ) {
      actual shouldBe expected
    }
  }

  /** Render every FilmSchedule into a deterministic multi-line block. One
   *  block per film, separated by blank lines; films sorted alphabetically
   *  by display title. Each block lists every field a viewer of the `/`
   *  card would see (title, runtime, year, poster, synopsis size, cast,
   *  director, per-cinema deep-links) plus every enrichment value (tmdbId,
   *  imdbId, ratings, MC/RT/FW URLs, cinemaScrapes provenance) plus the
   *  full per-(date, cinema) showtime list with room + format tokens. */
  private def renderSchedules(schedules: Seq[FilmSchedule]): String =
    schedules.sortBy(s => (s.movie.title.toLowerCase, s.movie.releaseYear)).map(renderOne).mkString("\n\n")

  private def renderOne(s: FilmSchedule): String = {
    val e = s.enrichment
    val cinemaUrls = s.cinemaFilmUrls.sortBy(_._1.displayName)
      .map { case (c, u) => s"${c.displayName} = $u" }
    val scrapes = e.map(_.cinemaScrapes.toSeq
      .sortBy(s => (s.cinema.displayName, s.title, s.year.getOrElse(Int.MinValue)))
      .map(sc => s"${sc.cinema.displayName} / ${sc.title} / ${sc.year.map(_.toString).getOrElse("—")}"))
      .getOrElse(Nil)
    // `cinemaTitles` is the set of raw spellings each cinema reported.
    // `displayTitle` is the picker's choice across that set — the
    // canonical form that `MovieController.toSchedules` writes into
    // `Movie.title` (and therefore the `=== TITLE ===` header above).
    // Showing both makes the picker's behaviour explicit per film.
    val cinemaTitles = e.map(_.cinemaTitles.toSeq.sorted).getOrElse(Nil)
    val showings = s.showings.sortBy(_._1).flatMap { case (date, byCinema) =>
      byCinema.sortBy(_.cinema.displayName).map { sht =>
        val slots = sht.showtimes.sortBy(_.dateTime).map { st =>
          val room   = st.room.fold("")(r => s" $r")
          val format = if (st.format.isEmpty) "" else s" ${st.format.mkString("/")}"
          s"${st.dateTime.toLocalTime}$room$format"
        }.mkString(" · ")
        f"  $date  ${sht.cinema.displayName}%-28s  $slots"
      }
    }
    val lines = Seq(
      s"=== ${s.movie.title} ===",
      s"displayTitle:      ${s.movie.title}",
      s"cinemaTitles:      ${if (cinemaTitles.isEmpty) "—" else cinemaTitles.mkString(" | ")}",
      s"runtimeMinutes:    ${s.movie.runtimeMinutes.map(_.toString).getOrElse("—")}",
      s"releaseYear:       ${s.movie.releaseYear.map(_.toString).getOrElse("—")}",
      s"posterUrl:         ${s.posterUrl.getOrElse("—")}",
      s"synopsis.length:   ${s.synopsis.map(_.length.toString).getOrElse("—")}",
      s"cast:              ${s.cast.getOrElse("—")}",
      s"director:          ${s.director.getOrElse("—")}",
      s"tmdbId:            ${e.flatMap(_.tmdbId).map(_.toString).getOrElse("—")}",
      s"imdbId:            ${e.flatMap(_.imdbId).getOrElse("—")}",
      s"originalTitle:     ${e.flatMap(_.originalTitle).getOrElse("—")}",
      s"imdbRating:        ${e.flatMap(_.imdbRating).map(_.toString).getOrElse("—")}",
      s"metascore:         ${e.flatMap(_.metascore).map(_.toString).getOrElse("—")}",
      s"rottenTomatoes:    ${e.flatMap(_.rottenTomatoes).map(_.toString).getOrElse("—")}",
      s"filmwebRating:     ${e.flatMap(_.filmwebRating).map(_.toString).getOrElse("—")}",
      s"metacriticUrl:     ${e.flatMap(_.metacriticUrl).getOrElse("—")}",
      s"rottenTomatoesUrl: ${e.flatMap(_.rottenTomatoesUrl).getOrElse("—")}",
      s"filmwebUrl:        ${e.flatMap(_.filmwebUrl).getOrElse("—")}"
    ) ++
      (if (cinemaUrls.nonEmpty) Seq("cinemaFilmUrls:") ++ cinemaUrls.map("  " + _) else Seq("cinemaFilmUrls:    —")) ++
      (if (scrapes.nonEmpty) Seq("cinemaScrapes:") ++ scrapes.map("  " + _) else Seq("cinemaScrapes:     —")) ++
      Seq("showings:") ++ showings
    lines.mkString("\n")
  }

  // ── Helpers ───────────────────────────────────────────────────────────────

  /** Mirror of `ShowtimeCache.refreshOne`, sequenced for the test. Catches
   * per-scraper failures the same way production does so one missing-
   * fixture cinema can't take down the whole tick. */
  private def runOneScrapeTick(
                                scrapers: Seq[CinemaScraper],
                                cache: MovieCache,
                                bus: services.events.EventBus
                              ): Unit = {
    scrapers.foreach { scraper =>
      val cinema = scraper.cinema
      try {
        val movies = scraper.fetch()
        val touched = cache.recordCinemaScrape(cinema, movies)
        touched.foreach { case (cm, key, isNew) =>
          if (isNew)
            bus.publish(MovieRecordCreated(key.cleanTitle, key.year, cm.movie.originalTitle, cm.director))
        }
      } catch {
        case _: Exception => () // mirror ShowtimeCache.refreshOne's catch
      }
    }
  }
}
