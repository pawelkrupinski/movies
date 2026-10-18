package controllers

import models._

import java.time.LocalDate

/** Deterministic sample cards for the `/debug/tune` page — built in process
 *  so the tuning page renders the real `_movieCard` partial without depending
 *  on live cache contents. The set is a deliberate spread of edge cases so
 *  every pill row, rating variant, and vertical gap is on screen at once:
 *
 *   1. `rich`        — all four ratings (RT fresh), two cinemas, two days.
 *   2. `manyTimes`   — long wrapping title + 3 genres, one cinema with many
 *                      showtimes whose format tokens all differ, so the pills
 *                      wrap across several rows with wide format badges.
 *   3. `rotten`      — RT below 60 (the `.rotten` red variant) + a low
 *                      single-digit IMDb, so the rotten styling and the
 *                      narrowest rating values show.
 *   4. `extremes`    — the widest possible values: IMDb 10.0, Metacritic 100,
 *                      RT 100%, Filmweb 10.0 — stress-tests pill width.
 *   5. `metaOnly`    — only the Metacritic bare-number pill, alone on its row.
 *   6. `noRatings`   — no enrichment at all, so the ratings row is absent and
 *                      the meta→date gap collapses to just the title gap.
 *   7. `seniorClub`  — a programme-prefixed long title (the separate-row case)
 *                      with a single no-booking showtime (the `<span>` badge
 *                      variant, not the `<a>` one).
 *   8. `sparse`      — one rating, one cinema, one showtime: the loosest case.
 */
private[controllers] object TuneSamples {

  private val base = LocalDate.of(2026, 6, 4)

  def films: Seq[FilmSchedule] =
    Seq(rich, manyTimes, rotten, extremes, metaOnly, noRatings, seniorClub, sparse)

  /** One fully-populated film (synopsis + cast + director, which the listing
   *  samples leave empty) for the `/debug/tune/movie` page, so every meta block
   *  renders and its fonts are tunable. Built off the rich sample's ratings +
   *  multi-cinema showings tree. */
  def film: FilmSchedule =
    rich.copy(
      synopsis       = Some(
        "Dom Cobb to wytrawny złodziej, najlepszy w niebezpiecznej sztuce ekstrakcji — " +
        "wykradania cennych sekretów z głębi podświadomości podczas snu. Tym razem dostaje " +
        "szansę na odkupienie: zadanie odwrotne, zaszczepienie idei zamiast jej kradzieży. " +
        "Tekst celowo długi, by dało się dostroić rozmiar i odstępy opisu na ekranie filmu."
      ),
      cast           = Seq("Leonardo DiCaprio", "Joseph Gordon-Levitt", "Elliot Page", "Tom Hardy", "Ken Watanabe"),
      director       = Seq("Christopher Nolan"),
      cinemaFilmUrls = Seq(Multikino -> "https://example.test/incepcja")
    )

  private def rich = schedule(
    resolved("Incepcja", Seq("Sci-Fi", "Akcja"), Some(148), Some(2010), imdb = Some(8.8), metascore = Some(74), rt = Some(87), filmweb = Some(7.6)),
    Seq(
      base -> Seq(
        CinemaShowtimes(Multikino, Seq(slot(base, 17, 30, List("2D", "NAP")), slot(base, 20, 15, List("2D")))),
        CinemaShowtimes(Helios,    Seq(slot(base, 18, 0, List("IMAX", "2D"))))
      ),
      base.plusDays(1) -> Seq(
        CinemaShowtimes(Multikino, Seq(slot(base.plusDays(1), 19, 45, List("2D", "DUB"))))
      )
    )
  )

  // One cinema, eight showtimes, every slot a different format token set so
  // none is stripped as "common" — the badges wrap to several rows and the
  // wide tokens (4DX, VOSE, ATMOS) stress the pill's max width.
  private def manyTimes = schedule(
    resolved("Spider-Man: Poprzez multiwersum (wersja rozszerzona)", Seq("Animacja", "Akcja", "Przygodowy"), Some(140), Some(2023), imdb = Some(8.6), metascore = Some(86), rt = Some(95), filmweb = Some(7.9)),
    Seq(base -> Seq(CinemaShowtimes(CinemaCityKinepolis, Seq(
      slot(base, 10, 0,  List("2D", "DUB")),
      slot(base, 12, 30, List("3D", "DUB")),
      slot(base, 14, 15, List("IMAX", "NAP")),
      slot(base, 16, 0,  List("4DX")),
      slot(base, 18, 20, List("VOSE")),
      slot(base, 20, 0,  List("ATMOS", "NAP")),
      slot(base, 21, 30, List("2D", "NAP", "ATMOS")),
      slot(base, 23, 0,  List("3D"))
    ))))
  )

  private def rotten = schedule(
    resolved("Morbius", Seq("Akcja", "Horror"), Some(104), Some(2022), imdb = Some(4.3), metascore = Some(35), rt = Some(15), filmweb = Some(4.1)),
    oneShowing(Helios, slot(base, 19, 0, List("2D", "NAP")))
  )

  private def extremes = schedule(
    resolved("Ojciec chrzestny", Seq("Dramat", "Kryminał"), Some(175), Some(1972), imdb = Some(10.0), metascore = Some(100), rt = Some(100), filmweb = Some(10.0)),
    oneShowing(KinoPalacowe, slot(base, 16, 45, List("2D", "NAP")))
  )

  private def metaOnly = schedule(
    resolved("Aftersun", Seq("Dramat"), Some(102), Some(2022), metascore = Some(95)),
    oneShowing(KinoMuza, slot(base, 20, 30, List("NAP")))
  )

  private def noRatings = schedule(
    resolved("Pokaz przedpremierowy: Niezatytułowany film", Seq("Dramat"), None, Some(2026)),
    oneShowing(Rialto, slot(base, 18, 15, List("NAP")))
  )

  private def seniorClub = schedule(
    resolved("Kino Seniora: Niebo nad Berlinem", Seq("Dramat", "Fantasy"), Some(128), Some(1987), imdb = Some(8.0), filmweb = Some(7.8)),
    oneShowing(KinoApollo, slot(base, 12, 0, List("NAP"), booking = false))
  )

  private def sparse = schedule(
    resolved("Cicha noc", Seq("Dramat"), Some(98), Some(2017), filmweb = Some(7.1)),
    oneShowing(KinoMuza, slot(base, 21, 0, List("2D")))
  )

  /** A single showtime at one cinema on the base day. */
  private def oneShowing(cinema: Cinema, showtime: Showtime): Seq[(LocalDate, Seq[CinemaShowtimes])] =
    Seq(base -> Seq(CinemaShowtimes(cinema, Seq(showtime))))

  private def slot(d: LocalDate, h: Int, m: Int, fmt: List[String], booking: Boolean = true): Showtime =
    Showtime(
      d.atTime(h, m),
      bookingUrl = if (booking) Some("https://example.test/book") else None,
      room       = Some("Sala 1"),
      format     = fmt
    )

  // Build a resolved-movie sample directly (the web no longer holds
  // MovieRecords). Rating hrefs are placeholders — this page tunes layout, not
  // links — and `weightedRating` uses the production formula so the grid's
  // data-rating sort behaves as in prod.
  private def resolved(
    title:     String,
    genres:    Seq[String],
    runtime:   Option[Int],
    year:      Option[Int],
    imdb:      Option[Double] = None,
    metascore: Option[Int]    = None,
    rt:        Option[Int]    = None,
    filmweb:   Option[Double] = None
  ): ResolvedMovie = {
    val ns       = Seq(imdb, filmweb, metascore.map(_ / 10.0), rt.map(_ / 10.0)).flatten
    val weighted = if (ns.isEmpty) 0.0 else ns.sum / ns.size
    ResolvedMovie(
      _id = title, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
      runtimeMinutes = runtime, releaseYear = year, genres = genres, countries = Seq.empty,
      directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
      ratings = ResolvedRatings(
        imdb = imdb, imdbUrl = imdb.map(_ => "https://www.imdb.com/"),
        metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
        rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
        filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
      ),
      weightedRating = weighted
    )
  }

  private def schedule(resolved: ResolvedMovie, showings: Seq[(LocalDate, Seq[CinemaShowtimes])]): FilmSchedule =
    FilmSchedule(
      movie          = Movie(resolved.title, runtimeMinutes = resolved.runtimeMinutes, releaseYear = resolved.releaseYear, genres = resolved.genres),
      posterUrl      = resolved.posterUrl,
      synopsis       = resolved.synopsis,
      cast           = resolved.cast,
      director       = resolved.directors,
      cinemaFilmUrls = Seq.empty,
      showings       = showings,
      resolved       = resolved,
      slug           = FilmHref.slugOf(resolved.title),
      asOf           = base
    )
}
