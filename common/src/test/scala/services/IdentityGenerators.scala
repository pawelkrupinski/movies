package services

import models._
import org.scalacheck.Gen
import services.movies.{CacheKey, MovieRecordMerge}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.{Candidate, FilmEvidence}

import java.time.LocalDateTime
import java.util.Locale

/**
 * ScalaCheck generators for the identity core's values: cinema and enrichment
 * slots, `MovieRecord`s built from them, `CacheKey`s, canonicaliser rows,
 * `FilmEvidence` and `Candidate`s.
 *
 * Every pool is deliberately SMALL — four cinemas, four base titles with a
 * handful of spelling / decoration variants each, years from three neighbours,
 * tmdbIds from {1, 2} — so that the shapes the identity rules exist for (two
 * rows of one film, one venue listing a film twice, a resolved row beside its
 * unresolved sibling, a same-titled different film) come up in nearly every
 * case instead of almost never.
 *
 * Strings are never empty: the merge normalises `Some("")` to `None`, which is
 * a cleanup rather than a merge decision, and the scrapers never store one.
 */
object IdentityGenerators {

  val cinemas: Seq[Cinema] = Seq(Multikino, Helios, KinoApollo, KinoMuza)

  val baseTitles: Seq[String] = Seq("Diuna", "Zaplątani", "Straszny film", "Obcy")

  /** The ways a venue spells one film: as is, shouted, undiacritic'd, with a
   *  dub marker `apiQuery` strips, under a programme banner it does not, and as
   *  a numbered sequel (a different film with the same words). */
  def titleVariants(base: String): Seq[String] = Seq(
    base,
    base.toUpperCase(Locale.ROOT),
    tools.TextNormalization.deburr(base),
    s"$base ukraiński dubbing",
    s"WAJDA: re-wizje: $base",
    s"$base 2")

  val genBaseTitle: Gen[String] = Gen.oneOf(baseTitles)
  val genTitle:     Gen[String] = genBaseTitle.flatMap(base => Gen.oneOf(titleVariants(base)))

  /** Credits in the shapes `SamePerson` was taught — the same person written
   *  surname-first, initialled, undiacritic'd, mis-transliterated — plus a
   *  near-miss pair and a credit the fold cannot read at all. */
  val latinNames: Seq[String] = Seq(
    "Yann Gozlan", "Pedro Almodóvar", "Pedro Almodovar", "Enyedi Ildikó", "Ildikó Enyedi",
    "Bong Joon Ho", "Bong Joon Il", "Andrzej Wajda", "A. Wajda", "Lars von Trier",
    "Robert Downey Jr.", "Fatih Akın", "Fatih Akin", "Alejandro G. Iñárritu",
    "Alejandro González Iñárritu", "Michael Gottli", "Michael Gottlieb")
  val names: Seq[String] = latinNames :+ "王家衛"

  val genLatinName: Gen[String] = Gen.oneOf(latinNames)
  val genName:      Gen[String] = Gen.oneOf(names)

  val genYear:    Gen[Option[Int]]    = Gen.oneOf(None, Some(2024), Some(2025), Some(2026))
  val genRuntime: Gen[Option[Int]]    = Gen.oneOf(None, Some(90), Some(100), Some(150))
  val genTmdbId:  Gen[Option[Int]]    = Gen.oneOf(None, Some(1), Some(2))
  val genImdbId:  Gen[Option[String]] = Gen.oneOf(None, Some("tt1"), Some("tt2"))

  val genOriginalTitle: Gen[Option[String]] =
    Gen.oneOf(None, None, Some("Dune"), Some("Tangled"), Some("Scary Movie"))

  val genShowtime: Gen[Showtime] = for {
    day    <- Gen.choose(1, 3)
    hour   <- Gen.oneOf(12, 18, 20)
    url    <- Gen.oneOf(None, Some("https://tickets.example/1"), Some("https://tickets.example/2"))
    room   <- Gen.oneOf(None, Some("Sala 1"))
    format <- Gen.oneOf(Nil, List("2D"), List("3D", "NAP"))
  } yield Showtime(LocalDateTime.of(2026, 9, day, hour, 0), url, room, format)

  /** Showtimes as the ingest boundary stores them: one per physical screening,
   *  totally ordered. */
  val genShowtimes: Gen[Seq[Showtime]] =
    Gen.choose(0, 4).flatMap(Gen.listOfN(_, genShowtime)).map(MovieRecordMerge.dedupShowtimes)

  private def genSlot(title: Option[String]): Gen[SourceData] = for {
    original  <- genOriginalTitle
    synopsis  <- Gen.option(Gen.oneOf("Krótki opis.", "A much longer synopsis of the very same film."))
    cast      <- Gen.someOf(latinNames.take(4))
    director  <- Gen.choose(0, 2).flatMap(Gen.listOfN(_, genName))
    runtime   <- genRuntime
    year      <- genYear
    countries <- Gen.oneOf(Nil, Seq("Polska"), Seq("USA", "Polska"))
    genres    <- Gen.oneOf(Nil, Seq("Horror"), Seq("Thriller"))
    poster    <- Gen.option(Gen.oneOf("https://posters.example/1.jpg", "https://posters.example/2.jpg"))
    filmUrl   <- Gen.option(Gen.oneOf("https://cinema.example/f1", "https://cinema.example/f2"))
    trailer   <- Gen.option(Gen.const("https://youtube.com/watch?v=x"))
    showtimes <- genShowtimes
    age       <- Gen.option(Gen.oneOf("12", "15"))
  } yield SourceData(
    title = title, rawTitle = title, originalTitle = original, synopsis = synopsis,
    cast = cast.toSeq, director = director.distinct, runtimeMinutes = runtime, releaseYear = year,
    countries = countries, genres = genres, posterUrl = poster, filmUrl = filmUrl,
    trailerUrl = trailer, showtimes = showtimes, ageRating = age)

  /** A venue's slot for one shown title, keyed the way the ingest keys it. */
  def genCinemaSlot(cinemaPool: Seq[Cinema] = cinemas, titles: Gen[String] = genTitle): Gen[(Source, SourceData)] =
    for {
      cinema <- Gen.oneOf(cinemaPool)
      title  <- titles
      slot   <- genSlot(Some(title))
    } yield CinemaShowing.keyFor(cinema, title, titleNormalizer) -> slot

  /** A Tmdb / Imdb / Filmweb slot — a previous resolution's derived data,
   *  titled by a bare base title so it can alias a cinema's spelling. */
  val genEnrichmentSlot: Gen[(Source, SourceData)] = for {
    source  <- Gen.oneOf(Tmdb, Imdb, Filmweb)
    base    <- genBaseTitle
    slot    <- genSlot(Some(base))
    english <- Gen.option(Gen.oneOf("Dune", "Tangled"))
    lang    <- Gen.oneOf(None, Some("pl-PL"), Some("de-DE"))
  } yield source -> slot.copy(rawTitle = None, englishTitle = english, filmUrl = None, showtimes = Nil, language = lang)

  val genAnySlot: Gen[(Source, SourceData)] = Gen.frequency(3 -> genCinemaSlot(), 1 -> genEnrichmentSlot)

  /** A record holding up to `maxSlots` of `slots` (later duplicates of a key
   *  win, as in a `Map`) with the single-source enrichment fields drawn freely. */
  def genMovieRecord(slots: Gen[(Source, SourceData)] = genAnySlot, maxSlots: Int = 4): Gen[MovieRecord] = for {
    n         <- Gen.choose(0, maxSlots)
    data      <- Gen.listOfN(n, slots).map(_.toMap)
    tmdbId    <- genTmdbId
    imdbId    <- genImdbId
    imdb      <- Gen.option(Gen.oneOf(6.5, 7.8))
    meta      <- Gen.option(Gen.oneOf(60, 80))
    filmweb   <- Gen.option(Gen.oneOf(7.1, 7.9))
    fwUrl     <- Gen.option(Gen.const("https://www.filmweb.pl/film/x"))
    rt        <- Gen.option(Gen.oneOf(70, 90))
    search    <- Gen.option(genBaseTitle)
    retained  <- Gen.someOf(data.keys.toSeq).flatMap(keys =>
                   Gen.listOfN(keys.size, Gen.oneOf("Krótki.", "A longer retained synopsis.")).map(keys.zip(_)))
  } yield MovieRecord(
    imdbId = imdbId, imdbRating = imdb, metascore = meta, filmwebUrl = fwUrl, filmwebRating = filmweb,
    rottenTomatoes = rt, tmdbId = tmdbId, searchTitle = search, data = data, retainedSynopses = retained.toMap)

  val genCacheKey: Gen[CacheKey] =
    for { title <- genTitle; year <- genYear } yield CacheKey(title, year, titleNormalizer)

  /** A corpus row as the canonicaliser sees it: a key, and a record whose cinema
   *  slots spell the film like the key does (or as another variant of the same
   *  base), resolved with a `Tmdb` slot when it carries a tmdbId. Unresolved rows
   *  carry no enrichment ids — nothing writes one before a resolution. */
  val genRow: Gen[(CacheKey, MovieRecord)] = for {
    base     <- genBaseTitle
    title    <- Gen.oneOf(titleVariants(base))
    year     <- genYear
    tmdbId   <- genTmdbId
    imdbId   <- if (tmdbId.isDefined) genImdbId else Gen.const(None)
    tmdbYear <- Gen.oneOf(Some(2024), Some(2025), Some(2026))
    n        <- Gen.choose(1, 2)
    cinema   <- Gen.listOfN(n, genCinemaSlot(titles = Gen.frequency(3 -> Gen.const(title), 1 -> Gen.oneOf(titleVariants(base)))))
    tmdb     <- genSlot(Some(base)).map(_.copy(rawTitle = None, releaseYear = tmdbYear, filmUrl = None, showtimes = Nil))
    english  <- Gen.option(Gen.oneOf("Dune", "Tangled"))
  } yield {
    val cinemaSlots = cinema.map { case (source, slot) => source -> slot.copy(releaseYear = year) }.toMap
    val data        = if (tmdbId.isDefined) cinemaSlots + (Tmdb -> tmdb.copy(englishTitle = english)) else cinemaSlots
    CacheKey(title, year, titleNormalizer) -> MovieRecord(tmdbId = tmdbId, imdbId = imdbId, data = data)
  }

  /** Up to six rows under DISTINCT keys — a corpus never holds two rows of one key. */
  val genRows: Gen[Seq[(CacheKey, MovieRecord)]] =
    Gen.choose(1, 6).flatMap(Gen.listOfN(_, genRow)).map(_.distinctBy(_._1))

  /** Evidence in the normal form `FilmEvidence.of` produces: sorted, distinct. */
  val genFilmEvidence: Gen[FilmEvidence] = for {
    titles    <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, genTitle))
    originals <- Gen.someOf(Seq("Dune", "Tangled", "Scary Movie"))
    directors <- Gen.someOf(names)
    cast      <- Gen.someOf(latinNames.take(4))
    runtimes  <- Gen.someOf(Seq(90, 100, 150))
    years     <- Gen.someOf(Seq(2024, 2025, 2026))
  } yield FilmEvidence(
    slotTitles = titles.sorted, originalTitles = originals.toSeq, directors = directors.toSeq.sorted,
    cast = cast.toSeq.sorted, runtimes = runtimes.toSeq.sorted, years = years.toSeq.sorted)

  val genCandidate: Gen[Candidate] = for {
    tmdbId  <- Gen.oneOf(1, 2)
    titles  <- Gen.someOf(baseTitles)
    year    <- genYear
    runtime <- genRuntime
    crew    <- Gen.someOf(names)
    cast    <- Gen.someOf(latinNames.take(4))
  } yield Candidate(tmdbId, titles.toSet, year, runtime, crew.toSeq, cast.toSeq)
}
