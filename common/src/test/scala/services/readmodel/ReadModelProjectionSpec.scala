package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{StoredMovieRecord, TitleNormalizer}

import java.time.LocalDateTime

class ReadModelProjectionSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  // Multikino (Poznań) + Helios Magnolia (Wrocław) both screen the film; TMDB
  // contributes synopsis/genres/original title. Two cities, two cinemas.
  private val record = MovieRecord(
    imdbId        = Some("tt0111161"),
    imdbRating    = Some(9.3),
    metascore     = Some(82),
    rottenTomatoes = Some(91),
    filmwebRating = Some(8.9),
    tmdbId        = Some(278),
    data = Map[Source, SourceData](
      Multikino -> SourceData(
        title = Some("Skazani na Shawshank"), releaseYear = Some(1994),
        posterUrl = Some("https://mk/poster.jpg"), filmUrl = Some("https://mk/film"),
        showtimes = Seq(at("2026-06-12T20:00"), at("2026-06-12T17:30"))
      ),
      HeliosMagnolia -> SourceData(
        title = Some("Skazani na Shawshank"), releaseYear = Some(1994),
        posterUrl = Some("https://he/poster.jpg"), filmUrl = Some("https://he/film"),
        showtimes = Seq(at("2026-06-13T19:00"))
      ),
      Tmdb -> SourceData(
        originalTitle = Some("The Shawshank Redemption"),
        synopsis = Some("Chronicle of life in a state penitentiary."),
        genres = Seq("Dramat")
      )
    )
  )

  private val id     = s"${TitleNormalizer.sanitize("Skazani na Shawshank")}|1994"
  private val stored = StoredMovieRecord.fromStorage(id, record)

  private val (movie, screenings) = ReadModelProjection.project(stored)

  "resolve" should "materialise merged metadata and carry no source data" in {
    movie._id shouldBe s"${TitleNormalizer.sanitize("Skazani na Shawshank")}|1994"
    movie.title shouldBe "Skazani na Shawshank"
    movie.originalTitle shouldBe Some("The Shawshank Redemption")
    movie.synopsis shouldBe Some("Chronicle of life in a state penitentiary.")
    movie.genres shouldBe Seq("Dramat")
    movie.releaseYear shouldBe Some(1994)
    movie.posterUrl shouldBe Some("https://mk/poster.jpg")
    movie.fallbackPosterUrls should contain("https://he/poster.jpg")
    // No field on ResolvedMovie exposes the per-source `data` map — it's a flat
    // projection by construction (compile-time guarantee; asserted on the
    // resolved values above).
  }

  it should "scope synopsis per city, with the TMDB blurb as the city-independent fallback" in {
    // Both cinemas carry their own (paragraphed) blurb; TMDB carries a different
    // one. Each city must show its own cinema's text, the fallback stays TMDB's.
    val rec = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino      -> SourceData(title = Some("X"),
        synopsis = Some("Poznański opis kina.\n\nAkapit drugi."), showtimes = Seq(at("2026-06-12T18:00"))),
      HeliosMagnolia -> SourceData(title = Some("X"),
        synopsis = Some("Wrocławski opis kina.\n\nAkapit drugi."), showtimes = Seq(at("2026-06-12T19:00"))),
      Tmdb           -> SourceData(title = Some("X"), synopsis = Some("Opis z TMDB."))
    ))
    val (m, _) = ReadModelProjection.project(StoredMovieRecord.fromStorage("x|", rec))
    m.synopsis shouldBe Some("Opis z TMDB.")                              // city-independent fallback (no cinema)
    m.synopsisByCity.keySet shouldBe Set("poznan", "wroclaw")
    m.synopsisFor(Poznan).get  should (include ("Poznański")  and not include ("Wrocławski"))
    m.synopsisFor(Wroclaw).get should (include ("Wrocławski") and not include ("Poznański"))
  }

  it should "omit a city from synopsisByCity when its pick ties the fallback" in {
    // Only Wrocław has a cinema blurb; Poznań's best ties the TMDB fallback, so
    // it isn't stored — `synopsisFor(Poznań)` resolves via the fallback instead.
    val rec = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino      -> SourceData(title = Some("X"), showtimes = Seq(at("2026-06-12T18:00"))),
      HeliosMagnolia -> SourceData(title = Some("X"),
        synopsis = Some("Wrocławski opis kina, wyraźnie dłuższy niż TMDB."), showtimes = Seq(at("2026-06-12T19:00"))),
      Tmdb           -> SourceData(title = Some("X"), synopsis = Some("Opis z TMDB."))
    ))
    val (m, _) = ReadModelProjection.project(StoredMovieRecord.fromStorage("x|", rec))
    m.synopsisByCity.keySet shouldBe Set("wroclaw")
    m.synopsisFor(Poznan)  shouldBe Some("Opis z TMDB.")
    m.synopsisFor(Wroclaw) shouldBe Some("Wrocławski opis kina, wyraźnie dłuższy niż TMDB.")
  }

  it should "scope synopsis correctly across MANY cities sharing one candidate pool" in {
    // Guards the memoised per-candidate processing (bestSynopsis reuses the collapse →
    // stripUrls → strip work per raw string across every city the film screens in):
    // three cities, each with its own longer cinema blurb, plus one shared TMDB blurb
    // that every city's candidate pool contains. A memo that cross-contaminated cities —
    // or keyed the processed result to the wrong raw string — would leak one city's text
    // into another; each city must still show only its own.
    val rec = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino      -> SourceData(title = Some("X"),
        synopsis = Some("Poznański opis kina, wyraźnie dłuższy niż blurb z TMDB."), showtimes = Seq(at("2026-06-12T18:00"))),
      HeliosMagnolia -> SourceData(title = Some("X"),
        synopsis = Some("Wrocławski opis kina, wyraźnie dłuższy niż blurb z TMDB."), showtimes = Seq(at("2026-06-12T19:00"))),
      KinoMuranow    -> SourceData(title = Some("X"),
        synopsis = Some("Warszawski opis kina, wyraźnie dłuższy niż blurb z TMDB."), showtimes = Seq(at("2026-06-12T20:00"))),
      Tmdb           -> SourceData(title = Some("X"), synopsis = Some("Opis z TMDB."))
    ))
    val (m, _) = ReadModelProjection.project(StoredMovieRecord.fromStorage("x|", rec))
    m.synopsis shouldBe Some("Opis z TMDB.")                              // shared, memoised, fallback
    m.synopsisByCity.keySet shouldBe Set("poznan", "wroclaw", "warszawa")
    m.synopsisFor(Poznan).get   should (include ("Poznański")  and not include ("Wrocławski") and not include ("Warszawski"))
    m.synopsisFor(Wroclaw).get  should (include ("Wrocławski") and not include ("Poznański")  and not include ("Warszawski"))
    m.synopsisFor(Warszawa).get should (include ("Warszawski") and not include ("Poznański")  and not include ("Wrocławski"))
  }

  it should "resolve every rating with its click-through URL" in {
    movie.ratings.imdb shouldBe Some(9.3)
    movie.ratings.imdbUrl shouldBe Some("https://www.imdb.com/title/tt0111161/")
    movie.ratings.metascore shouldBe Some(82)
    movie.ratings.rottenTomatoes shouldBe Some(91)
    movie.ratings.filmweb shouldBe Some(8.9)
    movie.ratings.metacriticUrl should not be empty
    movie.ratings.rottenTomatoesUrl should not be empty
    movie.ratings.filmwebUrl should not be empty
  }

  "screenings" should "emit one document per (city, cinema) keyed and indexed by city" in {
    screenings should have size 2
    val byCinema = screenings.map(s => s.cinema -> s).toMap

    val mk = byCinema("Multikino Stary Browar")
    mk.city shouldBe "poznan"
    mk.filmId shouldBe movie._id
    mk._id shouldBe s"${movie._id}|poznan|Multikino Stary Browar"
    mk.filmUrl shouldBe Some("https://mk/film")
    // All showtimes carried, sorted canonically (the document is a function of the
    // set, not scrape order).
    mk.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.parse("2026-06-12T17:30"),
      LocalDateTime.parse("2026-06-12T20:00")
    )

    val he = byCinema("Helios Magnolia Park")
    he.city shouldBe "wroclaw"
    he.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.parse("2026-06-13T19:00"))
  }

  it should "drop cinema slots with no showtimes" in {
    val noShows = record.copy(data = record.data + (Rialto -> SourceData(showtimes = Seq.empty)))
    val s = ReadModelProjection.screenings(StoredMovieRecord.fromStorage(id, noShows))
    s.map(_.cinema) should not contain "Kino Rialto"
  }

  it should "be deterministic — the same row projects to identical documents" in {
    ReadModelProjection.project(stored) shouldBe (movie, screenings)
  }

  it should "display TMDB's release year, overriding a cinema-reported one" in {
    // Cinemas report the production year (2025); TMDB has the theatrical year
    // (2026). The served year must be TMDB's.
    val record = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("X"), releaseYear = Some(2025)),
      Tmdb      -> SourceData(title = Some("X"), releaseYear = Some(2026))))
    val (m, _) = ReadModelProjection.project(StoredMovieRecord.fromStorage("x|2026", record))
    m.releaseYear shouldBe Some(2026)
  }

  it should "key the film id by the resolved (TMDB) year, not the source _id's raw year" in {
    // The source row was scrape-keyed by the cinema-reported year (2025) before
    // TMDB resolved it to 2026. The read-model id (and its screenings' filmId)
    // must follow the *resolved* year — the same notion the displayed
    // `releaseYear` uses — so that a later re-key of the source `_id` onto the
    // TMDB year can't leave a second, differently-keyed copy of the film behind
    // (the duplicate-card bug). Both `kumotry|2025` and `kumotry|2026` source
    // rows then project to the one id `kumotry|2026`.
    val record = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2025),
        showtimes = Seq(at("2026-06-12T18:00"))),
      Tmdb      -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026))))
    val (m, ss) = ReadModelProjection.project(StoredMovieRecord.fromStorage("kumotry|2025", record))
    m._id shouldBe "kumotry|2026"
    ss.map(_.filmId).distinct shouldBe Seq("kumotry|2026")
  }

  // One film, one stored record (one tmdbId), screened under two different shown
  // titles — a Polish "Iwan Groźny" (Multikino, Poznań) and a Cyrillic "Иван
  // Грозный" (Helios, Wrocław). The read model must SPLIT it into two cards keyed
  // by shown title, while year / director / cast / ratings stay derived from the
  // whole record, and each card's synopsis is drawn from the cinemas that used
  // ITS title (TMDB the shared fallback).
  private val twoTitleRecord = MovieRecord(
    tmdbId = Some(30530),
    data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Iwan Groźny"),
        synopsis = Some("Polski opis kina, wyraźnie dłuższy niż blurb z TMDB."),
        showtimes = Seq(at("2026-06-12T18:00"))),
      HeliosMagnolia -> SourceData(title = Some("Иван Грозный"),
        synopsis = Some("Русское описание кинотеатра, заметно длиннее TMDB."),
        showtimes = Seq(at("2026-06-13T19:00"))),
      Tmdb -> SourceData(title = Some("Iwan Groźny"), originalTitle = Some("Ivan Groznyy"),
        synopsis = Some("Opis z TMDB."), releaseYear = Some(1944),
        director = Seq("Siergiej Eisenstein"), cast = Seq("Mikołaj Czerkasow", "Ludmiła Cełikowska"),
        genres = Seq("Dramat"))
    )
  )
  private val twoTitleStored = StoredMovieRecord.fromStorage("iwangrozny|1944", twoTitleRecord)

  "projectAll" should "split one record into a card per shown title" in {
    val cards = ReadModelProjection.projectAll(twoTitleStored)
    cards.map(_._1.title) should contain theSameElementsAs Seq("Iwan Groźny", "Иван Грозный")
    ReadModelProjection.filmIds(twoTitleStored) should contain theSameElementsAs
      Seq("iwangrozny|1944", s"${TitleNormalizer.sanitize("Иван Грозный")}|1944")
  }

  "screeningsAll" should "return exactly projectAll's screenings (metadata-free), for single- and multi-variant rows" in {
    // The source-films census counts off screeningsAll instead of projectAll to skip
    // the unused ResolvedMovie work; the counts only stay identical if the screenings do.
    ReadModelProjection.screeningsAll(stored)         shouldBe ReadModelProjection.projectAll(stored).map(_._2)
    ReadModelProjection.screeningsAll(twoTitleStored) shouldBe ReadModelProjection.projectAll(twoTitleStored).map(_._2)
  }

  it should "derive year, director and cast for every card from the whole record" in {
    val cards = ReadModelProjection.projectAll(twoTitleStored).map(_._1)
    all (cards.map(_.releaseYear)) shouldBe Some(1944)
    all (cards.map(_.directors))   shouldBe Seq("Siergiej Eisenstein")
    all (cards.map(_.cast))        shouldBe Seq("Mikołaj Czerkasow", "Ludmiła Cełikowska")
  }

  it should "scope each card's synopsis to the cinemas that used its title, TMDB the fallback" in {
    val byTitle = ReadModelProjection.projectAll(twoTitleStored).map { case (m, _) => m.title -> m }.toMap
    val polish   = byTitle("Iwan Groźny")
    val cyrillic = byTitle("Иван Грозный")
    // City-independent fallback is the shared TMDB blurb on both cards.
    polish.synopsis   shouldBe Some("Opis z TMDB.")
    cyrillic.synopsis shouldBe Some("Opis z TMDB.")
    // Each card shows only its own title's cinema blurb where that cinema screens,
    // and the TMDB fallback in the other city.
    polish.synopsisFor(Poznan).get   should include ("Polski opis")
    polish.synopsisFor(Wroclaw)      shouldBe Some("Opis z TMDB.")
    cyrillic.synopsisFor(Wroclaw).get should include ("Русское описание")
    cyrillic.synopsisFor(Poznan)     shouldBe Some("Opis z TMDB.")
  }

  it should "give each card only the screenings listed under its title" in {
    val byTitle = ReadModelProjection.projectAll(twoTitleStored).map { case (m, ss) => m.title -> ss }.toMap
    byTitle("Iwan Groźny").map(_.cinema)   shouldBe Seq("Multikino Stary Browar")
    byTitle("Иван Грозный").map(_.cinema)  shouldBe Seq("Helios Magnolia Park")
  }
}
