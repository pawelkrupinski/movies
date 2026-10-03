package services.scrapes

import models.{Movie, Showtime}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalacheck.Gen
import services.IdentityPropertySpec

import java.time.{Instant, LocalDateTime}

/** Whatever a scraper hands the archive comes back from it unchanged: every stored row goes through
 *  the hand-written streaming film codec, which writes a film's booking URLs split at the prefix they
 *  share and drops absent optionals — so `None` and `Some("")`, empty and missing collections, URLs
 *  that share nothing, a whole URL, or a prefix ending mid-character, must all survive a write and a
 *  read, through the very registry `MongoScrapeArchiveRepository` uses. */
class ArchivedListingRoundTripPropertySpec extends IdentityPropertySpec {

  // Short strings over an alphabet chosen to collide: shared prefixes, Polish diacritics, an
  // astral-plane pair (two code units), quotes and BSON-meaningful characters.
  private val genText: Gen[String] =
    Gen.choose(0, 6).flatMap(Gen.listOfN(_, Gen.oneOf("a", "b", "/", "?", "=", "ł", "Ż", "😀", "😁", "\"", "$", ".", " ")))
      .map(_.mkString)
  private val genOptionalText: Gen[Option[String]] = Gen.option(genText)
  private val genUrl: Gen[Option[String]] =
    Gen.option(Gen.oneOf(Gen.const("https://kino.example/buy?show="), genText).flatMap(base => genText.map(base + _)))
  private val genList: Gen[List[String]] = Gen.choose(0, 3).flatMap(Gen.listOfN(_, genText))
  private val genDateTime: Gen[LocalDateTime] =
    Gen.choose(0L, 4L * 365 * 24 * 60).map(minutes => LocalDateTime.of(2025, 1, 1, 0, 0).plusMinutes(minutes))

  private val genShowtime: Gen[Showtime] = for {
    dateTime <- genDateTime
    url      <- genUrl
    room     <- genOptionalText
    format   <- genList
  } yield Showtime(dateTime, url, room, format)

  private val genMovie: Gen[Movie] = for {
    title    <- genText
    runtime  <- Gen.option(Gen.choose(0, 400))
    year     <- Gen.option(Gen.choose(1900, 2030))
    countries <- genList
    genres   <- genList
    original <- genOptionalText
    raw      <- genOptionalText
  } yield Movie(title, runtime, year, countries, genres, original, raw)

  private val genFilm: Gen[ArchivedFilmDto] = for {
    movie     <- genMovie
    poster    <- genOptionalText
    filmUrl   <- genUrl
    synopsis  <- genOptionalText
    cast      <- genList
    director  <- genList
    showtimes <- Gen.choose(0, 5).flatMap(Gen.listOfN(_, genShowtime))
    ids       <- Gen.mapOf(Gen.zip(Gen.oneOf("tmdb", "imdb", "filmweb"), genText))
    trailer   <- genOptionalText
    age       <- genOptionalText
  } yield ArchivedFilmDto(movie, poster, filmUrl, synopsis, cast, director, showtimes, ids, trailer, age)

  private val genBarren: Gen[BarrenAttemptDto] = for {
    at       <- Gen.choose(0L, 2000000000000L).map(Instant.ofEpochMilli)
    outcome  <- Gen.oneOf(ScrapeOutcome.all.map(_.label))
    error    <- genOptionalText
    since    <- Gen.option(Gen.choose(0L, 2000000000000L).map(Instant.ofEpochMilli))
    failed   <- Gen.option(Gen.choose(0, 50))
    vouched  <- Gen.oneOf(None, Some(true))
  } yield BarrenAttemptDto(at, outcome, error, since, failed, vouched)

  private val genRow: Gen[StoredScrapeDto] = for {
    city      <- genOptionalText
    scrapedAt <- Gen.option(Gen.choose(0L, 2000000000000L).map(Instant.ofEpochMilli))
    complete  <- Gen.option(Gen.oneOf(true, false))
    films     <- Gen.option(Gen.choose(0, 4).flatMap(Gen.listOfN(_, genFilm)))
    barren    <- Gen.option(genBarren)
  } yield StoredScrapeDto("Helios", city, scrapedAt, complete, films, barren)

  "an archived row" should "read back exactly as it was written" in {
    val codec = ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto])
    forAll(genRow) { row =>
      val written = new BsonDocument()
      codec.encode(new BsonDocumentWriter(written), row, EncoderContext.builder().build())
      val read = codec.decode(new BsonDocumentReader(written), DecoderContext.builder().build())
      read shouldBe row
      read.films.toSeq.flatten.flatMap(_.showtimes).map(_.bookingUrl) shouldBe
        row.films.toSeq.flatten.flatMap(_.showtimes).map(_.bookingUrl)
    }
  }
}
