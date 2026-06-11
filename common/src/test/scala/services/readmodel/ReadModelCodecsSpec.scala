package services.readmodel

import models.{CityScreening, ResolvedMovie, ResolvedRatings, Showtime}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * Round-trips the read-model documents through their codec registry against an
 * in-memory `BsonDocument` (no Mongo). Confirms `_id` maps to the document key,
 * every field survives, `None` optionals are omitted-and-restored, and
 * `Showtime.dateTime` round-trips through the shared `LocalDateTime` codec.
 */
class ReadModelCodecsSpec extends AnyFlatSpec with Matchers {

  private def roundTrip[T](codec: Codec[T], v: T): T = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), v, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
  }

  "ReadModelCodecs" should "round-trip a fully-populated ResolvedMovie" in {
    val codec = ReadModelCodecs.registry.get(classOf[ResolvedMovie])
    val movie = ResolvedMovie(
      _id                = "skazani na shawshank|1994",
      title              = "Skazani na Shawshank",
      originalTitle      = Some("The Shawshank Redemption"),
      posterUrl          = Some("https://poster.jpg"),
      fallbackPosterUrls = Seq("https://alt1.jpg", "https://alt2.jpg"),
      runtimeMinutes     = Some(142),
      releaseYear        = Some(1994),
      genres             = Seq("Dramat"),
      countries          = Seq("USA"),
      directors          = Seq("Frank Darabont"),
      cast               = Seq("Tim Robbins", "Morgan Freeman"),
      synopsis           = Some("Life in a state penitentiary."),
      trailerUrls        = Seq("https://www.youtube.com/embed/abc"),
      ratings            = ResolvedRatings(
        imdb = Some(9.3), imdbUrl = Some("https://www.imdb.com/title/tt0111161/"),
        metascore = Some(82), metacriticUrl = Some("https://mc"),
        rottenTomatoes = Some(91), rottenTomatoesUrl = Some("https://rt"),
        filmweb = Some(8.9), filmwebUrl = Some("https://fw")
      )
    )
    roundTrip(codec, movie) shouldBe movie
  }

  it should "round-trip a sparse ResolvedMovie (None optionals omitted)" in {
    val codec = ReadModelCodecs.registry.get(classOf[ResolvedMovie])
    val movie = ResolvedMovie(
      _id = "x|", title = "X", originalTitle = None, posterUrl = None,
      fallbackPosterUrls = Seq.empty, runtimeMinutes = None, releaseYear = None,
      genres = Seq.empty, countries = Seq.empty, directors = Seq.empty, cast = Seq.empty,
      synopsis = None, trailerUrls = Seq.empty,
      ratings = ResolvedRatings(None, None, None, None, None, None, None, None)
    )
    roundTrip(codec, movie) shouldBe movie
  }

  it should "round-trip a CityScreening with showtimes" in {
    val codec = ReadModelCodecs.registry.get(classOf[CityScreening])
    val screening = CityScreening(
      _id = "x|1994|poznan|Multikino Stary Browar", filmId = "x|1994", city = "poznan",
      cinema = "Multikino Stary Browar", filmUrl = Some("https://mk/film"),
      showtimes = Seq(
        Showtime(LocalDateTime.parse("2026-06-12T17:30"), bookingUrl = Some("https://b1"), room = Some("Sala 1"), format = List("2D", "NAP")),
        Showtime(LocalDateTime.parse("2026-06-12T20:00"), bookingUrl = None)
      )
    )
    roundTrip(codec, screening) shouldBe screening
  }
}
