package services.readmodel

import models.{CityScreening, ResolvedMovie, ResolvedRatings}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.ShowtimeGenerators.{genShowtime, genText, genUrl}


/** Every `web_movies` / `web_screenings` document the projector can write reads back as the
 *  value written — over generated values rather than the hand-picked rows of
 *  [[ReadModelCodecsSpec]]. The screening row is the sharp one: its booking URLs are written
 *  split at whatever prefix the row's URLs happen to share (a URL that IS the prefix, URLs
 *  differing only past a multi-byte character, rows with some URLs absent), and read back
 *  through the streaming codec, so a split the reader cannot undo would serve a wrong link. */
class ReadModelCodecsPropertySpec extends IdentityPropertySpec {

  private def roundTrip[T](codec: Codec[T], value: T): T = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), value, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
  }

  private val genScreening: Gen[CityScreening] = for {
    id          <- genText
    filmId      <- genText
    city        <- Gen.oneOf("poznan", "london", "new-york")
    cinema      <- genText
    filmUrl     <- Gen.option(genUrl)
    showtimes   <- Gen.listOf(genShowtime)
    listingKeys <- Gen.listOf(genText)
  } yield CityScreening(id, filmId, city, cinema, filmUrl, showtimes, listingKeys)

  private val genRatings: Gen[ResolvedRatings] = for {
    imdb       <- Gen.option(Gen.choose(0.0, 10.0))
    imdbUrl    <- Gen.option(genUrl)
    metascore  <- Gen.option(Gen.choose(0, 100))
    mcUrl      <- genUrl
    rt         <- Gen.option(Gen.choose(0, 100))
    rtUrl      <- genUrl
    filmweb    <- Gen.option(Gen.choose(0.0, 10.0))
    filmwebUrl <- genUrl
  } yield ResolvedRatings(imdb, imdbUrl, metascore, mcUrl, rt, rtUrl, filmweb, filmwebUrl)

  private val genMovie: Gen[ResolvedMovie] = for {
    id        <- genText
    title     <- genText
    original  <- Gen.option(genText)
    poster    <- Gen.option(genUrl)
    fallbacks <- Gen.listOf(genUrl)
    runtime   <- Gen.option(Gen.choose(1, 400))
    year      <- Gen.option(Gen.choose(1900, 2030))
    genres    <- Gen.listOf(genText)
    countries <- Gen.listOf(genText)
    directors <- Gen.listOf(genText)
    cast      <- Gen.listOf(genText)
    synopsis  <- Gen.option(genText)
    trailers  <- Gen.listOf(genUrl)
    ratings   <- genRatings
    weighted  <- Gen.choose(0.0, 10.0)
    byCity    <- Gen.mapOf(Gen.zip(Gen.oneOf("poznan", "london", "new-york"), genText))
    age       <- Gen.option(genText)
    card      <- Gen.option(genUrl)
    pending   <- Gen.oneOf(true, false)
  } yield ResolvedMovie(id, title, original, poster, fallbacks, runtime, year, genres, countries, directors, cast,
    synopsis, trailers, ratings, weighted, byCity, age, card, pending)

  "a web_screenings row" should "read back as the row written, every booking URL as it was spelled" in {
    val codec = ReadModelCodecs.registry.get(classOf[CityScreening])
    forAll(genScreening) { row =>
      val read = roundTrip(codec, row)
      read shouldBe row
      read.showtimes.map(_.bookingUrl) shouldBe row.showtimes.map(_.bookingUrl)
    }
  }

  "a web_movies document" should "read back as the document written" in {
    val codec = ReadModelCodecs.registry.get(classOf[ResolvedMovie])
    forAll(genMovie)(movie => roundTrip(codec, movie) shouldBe movie)
  }
}
