package services.movies

import models.{Helios, Imdb, Multikino, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime}

/**
 * Round-trips `StoredMovieDto` through the codec registry to confirm:
 *
 * 1. Every domain field on `MovieRecord` survives the trip.
 * 2. `Map[Source, SourceData]` keyed by Source instances re-keys through
 *    `Source.displayName` and back — Cinema, Tmdb, Imdb all round-trip.
 * 3. Sparse SourceData (only the fields the source actually filled) doesn't
 *    pick up spurious None entries on decode.
 * 4. `Showtime.dateTime` round-trips through `LocalDateTimeCodec`.
 *
 * No Mongo connection is involved — the codec runs against an in-memory
 * `BsonDocument`. This is the storage shape's regression guardrail.
 */
class MovieCodecsSpec extends AnyFlatSpec with Matchers {

  private val codec: Codec[StoredMovieDto] = MovieCodecs.registry.get(classOf[StoredMovieDto])

  private def roundTrip(dto: StoredMovieDto): StoredMovieDto = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), dto, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
  }

  "MovieCodecs" should "round-trip every top-level MovieRecord field" in {
    val record = MovieRecord(
      imdbId            = Some("tt0000001"),
      imdbRating        = Some(7.5),
      metascore         = Some(80),
      filmwebUrl        = Some("https://www.filmweb.pl/film/Test-1900-1"),
      filmwebRating     = Some(7.2),
      rottenTomatoes    = Some(91),
      tmdbId            = Some(424242),
      metacriticUrl     = Some("https://www.metacritic.com/movie/test"),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/test")
    )
    val dto = StoredMovieDto.fromDomain("test|1900", "Test", Some(1900), record, Instant.parse("2026-05-17T10:00:00Z"))

    val decoded = roundTrip(dto)
    val back = StoredMovieDto.toDomain(decoded)
    back.title         shouldBe "Test"
    back.year          shouldBe Some(1900)
    back.record        shouldBe record
    decoded.updatedAt  shouldBe dto.updatedAt
  }

  it should "round-trip an all-None MovieRecord (sparse row)" in {
    val record = MovieRecord(imdbId = Some("tt0000002"))
    val dto = StoredMovieDto.fromDomain("sparse|", "Sparse", None, record, Instant.parse("2026-05-17T10:00:00Z"))

    val back = StoredMovieDto.toDomain(roundTrip(dto))
    back.year                      shouldBe None
    back.record.imdbId             shouldBe Some("tt0000002")
    back.record.imdbRating         shouldBe None
    back.record.metascore          shouldBe None
    back.record.filmwebUrl         shouldBe None
    back.record.filmwebRating      shouldBe None
    back.record.rottenTomatoes     shouldBe None
    back.record.tmdbId             shouldBe None
    back.record.metacriticUrl      shouldBe None
    back.record.rottenTomatoesUrl  shouldBe None
    back.record.data               shouldBe Map.empty
  }

  it should "round-trip a Map[Source, SourceData] with mixed Source variants" in {
    val tmdbSlot = SourceData(originalTitle = Some("Test Original"))
    val imdbSlot = SourceData(synopsis = Some("imdb synopsis"))
    val cinemaSlot = SourceData(
      title          = Some("Test PL"),
      synopsis       = Some("cinema synopsis"),
      cast           = Some("cast list"),
      director       = Some("dir"),
      runtimeMinutes = Some(123),
      releaseYear    = Some(2025),
      countries      = Seq("Polska", "Francja"),
      posterUrl      = Some("https://example/poster.jpg"),
      filmUrl        = Some("https://example/film")
    )
    val record = MovieRecord(
      imdbId = Some("tt0000003"),
      data   = Map[Source, SourceData](Tmdb -> tmdbSlot, Imdb -> imdbSlot, Helios -> cinemaSlot)
    )
    val dto = StoredMovieDto.fromDomain("mixed|2025", "Mixed", Some(2025), record, Instant.now())

    val back = StoredMovieDto.toDomain(roundTrip(dto))
    back.record.data.keySet           shouldBe Set(Tmdb, Imdb, Helios)
    back.record.data(Tmdb)            shouldBe tmdbSlot
    back.record.data(Imdb)            shouldBe imdbSlot
    back.record.data(Helios)          shouldBe cinemaSlot
    back.record.cinemaData(Helios).countries shouldBe Seq("Polska", "Francja")
  }

  it should "round-trip Showtimes including dateTime, room, format" in {
    val showtimes = Seq(
      Showtime(LocalDateTime.of(2026, 5, 17, 19, 30), Some("https://example/booking/1"), Some("Sala 1"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 17, 21, 0),  None,                              None,           Nil)
    )
    val slot = SourceData(showtimes = showtimes)
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> slot))
    val dto = StoredMovieDto.fromDomain("st|2026", "Showtime Test", Some(2026), record, Instant.now())

    val back = StoredMovieDto.toDomain(roundTrip(dto))
    val decodedShowtimes = back.record.data(Multikino).showtimes
    decodedShowtimes              should have size 2
    decodedShowtimes.head         shouldBe showtimes.head
    decodedShowtimes(1).dateTime  shouldBe showtimes(1).dateTime
    decodedShowtimes(1).bookingUrl shouldBe None
    decodedShowtimes(1).format    shouldBe Nil
  }

  it should "drop unknown sub-doc keys on decode (legacy / deprecated Source displayName)" in {
    // Simulate a row whose `sourceData` sub-doc contains a key Source.byDisplayName
    // doesn't know (e.g. a cinema removed from `Cinema.all`). The current
    // decoder dropped these silently; this confirms the new path matches.
    val raw = new BsonDocument()
    val outerCodec = codec
    val tmdbSlot = SourceData(originalTitle = Some("Known"))
    val dto = StoredMovieDto.fromDomain("k|2025", "Known", Some(2025),
      MovieRecord(data = Map[Source, SourceData](Tmdb -> tmdbSlot)), Instant.now())
    outerCodec.encode(new BsonDocumentWriter(raw), dto, EncoderContext.builder().build())

    // Inject an unknown source into the encoded doc.
    val sourceDataDoc = raw.getDocument("sourceData")
    sourceDataDoc.put("DeprecatedCinema", new BsonDocument().append("title",
      org.bson.BsonString("legacy slot")))

    val decoded = outerCodec.decode(new BsonDocumentReader(raw), DecoderContext.builder().build())
    val back = StoredMovieDto.toDomain(decoded)
    back.record.data.keySet shouldBe Set(Tmdb)
  }
}
