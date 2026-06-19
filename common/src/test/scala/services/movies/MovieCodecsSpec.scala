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
    val dto = StoredMovieDto.fromDomain("test|1900", record, Instant.parse("2026-05-17T10:00:00Z"))

    val decoded = roundTrip(dto)
    val back = StoredMovieDto.toDomain(decoded)
    // title/year are derived, not stored: year from the _id, title from
    // sourceData (here there are no cinema slots, so it falls back to the
    // _id's sanitized prefix — `displayTitle` then re-cases that all-lowercase
    // fallback, so "test" surfaces as "Test").
    back.title         shouldBe "Test"
    back.year          shouldBe Some(1900)
    back.record        shouldBe record
    decoded.updatedAt  shouldBe dto.updatedAt
  }

  it should "round-trip an all-None MovieRecord (sparse row)" in {
    val record = MovieRecord(imdbId = Some("tt0000002"))
    val dto = StoredMovieDto.fromDomain("sparse|", record, Instant.parse("2026-05-17T10:00:00Z"))

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
      rawTitle       = Some("Kino bez barier: Test PL (AD)"),
      synopsis       = Some("cinema synopsis"),
      cast           = Seq("cast list"),
      director       = Seq("dir"),
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
    val dto = StoredMovieDto.fromDomain("mixed|2025", record, Instant.now())

    val back = StoredMovieDto.toDomain(roundTrip(dto))
    back.record.data.keySet           shouldBe Set(Tmdb, Imdb, Helios)
    back.record.data(Tmdb)            shouldBe tmdbSlot
    back.record.data(Imdb)            shouldBe imdbSlot
    back.record.data(Helios)          shouldBe cinemaSlot
    back.record.data(Helios).rawTitle shouldBe Some("Kino bez barier: Test PL (AD)")
    back.record.cinemaData(Helios).countries shouldBe Seq("Polska", "Francja")
  }

  it should "round-trip Showtimes including dateTime, room, format" in {
    val showtimes = Seq(
      Showtime(LocalDateTime.of(2026, 5, 17, 19, 30), Some("https://example/booking/1"), Some("Sala 1"), List("2D", "NAP")),
      Showtime(LocalDateTime.of(2026, 5, 17, 21, 0),  None,                              None,           Nil)
    )
    val slot = SourceData(showtimes = showtimes)
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> slot))
    val dto = StoredMovieDto.fromDomain("st|2026", record, Instant.now())

    val back = StoredMovieDto.toDomain(roundTrip(dto))
    val decodedShowtimes = back.record.data(Multikino).showtimes
    decodedShowtimes              should have size 2
    decodedShowtimes.head         shouldBe showtimes.head
    decodedShowtimes(1).dateTime  shouldBe showtimes(1).dateTime
    decodedShowtimes(1).bookingUrl shouldBe None
    decodedShowtimes(1).format    shouldBe Nil
  }

  it should "drop unknown sub-document keys on decode (legacy / deprecated Source displayName)" in {
    // Simulate a row whose `sourceData` sub-document contains a key Source.byDisplayName
    // doesn't know (e.g. a cinema removed from `Cinema.all`). The current
    // decoder dropped these silently; this confirms the new path matches.
    val raw = new BsonDocument()
    val outerCodec = codec
    val tmdbSlot = SourceData(originalTitle = Some("Known"))
    val dto = StoredMovieDto.fromDomain("k|2025",
      MovieRecord(data = Map[Source, SourceData](Tmdb -> tmdbSlot)), Instant.now())
    outerCodec.encode(new BsonDocumentWriter(raw), dto, EncoderContext.builder().build())

    // Inject an unknown source into the encoded document.
    val sourceDataDocument = raw.getDocument("sourceData")
    sourceDataDocument.put("DeprecatedCinema", new BsonDocument().append("title",
      org.bson.BsonString("legacy slot")))

    val decoded = outerCodec.decode(new BsonDocumentReader(raw), DecoderContext.builder().build())
    val back = StoredMovieDto.toDomain(decoded)
    back.record.data.keySet shouldBe Set(Tmdb)
  }

  it should "round-trip the tmdbNoMatch / detailPending conclusion markers when set" in {
    val record = MovieRecord(imdbId = Some("tt0000004"), tmdbNoMatch = true, detailPending = true)
    val back = StoredMovieDto.toDomain(roundTrip(StoredMovieDto.fromDomain("conc|2025", record, Instant.now())))
    back.record.tmdbNoMatch   shouldBe true
    back.record.detailPending shouldBe true
  }

  it should "default tmdbNoMatch / detailPending to false on a legacy document that lacks them" in {
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Legacy"))))
    val raw = new BsonDocument()
    codec.encode(new BsonDocumentWriter(raw), StoredMovieDto.fromDomain("legacy|2025", record, Instant.now()), EncoderContext.builder().build())
    raw.remove("tmdbNoMatch"); raw.remove("detailPending")  // a document written before the fields existed
    val back = StoredMovieDto.toDomain(codec.decode(new BsonDocumentReader(raw), DecoderContext.builder().build()))
    back.record.tmdbNoMatch   shouldBe false
    back.record.detailPending shouldBe false
  }

  it should "round-trip retainedSynopses (kept after a cinema's slot was pruned)" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](Tmdb -> SourceData(synopsis = Some("tmdb"))),
      retainedSynopses = Map[Source, String](Multikino -> "long retained blurb", Helios -> "short")
    )
    val back = StoredMovieDto.toDomain(roundTrip(StoredMovieDto.fromDomain("ret|2025", record, Instant.now())))
    back.record.retainedSynopses shouldBe Map[Source, String](Multikino -> "long retained blurb", Helios -> "short")
  }

  it should "decode a legacy document with no retainedSynopses field to an empty map" in {
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Legacy"))))
    val raw = new BsonDocument()
    codec.encode(new BsonDocumentWriter(raw), StoredMovieDto.fromDomain("legret|2025", record, Instant.now()), EncoderContext.builder().build())
    raw.remove("retainedSynopses")  // a document written before the field existed
    val back = StoredMovieDto.toDomain(codec.decode(new BsonDocumentReader(raw), DecoderContext.builder().build()))
    back.record.retainedSynopses shouldBe Map.empty
  }

  it should "drop a retainedSynopses key for an unknown (legacy) Source displayName on decode" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](Tmdb -> SourceData(title = Some("Known"))),
      retainedSynopses = Map[Source, String](Multikino -> "kept")
    )
    val raw = new BsonDocument()
    codec.encode(new BsonDocumentWriter(raw), StoredMovieDto.fromDomain("retunk|2025", record, Instant.now()), EncoderContext.builder().build())
    raw.getDocument("retainedSynopses").put("DeprecatedCinema", new org.bson.BsonString("orphan"))
    val back = StoredMovieDto.toDomain(codec.decode(new BsonDocumentReader(raw), DecoderContext.builder().build()))
    back.record.retainedSynopses shouldBe Map[Source, String](Multikino -> "kept")
  }

  // ── Derived title/year (no longer stored) ─────────────────────────────────

  it should "derive the display title from sourceData (dominant cinema form), not a stored field" in {
    val id = s"${TitleNormalizer.sanitize("Drzewo magii")}|2026"
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Drzewo Magii")),  // over-capitalised
      Helios    -> SourceData(title = Some("Drzewo magii")),
      Tmdb      -> SourceData(title = Some("Drzewo magii"))   // canonical casing
    ))
    val back = StoredMovieDto.toDomain(roundTrip(StoredMovieDto.fromDomain(id, record, Instant.now())))
    back.title shouldBe "Drzewo magii"
    back.year  shouldBe Some(2026)
  }

  it should "recover year=None from an _id with an empty year suffix" in {
    val id = s"${TitleNormalizer.sanitize("Some Event")}|"
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Some Event"))))
    StoredMovieDto.toDomain(roundTrip(StoredMovieDto.fromDomain(id, record, Instant.now()))).year shouldBe None
  }

  it should "derive a title that sanitizes back to the _id prefix (no re-keying churn)" in {
    // The derived title must recompute to the SAME documentId, or a later upsert
    // would orphan the row under a new _id.
    val id = s"${TitleNormalizer.sanitize("Top Gun: Maverick")}|2022"
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Top Gun: Maverick")),
      Helios    -> SourceData(title = Some("TOP GUN MAVERICK"))
    ))
    val back = StoredMovieDto.toDomain(roundTrip(StoredMovieDto.fromDomain(id, record, Instant.now())))
    back.title                           shouldBe "Top Gun: Maverick"  // ladder picks punct+mixed-case
    TitleNormalizer.sanitize(back.title) shouldBe id.split('|').head
  }

  it should "ignore stale top-level title/year columns on a legacy document (back-compat decode)" in {
    // Existing prod documents still carry the now-dropped `title`/`year` columns.
    // The codec must decode them without error, and `toDomain` must derive from
    // sourceData + the _id, NOT trust the stale (order-dependent) columns.
    val id = s"${TitleNormalizer.sanitize("Wonka")}|2023"
    val record = MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Wonka"))))
    val raw = new BsonDocument()
    codec.encode(new BsonDocumentWriter(raw), StoredMovieDto.fromDomain(id, record, Instant.now()), EncoderContext.builder().build())
    raw.put("title", new org.bson.BsonString("STALE PINNED TITLE"))
    raw.put("year",  new org.bson.BsonInt32(1999))

    val back = StoredMovieDto.toDomain(codec.decode(new BsonDocumentReader(raw), DecoderContext.builder().build()))
    back.title shouldBe "Wonka"     // from sourceData, not the stale column
    back.year  shouldBe Some(2023)  // from the _id, not the stale column
  }
}
