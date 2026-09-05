package services.movies

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.Instant

/**
 * A field on `MovieRecord` has to be wired into FOUR places, and forgetting any of
 * them fails SILENTLY:
 *
 *   1. `StoredMovieDto`          — or a whole-document write drops it.
 *   2. `MovieRecordPatch`        — or the field-level `$set` path (`updateIfPresent`)
 *                                  drops it, while whole-document writes keep it, so
 *                                  the value survives some writes and not others.
 *   3. `MovieRepository.patchToUpdate` — or the patch carries it and the wire doesn't.
 *   4. `ChangeStreamMetrics`     — or a change to it reads as the `other` catch-all
 *                                  and the change-stream metric quietly mis-labels.
 *
 * Three fields were found missing from (2) and (4) in one week — `tmdbBasis`,
 * `wikidataId`, `retainedSynopses` — each by accident rather than by a check. So the
 * check is here, and it is derived from the CASE CLASS rather than from a list
 * somebody has to remember to extend: `MovieRecord` and `StoredMovieDto` are
 * `Product`s, so a new field shows up in `productElementNames` the moment it is
 * declared, and the fixture guard below fails until it is populated — which then
 * drives it through every round-trip in this file.
 *
 * Adding a field to `MovieRecord` therefore breaks this spec FIRST, with a message
 * naming the field, instead of quietly losing writes in production.
 */
class MovieRecordFieldWiringSpec extends AnyFlatSpec with Matchers {

  private val codec: Codec[StoredMovieDto] = MovieCodecs.registry.get(classOf[StoredMovieDto])

  private def bsonRoundTrip(dto: StoredMovieDto): StoredMovieDto = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), dto, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
  }

  /** Every field carrying a value that is NOT its default, so a field left unwired
   *  anywhere downstream shows up as a difference rather than as two Nones agreeing. */
  private val everyFieldSet = MovieRecord(
    imdbId            = Some("tt0000001"),
    imdbRating        = Some(7.5),
    metascore         = Some(80),
    filmwebUrl        = Some("https://www.filmweb.pl/film/Test-1900-1"),
    filmwebRating     = Some(7.2),
    rottenTomatoes    = Some(91),
    tmdbId            = Some(424242),
    tmdbBasis         = Some("DirectorWalk"),
    wikidataId        = Some("Q42"),
    metacriticUrl     = Some("https://www.metacritic.com/movie/test"),
    rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/test"),
    searchTitle       = Some("Test"),
    tmdbNoMatch       = true,
    detailPending     = true,
    data              = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Test Original"))),
    retainedSynopses  = Map[Source, String](Helios -> "the longest synopsis this source ever published")
  )

  private def unsetFieldsOf(record: MovieRecord): Seq[String] =
    record.productElementNames.zip(record.productIterator).collect {
      case (name, None)                       => name
      case (name, false)                      => name
      case (name, c: Iterable[_]) if c.isEmpty => name
      case (name, m: Map[_, _]) if m.isEmpty   => name
    }.toSeq

  "the fixture" should "carry a non-default value for EVERY MovieRecord field" in {
    withClue("a new MovieRecord field is not exercised by this spec — populate it in " +
             "`everyFieldSet`, then let the round-trips below tell you what it still needs: ") {
      unsetFieldsOf(everyFieldSet) shouldBe empty
    }
  }

  "StoredMovieDto" should "round-trip every MovieRecord field through BSON" in {
    val dto  = StoredMovieDto.fromDomain("test|1900", everyFieldSet, Instant.parse("2026-05-17T10:00:00Z"))
    val back = StoredMovieDto.toDomain(bsonRoundTrip(dto), titleNormalizer)
    withClue("a field is missing from StoredMovieDto (or from fromDomain/toDomain): ") {
      back.record shouldBe everyFieldSet
    }
  }

  "MovieRecordPatch" should "carry every MovieRecord field onto an empty record" in {
    // `diff` + `applyTo` IS the contract — "the minimal set of per-field updates needed
    // to turn `before` into `after`" — so a field the patch forgot cannot satisfy it.
    val patch = MovieRecordPatch.diff(MovieRecord(), everyFieldSet)
    withClue("a field is missing from MovieRecordPatch, so `updateIfPresent` drops it: ") {
      patch.applyTo(MovieRecord()) shouldBe everyFieldSet
    }
  }

  it should "carry every field being CLEARED, too" in {
    // The `$unset` direction: a field that only diffs one way still strands a value.
    val patch = MovieRecordPatch.diff(everyFieldSet, MovieRecord())
    withClue("a field is missing from MovieRecordPatch's clear direction: ") {
      patch.applyTo(everyFieldSet) shouldBe MovieRecord()
    }
  }

  "ChangeStreamMetrics" should "classify every field the storage document persists" in {
    // Derived from the DTO, so a new persisted field is covered the moment it exists.
    // `_id` and `updatedAt` are the document's own metadata, not a film field.
    val structural = Set("_id", "updatedAt")
    val persisted  = StoredMovieDto
      .fromDomain("test|1900", everyFieldSet, Instant.now())
      .productElementNames.toSeq.filterNot(structural)

    val unclassified = persisted.filter { field =>
      ChangeStreamMetrics.updateKinds(Set(field, "updatedAt")) == Set(ChangeStreamMetrics.Kind.Other)
    }
    withClue("a persisted field is not classified, so a change to it reads as `other` " +
             "in the change-stream metric — add it to RatingFields/IdentityFields " +
             "(or the sourceData branch) in ChangeStreamMetrics: ") {
      unclassified shouldBe empty
    }
  }
}
