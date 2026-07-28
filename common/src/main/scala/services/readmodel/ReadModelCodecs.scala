package services.readmodel

import models.{CityScreening, ResolvedMovie, ResolvedRatings, Showtime}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros
import services.movies.JavaTimeCodecs

/**
 * BSON codec wiring for the read-model collections (`web_movies`,
 * `web_screenings`). The case classes carry their identity in a `_id` field, so
 * the driver macros map it straight to the Mongo document key — no parallel
 * DTO needed. `IgnoreNone` omits `None` optionals on write (and reads a missing
 * field back as `None`), matching `MovieCodecs`. `LocalDateTime` reuses the
 * shared `JavaTimeCodecs.localDateTime` so showtimes encode identically to the
 * `movies` collection.
 */
object ReadModelCodecs {

  /** The macro-derived registry — the shape everything is written with. */
  private val macroRegistry: CodecRegistry = fromRegistries(
    fromCodecs(JavaTimeCodecs.localDateTime),
    fromProviders(
      Macros.createCodecProviderIgnoreNone[Showtime](),
      Macros.createCodecProviderIgnoreNone[ResolvedRatings](),
      Macros.createCodecProviderIgnoreNone[ResolvedMovie](),
      Macros.createCodecProviderIgnoreNone[CityScreening]()
    ),
    DEFAULT_CODEC_REGISTRY
  )

  /** An empty instance per served type — the source of the decode defaults. Only its
   *  ENCODED form is used, so these values never reach a reader except as the fill for a
   *  field a stored document genuinely lacks. */
  private val emptyMovie = ResolvedMovie(
    _id = "", title = "", originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
    runtimeMinutes = None, releaseYear = None, genres = Seq.empty, countries = Seq.empty,
    directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
    ratings = ResolvedRatings(None, None, None, "", None, "", None, ""), weightedRating = 0.0)

  private val emptyScreening = CityScreening(
    _id = "", filmId = "", city = "", cinema = "", filmUrl = None, showtimes = Seq.empty)

  /** Reads tolerate a document missing any field; writes still emit the full shape. See
   *  [[DefaultingCodec]] — a missing non-`Option` field otherwise kills the whole keyset
   *  batch, and `web_movies` is the sharpest case because full re-projection is retired,
   *  so a quiescent row is never rewritten. */
  val registry: CodecRegistry = fromRegistries(
    fromCodecs(
      DefaultingCodec(macroRegistry.get(classOf[ResolvedMovie]),  emptyMovie),
      DefaultingCodec(macroRegistry.get(classOf[CityScreening]), emptyScreening)
    ),
    macroRegistry
  )
}
