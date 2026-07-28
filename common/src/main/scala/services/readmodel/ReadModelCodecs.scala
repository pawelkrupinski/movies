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

  /** An empty instance per defaulted type — the source of the decode defaults. Only its
   *  ENCODED form is used, so these values never reach a reader except as the fill for a
   *  field a stored document genuinely lacks. */
  private val emptyRatings = ResolvedRatings(None, None, None, "", None, "", None, "")

  private val emptyMovie = ResolvedMovie(
    _id = "", title = "", originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
    runtimeMinutes = None, releaseYear = None, genres = Seq.empty, countries = Seq.empty,
    directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
    ratings = emptyRatings, weightedRating = 0.0)

  private val emptyScreening = CityScreening(
    _id = "", filmId = "", city = "", cinema = "", filmUrl = None, showtimes = Seq.empty)

  /**
   * `ResolvedRatings` defaulted FIRST, and the top-level codecs re-derived against a
   * registry that already contains it.
   *
   * [[DefaultingCodec]] fills only the fields of the document it is handed, so wrapping
   * `ResolvedMovie` alone left the NESTED case untouched — and nested is the case that has
   * actually bitten: `ResolvedRatings` carries three required non-`Option` strings
   * (`metacriticUrl`, `rottenTomatoesUrl`, `filmwebUrl`), so a stored `ratings` sub-document
   * missing any one of them still threw, `decodeTolerant` skipped the row, and the film
   * disappeared from the served corpus behind a single WARN. Deriving `ResolvedMovie`'s
   * macro codec from `withDefaultedLeaves` makes its nested lookup resolve to the defaulting
   * `ResolvedRatings` codec instead of the bare macro one.
   *
   * `Showtime` is deliberately NOT defaulted. Its required field is `dateTime`, and a
   * document lacking it has no honest default: filling one would render a screening at an
   * invented time, which is worse than dropping the row. Missing-field tolerance is for a
   * row written under an older SHAPE; a screening with no time is corrupt.
   */
  private val withDefaultedLeaves: CodecRegistry = fromRegistries(
    fromCodecs(DefaultingCodec(macroRegistry.get(classOf[ResolvedRatings]), emptyRatings)),
    macroRegistry
  )

  /** Reads tolerate a document missing any field; writes still emit the full shape. See
   *  [[DefaultingCodec]] — a missing non-`Option` field otherwise kills the whole keyset
   *  batch, and `web_movies` is the sharpest case because full re-projection is retired,
   *  so a quiescent row is never rewritten. */
  val registry: CodecRegistry = fromRegistries(
    fromCodecs(
      DefaultingCodec(withDefaultedLeaves.get(classOf[ResolvedMovie]),  emptyMovie),
      DefaultingCodec(withDefaultedLeaves.get(classOf[CityScreening]), emptyScreening)
    ),
    withDefaultedLeaves
  )
}
