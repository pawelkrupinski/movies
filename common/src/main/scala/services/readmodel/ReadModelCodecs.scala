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
  val registry: CodecRegistry = fromRegistries(
    fromCodecs(JavaTimeCodecs.localDateTime),
    fromProviders(
      Macros.createCodecProviderIgnoreNone[Showtime](),
      Macros.createCodecProviderIgnoreNone[ResolvedRatings](),
      Macros.createCodecProviderIgnoreNone[ResolvedMovie](),
      Macros.createCodecProviderIgnoreNone[CityScreening]()
    ),
    DEFAULT_CODEC_REGISTRY
  )
}
