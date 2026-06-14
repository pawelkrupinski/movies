package services.users

import models.{User, UserState}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros

/**
 * BSON codec wiring for the user-related collections. The shapes are
 * small (only `String`, `Option[String]`, `Set[String]`, `Instant`) so
 * the macros derive everything from `User` / `UserState` directly —
 * there's no DTO indirection here, unlike `StoredMovieDto`. Domain
 * fields named `id` / `userId` end up as regular doc fields; the
 * `_id` is delegated to whichever field the repository chooses (it filters
 * by `id` / `userId` explicitly so the auto-generated ObjectId never
 * surfaces in domain code).
 *
 * `Instant` and `Set[String]` are both handled by the default codec
 * registry — only `MovieCodecs` needs a custom one (`LocalDateTime`,
 * which has no zone and so the driver doesn't ship a codec).
 */
object UserCodecs {

  val registry: CodecRegistry = fromRegistries(
    fromProviders(
      Macros.createCodecProviderIgnoreNone[User](),
      Macros.createCodecProviderIgnoreNone[UserState]()
    ),
    DEFAULT_CODEC_REGISTRY
  )

  // `IgnoreNone` keeps the docs tidy when the user opted out of
  // email/avatar/displayName sharing at consent time. Without it,
  // every optional field that's `None` would be written as `BsonNull`.
  // The trade-off is that a `None` write doesn't *clear* a previously
  // set field — for User we only do full-doc upserts, so that's fine;
  // a future partial update would need to use `$unset` explicitly.
}
