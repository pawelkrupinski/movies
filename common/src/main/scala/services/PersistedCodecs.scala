package services

import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.mongodb.scala.bson.codecs.Macros

import scala.compiletime.erasedValue

/**
 * A BSON codec registry together with the list of case classes it writes to Mongo.
 *
 * The list is a TUPLE TYPE, and the registry's macro codecs are derived FROM it (via
 * [[PersistedCodecs.omittingNone]] / [[PersistedCodecs.writingNone]]), so a type cannot
 * be written without being on the list. `PersistedCodecsRoundTripSpec` round-trips every
 * type on every list through the real codec and a real Mongo, with every `Option` set and
 * every collection non-empty — a new persisted type, or a new field on one, is covered the
 * moment it compiles. `PersistedCodecsLintSpec` keeps `Macros.createCodecProvider` from
 * being called anywhere else.
 *
 * Why this exists: on 2026-09-23 `SourceData` gained an `IArray[Int]` field, which has no
 * BSON codec, so every write of a record carrying it threw "Can't find a codec for class
 * [I" into a swallowed WARN. No codec spec populated that field, so nothing noticed.
 */
trait PersistedCodecs {
  /** Case classes whose codec OMITS a `None` field on write. */
  type OmittingNone <: Tuple
  /** Case classes whose codec writes a `None` field as BSON null. */
  type WritingNone <: Tuple
  def registry: CodecRegistry
}

object PersistedCodecs {

  /** One `createCodecProviderIgnoreNone` per type in `T`, in order. */
  inline def omittingNone[T <: Tuple]: List[CodecProvider] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) => Macros.createCodecProviderIgnoreNone[head]() :: omittingNone[tail]
  }

  /** One `createCodecProvider` per type in `T`, in order. */
  inline def writingNone[T <: Tuple]: List[CodecProvider] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) => Macros.createCodecProvider[head]() :: writingNone[tail]
  }
}
