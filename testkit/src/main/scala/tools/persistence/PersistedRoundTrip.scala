package tools.persistence

import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoDatabase, SingleObservableFuture}

import tools.{Env, IsolatedMongoDatabase}

import scala.compiletime.{erasedValue, summonInline}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Writes a [[FullyPopulated]] value of every type in a [[services.PersistedCodecs]] list
 * to a real Mongo through the real codec, reads it back, and reports every way the
 * value that came back differs from the one written.
 *
 * `dropped` names the fields a codec deliberately does not persist, as
 * `"<SimpleClassName>.<field>"` — a cache-only field, say. Anything else that fails to
 * encode, fails to decode, or comes back different is a finding.
 */
object PersistedRoundTrip {

  /** Round-trip BOTH of a registry's lists ([[services.PersistedCodecs.OmittingNone]] and
   *  `WritingNone`) in a throwaway database of their own, dropped afterwards. Returns the
   *  types covered and the findings. */
  inline def registry[OmittingNone <: Tuple, WritingNone <: Tuple](registry: CodecRegistry, dropped: Set[String]): (List[String], Seq[String]) =
    val findings = IsolatedMongoDatabase.withDatabase(Env.fromProcess().get("MONGODB_URI").get, "persisted-codecs") { database =>
      all[OmittingNone](registry, database, dropped) ++ all[WritingNone](registry, database, dropped)
    }
    (names[OmittingNone] ++ names[WritingNone], findings)

  /** One finding per type that did not survive, empty when every type did. */
  inline def all[T <: Tuple](registry: CodecRegistry, database: MongoDatabase, dropped: Set[String]): Seq[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        one[head](FullyPopulated.of[head](using summonInline[FullyPopulated[head]]), registry, database, dropped)(
          using summonInline[ClassTag[head]]) ++ all[tail](registry, database, dropped)
    }

  /** The simple names of every type in `T` — so a spec can say which it covered. */
  inline def names[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple      => Nil
    case _: (head *: tail) => summonInline[ClassTag[head]].runtimeClass.getSimpleName :: names[tail]
  }

  def one[T](value: T, registry: CodecRegistry, database: MongoDatabase, dropped: Set[String])(using tag: ClassTag[T]): Seq[String] = {
    val cls  = tag.runtimeClass.asInstanceOf[Class[T]]
    val name = cls.getSimpleName
    val attempt = for {
      codec   <- Try(registry.get(cls))
      encoded <- Try { val document = new BsonDocument(); codec.encode(new BsonDocumentWriter(document), value, EncoderContext.builder().build()); document }
      stored  <- Try(store(database, name, encoded))
      decoded <- Try(codec.decode(new BsonDocumentReader(stored), DecoderContext.builder().build()))
    } yield decoded
    attempt match {
      case Failure(e)    => Seq(s"$name: ${e.getClass.getSimpleName}: ${e.getMessage}")
      case Success(back) => differences(value, back, name, dropped).map(d => s"$name: $d")
    }
  }

  /** Insert, then read the document back by its `_id` — Mongo supplies an ObjectId when the
   *  type carries none, and the decoder skips it as it skips any unknown field. */
  private def store(database: MongoDatabase, name: String, document: BsonDocument): BsonDocument = {
    val collection = database.getCollection[BsonDocument](s"roundtrip_$name")
    Await.result(collection.insertOne(document).toFuture(), 30.seconds)
    Await.result(collection.find(Filters.eq("_id", document.get("_id"))).head(), 30.seconds)
  }

  /** Where `actual` departs from `expected`, as `path: expected → actual` lines. Walks
   *  case classes field by field, so a finding names the field rather than dumping two
   *  whole records. */
  def differences(expected: Any, actual: Any, path: String, dropped: Set[String]): Seq[String] =
    (expected, actual) match {
      case (e: Product, a: Product) if e.getClass == a.getClass && e.productArity > 0 && !e.isInstanceOf[Iterable[?]] =>
        val owner = e.getClass.getSimpleName
        e.productElementNames.zip(e.productIterator.zip(a.productIterator)).toSeq.flatMap {
          case (field, _) if dropped(s"$owner.$field") => Nil
          case (field, (ev, av))                       => differences(ev, av, s"$path.$field", dropped)
        }
      case (e: collection.Map[?, ?], a: collection.Map[?, ?]) =>
        val (em, am) = (e.asInstanceOf[collection.Map[Any, Any]], a.asInstanceOf[collection.Map[Any, Any]])
        if (em.keySet != am.keySet) Seq(s"$path: keys ${em.keySet} → ${am.keySet}")
        else em.keys.toSeq.flatMap(k => differences(em(k), am(k), s"$path[$k]", dropped))
      case (e: collection.Set[?], a: collection.Set[?]) =>
        if (e == a) Nil else Seq(s"$path: $e → $a")
      case (e: Iterable[?], a: Iterable[?]) =>
        if (e.size != a.size) Seq(s"$path: ${e.size} element(s) → ${a.size}")
        else e.zip(a).zipWithIndex.toSeq.flatMap { case ((ev, av), i) => differences(ev, av, s"$path[$i]", dropped) }
      case (e: Array[?], a: Array[?]) =>
        differences(e.toSeq, a.toSeq, path, dropped)
      case (e, a) if e == a => Nil
      case (e, a)           => Seq(s"$path: $e → $a")
    }
}
