package tools.persistence

import java.time.{Instant, LocalDate, LocalDateTime}
import scala.compiletime.summonAll
import scala.deriving.Mirror
import scala.reflect.ClassTag

/**
 * A value of `T` with EVERYTHING filled in: every `Option` is `Some`, every collection
 * holds two elements, every nested case class is itself fully populated — derived from
 * the case class's `Mirror`, so a field added to a persisted type is populated here the
 * moment it compiles, with no edit to any spec.
 *
 * Every leaf is DISTINCT (a running counter feeds each one), so a codec that reads one
 * field back into another is caught as surely as one that drops it.
 *
 * A field of a type with no instance below fails to COMPILE the spec that asks for it —
 * the right outcome for a persisted field nothing knows how to fill: add a `given` here.
 */
trait FullyPopulated[T] {
  def make(leaves: FullyPopulated.Leaves): T
}

object FullyPopulated extends DerivedFullyPopulated {

  /** The counter behind every leaf. One per top-level value. */
  final class Leaves {
    private var n = 0
    def next(): Int = { n += 1; n }
  }

  def of[T](using populated: FullyPopulated[T]): T = populated.make(new Leaves)

  given FullyPopulated[String]  = leaves => s"value-${leaves.next()}"
  given FullyPopulated[Int]     = leaves => leaves.next()
  given FullyPopulated[Long]    = leaves => leaves.next().toLong * 1000003L
  given FullyPopulated[Double]  = leaves => leaves.next() + 0.25
  // `true`, the non-default: a `Boolean` field defaulting to false would otherwise be
  // indistinguishable from one that was never written.
  given FullyPopulated[Boolean] = _ => true
  // Whole milliseconds and whole minutes: BSON dates carry millisecond precision, so a
  // finer instant would "fail" the round trip for a reason no production value has.
  given FullyPopulated[Instant]       = leaves => Instant.parse("2026-09-24T10:00:00Z").plusSeconds(leaves.next().toLong * 61)
  given FullyPopulated[LocalDateTime] = leaves => LocalDateTime.of(2026, 9, 24, 10, 0).plusMinutes(leaves.next().toLong * 7)
  given FullyPopulated[LocalDate]     = leaves => LocalDate.of(2026, 9, 24).plusDays(leaves.next().toLong)

  given [A](using a: FullyPopulated[A]): FullyPopulated[Option[A]] = leaves => Some(a.make(leaves))
  given [A](using a: FullyPopulated[A]): FullyPopulated[Seq[A]]    = leaves => Seq(a.make(leaves), a.make(leaves))
  given [A](using a: FullyPopulated[A]): FullyPopulated[List[A]]   = leaves => List(a.make(leaves), a.make(leaves))
  given [A](using a: FullyPopulated[A]): FullyPopulated[Vector[A]] = leaves => Vector(a.make(leaves), a.make(leaves))
  given [A](using a: FullyPopulated[A]): FullyPopulated[Set[A]]    = leaves => Set(a.make(leaves), a.make(leaves))
  given [V](using v: FullyPopulated[V]): FullyPopulated[Map[String, V]] =
    leaves => Map(s"key-${leaves.next()}" -> v.make(leaves), s"key-${leaves.next()}" -> v.make(leaves))
  given [A: ClassTag](using a: FullyPopulated[A]): FullyPopulated[IArray[A]] =
    leaves => IArray(a.make(leaves), a.make(leaves))
}

/** Case-class derivation, at lower priority than the explicit instances above so a
 *  collection or `Option` never resolves through its own `Mirror`. */
trait DerivedFullyPopulated {
  inline given derived[T](using mirror: Mirror.ProductOf[T]): FullyPopulated[T] =
    new ProductPopulated[T](mirror, summonAll[Tuple.Map[mirror.MirroredElemTypes, FullyPopulated]].toList
      .asInstanceOf[List[FullyPopulated[Any]]])
}

final class ProductPopulated[T](mirror: Mirror.ProductOf[T], elements: List[FullyPopulated[Any]]) extends FullyPopulated[T] {
  def make(leaves: FullyPopulated.Leaves): T =
    mirror.fromProduct(Tuple.fromArray(elements.map(_.make(leaves)).toArray[Any]))
}
