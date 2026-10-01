package services.movies

import scala.collection.mutable

/**
 * A key → set-of-values index that keeps only non-empty sets, and keeps them IMMUTABLE.
 *
 * [[CorpusIndex]] holds seven of these (cinema slot → rows, row × cinema → sources, alias →
 * rows, …), and nearly every set holds ONE value. A `mutable.HashSet` costs its object, a
 * bucket array and a node — over 100 bytes — even for one element; an immutable `Set` of up to
 * four elements is a single small object (`Set1`…`Set4`). On the US worker (~104k listings)
 * those tiny mutable sets were ~219k objects of the live heap.
 */
private[movies] final class SetIndex[K, V] {
  private val sets = mutable.HashMap.empty[K, AnyRef]

  def add(key: K, value: V): Unit = sets.updateWith(key)(held => Some(OneOrMany.add(held, value)))

  /** Drop `value` from `key`'s set, and the key with its last value. */
  def remove(key: K, value: V): Unit = sets.updateWith(key)(_.flatMap(OneOrMany.remove(_, value)))

  def removeAll(key: K): Unit = sets -= key

  def get(key: K): Set[V] = sets.get(key).fold(Set.empty[V])(OneOrMany.values[V])

  def holds(key: K): Boolean = sets.contains(key)

  def toMap: Map[K, Set[V]] = sets.iterator.map { case (k, held) => k -> OneOrMany.values[V](held) }.toMap
}

/** The values of one index entry as stored: a lone value bare, two or more as a [[Many]] — nearly
 *  every entry holds one, and even a one-element immutable Set (`Set1`) was an object of its own:
 *  ~440k of them, 7 MB of the US worker's live heap (2026-10-01). A bare value is never a `Many`,
 *  which is private here, so the two cannot be confused whatever `V` is. */
private[movies] object OneOrMany {
  private final class Many(val values: Set[Any])

  def add(held: Option[AnyRef], value: Any): AnyRef = held match {
    case None                        => value.asInstanceOf[AnyRef]
    case Some(many: Many)            => new Many(many.values + value)
    case Some(one) if one == value   => one
    case Some(one)                   => new Many(Set(one, value))
  }

  /** `held` without `value`: None when it held nothing else. */
  def remove(held: AnyRef, value: Any): Option[AnyRef] = held match {
    case many: Many =>
      val rest = many.values - value
      if (rest.isEmpty) None else if (rest.sizeIs == 1) Some(rest.head.asInstanceOf[AnyRef]) else Some(new Many(rest))
    case one => if (one == value) None else Some(one)
  }

  def values[V](held: AnyRef): Set[V] = held match {
    case many: Many => many.values.asInstanceOf[Set[V]]
    case one        => Set(one.asInstanceOf[V])
  }

  def contains(held: AnyRef, value: Any): Boolean = held match {
    case many: Many => many.values.contains(value)
    case one        => one == value
  }
}

/**
 * A [[SetIndex]] keyed on a PAIR, nested by the pair's first component instead of keyed by a
 * tuple: a tuple per entry was its own object beside the map node (on the US worker's live dump,
 * [[CorpusIndex]]'s two pair-keyed indexes held 209k entries with 6.7 MB of Tuple2 keys), while
 * the first components — cinemas, rows — are few and each holds many seconds. The inner maps are
 * immutable, a single small object while they hold one to four.
 */
private[movies] final class PairSetIndex[A, B, V] {
  private val byFirst = mutable.HashMap.empty[A, Map[B, AnyRef]]

  def add(a: A, b: B, value: V): Unit =
    byFirst.updateWith(a)(inner => Some(inner.fold(Map(b -> OneOrMany.add(None, value)))(held => held.updated(b, OneOrMany.add(held.get(b), value)))))

  /** Drop `value` from `(a, b)`'s set, the pair with its last value, and `a` with its last pair. */
  def remove(a: A, b: B, value: V): Unit =
    byFirst.updateWith(a)(_.map { held =>
      held.get(b).fold(held)(values => OneOrMany.remove(values, value).fold(held - b)(rest => held.updated(b, rest)))
    }.filter(_.nonEmpty))

  def removeAll(a: A, b: B): Unit = byFirst.updateWith(a)(_.map(_ - b).filter(_.nonEmpty))

  def get(a: A, b: B): Set[V] = byFirst.get(a).flatMap(_.get(b)).fold(Set.empty[V])(OneOrMany.values[V])

  def holds(a: A, b: B): Boolean = byFirst.get(a).exists(_.contains(b))

  def toMap: Map[(A, B), Set[V]] =
    byFirst.iterator.flatMap { case (a, inner) => inner.iterator.map { case (b, values) => (a, b) -> OneOrMany.values[V](values) } }.toMap
}
