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
  private val sets = mutable.HashMap.empty[K, Set[V]]

  def add(key: K, value: V): Unit = sets.updateWith(key)(held => Some(held.fold(Set(value))(_ + value)))

  /** Drop `value` from `key`'s set, and the key with its last value. */
  def remove(key: K, value: V): Unit = sets.updateWith(key)(_.map(_ - value).filter(_.nonEmpty))

  def removeAll(key: K): Unit = sets -= key

  def get(key: K): Set[V] = sets.getOrElse(key, Set.empty)

  def holds(key: K): Boolean = sets.contains(key)

  def toMap: Map[K, Set[V]] = sets.toMap
}

/**
 * A [[SetIndex]] keyed on a PAIR, nested by the pair's first component instead of keyed by a
 * tuple: a tuple per entry was its own object beside the map node (on the US worker's live dump,
 * [[CorpusIndex]]'s two pair-keyed indexes held 209k entries with 6.7 MB of Tuple2 keys), while
 * the first components — cinemas, rows — are few and each holds many seconds. The inner maps are
 * immutable, a single small object while they hold one to four.
 */
private[movies] final class PairSetIndex[A, B, V] {
  private val byFirst = mutable.HashMap.empty[A, Map[B, Set[V]]]

  def add(a: A, b: B, value: V): Unit =
    byFirst.updateWith(a)(inner => Some(inner.fold(Map(b -> Set(value)))(held => held.updated(b, held.get(b).fold(Set(value))(_ + value)))))

  /** Drop `value` from `(a, b)`'s set, the pair with its last value, and `a` with its last pair. */
  def remove(a: A, b: B, value: V): Unit =
    byFirst.updateWith(a)(_.map { held =>
      held.get(b).map(_ - value).fold(held)(rest => if (rest.isEmpty) held - b else held.updated(b, rest))
    }.filter(_.nonEmpty))

  def removeAll(a: A, b: B): Unit = byFirst.updateWith(a)(_.map(_ - b).filter(_.nonEmpty))

  def get(a: A, b: B): Set[V] = byFirst.get(a).flatMap(_.get(b)).getOrElse(Set.empty)

  def holds(a: A, b: B): Boolean = byFirst.get(a).exists(_.contains(b))

  def toMap: Map[(A, B), Set[V]] = byFirst.iterator.flatMap { case (a, inner) => inner.iterator.map { case (b, values) => (a, b) -> values } }.toMap
}
