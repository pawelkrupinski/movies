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
