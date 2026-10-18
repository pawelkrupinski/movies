package services.identity

import scala.collection.mutable

/** A mutable map read as a multimap — per key a set, a map or a count — each key dropped once it holds nothing, so a
 *  key held is a key something files under ([[LiveCorpus]]'s per-key facts). */
private[identity] object MultiMaps {
  extension [K, V](map: mutable.Map[K, Set[V]]) {
    def addValue(key: K, value: V): Unit    = map.updateWith(key)(held => Some(held.getOrElse(Set.empty) + value))
    def removeValue(key: K, value: V): Unit = map.updateWith(key)(_.map(_ - value).filter(_.nonEmpty))
  }
  extension [K, I, V](map: mutable.Map[K, Map[I, V]]) {
    def addEntry(key: K, entry: (I, V)): Unit = map.updateWith(key)(held => Some(held.getOrElse(Map.empty) + entry))
    def removeEntry(key: K, inner: I): Unit   = map.updateWith(key)(_.map(_ - inner).filter(_.nonEmpty))
  }
  extension [K](map: mutable.Map[K, Int]) {
    def countUp(key: K): Unit   = map.updateWith(key)(count => Some(count.getOrElse(0) + 1))
    def countDown(key: K): Unit = map.updateWith(key)(_.map(_ - 1).filter(_ > 0))
  }
}
