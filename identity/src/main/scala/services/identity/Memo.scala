package services.identity

import scala.collection.concurrent.TrieMap

/**
 * A value cached by its FULL dependency set (docs/design/identity-no-drift.md: stored state is persisted inputs, or a
 * cache keyed by everything it read). Each entry is a value with its recorded dependencies — `dependenciesOf` it: each
 * [[Dependency]] with the digest of what was read of it — and it is valid only while every one of those digests is
 * unchanged. Dropping an entry never changes a result, only what it costs to reach it again.
 *
 * Two ways to tell an entry still stands, which must agree:
 *  - [[clean]], the DIRTY INDEX: found standing (`check`ed) at a `version` of the inputs, and none of its dependencies'
 *    filings ([[Dependency.filing]]) has come since — the cheap check every read makes;
 *  - [[stands]], FULL VALIDATION: every dependency read again, each digest as recorded.
 * [[verified]] runs the second over an entry the first calls clean — the caller's full-validation pass, a budget at a
 * time: one the index calls clean whose dependencies moved is DRIFT — counted, and unchecked so it is decided again. The
 * drift count is the proof the index is sound; it must read zero.
 *
 * `indexed` picks the dependencies whose filings [[reads]] answers for — what a feed must hand over because an entry read
 * it. `pushed` picks the dependencies no filing announces (the year, an attempt): each kept with the entries that read
 * it, so a change of one ([[moved]]) reaches exactly the entries whose recorded digest of it moved — by value, never by
 * a receipt merely naming it (docs/design/identity-no-drift.md, "One dependency index"). Not thread-safe for writers:
 * one thread writes (the stage's apply); [[reads]] is read from any.
 */
final class Memo[K, V](dependenciesOf: V => collection.Map[Dependency, Long], indexed: Dependency => Boolean = _ => true,
                       pushed: Dependency => Boolean = _ => false) {
  private val entries = TrieMap.empty[K, V]
  /** Each entry found standing: the stamp it was found under (what of the entry's key the caller compares, a listings'
   *  digest) and the version of the inputs it was found at. */
  private val checks  = TrieMap.empty[K, (Long, Long)]
  /** The dirty index: each indexed filing, with how many entries read it. */
  private val readers = new java.util.concurrent.ConcurrentHashMap[String, Integer]()
  /** The push index: each [[pushed]] dependency an entry read, with the keys of the entries that read it. */
  private val pushedReaders = scala.collection.mutable.HashMap.empty[Dependency, scala.collection.mutable.HashSet[K]]
  /** Each version's filings since, read once per [[newPass]]. */
  private val filings = scala.collection.mutable.HashMap.empty[Long, Option[Set[String]]]

  def get(key: K): Option[V]          = entries.get(key)
  def contains(key: K): Boolean       = entries.contains(key)
  def keys: Iterator[K]               = entries.keysIterator
  def values: Iterator[V]             = entries.valuesIterator
  def size: Int                       = entries.size

  /** `value` held under `key`, the one it replaces dropped from the index. Its check is the caller's to make. */
  def put(key: K, value: V): Unit = {
    entries.put(key, value).foreach { was => counted(was, -1); unpushed(key, was) }
    counted(value, 1)
    dependenciesOf(value).keysIterator.filter(pushed).foreach(read => pushedReaders.getOrElseUpdate(read, scala.collection.mutable.HashSet.empty) += key)
  }

  /** The entry under `key`, if any, dropped with its check. */
  def remove(key: K): Option[V] = {
    checks -= key
    entries.remove(key).map { value => counted(value, -1); unpushed(key, value); value }
  }

  /** Does an entry read a dependency filed as `filing`? Read from any thread. */
  def reads(filing: String): Boolean = readers.containsKey(filing)

  /** The entry under `key` found standing under `stamp` at `version`. */
  def check(key: K, stamp: Long, version: Long): Unit = checks(key) = (stamp, version)
  def uncheck(key: K): Unit = checks -= key
  /** A change of the [[pushed]] dependencies `kind` picks — inputs no filing announces: the year turning, an attempt
   *  marked. Each is read ONCE (`now`, None for one nothing reads again: moved for every reader), and every entry whose
   *  receipt recorded another digest of it is unchecked, so the next pass decides it again; an entry that read it as it
   *  reads now stays as it was, and an entry that never read it is never visited. How many entries read one that moved. */
  def moved(kind: Dependency => Boolean, now: Dependency => Option[Long]): Int = {
    val reached = scala.collection.mutable.HashSet.empty[K]
    pushedReaders.foreachEntry { (read, keys) =>
      if (kind(read)) {
        val digest = now(read)
        keys.foreach(key => if (entries.get(key).exists(value => !digest.exists(dependenciesOf(value).get(read).contains))) reached += key)
      }
    }
    reached.foreach(checks -= _)
    reached.size
  }
  /** The entries whose receipt names `read`, a [[pushed]] dependency. */
  def readersOf(read: Dependency): collection.Set[K] = pushedReaders.getOrElse(read, Set.empty[K])
  def checked(key: K): Option[(Long, Long)] = checks.get(key)
  def isChecked(key: K): Boolean = checks.contains(key)
  /** The check of `from` moved to `to` (a value filed under another key), as it was under `stamp`. */
  def moveCheck(from: K, to: K, stamp: Long): Unit = checks.get(from).filter(_._1 == stamp).fold(checks -= to)(checks(to) = _)

  /** A new pass over the entries: the filings since each version are read again. */
  def newPass(): Unit = filings.clear()

  /** Is the entry under `key` clean by the index at `version`: checked under `stamp` at it, or at an earlier version
   *  every dependency filed since (`filedSince`: the filings since a version, None where it cannot tell) reads `again`
   *  as recorded — by default none may be? Found so, it is checked at `version`. */
  def clean(key: K, stamp: Long, version: Long, filedSince: Long => Option[Set[String]],
            again: (Dependency, Long) => Boolean = (_, _) => false): Boolean =
    entries.get(key).exists(value => checks.get(key).exists { case (was, at) =>
      was == stamp && (at == version || unfiled(value, at, filedSince, again) && { checks(key) = (stamp, version); true })
    })

  /** Does each of `value`'s dependencies filed since `at` read `again` as recorded (by default: was none filed)? */
  def unfiled(value: V, at: Long, filedSince: Long => Option[Set[String]], again: (Dependency, Long) => Boolean = (_, _) => false): Boolean =
    filings.getOrElseUpdate(at, filedSince(at)).exists(filed => dependenciesOf(value).forall { case (read, digest) => !filed(read.filing) || again(read, digest) })

  /** Does `value` stand by reading every dependency again (`now`: what a dependency reads now, None for one nothing
   *  reads again — what holds it never stands)? */
  def stands(value: V, now: Dependency => Option[Long]): Boolean =
    dependenciesOf(value).forall { case (read, digest) => now(read).contains(digest) }

  /** FULL VALIDATION of an entry the index called clean ([[clean]]): `value` read again ([[stands]]). One that does not
   *  stand is DRIFT — the index called clean what moved unannounced — unchecked, so the caller decides it again. */
  def verified(key: K, value: V, now: Dependency => Option[Long]): Memo.Validation =
    if (stands(value, now)) Memo.Validation(1, 0) else { checks -= key; Memo.Validation(1, 1) }

  private def unpushed(key: K, value: V): Unit =
    dependenciesOf(value).keysIterator.filter(pushed).foreach(read => pushedReaders.get(read).foreach { keys =>
      keys -= key
      if (keys.isEmpty) pushedReaders -= read
    })

  private def counted(value: V, by: Int): Unit =
    dependenciesOf(value).keysIterator.filter(indexed).map(_.filing).toSet.foreach { (filing: String) =>
      readers.merge(filing, by, (a, b) => { val n = a + b; if (n == 0) null else n })
    }
}

object Memo {
  /** What full validation came to: how many clean entries it read again, and how many of them had drifted. */
  final case class Validation(read: Int, drifted: Int) {
    def +(other: Validation): Validation = Validation(read + other.read, drifted + other.drifted)
  }
  object Validation { val None: Validation = Validation(0, 0) }
}
