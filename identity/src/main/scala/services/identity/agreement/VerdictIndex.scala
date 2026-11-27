package services.identity.agreement

import scala.collection.mutable

import services.identity.vocabulary.{Dependency, Memo}

/**
 * The verdicts the agreement stage keeps, by their cluster's key — each a cache of its receipt ([[Memo]]), standing while
 * none of it moved: checked at a version under its listings' digest, and indexed by every TMDB record and search it read
 * — and beside them the same verdicts by their listings' digest, a model take's correction apart from a cluster's: what a
 * verdict is FOUND by, its key only naming it ([[heldFor]]). Loaded from `sources`' stored verdicts when first read.
 *
 * `sources`' changes: the answers filed since a version, which a kept verdict's [[clean]] reads.
 */
private[identity] final class VerdictIndex(sources: AgreementSources.VerdictSource) {
  /** The verdicts kept, by their cluster's key. */
  lazy val memo: Memo[String, StoredVerdict] = {
    val loaded = new Memo[String, StoredVerdict](_.reads, _.readsTmdb, VerdictIndex.pushed)
    sources.stored.all().foreach(v => loaded.put(v.id, v))
    loaded
  }
  /** The held verdicts by their listings' digest, a model take's correction apart from a cluster's. Kept beside
   *  [[memo]] by every [[file]] and [[unfile]]. */
  private lazy val byDigest: (mutable.LongMap[StoredVerdict], mutable.LongMap[StoredVerdict]) = {
    val index = (mutable.LongMap.empty[StoredVerdict], mutable.LongMap.empty[StoredVerdict])
    memo.values.foreach(indexed(index, _))
    index
  }

  def get(id: String): Option[StoredVerdict] = memo.get(id)
  /** The keys of the verdicts kept. */
  def ids: Set[String] = memo.keys.toSet
  /** The verdicts kept, read afresh at each call. */
  def values: Iterator[StoredVerdict] = memo.values
  /** How many verdicts are kept. */
  def size: Int = memo.size
  /** Does a verdict kept read the TMDB record or search a filing under `filing` comes as? */
  def reads(filing: String): Boolean = memo.reads(filing)
  /** An apply's pass begins: the filings met in the last one forgotten ([[Memo.newPass]]). */
  def newPass(): Unit = memo.newPass()
  /** The verdicts that read a dependency of `kind` as it no longer reads `now`, unchecked: how many ([[Memo.moved]]). */
  def moved(kind: Dependency => Boolean, now: Dependency => Option[Long]): Int = memo.moved(kind, now)
  /** `id`'s verdict checked at `version` over the listings digesting to `digest`. */
  def check(id: String, digest: Long, version: Long): Unit = memo.check(id, digest, version)
  /** `id`'s verdict checked no more: read again at its next decision ([[Memo.uncheck]]). */
  def uncheck(id: String): Unit = memo.uncheck(id)
  /** Does `verdict` stand on what it read, each read as it reads `now` ([[Memo.stands]])? */
  def stands(verdict: StoredVerdict, now: Dependency => Option[Long]): Boolean = memo.stands(verdict, now)
  /** `id`'s `verdict` read again whole, each read as it reads `now`: what was read, and what drifted ([[Memo.verified]]). */
  def verified(id: String, verdict: StoredVerdict, now: Dependency => Option[Long]): Memo.Validation = memo.verified(id, verdict, now)
  /** Does `id`'s verdict stand at `version` over the listings digesting to `digest`: none of its reads filed since it was
   *  checked, or each filed one read `again` alike ([[Memo.clean]])? */
  def clean(id: String, digest: Long, version: Long, again: (Dependency, Long) => Boolean): Boolean =
    memo.clean(id, digest, version, sources.changes.changedSince, again)

  /** The verdict held of a cluster (or, `correction`, of a model take's correction) whose listings digest to `digest`,
   *  under whichever key it was filed. */
  def byListings(digest: Long, correction: Boolean): Option[StoredVerdict] =
    (if (correction) byDigest._2 else byDigest._1).get(digest).filter(_.listings == digest)

  /** The verdict held for the cluster `key` names with listings digesting to `digest` — under its key, else under
   *  whichever key it was filed by (a restart's keys lost or numbered otherwise, a pre-key verdict under its first
   *  listing): filed under `key` from now, the move handed to `moved`. A verdict stands on its listings and receipt,
   *  never on the key it was filed by. */
  def heldFor(key: String, digest: Long, moved: StoredVerdict => Unit): Option[StoredVerdict] =
    memo.get(key).filter(_.listings == digest).orElse {
      byListings(digest, correction = key.startsWith(VerdictIndex.CorrectionPrefix)).map { found =>
        val under = found.copy(id = key)
        file(under); moved(under)
        // checked as it was under the key it was filed by (a boot's every standing verdict)
        memo.moveCheck(found.id, key, digest)
        under
      }
    }

  /** `verdict` held under its id — found by its digest — the verdict it replaces dropped. */
  def file(verdict: StoredVerdict): Unit = {
    val index = byDigest   // built from what is held before it moves
    memo.get(verdict.id).foreach(dropped)
    memo.put(verdict.id, verdict); indexed(index, verdict)
  }

  /** The verdict held under `id`, if any, dropped. */
  def unfile(id: String): Unit = {
    val _ = byDigest
    memo.remove(id).foreach(dropped)
  }

  /** The verdicts gone (`removed`) and moved stored, and held so. */
  def store(removed: Set[String], moved: Seq[StoredVerdict]): Unit = {
    sources.stored.replace(removed, moved)
    removed.foreach(unfile)
    moved.foreach(file)
  }

  private def indexed(index: (mutable.LongMap[StoredVerdict], mutable.LongMap[StoredVerdict]), verdict: StoredVerdict): Unit =
    (if (verdict.correction.isDefined) index._2 else index._1)(verdict.listings) = verdict

  private def dropped(verdict: StoredVerdict): Unit = {
    val index = if (verdict.correction.isDefined) byDigest._2 else byDigest._1
    if (index.get(verdict.listings).exists(_ eq verdict)) index -= verdict.listings
  }
}

private[identity] object VerdictIndex {
  /** What a correction's stored id is its cluster's id behind. */
  val CorrectionPrefix = "correction|"
  /** The dependencies no filing announces, which the held verdicts' index keeps their readers of ([[Memo.moved]]): the
   *  year, and the question memory's attempts. */
  val pushed: Dependency => Boolean = read => read == Dependency.Year || isAttempt(read)
  val isAttempt: Dependency => Boolean = {
    case Dependency.Attempted(_) => true
    case _                       => false
  }
}
