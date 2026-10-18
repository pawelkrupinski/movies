package services.identity

/**
 * A set of hashes held as one sorted array, each once — what [[VenueNames]] and [[ProseWords]] keep of a text instead
 * of the text: a corpus holds one per listing and per film read. `S` is the set's own kind, which every operation
 * answers in and which [[equals]] compares within.
 */
abstract class SortedHashes[S <: SortedHashes[S]](private[identity] final val held: Array[Int]) { self: S =>
  /** A set of `S`'s kind holding `sorted` (sorted, each once, possibly empty). */
  protected def holding(sorted: Array[Int]): S
  /** What one hash stands for, as [[toString]] counts them ("words", "pairs"). */
  protected def unit: String

  def isEmpty: Boolean  = held.length == 0
  def nonEmpty: Boolean = !isEmpty
  /** How many hashes it holds. */
  def size: Int = held.length
  /** Its hashes, as a recorded fixture or a stored record keeps them (the companion's `ofHashes`). */
  def hashes: Seq[Int] = held.toSeq

  /** Does it hold every one of `hashes` (and at least one)? */
  protected final def holdsAll(hashes: Array[Int]): Boolean =
    hashes.length > 0 && hashes.forall(java.util.Arrays.binarySearch(held, _) >= 0)

  def ++(other: S): S =
    if (other.isEmpty) this else if (isEmpty) other else holding((held ++ other.held).distinct.sorted)
  /** The hashes it holds that `other` does not. */
  def --(other: S): S =
    if (isEmpty || other.isEmpty) this else holding(held.filter(java.util.Arrays.binarySearch(other.held, _) < 0))
  /** The hashes both hold. */
  def &(other: S): S =
    if (isEmpty) this else if (other.isEmpty) other else holding(held.filter(java.util.Arrays.binarySearch(other.held, _) >= 0))

  override def equals(other: Any): Boolean = other match {
    case that: SortedHashes[?] => (that.getClass eq getClass) && java.util.Arrays.equals(held, that.held)
    case _                     => false
  }
  override def hashCode: Int = java.util.Arrays.hashCode(held)
  override def toString: String = {
    val kind = getClass.getSimpleName
    if (isEmpty) s"$kind()" else f"$kind(${held.length} $unit #$hashCode%08x)"
  }
}

object SortedHashes {
  /** `hashes` sorted, each once, in an array of its own. */
  def of(hashes: Seq[Int]): Array[Int] = hashes.toArray.distinct.sorted

  /** The first `count` of `hashes`, sorted, each once — in place, no boxing: for a set read at intake off every listing. */
  def sortedDistinct(hashes: Array[Int], count: Int): Array[Int] =
    if (count == 0) Array.emptyIntArray
    else {
      java.util.Arrays.sort(hashes, 0, count)
      var kept = 1
      var i    = 1
      while (i < count) { if (hashes(i) != hashes(kept - 1)) { hashes(kept) = hashes(i); kept += 1 }; i += 1 }
      java.util.Arrays.copyOf(hashes, kept)
    }
}
