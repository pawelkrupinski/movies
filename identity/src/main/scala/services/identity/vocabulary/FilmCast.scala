package services.identity.vocabulary

/** A film's top-billed cast as the cast MISMATCH reads it: each name of two words or more in
 *  Latin letters as its unordered pairs ([[ListedCast.pairsOf]]). */
final class FilmCast private (private val names: Array[Array[Int]]) {
  /** How many of its names `listed` names. */
  def namedBy(listed: ListedCast): Int = names.count(listed.namesPairs)
  /** Each name's pairs, as a stored record keeps them ([[FilmCast.ofPairs]]). */
  def pairs: Seq[Seq[Int]] = names.toSeq.map(_.toSeq)
  override def equals(other: Any): Boolean = other match {
    case that: FilmCast => java.util.Arrays.deepEquals(names.asInstanceOf[Array[AnyRef]], that.names.asInstanceOf[Array[AnyRef]])
    case _              => false
  }
  override def hashCode: Int = java.util.Arrays.deepHashCode(names.asInstanceOf[Array[AnyRef]])
  override def toString: String = s"FilmCast(${names.length})"
}

object FilmCast {
  /** A cast as [[FilmCast.pairs]] gave it. */
  def ofPairs(pairs: Seq[Seq[Int]]): Option[FilmCast] = Option.when(pairs.nonEmpty)(new FilmCast(pairs.map(_.toArray).toArray))

  /** `None` unless TMDB bills [[ListedCast.Names]] names or more in Latin letters: a Greek or Japanese record's cast is
   *  another spelling of the names a venue transliterates. */
  def of(names: Seq[String]): Option[FilmCast] = {
    val latin = names.distinct.filter(_.forall(c => !Character.isLetter(c) || c < 0x0250)).map(ListedCast.pairsOf).filter(_.nonEmpty)
    Option.when(latin.sizeIs >= ListedCast.Names)(new FilmCast(latin.toArray))
  }
}
