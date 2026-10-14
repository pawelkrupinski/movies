package services.identity

import services.identity.IdentityMeasures.{Category, Measure, MissingFilm, MissingListing, SamePersonMeasure}
import services.movies.TitleContainment

/**
 * How two credit lists relate ([[IdentityMeasures.directorRelation]]): the credits parsed once ([[CreditRelation.Credits]]),
 * compared by name in one script or, across scripts, in Latin letters.
 */
object CreditRelation {

  private def credits(names: Iterable[String]): Seq[String] =
    names.iterator.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSeq

  private def latin(names: Iterable[String]): Boolean =
    names.exists(_.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN))

  /** A name in Latin letters: ICU's general Any-Latin transliteration, then to ASCII. Pinyin for
   *  Han, ISO-style for Cyrillic, Greek, Georgian, …: no table of names. One instance per thread
   *  (an ICU transliterator is not documented as safe to share). */
  private val toLatin = ThreadLocal.withInitial(() => com.ibm.icu.text.Transliterator.getInstance("Any-Latin; Latin-ASCII"))
  private[identity] def latinized(name: String): String = toLatin.get.transliterate(name)
  private[identity] def isAscii(s: String): Boolean = { var i = 0; while (i < s.length && s.charAt(i) < 0x80) i += 1; i == s.length }

  /** Credit lists as the director relation compares them, each written form found once. */
  final class Credits(raw: Iterable[String]) {
    val names: Seq[String] = credits(raw)
    lazy val keys: Set[String]              = names.map(services.movies.PersonKey.of).filter(_.nonEmpty).toSet
    lazy val words: Seq[Seq[String]]        = names.map(TitleContainment.tokens).filter(_.nonEmpty)
    lazy val isLatin: Boolean               = latin(names)
    // ASCII names are their own Latin form ("Any-Latin; Latin-ASCII" leaves them as they are): no transliteration —
    // ICU's, per name, was ~150 MB of a PL hard-cluster run's resolve allocation, most of it on the Latin side.
    lazy val inLatin: Credits               = if (names.forall(isAscii)) this else new Credits(names.map(latinized))
    def isEmpty: Boolean = keys.isEmpty

    /** Some credit names the same person as some credit of `other`: the same words in any order
     *  ([[services.movies.PersonKey]]) or the same letters split otherwise ([[sameLetters]]). */
    def samePerson(other: Credits): Boolean =
      (keys intersect other.keys).nonEmpty || words.exists(a => other.words.exists(b => sameLetters(a, b) || spelledAlike(a, b)))
  }

  /** Two credits of as many words, in some order each word the same or a letter apart where both
   *  spellings run to five letters: a transliteration's spelling ("Rajeesh Parmeswaran", TMDB's
   *  "Rajesh Parameswaran"). Short words are other names ("Jan"/"Jon"), not spellings. */
  private def spelledAlike(a: Seq[String], b: Seq[String]): Boolean =
    a.sizeIs >= 2 && a.size == b.size && a.sizeIs <= MaxOrderedWords && a != b &&
      b.permutations.exists(p => a.zip(p).forall { case (x, y) => x == y || (x.length >= 5 && y.length >= 5 && tools.EditDistance.within(x, y, 1)) })

  /** The most words a credit may have for [[sameLetters]] to try its orders (it enumerates them). */
  private val MaxOrderedWords = 5

  /** Two credits are one written name whatever its ORDER, CASE and SPLITTING: some order of each
   *  one's words, written out, is the same letters ("Jungjae HA" and "Ha Jung-jae" are both
   *  "hajungjae"). Every letter must be accounted for, so "Jung Ha" is not "Ha Jung-jae". */
  private def sameLetters(a: Seq[String], b: Seq[String]): Boolean =
    a.sizeIs <= MaxOrderedWords && b.sizeIs <= MaxOrderedWords && a.mkString.sorted == b.mkString.sorted && {
      val written = b.permutations.map(_.mkString).toSet
      a.permutations.exists(p => written(p.mkString))
    }

  private def byName(a: Credits, b: Credits, disagree: String): Category =
    if (a.samePerson(b)) SamePersonMeasure
    else {
      val wa = a.keys.flatMap(_.split(" ")).filter(_.length >= 3)
      val wb = b.keys.flatMap(_.split(" ")).filter(_.length >= 3)
      Category(if ((wa intersect wb).nonEmpty) "shared_name" else disagree)
    }

  /** [[directorRelation]] over credits parsed once: a listing's and a record's directors meet every
   *  pair of a family's pool, and re-parsing both per pair allocated the names again each time. */
  def of(ca: Credits, cb: Credits): Measure = {
    if (ca.isEmpty) MissingListing
    else if (cb.isEmpty) MissingFilm
    else if (ca.isLatin == cb.isLatin) byName(ca, cb, "different")
    else {
      val (la, lb) = (ca.inLatin, cb.inLatin)
      if (la.isEmpty || lb.isEmpty) Category("incomparable") else byName(la, lb, "different_script")
    }
  }
  /** Do the two credit lists name one person ([[of]]'s `same_person`)? */
  def onePerson(ca: Credits, cb: Credits): Boolean = of(ca, cb) == SamePersonMeasure
}
