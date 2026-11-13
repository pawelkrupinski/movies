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
    lazy val isLatin: Boolean               = names.exists(tools.LatinScript.hasLetter)
    // ASCII names are their own Latin form ("Any-Latin; Latin-ASCII" leaves them as they are): no transliteration —
    // ICU's, per name, was ~150 MB of a PL hard-cluster run's resolve allocation, most of it on the Latin side.
    lazy val inLatin: Credits               = if (names.forall(isAscii)) this else new Credits(names.map(latinized))
    def isEmpty: Boolean = keys.isEmpty

    /** Some credit names the same person as some credit of `other`: the same words in any order
     *  ([[services.movies.PersonKey]]) or the same letters split otherwise ([[sameLetters]]). */
    def samePerson(other: Credits): Boolean =
      (keys intersect other.keys).nonEmpty ||
        words.exists(a => other.words.exists(b => sameLetters(a, b) || spelledAlike(a, b) || mononymRespelled(a, b)))
  }

  /** Two credits of as many words, in some order each word the same, a letter apart where both
   *  spellings run to five letters, or one [[transliterated]] spelling where both run to four: a
   *  transliteration's spelling ("Rajeesh Parmeswaran", TMDB's "Rajesh Parameswaran"). Short words
   *  are other names ("Jan"/"Jon"), not spellings. */
  private def spelledAlike(a: Seq[String], b: Seq[String]): Boolean =
    a.sizeIs >= 2 && a.size == b.size && a.sizeIs <= MaxOrderedWords && a != b &&
      b.permutations.exists(p => a.zip(p).forall { case (x, y) =>
        x == y || (x.length >= 5 && y.length >= 5 && tools.EditDistance.within(x, y, 1)) || (x.length >= 4 && y.length >= 4 && transliterated(x, y))
      })

  /** Two one-word credits (a mononym: Tamil and Telugu directors are often billed by one name) of
   *  six letters or more, one [[transliterated]] spelling of the other: UK Cineworld's "John Doe"
   *  credits "Madhumitha", Odeon's and TMDB's "Madhumita". No other word anchors a mononym, so any
   *  other letter's slip ("Shankar"/"Sankar", "Bala"/"Balu") is another name. */
  private def mononymRespelled(a: Seq[String], b: Seq[String]): Boolean =
    a.sizeIs == 1 && b.sizeIs == 1 && a.head.length >= 6 && b.head.length >= 6 && transliterated(a.head, b.head)

  /** Two spellings of one word that romanisations write either way: an aspirate's `h` written or
   *  not ("th"/"t", "dh"/"d", "bh"/"b", "gh"/"g", "kh"/"k", "ph"/"p") and a letter doubled or not
   *  ("Madhumitha"/"Madhumita", "Mathew"/"Matthew"). Words already folded to lowercase ASCII
   *  ([[TitleContainment.tokens]]: diacritics are folded before). "sh"/"ch"/"zh" are other sounds. */
  private[identity] def transliterated(x: String, y: String): Boolean = x != y && transliterationKey(x) == transliterationKey(y)
  private def transliterationKey(word: String): String = {
    val out = new java.lang.StringBuilder(word.length)
    var i = 0
    while (i < word.length) {
      val c = word.charAt(i)
      val previous = if (out.length == 0) '\u0000' else out.charAt(out.length - 1)
      if (c == 'h' && Aspirated.indexOf(previous) >= 0) ()
      else if (c != previous) out.append(c)
      i += 1
    }
    out.toString
  }
  private val Aspirated = "bdgkpt"

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
