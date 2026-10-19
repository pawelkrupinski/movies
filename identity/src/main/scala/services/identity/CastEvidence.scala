package services.identity

import scala.util.hashing.MurmurHash3

/**
 * The people a venue's OWN text names — its synopsis and its cast field — as [[CastEvidence]] reads them: each pair of
 * neighbouring words of a run of capitalised words ("Marcin Dorociński", "Bolesława Prusa"), folded (accents off, `ł` as
 * `l`, lower case) and hashed; in a cast field, whose entries are names whatever their case, every pair of an entry's
 * neighbouring words. A comma, bracket, quote or line keeps two runs apart, a space, hyphen, apostrophe or full stop
 * does not ("Richard E. Grant", "Glynn-Carney"). Held as the sorted hashes, never the text: a corpus holds one per
 * listing, and a synopsis names a handful of people among hundreds of words.
 */
final class VenueNames private (pairs: Array[Int]) extends SortedHashes[VenueNames](pairs) {
  protected def holding(sorted: Array[Int]): VenueNames = if (sorted.isEmpty) VenueNames.None else new VenueNames(sorted)
  protected def unit: String = "pairs"

  /** Does the text name `name` WHOLE: two words at least, every pair of its neighbouring words held — never a surname
   *  alone, nor a first name. */
  def names(name: String): Boolean = holdsAll(VenueNames.pairsOf(name))
}

object VenueNames {
  val None: VenueNames = new VenueNames(Array.emptyIntArray)

  /** The names `texts` (a synopsis, a description) and `cast` (a cast field's entries) hold. */
  def of(texts: Iterable[String], cast: Iterable[String] = Nil): VenueNames = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    texts.foreach(text => collect(text, capitalisedOnly = true, found))
    cast.foreach(name => collect(name, capitalisedOnly = false, found))
    val all = found.result()
    if (all.isEmpty) None else new VenueNames(SortedHashes.sortedDistinct(all, all.length))
  }

  /** Names as [[VenueNames.hashes]] gave them. */
  def ofHashes(hashes: Seq[Int]): VenueNames = if (hashes.isEmpty) None else new VenueNames(SortedHashes.of(hashes))

  /** The hashed pairs of `name`'s neighbouring words, case aside: a name of one word has none. */
  private[identity] def pairsOf(name: String): Array[Int] = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    collect(name, capitalisedOnly = false, found)
    found.result()
  }

  /** A letter folded as `TextNormalization.deburr` and lower case fold it — accents off, `ł` as `l` — looked up for the
   *  Latin letters, which is every name a venue in these countries prints; any other only lower-cased. */
  private[identity] def fold(c: Char): Char = if (c < Folded.length) Folded(c) else Character.toLowerCase(c)
  private val Folded: Array[Char] = Array.tabulate(0x0250) { code =>
    val folded = tools.TextNormalization.deburr(code.toChar.toString).toLowerCase(java.util.Locale.ROOT)
    if (folded.length == 1) folded.charAt(0) else Character.toLowerCase(code.toChar)
  }

  private def pairHash(a: Int, b: Int): Int = MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(0x4e616d65, a), b), 2)

  /** Characters a run of words goes on over: a name's spaces, hyphens, apostrophes and an initial's full stop. */
  private def joins(c: Char): Boolean = Character.isWhitespace(c) && c != '\n' && c != '\r' || c == '-' || c == '\'' || c == '’' || c == '.'

  /** Every pair of neighbouring words of each run of `text`, by hash — of capitalised words only when `capitalisedOnly`. */
  private def collect(text: String, capitalisedOnly: Boolean, into: scala.collection.mutable.ArrayBuilder[Int]): Unit = {
    var previous = 0          // the previous word's hash, while it stands beside the next in one run
    var inRun    = false
    var i        = 0
    val n        = text.length
    while (i < n) {
      val c = text.charAt(i)
      if (Character.isLetterOrDigit(c)) {
        var j = i
        if (capitalisedOnly && !Character.isUpperCase(c)) {
          // prose: skipped unread, and it ends the run
          while (j < n && Character.isLetterOrDigit(text.charAt(j))) j += 1
          inRun = false
        } else {
          // the folded word's hash, as its String's would be, built char by char: no word is copied
          var h = 0
          while (j < n && Character.isLetterOrDigit(text.charAt(j))) { h = 31 * h + fold(text.charAt(j)); j += 1 }
          if (inRun) into += pairHash(previous, h)
          previous = h; inRun = true
        }
        i = j
      } else {
        if (!joins(c)) inRun = false
        i += 1
      }
    }
  }
}

/**
 * The CAST signal: the one candidate whose top-billed TMDB cast the venue's own text names two whole names of
 * ([[VenueNames]]), no other candidate's cast named at all. Measured offline 2026-10-06 over 244 labelled clusters of the
 * five corpora (the venue pages fetched, every candidate's TMDB credits): 24 decided, 24 right, 0 wrong, 11 of them
 * clusters the resolver left unmatched (PL Kino CK Lublin's "Lalka": Dorociński, Urzędowska and Kondrat name Kawalski's
 * 2026 film). ONE name is no evidence — it took 2 wrong films there: performers sing and act in several productions
 * (US "MetOpera: Medea (2022–23)" names Sondra Radvanovsky, the Naples Medea's lead too). A double bill naming each
 * film's cast names more than one candidate: nothing. A candidate whose cast is not known might be the one named:
 * nothing either.
 */
object CastEvidence {
  /** The whole names a venue's text must name of one candidate's cast. */
  val Names = 2

  /** The TIE-BREAK (experiment `cast-tiebreak`): of the candidates the billing names alike (`sameTitled`, each with its
   *  top-billed cast, `None` while not known), the one whose cast the venue's text names [[Names]] whole names of, no
   *  other doing so — a preference among namesakes, never a take of a lone candidate. A rival named by one name does not
   *  stand in its way (sequels and remakes share an actor), nor is a rival named by none held against it: only two names
   *  decide. Nothing while fewer than two candidates tie, or one's cast is not known: it might be the one named. */
  def tieBreak[K](venue: VenueNames, sameTitled: Seq[(K, Option[Seq[String]])]): Option[(K, Seq[String])] =
    if (venue.isEmpty || sameTitled.sizeIs < 2 || sameTitled.exists(_._2.isEmpty)) Option.empty
    else sameTitled.map { case (candidate, names) => candidate -> names.get.filter(venue.names).distinct }.filter(_._2.sizeIs >= Names) match {
      case Seq(only) => Some(only)
      case _         => Option.empty
    }

  /** The one candidate of `cast` (each with its top-billed cast, `None` while not known) the venue's text names [[Names]]
   *  of, with the names it names — none while another candidate's cast is named at all, or one's is not known. */
  def take[K](venue: VenueNames, cast: Seq[(K, Option[Seq[String]])]): Option[(K, Seq[String])] =
    if (venue.isEmpty || cast.isEmpty || cast.exists(_._2.isEmpty)) Option.empty
    else cast.map { case (candidate, names) => candidate -> names.get.filter(venue.names).distinct }.filter(_._2.nonEmpty) match {
      case Seq((candidate, named)) if named.sizeIs >= Names => Some(candidate -> named)
      case _                                                => Option.empty
    }
}
