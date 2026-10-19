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
 * The CAST signal, a tie-break between namesakes: of the candidates a billing names alike, the one whose top-billed TMDB
 * cast the venue's own text names two whole names of ([[VenueNames]]), no other's twice. Measured offline 2026-10-06 over 244 labelled clusters of the
 * five corpora (the venue pages fetched, every candidate's TMDB credits): 24 decided, 24 right, 0 wrong, 11 of them
 * clusters the resolver left unmatched (PL Kino CK Lublin's "Lalka": Dorociński, Urzędowska and Kondrat name Kawalski's
 * 2026 film). ONE name is no evidence — it took 2 wrong films there: performers sing and act in several productions
 * (US "MetOpera: Medea (2022–23)" names Sondra Radvanovsky, the Naples Medea's lead too). A double bill naming each
 * film's cast names more than one candidate: nothing. A candidate whose cast is not known might be the one named:
 * nothing either. Narrowed to the tie-break 2026-10-19 (docs/identity-experiments.md, `cast-tiebreak`): once the
 * captures held casts, the wider rule took a tribute named after a film ("Straight Talk: A Tribute to Dolly Parton") as
 * the film; the tie-break took 6 UK no-director ties right, none wrong.
 */
object CastEvidence {
  /** The whole names a venue's text must name of one candidate's cast. */
  val Names = 2

  /** The TIE-BREAK: of the candidates the billing names alike (`sameTitled`, each with its
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

  /** The MISMATCH: why the venues' own cast FIELDS rule `film` out, if they do — each
   *  listing listing a cast ([[ListedCast]]) names none of the film's top-billed cast ([[FilmCast]]), and no listing states
   *  a fact of its own that agrees with the film (a year within one, a director the film credits): then the cast is
   *  another film's. Read only where the cast is the film's: never beside a stage work, a relay, a performance (a
   *  tribute's cast field lists its performers) or a bill of several works, only for a film of the venues' own language
   *  (`language`) — a dub's cast field lists its voices, a foreign film's its transliterations — and only for a film whose
   *  genres are known and say its cast is on screen ([[onScreenCast]]). Measured 2026-10-19 (recording 38713274617): ES
   *  "La maleta" ×14 cured (Objetos 2022 → Therón's 2026 film), 9 right gained, nothing else moved. */
  def mismatch(listings: Seq[Listing], film: IdentityMeasures.Film, language: Option[String], castIsTheFilms: => Boolean): Option[String] = {
    val listing = listings.filter(_.listedCast.nonEmpty)
    if (listing.isEmpty || film.cast.isEmpty || language.isEmpty || film.originalLanguage != language || !onScreenCast(film) || !castIsTheFilms) None
    else {
      def ownFact(l: Listing) = l.year.exists(y => film.year.exists(f => math.abs(f - y) <= 1)) ||
        (l.directors.nonEmpty && FactRelations.samePerson(l.directors, film.directors.getOrElse(Nil)))
      Option.when(listing.forall(l => film.cast.get.namedBy(l.listedCast) == 0) && !listings.exists(ownFact))(
        "its venue's own cast field names none of its top-billed cast, and no fact the venue states agrees with it")
    }
  }

  /** TMDB's genres whose cast is no on-screen cast of the film: Animation (a dub's voices), Documentary (whoever appears). */
  val VoiceOrContributorGenres: Seq[Int] = Seq(16, 99)

  /** Is the film's TMDB cast the people a venue's cast field would list — its genres KNOWN (a record filed before records
   *  kept them reads as unknown, and the mismatch stands down) and none of [[VoiceOrContributorGenres]]? */
  def onScreenCast(film: IdentityMeasures.Film): Boolean = film.genreBits != 0 && !VoiceOrContributorGenres.exists(film.hasGenre)

  /** Is what the listings' cast fields list the film's cast — none billing a stage work, a relay, a performance or
   *  several works ([[mismatch]])? */
  def castIsTheFilms(listings: Seq[Listing]): Boolean = !listings.exists { l =>
    ListingShape.stagesAWork(l) || ListingShape.relays(l, l.rawTitle.toLowerCase(java.util.Locale.ROOT)) || ListingShape.billsSeveral(l) ||
      services.cinemas.pl.NonMovieEventClassifier.isPerformance(l.rawTitle)
  }
}

/**
 * A venue's own cast FIELD — never its synopsis — as the cast MISMATCH reads it: each entry's
 * neighbouring words as UNORDERED pairs, folded as [[VenueNames]] folds them, so a name given family name first ("Gong
 * Li", "Li Gong") names the same person. Held only where the field is evidence about the film at all: two entries of two
 * words or more, and no entry of one word — a mononymous star ("Rajinikanth"), a puppet ("Animal") or a role in brackets
 * ("Paul Marque (Romeo)") no pair can name. A part between commas, semicolons, slashes or brackets is an entry.
 */
final class ListedCast private (pairs: Array[Int]) extends SortedHashes[ListedCast](pairs) {
  protected def holding(sorted: Array[Int]): ListedCast = if (sorted.isEmpty) ListedCast.None else new ListedCast(sorted)
  protected def unit: String = "pairs"

  /** Does the field name a name given as its unordered pairs ([[FilmCast]]'s)? */
  def namesPairs(pairs: Array[Int]): Boolean = holdsAll(pairs)
}

object ListedCast {
  val None: ListedCast = new ListedCast(Array.emptyIntArray)
  /** The names a field must list for its silence about a film's cast to count. */
  val Names = 2

  /** The field `cast` lists, where it is evidence ([[ListedCast]]); none otherwise. Read char by char, as [[VenueNames]]
   *  is: a corpus reads one per listing. */
  def of(cast: Seq[String]): ListedCast = {
    var found   = new Array[Int](16)
    var count   = 0
    var names   = 0
    var oneWord = false
    val it = cast.iterator
    while (it.hasNext && !oneWord) {
      val text = it.next()
      var words = 0; var previous = 0
      var i = 0
      val n = text.length
      while (i <= n && !oneWord) {
        val c = if (i < n) text.charAt(i) else '\n'
        if (Character.isLetterOrDigit(c)) {
          var h = 0
          while (i < n && Character.isLetterOrDigit(text.charAt(i))) { h = 31 * h + VenueNames.fold(text.charAt(i)); i += 1 }
          if (words > 0) {
            if (count == found.length) found = java.util.Arrays.copyOf(found, count * 2)
            found(count) = pair(previous, h); count += 1
          }
          previous = h; words += 1
        } else {
          if (EntryEnds.indexOf(c) >= 0) {
            if (words == 1) oneWord = true else if (words >= 2) names += 1
            words = 0
          }
          i += 1
        }
      }
    }
    if (oneWord || names < Names) None else new ListedCast(SortedHashes.sortedDistinct(found, count))
  }
  private val EntryEnds = ",;/|()[]\r\n"

  /** The folded words of `name`, each as its hash. */
  private[identity] def wordsOf(name: String): Array[Int] = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    var i = 0
    val n = name.length
    while (i < n) {
      if (Character.isLetterOrDigit(name.charAt(i))) {
        var h = 0
        while (i < n && Character.isLetterOrDigit(name.charAt(i))) { h = 31 * h + VenueNames.fold(name.charAt(i)); i += 1 }
        found += h
      } else i += 1
    }
    found.result()
  }

  /** The unordered pairs of neighbouring words, by hash. */
  private[identity] def pairs(words: Array[Int]): Array[Int] = words.indices.drop(1).map(i => pair(words(i - 1), words(i))).toArray

  /** One unordered pair of words, by hash: either order hashes alike. */
  private def pair(a: Int, b: Int): Int =
    MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(0x4c697374, math.min(a, b)), math.max(a, b)), 2)

  /** `name`'s unordered pairs: none for a name of one word. */
  private[identity] def pairsOf(name: String): Array[Int] = pairs(wordsOf(name)).distinct.sorted
}

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
