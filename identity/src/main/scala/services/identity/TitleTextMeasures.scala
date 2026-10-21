package services.identity

import java.util.Locale

import services.movies.{TitleContainment, TitleSeason}

import IdentityMeasures.*

/** A title's words and comparison keys as the measures read them: the words that count, the pieces a title breaks
 *  into, its years and seasons stripped, the qualifiers a pool's records bill, and the accent- and script-folded keys and
 *  forms every title relation compares.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object TitleTextMeasures {
  /** The words two titles share that are a number, or longer than an article — no year, which dates rather than names. */
  private[identity] def sharedWords(a: String, b: String): Int = (countedWords(a).toSet intersect countedWords(b).toSet).size
  /** A title's words that count as naming something: a number, or a word longer than an article — no year. */
  private[identity] def countedWords(title: String): Seq[String] =
    TitleContainment.tokens(title).filterNot(YearWord.matches).filter(w => w.length > 3 || w.forall(_.isDigit))
  private[identity] def shareAWord(a: String, b: String): Boolean = sharedWords(a, b) > 0

  /** Is `a` a word-for-word cognate of `b` — as many words (two or more), each a spelling of the other's word in its place,
   *  at most half its letters re-spelt? A title translated between related languages: DE "Mein Opa der Bankräuber" of the
   *  Dutch "Mijn Opa de Bankrover", which shares no counted word with it. */
  private[identity] def cognateOf(a: String, b: String): Boolean = {
    val (as, bs) = (TitleContainment.tokens(a), TitleContainment.tokens(b))
    as.sizeIs >= 2 && as.size == bs.size && as.zip(bs).forall { case (x, y) =>
      tools.EditDistance.within(x, y, math.max(x.length, y.length) / 2) }
  }

  /** A title word that is a year ("1986" of "Legend (1986)"). */
  private[identity] val YearWord = """(?:18|19|20)\d\d""".r

  /** Does the title bill the print it screens — a 16, 35 or 70 mm gauge ("(35mm)", "[35mm]", "70mm", "on 35mm")? */
  private val PrintGauge = """(?i)(?<![\p{L}\d])(?:16|35|70)\s?mm(?![\p{L}\d])""".r
  def billsAPrint(title: String): Boolean = PrintGauge.findFirstIn(title).isDefined

  /** A running time in brackets, marked as minutes by a prime or an apostrophe ("97’", "97'", "97′")
   *  or by "min" — the only way a number in a title says it is a duration. One value, or none. */
  private val BracketedRuntime = """(?i)[(\[]\s*(\d{2,3})\s*(?:['’′]|min\.?|mins\.?)\s*[)\]]""".r
  def bracketedRuntime(titles: Seq[String]): Option[Int] =
    titles.iterator.flatMap(BracketedRuntime.findAllMatchIn).map(_.group(1).toInt).filter(_ > 0).toSeq.distinct match {
      case Seq(one) => Some(one)
      case _        => None
    }


  /** Which pieces of a title are its QUALIFIER rather than its work, LEARNED from how the film
   *  records of one family's pool bill each piece, and on which SIDE of the work: TMDB bills
   *  "Director's Cut" after many works ("Chocolate - Director's Cut", "The Great War: Director's
   *  Cut", "The Promise (Director's Cut)") and "The Final Cut" after Pink Floyd and Straw Dogs, as
   *  "Dark City: Director's Cut" and "Michael Mann's Manhunter: The Final Cut" do — so there they
   *  are the listings' qualifiers. A franchise LEADS its sequels ("Obcy: Przymierze", "Obcy:
   *  Romulus"), so under a banner ("Tani wtorek: Obcy") it is still the work. `companions`: for
   *  each piece, by its yearless key and whether it trails, how many distinct other pieces a record
   *  title bills it beside ([[Qualifiers.split]]). No edition, cut or banner word is known in
   *  advance. */
  final case class Qualifiers(companions: Map[(String, Boolean), Int], works: Set[Seq[String]] = Set.empty) {
    private def count(piece: Seq[String], trails: Boolean): Int = companions.getOrElse((piece.mkString, trails), 0)
    /** The listing's qualifier pieces, by key: each piece its whole title adds another to along one
     *  edge, which records bill on the same side beside at least two works — one is a coincidence,
     *  as a house's is ([[StageRelays.Houses.learn]]) — and beside more than they bill the rest of the title on
     *  its side. A tie is no qualifier, and neither is a piece the listing publishes as its original
     *  title: the venue names it as the film ("Cineworld 30: The Dark Knight", originally "The Dark
     *  Knight", though TMDB also bills "Enter the World of Hans Zimmer: The Dark Knight"). Nor is a
     *  LEADING piece some record is titled whole when the rest carries no record's whole title: a
     *  work leads its sequels, so "Dracula (4K Restoration)" is Dracula however many TMDB bills after
     *  "Dracula" — nor a TRAILING one after a learned venue decoration, for the same reason: "KINO SENIORA |
     *  Primetime" is Primetime however many records bill "Primetime" after a work. Beside a work the piece stays a cut or an edition
     *  ("Dark City: Director's Cut"), whatever record carries it alone. */
    def of(l: Listing): Set[String] = memo.getOrElseUpdate(l.titleInputs, {
      val named = l.originalTitle.map(yearlessTokens(_).mkString)
      // A trailing piece only when what leads it is a learned venue decoration ("KINO SENIORA"): a rest
      // merely missing from these records may still be a work ("Dark City" of "Dark City: Director's Cut").
      lazy val undecorated = l.ownTitles.flatMap(l.decorations.strip).map(yearlessTokens).toSet
      def leavesNoWork(piece: Seq[String], rest: Seq[String], trails: Boolean) =
        works(piece) && !works.exists(work => rest.containsSlice(work)) && (!trails || undecorated(piece))
      l.ownTitles.flatMap(t => Qualifiers.split(t, l.shapesOfOwn(t))).collect {
        case (piece, rest, trails) if count(piece, trails) >= 2 && count(piece, trails) > count(rest, !trails) && !named.contains(piece.mkString) &&
            !leavesNoWork(piece, rest, trails) =>
          piece.mkString
      }.toSet
    })
    // By the titles it reads, hashed once per listing: a listing's own hash walks its decorations' whole learned set.
    private val memo = scala.collection.concurrent.TrieMap.empty[FamilyScope.TitleInputs, Set[String]]
  }
  object Qualifiers {
    val Unknown: Qualifiers = Qualifiers(Map.empty)

    /** A title's pieces, each with the rest of the title and whether it TRAILS the rest: every
     *  delimited piece of it (its [[shapes]]) that is a token run along one of its edges, and the rest,
     *  both ways round, as yearless tokens ("Dark City: Director's Cut" → `director s cut` trailing
     *  `dark city`, and `dark city` leading `director s cut`). */
    def split(title: String, titleShapes: Seq[String]): Seq[(Seq[String], Seq[String], Boolean)] = {
      val whole = yearlessTokens(title)
      titleShapes.map(yearlessTokens).filter(p => TitleContainment.isTokenRun(p, whole)).flatMap { p =>
        val trails = !whole.startsWith(p)
        val rest   = if (trails) whole.dropRight(p.length) else whole.drop(p.length)
        Seq((p, rest, trails), (rest, p, !trails))
      }.distinct
    }

    /** Learn from the candidate records one family's listings searched up, by their titles and
     *  original titles. */
    def learn(records: Seq[Film]): Qualifiers = {
      val titles = records.flatMap(_.ownTitles).distinct
      Qualifiers(records.flatMap(_.ownSplits).map { case (p, r, trails) => ((p.mkString, trails), r.mkString) }.distinct.groupMapReduce(_._1)(_ => 1)(_ + _),
        titles.map(yearlessTokens).filter(_.nonEmpty).toSet)
    }
  }

  /** Is `edition` a record of `work` under a qualifier: a later record one of whose own titles
   *  carries one of the work's as a delimited piece or a token run along one edge ("Radiohead X
   *  Nosferatu: A Symphony of Horror" of Murnau's "Nosferatu", whose record carries "Nosferatu: A
   *  Symphony of Horror")? A record with a title of the work's own is a namesake or a translation
   *  ("Die Puppe", 1975, originally "The Doll", is not an edition of Has's "Lalka", which TMDB
   *  also calls "The Doll"). Each title is read alone: an original title is a whole title, never
   *  a piece. */
  def editionOf(edition: Film, work: Film): Boolean = {
    val relations = edition.ownTitles.distinct.map(t => titleRelation(Listing(t), work).value)
    edition.year.exists(y => work.year.exists(_ <= y)) &&
      relations.exists(Set("segment", "decorated")) && !relations.exists(Rivalling)
  }

  /** The fewest letters a word needs to name something by itself, whole or as an inflected stem: "Kadr" of "Kino w
   *  Kadrze" (the venue's name), a director's surname a billing names ([[Film.directorSurnames]]: "Lang" of "Fritz Lang:
   *  M"), a word of a title billed in another language. */
  val StemLetters = services.billing.NumberedTitles.StemLetters

  /** Where a title's own delimiters cut it into pieces: a colon or pipe, or a dash or slash spaced on both sides. */
  private[identity] val PieceBreak = java.util.regex.Pattern.compile("""\s*[:|]\s*|\s+[-–—/]\s+""")
  /** A title's delimited pieces, its seasons and bracketed years out. */
  private[identity] def pieces(title: String): Seq[String] = PieceBreak.split(withoutYears(title)).toSeq.map(_.trim).filter(_.nonEmpty)

  /** A year in brackets ("(2026)"): a screening's or a production's date, as a season is. */
  private[identity] val BracketedYear = services.billing.NumberedTitles.BracketedYear
  /** `t` without its seasons and bracketed years: how a house bills a work, whatever it dates it by. */
  def withoutYears(t: String): String = services.billing.BillingParse(t).yearless
  private[identity] def yearlessTokens(t: String): Seq[String] = TitleContainment.tokens(withoutYears(t))

  /** `t` with every season removed, so a season's end year is never read as a bracketed year. */
  def withoutSeasons(t: String): String = TitleSeason.without(t)

  // ── keys ─────────────────────────────────────────────────────────────────────────────


  /** A title or name as a comparison key: accents folded, lowercased, every non-letter and
   *  non-digit dropped. Script-preserving, rule-free: no title-specific canonicalisation. */
  def key(s: String): String =
    tools.TextNormalization.lettersAndDigitsOnly(tools.TextNormalization.deburr(withoutPossessives(s)).toLowerCase(Locale.ROOT))

  /** A possessive "'s" dropped: venues write "Andre Rieu 2026 Christmas Concert" for TMDB's "Andre
   *  Rieu's …" (UK Odeon ×76 read it as a different title and vetoed the concert). Only after an
   *  apostrophe, straight or curly: "Schindlers" stays another spelling. */
  private[identity] def withoutPossessives(s: String): String =
    // Most titles carry no apostrophe, and the pattern needs one: they skip the regex (every key and word split asks).
    if (s.indexOf('\'') < 0 && s.indexOf('\u2019') < 0) s else Possessive.matcher(s).replaceAll("")
  private val Possessive = java.util.regex.Pattern.compile("(?<=\\p{L})['\u2019][sS]\\b")

  /** [[key]] of the title spelt in Latin letters: Cyrillic transliterated letter by letter, as
   *  Polish venues list Ukrainian films ("Potyag Chervona ruta" for "Потяг «Червона Рута»"). Only an
   *  EQUAL transliteration names a title: one letter-by-letter scheme, no fuzzing. */
  def latinKey(s: String): String = {
    val lower = s.toLowerCase(Locale.ROOT)
    if (!lower.exists(Cyrillic.contains)) key(s)
    else key(lower.flatMap(c => Cyrillic.getOrElse(c, c.toString)))
  }
  private val Cyrillic: Map[Char, String] = Map(
    'а' -> "a", 'б' -> "b", 'в' -> "v", 'г' -> "g", 'ґ' -> "g", 'д' -> "d", 'е' -> "e", 'є' -> "ye", 'ё' -> "yo",
    'ж' -> "zh", 'з' -> "z", 'и' -> "y", 'і' -> "i", 'ї' -> "yi", 'й' -> "y", 'к' -> "k", 'л' -> "l", 'м' -> "m",
    'н' -> "n", 'о' -> "o", 'п' -> "p", 'р' -> "r", 'с' -> "s", 'т' -> "t", 'у' -> "u", 'ф' -> "f", 'х' -> "kh",
    'ц' -> "ts", 'ч' -> "ch", 'ш' -> "sh", 'щ' -> "shch", 'ъ' -> "", 'ы' -> "y", 'ь' -> "", 'э' -> "e", 'ю' -> "yu",
    'я' -> "ya")

  /** One title's comparison forms, each computed on first use: the same titles are compared across
   *  a whole family pool, and normalising them again per pair was nearly all of a resolve's time. */
  private[identity] final case class TitleForm(text: String) {
    lazy val key: String            = IdentityMeasures.key(text)
    lazy val words: Seq[String]     = IdentityMeasures.words(text)
    lazy val wordSet: Set[String]   = words.toSet
    lazy val yearlessWords: Seq[String] = yearlessTokens(text)
    lazy val yearless: String       = yearlessWords.mkString
    lazy val latinKey: String       = IdentityMeasures.latinKey(text)
    /** Its words after a leading article ([[LeadingArticles]]), when it has one — what a venue that drops the article lists. */
    lazy val afterArticle: Option[Seq[String]] = Some(withoutLeadingArticle(words)).filter(_.sizeIs < words.size)
    /** The title before a trailing bracketed gloss, keyed — "TKT (T'inquiète)" is "TKT" too. */
    lazy val unglossedKey: Option[String] =
      TrailingGloss.findFirstMatchIn(text).map(m => IdentityMeasures.key(m.group(1))).filter(k => k.nonEmpty && k != key)
  }
  private val TrailingGloss = """^(.*\S)\s*\([^()]*\)\s*$""".r
  /** Articles a title may lead with that a venue drops — English, German, French, Spanish, Italian: DE "Camp der
   *  Verlorenen" bills TMDB's "Das Camp der Verlorenen". ONE list for every rule that reads a title without its article:
   *  the measures' article-less exact title ([[TitleForm.afterArticle]]) and the agreement's naming
   *  ([[services.identity.agreement.Agreement]]). */
  val LeadingArticles: Set[String] = services.movies.TitleText.Articles
  /** A title's words without the article it leads with ([[LeadingArticles]]) — when a word is left beside it. */
  def withoutLeadingArticle(words: Seq[String]): Seq[String] =
    if (words.sizeIs >= 2 && LeadingArticles(words.head)) words.tail else words

  private[identity] def words(s: String): Seq[String] = TitleContainment.tokens(withoutPossessives(s))
}
