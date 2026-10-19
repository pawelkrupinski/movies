package services.identity

import services.movies.TitleContainment
import services.resolution.SearchTitles

import IdentityMeasures.*

/** The parts a title numbers, its capitalised tags, and the SHAPES it is billed in — the titles a listing's billing
 *  can stand for, delimited, quoted, or with its screening year dropped.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object TitleShapeMeasures {
  /** A part a title numbers and the subtitle it gives that part, after a comma, a colon or a dash — or an episode and
   *  the name it gives it, numbered or not: DE "DAS INDISCHE GRABMAL (TEIL 1, DIE SENDUNG DES YOGHI)", UK "De Gaulle Part
   *  Two: Liberté", DE "LOVE ISLAND - EPISODE NATURE", US "Star Wars: Episode VII - The Force Awakens". */
  private val PartWord    = """(?:teil|part|część|parte|partie|kapitel|chapter)"""
  private val PartNumber  = """(\d+|[ivx]+|one|two|three|eins|zwei|drei)\b"""
  private val EpisodeWord = """(?:episode|épisode|episodio|folge|odcinek)"""
  private val TitledPart =
    (s"""(?iU)\\b(?:$PartWord\\s*$PartNumber\\s*[,:–—-]""" +
      s"""|$EpisodeWord\\b\\s*(?:(\\d+|[ivx]+)\\b)?\\s*[,:–—-]?)\\s*([^()\\[\\]|]+)""").r
  /** A part a title numbers by name — "Dune: Part Three", "Teil 2", "Episode VII" — not a bare number beside it. */
  private val NamedPart = s"""(?iU)\\b(?:$PartWord|$EpisodeWord)\\s*$PartNumber""".r
  /** The parts the listing's own titles number and subtitle ([[TitledPart]]): each one's number, if it has one, and its
   *  subtitle's words of four letters or more. */
  private[identity] def titledParts(titles: Seq[String]): Seq[(Option[String], Seq[String])] =
    titles.flatMap(t => TitledPart.findAllMatchIn(t).map { m =>
      (Option(m.group(1)).orElse(Option(m.group(2))).flatMap(TitleContainment.tokens(_).headOption).map(partNumber),
        TitleContainment.tokens(m.group(3)).filter(_.length >= StemLetters))
    }).filter(_._2.nonEmpty).distinct

  /** A part's number however it is spelled — "2", "II", "two", "zwei" — so "Dune: Part 2 - …" is "Dune: Part Two". */
  private def partNumber(token: String): String = PartNumbers.getOrElse(token, token)
  private val PartNumbers: Map[String, String] = Map(
    "i" -> "1", "one" -> "1", "eins" -> "1", "ii" -> "2", "two" -> "2", "zwei" -> "2", "iii" -> "3", "three" -> "3",
    "drei" -> "3", "iv" -> "4", "v" -> "5", "vi" -> "6", "vii" -> "7", "viii" -> "8", "ix" -> "9", "x" -> "10")

  /** Does the listing's title give the part it numbers a subtitle ([[titledParts]]) not one word of which is a word
   *  of the film's titles? Then it is another film's part: DE Union Filmtheater Friedrichshagen's "DAS INDISCHE GRABMAL
   *  (TEIL 1, DIE SENDUNG DES YOGHI)" is Joe May's 1921 first part, not Lang's 1959 "Das indische Grabmal", and DE Acud
   *  Kino's "LOVE ISLAND - EPISODE NATURE" an episode of a series, not the 1952 "Love Island". A part
   *  numbered with no subtitle ("SHOAH (TEIL I)"), titled as the film is ("De Gaulle Part Two: Liberté") or numbered
   *  as the film's titles name a part ("Dune: Part Three - ERLY FAN PREVIEW") is not — a bare number among them is no
   *  part named: Lang's 1959 film also goes by "2 - Das indische Grabmal", and is no "TEIL 2, DER TIGER VON ESCHNAPUR". */
  def titlesAnotherPart(l: Listing, f: Film): Boolean = {
    val parts = titledParts(l.ownTitles)
    // a film its titles name as that part is that part, whatever note follows it: "Dune: Part Three - ERLY FAN PREVIEW"
    lazy val numbered = f.titles.flatMap(t => NamedPart.findAllMatchIn(t).flatMap(m => TitleContainment.tokens(m.group(1)).headOption.map(partNumber))).toSet
    parts.nonEmpty && parts.forall { case (number, words) => !number.exists(numbered) && !words.exists(f.titleWords) }
  }

  /** Is a listing's credited "director" the film's HOUSE — a name two words or more long that runs inside the film's
   *  own title, and none of the people the film credits? UK venues credit "The Metropolitan Opera" for its 2026/27
   *  "The Metropolitan Opera: Così fan tutte" ×97, which read as a different director than Phelim McDermott and
   *  vetoed the film. "Guillermo del Toro" of "Guillermo del Toro's Pinocchio" directed it, and stays a person. */
  private[identity] def namesItsHouse(name: String, words: Seq[String], f: Film): Boolean =
    words.sizeIs >= 2 && f.ownTitleTokens.exists(_.containsSlice(words)) &&
      !f.directorCredits.exists(CreditRelation.onePerson(new CreditRelation.Credits(Seq(name)), _))

  /** Does the listing credit one of the film's directors ([[CreditRelation.onePerson]])? */
  private[identity] def creditsItsDirector(l: Listing, f: Film): Boolean = f.directorCredits.exists(CreditRelation.onePerson(l.directorCredits, _))

  /** The pieces of a listing's title AS BILLED ([[shapes]]) that are written in capitals where the title is not — the
   *  case change a venue's programme tag makes beside a work's own spelling ("Alim | UFF", "DKF: Lalka"). None in a title
   *  billed all in capitals ("BEZ KOŃCA 2D PL LOLO"): its case says nothing. */
  def capitalisedTags(l: Listing): Seq[String] = {
    val billed = l.rawTitle.getOrElse(l.title)
    if (!billed.exists(_.isLower)) Nil
    else l.shapesOfOwn(billed).filter(piece => piece != billed.trim && piece.exists(_.isLetter) && !piece.exists(_.isLower))
  }

  private[identity] def shapesOf(l: Listing): Seq[String] = {
    shapes(l.ownTitles ++ l.searchTitles ++ SearchTitles.candidates(l.title, l.originalTitle) ++
      l.rawTitle.toSeq.flatMap(SearchTitles.candidates(_, None)) ++ l.ownTitles.flatMap(creditedWork), l.decorations)
  }

  /** `titles` and every part a split leaves, each de-decorated in turn ("Throwback: Donnie Darko
   *  (25th Anniversary)" → "Donnie Darko (25th Anniversary)" → "Donnie Darko"; "(4DX Rewind) Shrek"
   *  → "Shrek" by a learned `decorations` run) and cut before a bracketed year ("Człowiek z żelaza
   *  (1981) 4K" → "Człowiek z żelaza"), to a fixpoint: every shape still a whole delimited or
   *  decorated piece of one of the titles. */
  private[identity] def shapes(titles: Seq[String], decorations: TitleDecorations = TitleDecorations.None): Seq[String] = {
    // A worklist, each shape split once: what one round adds is what its NEW shapes split into — the older shapes'
    // pieces are all held already — in the order a round over every shape would add them (by splitter, then shape).
    val held     = scala.collection.mutable.LinkedHashSet.empty[String]
    // a box office's certificate is no part of any shape: searched whole, "Obsession (18)" finds "80 Day Obsession: Day 18"
    var frontier = titles.map(SearchTitles.uncertified).filter(_.nonEmpty).distinct
    // a title quoting two works bills both: none of its spellings is cut to one quoted title
    val quotesOne = !frontier.exists(title => Quoted.findAllMatchIn(title).size >= 2)
    held ++= frontier
    while (frontier.nonEmpty) {
      // a learned decoration never cuts into an author's credit: its name is no film's title ("… by Noël Coward")
      frontier = (frontier.flatMap(SearchTitles.candidates(_, None)) ++ frontier.flatMap(t => decorations.strip(creditedWork(t).getOrElse(t))) ++ frontier.flatMap(beforeItsYear) ++
        frontier.flatMap(delimitedPieces(_, quotesOne))).map(_.trim).filter(shape => shape.nonEmpty && !held(shape)).distinct
      held ++= frontier
    }
    held.toSeq
  }

  /** The pieces more banner separators leave, beside the ones `SearchTitles` splits: a slash with a
   *  space on either side ("MISTYCZKA /film polski/", "Róża / Spotkanie Filozoficzne"), a dash with a
   *  space on one side ("Fregata dla seniorów- 500 Mil"), a code of up to three letters before a colon
   *  with no space ("MS:HOT SPOT"), a square-bracketed format and a title
   *  quoted at the head of its billing (`quotesOne`: none where a title quotes two), or at its tail after a banner's
   *  stop ('Kino Kobiet - KLAPS! "Jak żyć żeby nie zwariować"'); what an upper-case banner of two words or more runs
   *  into with an unspaced colon ("KINO NA NIEDZIELE:CAMINO DLA OPORNYCH"). A slash or colon inside a word is the
   *  title's own ("Face/Off", "AC/DC", "Star Wars:Episode I"); so is a sentence's stop: "Szkoła magicznych zwierząt.
   *  Tajemnica szkolnego podwórka" is the sequel, and its head the first film (whole-corpus dump, 2026-10-05: the
   *  head cut switched it and lost "Avengers: Koniec Gry. Dogrywka"). Here, not in `SearchTitles`, which the old
   *  pipeline also reads. */
  private def delimitedPieces(title: String, quotesOne: Boolean): Seq[String] = {
    val slashed = Seq(SpacedSlash, HalfSpacedDash).flatMap(separator =>
      if (separator.findFirstIn(title).isDefined) separator.split(title).toSeq.map(_.trim) else Nil)
    val coded   = CodeBeforeColon.findFirstMatchIn(title).map(m => title.substring(m.end)).toSeq
    val undated = (if (endsInScreeningYear(title)) ScreeningYearSuffix.findFirstMatchIn(title).map(_.group(1)) else None).toSeq
    val format  = (if (endsIn(title, _ == ']')) TrailingSquareBracket.findFirstMatchIn(title).map(_.group(1)) else None).toSeq
    val quoted  = (if (quotesOne && Quoted.findAllMatchIn(title).size == 1)
      LeadingQuoted.findFirstMatchIn(title).orElse(TrailingQuoted.findFirstMatchIn(title)).map(_.group(1)) else None).toSeq
    val banner  = BannerBeforeColon.findFirstMatchIn(title).map(m => title.substring(m.end)).toSeq
    (slashed ++ coded ++ undated ++ format ++ quoted ++ banner).map(_.trim).filter(_.exists(_.isLetter)).filter(_ != title.trim)
  }
  /** Whether a title can match a pattern anchored at its end, read off its last characters before the pattern scans it
   *  from the start: anchored at the end, a scan backtracks over every title that does not end so (11.8% of a
   *  convergence leg's single-threaded take-up, JFR run 38553692147). A `$` also matches before a final line break, so a
   *  title ending in one is always scanned. */
  private def endsIn(title: String, last: Char => Boolean): Boolean =
    title.nonEmpty && (last(title.last) || LineBreaks(title.last))
  private def endsInScreeningYear(title: String): Boolean = {
    def digit(c: Char) = c >= '0' && c <= '9'
    val n = title.length
    (n >= 5 && title(n - 4) == '2' && title(n - 3) == '0' && (title(n - 2) == '2' || title(n - 2) == '3') &&
      digit(title(n - 1))) || endsIn(title, _ => false)
  }
  private val LineBreaks = Set('\n', '\r', '\u0085', '\u2028', '\u2029')
  /** A format or version tag in square brackets after the title ("Vivaldi i ja [2D LEKTOR]"); a round bracket is
   *  `SearchTitles`'s. */
  private val TrailingSquareBracket = """^(.*\p{L}.*?)\s*\[[^\]\[]*\]$""".r
  /** A title quoted at the head of a programme's billing ("„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe"),
   *  the title's only quotes: two quoted titles bill two works, and a quote further in names what a talk, a concert or a
   *  show is about ('Wojciech Cejrowski i „Prawo Dżungli”', 'ZADUSZKI JAZZOWE GABA JANUSZ "NIEOBECNI"'). */
  private val Quoted        = """[„"“«][^"”„“«»]*["”“»]""".r
  /** A quoted title of words, wherever it stands: its words the first group. */
  private[identity] val QuotedTitle = """[„"“«]([^"”„“«»]*\p{L}[^"”„“«»]*)["”“»]""".r
  private val LeadingQuoted = ("""^\s*""" + QuotedTitle.regex).r
  private val SpacedSlash     = """\s+/\s*|\s*/\s+""".r
  /** A dash with a space on ONE side ("Fregata dla seniorów- 500 Mil"); both sides is `SearchTitles`'s. */
  private val HalfSpacedDash  = """(?<=\S)[-–—]\s+|\s+[-–—](?=\S)""".r
  private val CodeBeforeColon = """^\p{L}{1,3}:(?=\S)""".r
  private val TrailingQuoted  = """[:!\-–—]\s*[„"“«]([^"”„“«»]*\p{L}[^"”„“«»]*)["”“»]\s*$""".r
  private val BannerBeforeColon = """^[\p{Lu}\d]+(?:\s+[\p{Lu}\d]+)+:(?=\p{L})""".r
  /** A screening year after a title, unbracketed ("Ma to sens 2026"): 2020–2039 only, so a title that IS a
   *  number ("2046", "Blade Runner 2049") keeps it. A shape, never the old pipeline's lookup query. */
  private val ScreeningYearSuffix = """^(.*\p{L}.*?)\s+20[23]\d$""".r

  /** Two titles that are the same words but for ONE, a letter apart — a venue's typo ("The Beast of
   *  Mossy Botton", "Pradhama Drishtiya Kuttakkar") — where both spellings of that word run to five
   *  letters and neither is a number, in a title of two words or more: a sequel's numeral ("Scary Movie 3", "Mission: Impossible II")
   *  or a short word ("Hunt"/"Hurt") is a different title, not a typo. */
  private[identity] def oneTypoApart(a: Seq[String], b: Seq[String]): Boolean =
    // Two words at least: a one-word title a letter from another is another film ("Lalka"/"Lalkar").
    a.size >= 2 && a.size == b.size && a != b && {
      // The one index the two differ at, or -1 for more than one: counted in place, not as a vector of them per pair.
      val differing = { var at = -1; var i = 0; var n = 0
        while (i < a.size && n < 2) { if (a(i) != b(i)) { at = i; n += 1 }; i += 1 }
        if (n == 1) at else -1 }
      differing >= 0 && {
        val (x, y) = (a(differing), b(differing))
        x.length >= 5 && y.length >= 5 && !Seq(x, y).exists(w => w.exists(_.isDigit) || RomanNumeral.pattern.matcher(w).matches()) &&
          tools.EditDistance.within(x, y, 1)
      }
    }
}
