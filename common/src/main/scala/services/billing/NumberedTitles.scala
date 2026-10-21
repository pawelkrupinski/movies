package services.billing

import services.movies.{TitleContainment, TitleSeason, TitleWords}

/** The grammar of the dates and numbers a billing carries: the seasons and bracketed years it dates a work by, the
 *  instalment its title numbers, the subtitle it gives that instalment and the parts it titles. Read through
 *  [[BillingParse]] ([[BillingParse.yearless]], [[BillingParse.instalment]], [[BillingParse.subtitle]],
 *  [[BillingParse.titledParts]]); the identity measures compare what it finds against a film's titles. */
object NumberedTitles {

  /** The fewest letters a word needs to name something by itself, whole or as an inflected stem. */
  val StemLetters = 4

  /** A year in brackets ("(2026)"): a screening's or a production's date, as a season is. */
  val BracketedYear = """[(\[]\s*(?:18|19|20)\d{2}\s*[)\]]""".r
  /** `t` without its seasons and bracketed years: how a house bills a work, whatever it dates it by. */
  private[billing] def withoutYears(t: String): String =
    if (!TitleSeason.hasAsciiDigit(t)) t else BracketedYear.replaceAllIn(TitleSeason.without(t), " ")

  /** A Roman numeral of the size a series numbers its instalments by (I to XXXIX), by its grammar, never a list of
   *  values: tens, then units. The larger letters (L, C, D, M) spell words far more often than instalments ("M", "Mix",
   *  "DC"). */
  val RomanNumeral = "(?=[xvi])(x{0,3})(ix|iv|v?i{0,3})".r
  private val RomanDigit = Map('i' -> 1, 'v' -> 5, 'x' -> 10)
  /** The value of a folded Roman numeral ([[RomanNumeral]]), if `t` is one. */
  def romanValue(t: String): Option[Int] =
    Option.when(RomanNumeral.matches(t))(t.map(RomanDigit).foldRight((0, 0)) { case (v, (sum, max)) =>
      if (v < max) (sum - v, max) else (sum + v, v) }._1)
  /** A title word that is a Roman numeral ([[RomanNumeral]]) of two letters or more — "i" is the Polish "and". */
  def numbersAnInstalment(word: String): Boolean = word.length > 1 && RomanNumeral.pattern.matcher(word).matches()

  /** A part a title numbers and the subtitle it gives that part, after a comma, a colon or a dash — or an episode and
   *  the name it gives it, numbered or not: DE "DAS INDISCHE GRABMAL (TEIL 1, DIE SENDUNG DES YOGHI)", UK "De Gaulle Part
   *  Two: Liberté", DE "LOVE ISLAND - EPISODE NATURE", US "Star Wars: Episode VII - The Force Awakens". */
  val PartWord    = """(?:teil|part|część|parte|partie|kapitel|chapter)"""
  val PartNumber  = """(\d+|[ivx]+|one|two|three|eins|zwei|drei)\b"""
  val EpisodeWord = """(?:episode|épisode|episodio|folge|odcinek)"""
  private val TitledPart =
    (s"""(?iU)\\b(?:$PartWord\\s*$PartNumber\\s*[,:–—-]""" +
      s"""|$EpisodeWord\\b\\s*(?:(\\d+|[ivx]+)\\b)?\\s*[,:–—-]?)\\s*([^()\\[\\]|]+)""").r
  /** The parts `title` numbers and subtitles ([[TitledPart]]): each one's number, if it has one, and its subtitle's
   *  words of four letters or more. */
  private[billing] def titledParts(title: String): Seq[(Option[String], Seq[String])] =
    TitledPart.findAllMatchIn(title).map { m =>
      (Option(m.group(1)).orElse(Option(m.group(2))).flatMap(TitleContainment.tokens(_).headOption).map(partNumber),
        TitleContainment.tokens(m.group(3)).filter(_.length >= StemLetters))
    }.toSeq

  /** A part's number however it is spelled — "2", "II", "two", "zwei" — so "Dune: Part 2 - …" is "Dune: Part Two". */
  def partNumber(token: String): String = PartNumbers.getOrElse(token, token)
  private val PartNumbers: Map[String, String] = Map(
    "i" -> "1", "one" -> "1", "eins" -> "1", "ii" -> "2", "two" -> "2", "zwei" -> "2", "iii" -> "3", "three" -> "3",
    "drei" -> "3", "iv" -> "4", "v" -> "5", "vi" -> "6", "vii" -> "7", "viii" -> "8", "ix" -> "9", "x" -> "10")

  /** A break between a title's series and the subtitle it gives an instalment: "The Lord of the Rings: The Two Towers",
   *  "Terminator 2 - Judgment Day", "Der Herr der Ringe – Die zwei Türme". */
  private val SubtitleBreak = java.util.regex.Pattern.compile("""\s*(?::\s|\s[–—-]\s)\s*""")
  /** The words of a subtitle that tag a VERSION of the film, never another instalment, wherever the closing tag the parse
   *  reads off first ([[VersionTags.versionless]]) leaves them — the version, edition and anniversary vocabulary of every
   *  language ([[TitleWords.Concept.VersionTag]], [[DecorationWords.EditionWords]]: "Extended", "Director's Cut",
   *  "wersja reżyserska", "versión extendida") — a print's format, an intermission, and the words that number a part
   *  ("… Hallows: Part 1"), which name no subtitle. */
  private val NoSubtitleWords: Set[String] =
    TitleWords.tokens(TitleWords.Concept.VersionTag) ++ DecorationWords.EditionWords ++ Set(
      "version", "versions", "release", "imax", "dolby", "atmos", "screenx", "intermission", "pause",
      "part", "teil", "episode", "chapter", "kapitel", "folge", "parte", "partie")
  /** Numbers spelled out, in English and German: a series NAMED by its number ("The Fantastic Four", "Ocean's Eleven")
   *  is billed "Fantastic 4", "Ocean's 11" — the number its name, not an instalment's. */
  private val SpelledNumbers: Set[String] = Set(
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "eins", "zwei",
    "drei", "vier", "funf", "sechs", "sieben", "acht", "neun", "zehn", "elf", "zwolf")

  /** The subtitle a title gives the instalment its series names ([[BillingParse.subtitle]]). */
  final case class Subtitle(words: Seq[String], seriesSpellsNumber: Boolean)

  /** The instalment a title's series and subtitle name: its subtitle's words of four letters or more that tag no
   *  version ([[NoSubtitleWords]]), and whether its series spells a number out ([[SpelledNumbers]]). None for a title
   *  with no subtitle. */
  private[billing] def subtitleOf(title: String): Option[Subtitle] = {
    val pieces = SubtitleBreak.split(withoutYears(VersionTags.versionless(title).getOrElse(title)), 2)
    Option.when(pieces.length == 2) {
      val words = TitleContainment.tokens(pieces(1)).filter(w => w.length >= StemLetters && w.exists(_.isLetter) && !NoSubtitleWords(w))
      Subtitle(words, TitleContainment.tokens(pieces(0)).exists(SpelledNumbers))
    }.filter(_.words.nonEmpty)
  }

  /** The break that closes a title's series: "Der Herr der Ringe 2 (Extended Version)", "Avatar 2 – The Way of Water". */
  private val SeriesBreak = java.util.regex.Pattern.compile("""[:|/()\[\]–—,.;!?]|\s-\s""")
  /** A year written with its century elided — PL "Halloween '26: Inni" — no instalment's number. */
  private val ElidedYear  = """['’`]\d\d\b""".r
  /** Numbers a slash joins — a date ("10/19/26", "02/12/2026"), a short season ("RB&O 26/27"), a fraction ("Nine 1/2
   *  Weeks") — no instalment's number. */
  private val SlashedNumbers = """\d+(?:\s*/\s*\d+)+""".r
  private val PartWords   = Set("teil", "part", "parte", "partie", "kapitel", "chapter")
  /** The instalment a title numbers: the number closing its first piece, after at least one word and not after a number
   *  opening it, a date's and a season's numbers aside — an Arabic one of at
   *  most three digits, a Roman one of two letters or more, or a number spelled after a part word ("… Part Two"). A year
   *  ("Blade Runner 2049"), a number alone ("2001: A Space Odyssey"), one mid-title ("9½ Wochen") or in a later piece
   *  ("WAJDA: re- wizje. Przegląd … w 100. rocznicę urodzin") numbers no instalment. */
  private[billing] def instalmentNumber(title: String): Option[Int] = {
    val dateless = SlashedNumbers.replaceAllIn(ElidedYear.replaceAllIn(withoutYears(title), " "), " ")
    val series   = SeriesBreak.split(dateless).headOption.map(TitleContainment.tokens).getOrElse(Nil)
    series.lastOption.filter(_ => series.sizeIs >= 2).flatMap { last =>
      if (series.init.lastOption.exists(PartWords) && PartNumbers.contains(last)) PartNumbers(last).toIntOption
      // a series opening with a number as well as closing on one is named by its numbers: "13 Going on 30", "9 to 5"
      else if (series.head.forall(_.isDigit)) None
      // an Arabic number of ASCII digits only: another script's digits ("٢") number nothing
      else if (last.length <= 3 && last.forall(_.isDigit)) Option.when(last.forall(c => c >= '0' && c <= '9'))(last.toInt)
      else if (numbersAnInstalment(last)) romanValue(last)
      else None
    }
  }
}
