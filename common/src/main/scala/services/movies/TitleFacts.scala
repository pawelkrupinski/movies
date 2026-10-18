package services.movies

import services.movies.TitleWords.Concept

/**
 * The facts a venue bills INSIDE a listing's title, peeled off at intake as its format tags are ([[FormatTags]]) and
 * read into the listing's own fields ([[ScrapeListing.prepare]]), so no matching rule has to read them out of a title
 * again: a director after a credit word (PL "Lalka, reż. Maciej Kawalski (2026)", DE "Alpha (dir. Ducournau)"), an
 * original title after its label (DE "(Originaltitel: La petite cuisine de Mehdi)"), the age rating in the country's own
 * system (DE kinoheld's "Das siebte Kreuz D, ab 12"), and what the format peel takes beside the formats: a BBFC
 * certificate and a running time (UK "Wicker (15) 105 mins").
 *
 * Per country ([[Rules]], `models.Country.titleFacts`): a country reads the credit and label words of its own languages
 * and English (the title-word table's, [[TitleWords.Concept.DirectorCredit]], [[TitleWords.Concept.OriginalTitle]]), and
 * the rating system its venues bill ([[RatingSystem]]) — a bare "(15)" is a BBFC certificate in the UK only.
 */
object TitleFacts {

  final case class Facts(directors: Seq[String] = Nil, originalTitle: Option[String] = None,
                         runtimeMinutes: Option[Int] = None, ageRating: Option[String] = None)
  object Facts { val empty: Facts = Facts() }

  /** A title with the facts it billed taken off it. */
  final case class Peeled(title: String, facts: Facts)

  /** How a country's venues bill an age rating in a title, and the rating as that country's clients store it. */
  sealed trait RatingSystem {
    /** `title` without the rating note it closes on, and the rating the note states (`None` while one is awaited). */
    def peel(title: String): Option[(String, Option[String])]
    /** The rating a certificate the format peel took off states, in this system ([[FormatTags.Peeled.certificate]]). */
    def certificate(value: String): Option[String]
  }
  object RatingSystem {
    /** `title` before the rating note `note` closes it on, never the whole title, and the rating `stated` reads off it. */
    private def closingNote(note: scala.util.matching.Regex, title: String)(stated: scala.util.matching.Regex.Match => Option[String]) =
      note.findFirstMatchIn(title).filter(_.start > 0).map(m => (title.substring(0, m.start).trim, stated(m)))
    /** UK: the BBFC certificate the format peel takes off ("Digger (15)", "EOS - Monet (Cert. TBC)"), stored as billed. */
    case object Bbfc extends RatingSystem {
      def peel(title: String): Option[(String, Option[String])] = None
      def certificate(value: String): Option[String] = Some(value)
    }
    /** PL: an age bracket closing a title — Kino w Kadrze's "Kandydaci śmierci: Kadr Non-Fiction (16+)", "… KADR JUNIOR
     *  (6+)" — stored as the Polish clients badge a rating ("16+"). A "60+" of an audience ("dla widzów 60+") is no
     *  bracket, and no rating. */
    case object PolishAge extends RatingSystem {
      private val Note = """\s*\(\s*(0|3|6|7|10|12|13|15|16|18)\s*\+\s*\)\s*$""".r
      def peel(title: String): Option[(String, Option[String])] = closingNote(Note, title)(m => Some(s"${m.group(1)}+"))
      def certificate(value: String): Option[String] = None
    }
    /** DE: kinoheld's version-and-FSK note closing a title — "Das siebte Kreuz D, ab 12", "Dune: Part Three D, FSK folgt"
     *  (the German version, the release pending), "Hin und weg D, ab 12 / ab 6 mit Begleitung" — stored as the German
     *  clients store an FSK rating ("FSK 12"). */
    case object Fsk extends RatingSystem {
      private val Note = """(?iu)\s+D,\s*(?:ab\s+(0|6|12|16|18)\b|FSK\s+(0|6|12|16|18)\b|FSK\s+folgt\b)[^,]*$""".r
      def peel(title: String): Option[(String, Option[String])] =
        closingNote(Note, title)(m => Option(m.group(1)).orElse(Option(m.group(2))).map("FSK " + _))
      def certificate(value: String): Option[String] = None
    }
  }

  /** One country's reading: the languages whose credit and label words it reads, and its rating system, if its venues
   *  bill one in a title. */
  final case class Rules(languages: Set[String], rating: Option[RatingSystem]) {
    // a name: capitalised words or initials, a family particle between ("Julia Ducournau", "Rachel J. Morrison",
    // "Lars von Trier", "Guillermo del Toro"), ending where its sentence or the billing goes on — a full stop
    // ("Maciej Drygas. • Spotkanie"), a separator, a lowercase word. Several are joined by "&" or the language's "and",
    // or by a comma when what follows is one more whole name of two words ("Rodrigo Areias, Aaron Brookner"), so
    // "…, reż. Wim Wenders, Dyskusyjny Klub Filmowy" credits him alone
    private val Part     = """(?:\p{Lu}\.|\p{Lu}[\p{L}'’-]*)"""
    private val Particle = """(?:(?:van|von)(?:\s+(?:der|den))?|de(?:\s+la)?|da|del|della|di|do|dos|du|la|le|bin|al)"""
    private val Person   = s"""$Part(?:\\s+(?:$Particle\\s+)?$Part)*"""
    private val Whole    = s"""$Part\\s+(?:$Particle\\s+)?$Part"""
    private val AndWord  = """(?:&|and|i|und|y|et)"""
    private val Ends     = """(?=\s*(?:[,;|•/()\[\]:–—]|-\s|\.?$|\.\s)|\s+\p{Ll}|\s+""" + AndWord + """\s)"""
    private lazy val Credit = Option.when(languages.nonEmpty)(TitleWords.rawPatternIn(languages, Concept.DirectorCredit)).filter(_ != "(?:)")
      .map(words => (s"""(?U)(?:^|(?<=[\\s(\\[,]))(?<word>(?i:$words)(?:(?:\\s*:\\s*|\\s+)(?i:$words))*)(?<gap>\\s*:\\s*|\\s+)""" +
        s"""(?<names>$Person$Ends(?:(?:\\s*,\\s*$Whole|\\s+$AndWord\\s+$Person)$Ends)*)""").r)
    private lazy val Label  = Option.when(languages.nonEmpty)(TitleWords.rawPatternIn(languages, Concept.OriginalTitle)).filter(_ != "(?:)")
      .map(words => s"""(?U)\\(\\s*(?i:$words)\\s*:\\s*(?<original>[^()]+?)\\s*\\)""".r)
    private lazy val Joined = s"""(?U)\\s*,\\s*|\\s+$AndWord\\s+""".r
    // where the billing goes on past a credit outside brackets: a separator and what follows it stay
    private val GoesOn = """(?U)(?:^|\s)[|•/(\[]|:|\s[-–—]\s""".r

    /** The facts `title` bills, taken off it: its rating note, its director's credit, its labelled original title. A
     *  credit or a label that IS the whole title stays: taken, it would leave no title ("Ein Film von Wim Wenders"). */
    def peel(title: String): Peeled = {
      val (rated, stated) = rating.flatMap(_.peel(title)).getOrElse((title, None))
      val (credited, directors) = Credit.iterator.flatMap(_.findAllMatchIn(rated)).filter(credits)
        .map(m => (cutCredit(rated, m.start, m.end), Joined.split(m.group("names")).toSeq.map(_.trim).filter(_.nonEmpty)))
        .find(_._1.nonEmpty).getOrElse((rated, Seq.empty[String]))
      val (labelled, original) = Label.iterator.flatMap(_.findAllMatchIn(credited))
        .map(m => (joined(credited.substring(0, m.start), credited.substring(m.end)), Option(m.group("original"))))
        .find(_._1.nonEmpty).getOrElse((credited, None))
      Peeled(labelled, Facts(directors, original, None, stated))
    }

    // a lone surname is a credit only after a marked word — "reż. Ozon", "dir. Ducournau", "Regie: Petzold" — so
    // "Heimat (Regie Gespräch im Anschluss)" and "Film von Studierenden der Filmhochschule" credit nobody
    private def credits(m: scala.util.matching.Regex.Match): Boolean =
      Joined.split(m.group("names")).headOption.exists(_.trim.contains(' ')) || m.group("gap").contains(':') || m.group("word").endsWith(".")

    /** `title` without the credit at `start`–`end`: inside brackets, the comma-separated piece holding it (the bracket
     *  goes when nothing else is in it: "(reż. Ozon)", "(Der neue Film von Andreas Dresen mit …)"), else the credit and
     *  what it runs on to before the billing goes on (", Polska"; a separator and what follows it stay). */
    private def cutCredit(title: String, start: Int, end: Int): String = {
      val open  = title.lastIndexWhere(c => c == '(' || c == '[', start - 1)
      val close = if (open >= 0 && title.lastIndexWhere(c => c == ')' || c == ']', start - 1) < open) title.indexWhere(c => c == ')' || c == ']', end) else -1
      if (close >= 0) {
        val inside = title.substring(open + 1, close)
        val from   = inside.lastIndexOf(',', start - open - 1) + 1
        val comma  = inside.indexOf(',', end - open - 1)
        val kept   = (inside.substring(0, from) + (if (comma >= 0) inside.substring(comma + 1) else "")).split(',').map(_.trim).filter(_.nonEmpty)
        joined(title.substring(0, open), (if (kept.isEmpty) "" else kept.mkString(s"${title(open)}", ", ", s"${title(close)}")) + " " + title.substring(close + 1))
      } else joined(title.substring(0, start), GoesOn.findFirstMatchIn(title.substring(end)).fold("")(m => title.substring(end + m.start)))
    }

    /** `before` and `after` as one title: no comma, colon or open bracket left dangling between or around them. */
    private def joined(before: String, after: String): String = {
      val a0 = after.trim
      // a credit between two separators leaves one: "Film | reżyseria: A. Kowalski | Polska 2024"
      val b  = before.replaceAll(if (a0.matches("""[|•/].*""")) """[\s,(\[|•/:]+$""" else """[\s,(\[]+$""", "")
      val a  = a0.replaceFirst(if (b.isEmpty) """^[\s:,;|•/–—-]+""" else """^,\s*""", "")
      (if (a.isEmpty) b else if (b.isEmpty) a else s"$b $a").trim
    }
  }
  object Rules {
    /** Reads nothing: a normalizer built without a country's rules. */
    val none: Rules = Rules(Set.empty, None)
  }

  /** The facts a listing's title bills, as one country reads them: what [[Rules.peel]] takes off the title before the
   *  venue's rules, with what the format peel took off after them ([[FormatTags.Peeled]]) — its certificate where it is
   *  the country's rating system, its running time. */
  def of(rules: Rules, credits: Facts, format: FormatTags.Peeled): Facts =
    credits.copy(runtimeMinutes = credits.runtimeMinutes.orElse(format.runtimeMinutes),
      ageRating = credits.ageRating.orElse(format.certificate.flatMap(c => rules.rating.flatMap(_.certificate(c)))))
}
