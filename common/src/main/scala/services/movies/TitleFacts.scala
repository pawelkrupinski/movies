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
    /** UK: the BBFC certificate the format peel takes off ("Digger (15)", "EOS - Monet (Cert. TBC)"), stored as billed. */
    case object Bbfc extends RatingSystem {
      def peel(title: String): Option[(String, Option[String])] = None
      def certificate(value: String): Option[String] = Some(value)
    }
    /** DE: kinoheld's version-and-FSK note closing a title — "Das siebte Kreuz D, ab 12", "Dune: Part Three D, FSK folgt"
     *  (the German version, the release pending), "Hin und weg D, ab 12 / ab 6 mit Begleitung" — stored as the German
     *  clients store an FSK rating ("FSK 12"). */
    case object Fsk extends RatingSystem {
      private val Note = """(?iu)\s+D,\s*(?:ab\s+(0|6|12|16|18)\b|FSK\s+(0|6|12|16|18)\b|FSK\s+folgt\b)[^,]*$""".r
      def peel(title: String): Option[(String, Option[String])] =
        Note.findFirstMatchIn(title).filter(_.start > 0).map { m =>
          (title.substring(0, m.start).trim, Option(m.group(1)).orElse(Option(m.group(2))).map("FSK " + _))
        }
      def certificate(value: String): Option[String] = None
    }
  }

  /** One country's reading: the languages whose credit and label words it reads, and its rating system, if its venues
   *  bill one in a title. */
  final case class Rules(languages: Set[String], rating: Option[RatingSystem]) {
    // a name: a capitalised word, or a run of them, a family particle between ("Julia Ducournau", "Agnès Varda",
    // "Lars von Trier", "Guillermo del Toro"); several joined by a comma, "&" or the language's "and" — a name after a
    // comma only of two words or more, so "…, reż. Maciej Kawalski, Polska" credits him alone
    private val Name     = """\p{Lu}[\p{L}'’.-]*"""
    private val Particle = """(?:de|da|del|della|di|do|dos|du|la|le|van|von|der|den|ter|ten|bin|al)"""
    private val Person   = s"""$Name(?:\\s+(?:$Particle\\s+)*$Name)*"""
    private val Persons  = s"""$Name(?:\\s+(?:$Particle\\s+)*$Name)+"""
    private val And      = """(?:\s*(?:&|,)\s*|\s+(?:and|i|und|y|et)\s+)"""
    private lazy val Credit   = Option.when(languages.nonEmpty)(TitleWords.rawPatternIn(languages, Concept.DirectorCredit)).filter(_ != "(?:)")
      .map(words => s"""(?U)(?:^|(?<=[\\s(\\[,]))(?i:$words)(?:(?:\\s*:\\s*|\\s+)(?i:$words))*(?:\\s*:\\s*|\\s+)(?<names>$Person(?:$And$Persons)*)""".r)
    private lazy val Label    = Option.when(languages.nonEmpty)(TitleWords.rawPatternIn(languages, Concept.OriginalTitle)).filter(_ != "(?:)")
      .map(words => s"""(?U)\\(\\s*(?i:$words)\\s*:\\s*(?<original>[^()]+?)\\s*\\)""".r)
    private lazy val Joined   = s"""(?U)$And""".r

    /** The facts `title` bills, taken off it: its rating note, its director's credit (a bracket holding it goes whole:
     *  "Alte Liebe (Der neue Film von Andreas Dresen mit Dagmar Manzel)"), its labelled original title. */
    def peel(title: String): Peeled = {
      val (rated, stated) = rating.flatMap(_.peel(title)).getOrElse((title, None))
      val (credited, directors) = Credit.flatMap(_.findFirstMatchIn(rated)).fold((rated, Seq.empty[String])) { m =>
        // a name's own full stop ends the credit's sentence, an initial's stays ("Maciej Drygas. • Spotkanie", "J. Doe")
        val names = Joined.split(m.group("names")).toSeq.map(_.trim.replaceAll("""(?<=\p{L}{2})\.$""", "")).filter(_.nonEmpty)
        (cut(rated, m.start, m.end), names)
      }
      val (labelled, original) = Label.flatMap(_.findFirstMatchIn(credited)).fold((credited, Option.empty[String])) { m =>
        (cut(credited, m.start, m.end), Some(m.group("original")))
      }
      Peeled(labelled, Facts(directors, original, None, stated))
    }

    /** `title` without its `start`–`end` span: the whole bracket the span opens inside, and the comma it follows. */
    private def cut(title: String, start: Int, end: Int): String = {
      val open  = title.lastIndexWhere(c => c == '(' || c == '[', start - 1)
      val close = if (open >= 0 && title.lastIndexWhere(c => c == ')' || c == ']', start - 1) < open) title.indexWhere(c => c == ')' || c == ']', end) else -1
      val (from, to) = if (close >= 0) (open, close + 1) else (start, end)
      val before = title.substring(0, from).replaceAll("""[\s,]+$""", "")
      // the comma that closed the credit goes too where only a bracket follows it ("…, reż. K. Agopsowicz, (2026)")
      val after  = title.substring(to).trim.replaceFirst("""^,\s*(?=[(\[]|$)""", "")
      (if (after.isEmpty) before else if (after.startsWith(",")) s"$before$after" else s"$before $after").trim
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
