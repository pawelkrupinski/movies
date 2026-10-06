package services.cinemas.common

import services.cinemas.CountryNames

import java.util.Locale
import scala.util.matching.Regex

/**
 * The film facts a Polish venue types into the free-text description of its own event page —
 * director, cast, original title, production countries and year, running time — read off its
 * lines. A ticketing platform such as bilety24 gives each venue one description box and no form,
 * so every venue words it its own way. Measured across the bilety24 roster on 2026-10-06:
 *
 *   - labelled lines: "reżyseria: David Cronenberg", "Obsada: …", "tytuł oryg.: …",
 *     "Kraj i rok produkcji: USA 2026", "kraj, rok: Austria, Niemcy, 2026", "Rok produkcji: 2024",
 *     "Czas trwania: 1 godz. 35 min.", "czas: 110 min / kat. wiekowa - 8+", "Czas trwania: 93’";
 *   - runs of them joined by pipes or semicolons: "BEZ KOŃCA | PREMIERA: 11.09.2026 | CZAS
 *     TRWANIA: 108 minut | PRODUKCJA: POLSKA, FRANCJA", "reżyseria: …; scenariusz: …; obsada: …";
 *   - one credit line: "GRADIVA | LA GRADIVA reż. Marine Atlan | Francja, Włochy 2026 | 145 min",
 *     "reż. Natxo Leuza, Hiszpania 2025, 85'";
 *   - an unlabelled production line: "Kanada/Wielka Brytania 1996, thriller erotyczny, 100 min".
 *
 * Absence is normal — most venues paste only a synopsis — and yields an empty [[FilmDetail]], never
 * a guess. A premiere date ("PREMIERA: 11.09.2026") is not a production year and is never read as
 * one. Fields the model has no slot for (screenwriter, distributor, premiere) are not read.
 *
 * A page that credits more than one film — a festival block "Linia logiczna, Bartne" with a
 * "reż. …" line per film — must not lend one film's facts to the whole block: when any fact is
 * stated twice with different values, the page yields none of them.
 */
object VenueCredits {

  private sealed trait Field
  private case object Director      extends Field
  private case object Cast          extends Field
  private case object OriginalTitle extends Field
  private case object Runtime       extends Field
  private case object Year          extends Field
  private case object Countries     extends Field
  /** A labelled production value: countries, and a year when it carries one ("Polska 2026"). */
  private case object Production    extends Field

  private type Fact = (Field, Any)

  // Longest spelling first, so "kraj i rok produkcji" is not read as "kraj".
  private val Labels: Seq[(String, Field)] = Seq(
    "kraj i rok produkcji" -> Production, "kraj produkcji" -> Production, "miejsce produkcji" -> Production,
    "kraj, rok" -> Production,
    "kraj" -> Production, "produkcja" -> Production, "prod." -> Production,
    "rok produkcji" -> Year,
    "tytuł oryginalny" -> OriginalTitle, "tytuł oryginału" -> OriginalTitle, "oryginalny tytuł" -> OriginalTitle,
    "tytuł oryg." -> OriginalTitle,
    "reżyseria" -> Director, "reżyserka" -> Director, "reżyser" -> Director,
    "w rolach głównych" -> Cast, "występują" -> Cast, "występuje" -> Cast, "obsada" -> Cast,
    "czas trwania" -> Runtime, "czas" -> Runtime
  )
  private val LabelOf: Map[String, Field] = Labels.toMap

  // "<label>: value" / "<label> - value", the label at the start of a segment, any case. A label
  // ending in a dot needs no separator ("Prod. Polska 2026, …").
  private val Labelled: Regex = {
    val alternatives = Labels.map(_._1).map(Regex.quote).mkString("|")
    s"""(?iu)^($alternatives)(?:\\s*[:\\-–—]\\s*|(?<=\\.)\\s+)(.+)$$""".r
  }

  /** "reż." anywhere in a line, and the credit after it. */
  private val InlineDirector = """(?iu)(?:^|[\s|(])reż\.\s*(.+)$""".r
  private val YearAtEnd      = """^(.*?)(?:^|\s)((?:19|20)\d{2})$""".r
  private val RuntimeItem    = """(?iu)^(?:ok\.\s*|około\s*)?(\d{1,3})\s*(?:min\.?|minut[ay]?\.?|['’′])$""".r
  private val Apostrophe     = """(\d{1,3})\s*['’′]""".r
  private val HoursOrMinutes = """(?iu)\d\s*(?:godz|min|h\b)""".r
  private val Parenthetical  = """\s*\([^)]*\)""".r
  // bilety24 glues its "*******" divider onto the last line before its refund boilerplate.
  private val TrailingStars  = """\s*\*+\s*$""".r
  private val Separator      = """\s*[|;]\s*""".r

  /** Longer than any credit line seen; a longer line is prose and states nothing. */
  private val MaxCreditLine = 220

  /** The facts `lines` (the description's visual lines) state, as a [[FilmDetail]] with only its
   *  identity fields set. */
  def parse(lines: Seq[String]): FilmDetail = {
    val byField = lines.iterator.map(clean).filter(l => l.nonEmpty && l.length <= MaxCreditLine)
      .flatMap(factsOf).toSeq.groupMap(_._1)(_._2).view.mapValues(_.distinct).toMap
    // One value per fact — or the page bills more than one film, and none of its facts is the listing's.
    if (byField.values.exists(_.size > 1)) FilmDetail()
    else {
      def one[A](field: Field): Option[A] = byField.get(field).flatMap(_.headOption).map(_.asInstanceOf[A])
      val production = one[(Seq[String], Option[Int])](Production)
      FilmDetail(
        director       = one[Seq[String]](Director).getOrElse(Nil),
        cast           = one[Seq[String]](Cast).getOrElse(Nil),
        originalTitle  = one[String](OriginalTitle),
        runtimeMinutes = one[Int](Runtime),
        releaseYear    = one[Int](Year).orElse(production.flatMap(_._2)),
        countries      = production.map(_._1).orElse(one[Seq[String]](Countries)).getOrElse(Nil))
    }
  }

  /** Does `line` state a film fact [[parse]] reads — a line a synopsis should leave out? */
  def statesFacts(line: String): Boolean = {
    val cleaned = clean(line)
    cleaned.nonEmpty && cleaned.length <= MaxCreditLine && factsOf(cleaned).nonEmpty
  }

  private def clean(line: String): String =
    TrailingStars.replaceFirstIn(line.replace(' ', ' ').replaceAll("\\s+", " ").trim, "")

  private def factsOf(line: String): Seq[Fact] = {
    val segments = Separator.split(line).iterator.map(_.trim).filter(_.nonEmpty).toSeq
    val labelled = segments.flatMap(labelledFacts)
    if (labelled.exists(_._1 == Director)) labelled
    else inlineCredit(line) match {
      case credit if credit.nonEmpty => labelled ++ credit
      case _ =>
        // Unlabelled pieces state a production only beside something that marks the line as the
        // film's facts — a year, a running time, or a labelled fact on the same line.
        val loose      = segments.filter(labelledFacts(_).isEmpty).flatMap(items)
        val countries  = loose.filter(isCountry)
        val year       = loose.collectFirst { case YearAtEnd(before, y) if before.trim.isEmpty || isCountry(before) => y.toInt }
        val runtime    = loose.flatMap(runtimeOf).headOption
        val production =
          if (countries.isEmpty && year.isEmpty) Nil
          else if (year.isEmpty && runtime.isEmpty && labelled.isEmpty) Nil
          else countries.map(stripYear).distinct match {
            case Nil => year.map(Year -> _).toSeq
            case cs  => Seq(Countries -> cs) ++ year.map(Year -> _)
          }
        labelled ++ production ++ runtime.filter(_ => production.nonEmpty).map(Runtime -> _)
    }
  }

  private def labelledFacts(segment: String): Seq[Fact] = segment match {
    case Labelled(label, value) =>
      LabelOf(label.toLowerCase(Locale.ROOT)) match {
        case Director      => Some(names(value)).filter(_.nonEmpty).map(Director -> _).toSeq
        case Cast          => Some(names(value)).filter(_.nonEmpty).map(Cast -> _).toSeq
        case OriginalTitle => Seq(OriginalTitle -> value.trim)
        case Runtime       => runtimeOf(value).map(Runtime -> _).toSeq
        case Year          => ScraperParse.yearIn(value).map(Year -> _).toSeq
        case _             =>
          // Only names the dictionary knows: "Produkcja" names a studio as often as a country
          // ("Produkcja: Detours Film" beside "Miejsce produkcji: Szwajcaria"), and a genre rides
          // on the same value ("Prod. Kanada/USA/Wlk. Brytania 2026, animacja, 95 min").
          val its       = items(value)
          val countries = its.filter(isCountry).map(stripYear).distinct
          val year      = its.collectFirst { case YearAtEnd(before, y) if before.trim.isEmpty || isCountry(before) => y.toInt }
          Option.when(countries.nonEmpty || year.isDefined)(Production -> (countries, year)).toSeq ++
            its.flatMap(runtimeOf).headOption.map(Runtime -> _)
      }
    case _ => Nil
  }

  /** "… reż. A, B | Polska | 2025 | 34 min" / "reż. Natxo Leuza, Hiszpania 2025, 85'": the directors,
   *  then what the line says of the production. A "reż." inside a sentence ("w reż. Andrzeja Wajdy")
   *  is a credit only when the rest of its line states a country, a year or a running time. */
  private def inlineCredit(line: String): Seq[Fact] =
    InlineDirector.findFirstMatchIn(line).toSeq.flatMap { m =>
      val after = m.group(1)
      val (directors, rest) =
        if (after.contains("|")) { val (d, r) = after.span(_ != '|'); (names(d), items(r.drop(1))) }
        else {
          val (d, r) = after.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq.span(!isProductionItem(_))
          (d.flatMap(names), r.flatMap(items))
        }
      val countries = rest.filter(isCountry).map(stripYear).distinct
      val year      = rest.collectFirst { case YearAtEnd(before, y) if before.trim.isEmpty || isCountry(before) => y.toInt }
      val runtime   = rest.flatMap(runtimeOf).headOption
      val corroborated = countries.nonEmpty || year.isDefined || runtime.isDefined
      if (directors.isEmpty || (m.start > 0 && !corroborated)) Nil
      else Seq(Director -> directors) ++ Option.when(countries.nonEmpty)(Countries -> countries) ++
        year.map(Year -> _) ++ runtime.map(Runtime -> _)
    }

  /** A fact's pieces: commas, slashes and pipes all separate them. */
  private def items(value: String): Seq[String] =
    value.split("""[,/|]""").iterator.map(_.trim.stripSuffix(".").trim).filter(_.nonEmpty).toSeq

  private def stripYear(item: String): String = item match {
    case YearAtEnd(before, _) => before.trim
    case other                => other
  }

  /** A country the dictionary knows, with or without a year after it ("Wielka Brytania 1996"). */
  private def isCountry(item: String): Boolean = {
    val name = stripYear(item)
    name.nonEmpty && CountryNames.isPolish(name)
  }

  private def isProductionItem(item: String): Boolean =
    runtimeOf(item).isDefined || isCountry(item) || (item match {
      case YearAtEnd(before, _) => before.trim.isEmpty
      case _                    => false
    })

  private def runtimeOf(value: String): Option[Int] = value.trim match {
    case RuntimeItem(minutes) => Some(minutes.toInt).filter(_ > 0)
    case other =>
      Apostrophe.findFirstMatchIn(other).map(_.group(1).toInt).filter(_ > 0)
        .orElse(HoursOrMinutes.findFirstIn(other).flatMap(_ => ScraperParse.hoursMinutesRuntime(other)))
  }

  /** "Wilhelm Sasnal, Anna Sasnal" / "David Lynch (scenariusz i reżyseria)" → the names. */
  private def names(value: String): Seq[String] =
    Parenthetical.replaceAllIn(value, "").split(",").iterator
      .map(_.trim.stripSuffix(".").trim).filter(_.exists(_.isLetter)).toSeq
}
