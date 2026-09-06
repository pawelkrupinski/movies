package services.movies

/**
 * Does a longer title that CONTAINS a film's title name a different film in the
 * same series, rather than a decorated screening of that film?
 *
 * The containment edge in [[FilmCanonicalizer.groupByFilm]] folds an unresolved
 * edition onto a resolved base when the base's tokens run along one edge of the
 * edition's: "Toddler Club: Toy Story 5" is a screening of "Toy Story 5", and the
 * edition is what the venue calls it. A sequel has the same shape and is not: the
 * logs for 2026-08-29→09-06 show "The Hunger Games: Mockingjay Pt 2 (2026
 * Re-Release)" folded onto the 2012 "The Hunger Games" twenty times, so UK cinemas
 * screening the re-release served the first film's poster, cast and ratings. The
 * cinemas' own evidence could not refuse it — UK slots publish an original title
 * one time in nine — so the refusal has to come from the title itself.
 *
 * Two shapes say "another film in the series", and only these two:
 *   - the token right AFTER the base run is an ordinal — a small number ("Toy Story"
 *     inside "Toy Story 5") or a roman numeral ("Rocky" inside "Rocky II");
 *   - a part marker anywhere among the extra tokens is followed by one ("Pt 2",
 *     "Part II", "Chapter 3", "Część 2").
 * A four-digit year is NOT an ordinal ("Casablanca 1942" is Casablanca; "Blade
 * Runner 2049" is not a plausible year, so it still counts). A number BEFORE the base
 * ("Cineworld 30: The Matrix") is a banner, not a sequel, and folds as before.
 */
object SequelMarker {

  private val PartMarkers: Set[String] =
    Set("part", "pt", "chapter", "chap", "vol", "volume", "episode", "ep",
        "czesc", "cz", "teil", "parte", "capitulo", "kapitel", "partie")

  private val Roman = "^(ii|iii|iv|v|vi|vii|viii|ix|x|xi|xii)$".r

  /** A plausible release year is a year, never an ordinal. */
  private def isYear(t: String): Boolean =
    t.length == 4 && t.forall(_.isDigit) && { val y = t.toInt; y >= 1888 && y <= java.time.Year.now().getValue + 1 }

  def isOrdinal(t: String): Boolean =
    (t.nonEmpty && t.length <= 2 && t.forall(_.isDigit)) ||
      (t.forall(_.isDigit) && t.nonEmpty && !isYear(t)) ||
      Roman.matches(t)

  /** True when `whole` (an edition's tokens, which contain `base`'s tokens as a
   *  prefix or suffix run) names a different film in `base`'s series. */
  def namesAnotherEntry(base: Seq[String], whole: Seq[String]): Boolean = {
    val extras =
      if (whole.startsWith(base)) whole.drop(base.length)
      else if (whole.endsWith(base)) whole.dropRight(base.length)
      else Nil
    val ordinalRightAfterBase = whole.startsWith(base) && extras.headOption.exists(isOrdinal)
    val partThenOrdinal = extras.sliding(2).exists {
      case Seq(marker, ordinal) => PartMarkers.contains(marker) && isOrdinal(ordinal)
      case _                    => false
    }
    ordinalRightAfterBase || partThenOrdinal
  }
}
