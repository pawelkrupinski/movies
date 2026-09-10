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
 *     "Part II", "Chapter 3", "Część 2") — or by a spelled-out one ("Part Two",
 *     "Część druga"), which only counts after a marker: a bare word after the base
 *     ("Toy Story Two"?) is not a shape the catalogue uses.
 * A four-digit year is NOT an ordinal ("Casablanca 1942" is Casablanca; "Blade
 * Runner 2049" is not a plausible year, so it still counts). A number BEFORE the base
 * ("Cineworld 30: The Matrix") is a banner, not a sequel, and folds as before.
 */
object SequelMarker {

  private val PartMarkers: Set[String] =
    Set("part", "pt", "chapter", "chap", "vol", "volume", "episode", "ep",
        "czesc", "cz", "teil", "parte", "capitulo", "kapitel", "partie")

  private val Roman = "^(ii|iii|iv|v|vi|vii|viii|ix|x|xi|xii)$".r

  /** Spelled-out ordinals, English and the catalogue's other languages, sanitized
   *  (no diacritics) the way `TitleContainment.tokens` hands them over. */
  private val WordOrdinals: Set[String] =
    Set("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
        "second", "third", "fourth", "fifth",
        "druga", "trzecia", "czwarta", "piata", "drugi", "trzeci", "czwarty",
        "zwei", "drei", "vier", "zweiter", "dritter",
        "dos", "tres", "cuatro", "segunda", "tercera")

  /** A plausible release year is a year, never an ordinal. */
  private def isYear(t: String): Boolean =
    t.length == 4 && t.forall(_.isDigit) && { val y = t.toInt; y >= 1888 && y <= java.time.Year.now().getValue + 1 }

  def isOrdinal(t: String): Boolean =
    (t.nonEmpty && t.forall(_.isDigit) && !isYear(t)) || Roman.matches(t)

  /** True when `whole` (an edition's tokens, which contain `base`'s tokens as a
   *  prefix or suffix run) names a different film in `base`'s series. */
  def namesAnotherEntry(base: Seq[String], whole: Seq[String]): Boolean = {
    val extras =
      if (whole.startsWith(base)) whole.drop(base.length)
      else if (whole.endsWith(base)) whole.dropRight(base.length)
      else Nil
    val ordinalRightAfterBase = whole.startsWith(base) && extras.headOption.exists(isOrdinal)
    val partThenOrdinal = extras.sliding(2).exists {
      case Seq(marker, ordinal) => PartMarkers.contains(marker) && (isOrdinal(ordinal) || WordOrdinals.contains(ordinal))
      case _                    => false
    }
    ordinalRightAfterBase || partThenOrdinal
  }

  /** Symmetric check: do `a` and `b` name two DIFFERENT instalments of the same
   *  series — either one's tokens contain the other's plus a trailing ordinal
   *  ([[namesAnotherEntry]], either direction), or the two run the same length
   *  and END in a different ordinal token ("Part 1" vs "Part 2", "Rocky II" vs
   *  "Rocky III"). The equal-length shape is one character apart once sanitized
   *  ("...mockingjaypart1" / "...mockingjaypart2") — well inside
   *  `TitleMatch.close`'s edit-distance bound, which exists for spelling drift
   *  ("guru"→"gourou"), not for telling two sequels apart.
   *
   *  Deliberately does NOT also require every token before the last to match: a
   *  caller only reaches here once `TitleMatch.close` has already judged the two
   *  titles close overall, so a stray typo earlier in the title ("Mockinjay" for
   *  "Mockingjay") must not defeat the one signal that actually separates the
   *  films — a typo AND the digit both landing inside the same ≤2-edit budget is
   *  common precisely because these titles are long. The trailing ordinal is
   *  what a series numbers itself by; everything else is spelling.
   *
   *  Guards `TmdbCandidateSearch.directorWalk`'s fuzzy title match, which would
   *  otherwise let a same-director sequel pair tie and fall to the lowest-id
   *  tie-break — pinning "Mockingjay - Part 2" to "Part 1"'s (older, lower-id)
   *  film whenever no candidate title matched either spelling exactly. */
  def differentInstalments(a: Seq[String], b: Seq[String]): Boolean = {
    def ordinalish(t: String): Boolean = isOrdinal(t) || WordOrdinals.contains(t)
    if (a.isEmpty || b.isEmpty) false
    else if (a.length == b.length) a.last != b.last && (ordinalish(a.last) || ordinalish(b.last))
    else if (a.length < b.length) namesAnotherEntry(a, b)
    else namesAnotherEntry(b, a)
  }
}
