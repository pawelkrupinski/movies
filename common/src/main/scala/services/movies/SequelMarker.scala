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

  private val RomanValues: Map[String, Int] =
    Map("ii" -> 2, "iii" -> 3, "iv" -> 4, "v" -> 5, "vi" -> 6, "vii" -> 7,
        "viii" -> 8, "ix" -> 9, "x" -> 10, "xi" -> 11, "xii" -> 12)

  /** Spelled-out ordinals, English and the catalogue's other languages, sanitized
   *  (no diacritics) the way `TitleContainment.tokens` hands them over. */
  private val WordOrdinals: Set[String] =
    Set("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
        "second", "third", "fourth", "fifth",
        "druga", "trzecia", "czwarta", "piata", "drugi", "trzeci", "czwarty",
        "zwei", "drei", "vier", "zweiter", "dritter",
        "dos", "tres", "cuatro", "segunda", "tercera")

  /** The number each spelled-out ordinal above names, so a comparison across
   *  languages ("Part Two" vs "Część druga") and across notations (below) means
   *  the same thing as comparing the digits. */
  private val WordOrdinalValues: Map[String, Int] =
    Map("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5,
        "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9, "ten" -> 10,
        "second" -> 2, "third" -> 3, "fourth" -> 4, "fifth" -> 5,
        "druga" -> 2, "trzecia" -> 3, "czwarta" -> 4, "piata" -> 5,
        "drugi" -> 2, "trzeci" -> 3, "czwarty" -> 4,
        "zwei" -> 2, "drei" -> 3, "vier" -> 4, "zweiter" -> 2, "dritter" -> 3,
        "dos" -> 2, "tres" -> 3, "cuatro" -> 4, "segunda" -> 2, "tercera" -> 3)

  /** A plausible release year is a year, never an ordinal. */
  private def isYear(t: String): Boolean =
    t.length == 4 && t.forall(_.isDigit) && { val y = t.toInt; y >= 1888 && y <= java.time.Year.now().getValue + 1 }

  def isOrdinal(t: String): Boolean =
    (t.nonEmpty && t.forall(_.isDigit) && !isYear(t)) || Roman.matches(t)

  /** The instalment NUMBER `t` names, however it's written — "2", "ii" and
   *  "two"/"Część druga" are the same value. `differentInstalments` compares
   *  these, not the raw tokens, so a franchise catalogued under one notation by
   *  one cinema and another by a second ("Mortal Kombat 2" vs TMDB's "Mortal
   *  Kombat II") is never mistaken for two different films. */
  private def ordinalValue(t: String): Option[Int] =
    if (t.nonEmpty && t.forall(_.isDigit) && !isYear(t)) Some(t.toInt)
    else RomanValues.get(t).orElse(WordOrdinalValues.get(t))

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
   *  and END in ordinals naming different NUMBERS ("Part 1" vs "Part 2", "Rocky
   *  II" vs "Rocky III"). Comparing by VALUE, not by raw token, is what keeps a
   *  franchise cinemas number two different ways from tripping this: Multikino's
   *  "Mortal Kombat 2" and TMDB's own "Mortal Kombat II" are the SAME film — "2"
   *  and "ii" both resolve to 2 — while "Mockingjay - Part 1" and "- Part 2" (1
   *  vs 2) are not. The equal-length shape is one character apart once sanitized
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
  def differentInstalments(a: Seq[String], b: Seq[String]): Boolean =
    if (a.isEmpty || b.isEmpty) false
    else if (a.length == b.length)
      (ordinalValue(a.last), ordinalValue(b.last)) match {
        case (Some(va), Some(vb)) => va != vb
        case _                    => false
      }
    else if (a.length < b.length) namesAnotherEntry(a, b)
    else namesAnotherEntry(b, a)
}
