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
    (t.nonEmpty && t.forall(_.isDigit) && !isYear(t) && t.toIntOption.isDefined) || Roman.matches(t)

  /** The instalment NUMBER `t` names, however it's written — "2", "ii" and
   *  "two"/"Część druga" are the same value. `differentInstalments` compares
   *  these, not the raw tokens, so a franchise catalogued under one notation by
   *  one cinema and another by a second ("Mortal Kombat 2" vs TMDB's "Mortal
   *  Kombat II") is never mistaken for two different films. */
  private def ordinalValue(t: String): Option[Int] =
    // `toIntOption`: a number past Int (a phone number or ticket code left in a title) names no
    // instalment, and `toInt` threw out of every comparison that met it.
    if (t.nonEmpty && t.forall(_.isDigit) && !isYear(t)) t.toIntOption
    else RomanValues.get(t).orElse(WordOrdinalValues.get(t))

  /** Sequels that don't NUMBER themselves — a subtitle change instead of an
   *  ordinal/part-marker, so neither `ordinalRightAfterBase` nor
   *  `partThenOrdinal` below can see them. The pattern (base is a token-run
   *  PREFIX, extras are ordinary words) is indistinguishable in general from a
   *  genuine prefix-anchored decoration — `"Casablanca 1942"` and
   *  `"Ojczyzna - pokaz przedpremierowy"` are both PREFIX-shaped and both fold
   *  correctly (`SequelMarkerSpec`), so widening the ordinal check to "any
   *  trailing words" would break them. A curated, per-franchise list — same
   *  idiom as `ExtraTitleRules`' curated banner exceptions — is the only safe
   *  way to name the ones that AREN'T decorations, evidenced as they're found.
   *
   *  UK convergence, 2026-09-16: "The Hunger Games" (the resolved 2012
   *  original) swallowed "The Hunger Games: Catching Fire" via the containment
   *  edge — Catching Fire carries no ordinal, so the existing guard let it
   *  through, and the ORDER two settle passes discovered the row in decided
   *  whether Catching Fire's screenings folded onto the original or stayed
   *  their own row. "The Ballad of Songbirds and Snakes" (2023) is the same
   *  franchise's other non-ordinal entry — added alongside since it fits the
   *  identical shape, though not itself confirmed in a corpus yet. */
  private val KnownFranchiseSubtitles: Map[Seq[String], Set[Seq[String]]] = Map(
    Seq("the", "hunger", "games") -> Set(
      Seq("catching", "fire"),
      Seq("the", "ballad", "of", "songbirds", "and", "snakes"),
      Seq("the", "ballad", "of", "songbirds", "snakes"),
      // UK convergence, 2026-09-15→17: a newly-trending, not-yet-released entry
      // in the SAME franchise ("Sunrise on the Reaping", 2026-11-18, also
      // Francis Lawrence) shares nothing with "Catching Fire"/"Mockingjay - Part
      // 1"/"Part 2" but the "the hunger games" prefix — so a bare, undated
      // rerelease listing (Odeon's rerelease-season pages stamp every title with
      // the season's current year, exactly the "Catching Fire" 2013 vs the
      // resolved cluster's own trap `437d1fa21` already names) let
      // `TmdbCandidateSearch.directorWalk`'s year-pinned tier resolve straight to
      // it: nothing here previously told `isDifferentInstalment` this was a
      // DIFFERENT entry rather than the same one under an unfamiliar subtitle,
      // so `corroboratedByTitle`'s "shares Hunger/Games with the query" was
      // enough on its own. Curating it closes the same gap `catching fire` and
      // `the ballad of songbirds and snakes` were added for.
      Seq("sunrise", "on", "the", "reaping")
    ),
    // US prod, 2026-09-16: "Bring It On: All or Nothing" (2006, dir. Steve
    // Rash) folded onto the resolved "Bring It On" (2000, dir. Peyton Reed)
    // the same way — a franchise entry that renames itself instead of
    // numbering itself. Found via a `CinemaCorroboration` director
    // contradiction, not a re-key log; fixed by hand on the one row, added
    // here so the containment edge refuses it on its own next time.
    Seq("bring", "it", "on") -> Set(
      Seq("all", "or", "nothing")
    )
  )

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
    val knownSubtitle = curatedSubtitle(base, extras).isDefined
    ordinalRightAfterBase || partThenOrdinal || knownSubtitle
  }

  /** The curated subtitle `extras` (the tokens after a franchise `base`) OPENS with, if
   *  any — the longest, so "songbirds and snakes" wins over a shorter overlapping entry.
   *  A prefix, not the whole of `extras`: whatever trails the subtitle — a year a venue
   *  brackets onto a rerelease, "Re-Release" — does not make it a different entry, and
   *  an exact match let "The Hunger Games: The Ballad of Songbirds and Snakes (2023)"
   *  read as a decoration of the 2012 original and land a Vue venue's screenings on it. */
  private def curatedSubtitle(base: Seq[String], extras: Seq[String]): Option[Seq[String]] =
    KnownFranchiseSubtitles.get(base).flatMap(_.filter(extras.startsWith(_)).maxByOption(_.length))

  /** True when `a` and `b` share a CURATED franchise base as a common prefix and
   *  each independently qualifies, against that base, as [[namesAnotherEntry]] —
   *  the SIBLING shape the base/whole check above can't see, because neither
   *  title's tokens run along the other's at all: "The Hunger Games: Catching
   *  Fire" and "The Hunger Games: Mockingjay - Part 2" don't contain each other,
   *  they only share the franchise's own opening. UK convergence, 2026-09-16,
   *  round two: closing the containment edge for Catching Fire alone left this
   *  gap, which surfaced as Catching Fire's screenings folding onto whichever
   *  Mockingjay part `directorWalk` resolved first, instead of the original film.
   *
   *  `private[movies]` so `FilmCanonicalizer`'s tmdbId/imdbId-sharing folds can ask
   *  this ALONE, without the general ordinal/containment logic below — those are
   *  vetted against clean TMDB candidate titles (`directorWalk`) or an
   *  already-resolved base (the containment edge), and misfire on raw, messy
   *  CINEMA-published title text: a synthetic disambiguating suffix ("Ghost 2 (1)")
   *  reads as a false ordinal split. The curated list is manually vetted per
   *  franchise, so it alone is safe to apply to bare cinema titles too. */
  private[movies] def curatedSiblings(a: Seq[String], b: Seq[String]): Boolean =
    KnownFranchiseSubtitles.keySet.exists { base =>
      a.startsWith(base) && b.startsWith(base) &&
      ((entryNamed(base, a), entryNamed(base, b)) match {
        case (Some(entryA), Some(entryB)) => entryA.differsFrom(entryB)
        case _                            => false
      })
    }

  /** One entry of a curated series: the words that name it and, when it numbers itself,
   *  the instalment's VALUE. Two entries differ when their numbers differ, or when their
   *  words are further apart than a venue's typo — "Mockinjay - Part 2" is Mockingjay
   *  Part 2 misspelt, not another film, exactly as `differentInstalments` has always read
   *  it for an uncurated series (f430c1de5). Comparing the words verbatim made the one
   *  correct credit a "sibling" of the typo'd listing, so the director walk refused the
   *  right film and resolved nothing. */
  private final case class Entry(words: Seq[String], number: Option[Int]) {
    def differsFrom(other: Entry): Boolean =
      number != other.number || !services.resolution.TitleMatch.close(words.mkString, other.words.mkString)
  }

  /** WHICH entry of the curated `base`'s series `whole` names — or
   *  `None` when it names none [[namesAnotherEntry]] would recognise. Comparing these,
   *  not the raw tokens after the base, is what keeps a decoration from reading as a
   *  second entry: UK convergence run 35948292875 (2026-09-24) had 64 Flicks venues list
   *  "The Hunger Games: Mockingjay - Part 1 (2026)" and 14 list plain "... - Part 1" on
   *  one row, and a differing-extras test split the 14 off on every settle over nothing
   *  but the rerelease year.
   *
   *  The entry is the curated subtitle (with "and" dropped, so "Songbirds & Snakes" and
   *  "Songbirds and Snakes" are one entry), or the words before a part marker plus the
   *  instalment's VALUE ("Pt 2" and "Part Two" are one entry), or a bare ordinal's value
   *  right after the base. Whatever trails the instalment — a year, "Re-Release" — is not
   *  part of which entry it is. */
  private def entryNamed(base: Seq[String], whole: Seq[String]): Option[Entry] = {
    val extras = whole.drop(base.length)
    val subtitle = curatedSubtitle(base, extras).map(s => Entry(s.filterNot(_ == "and"), None))
    def partThenOrdinal = extras.indices.iterator.flatMap { at =>
      extras.lift(at).filter(PartMarkers.contains)
        .flatMap(_ => extras.lift(at + 1).flatMap(ordinalValue))
        .map(value => Entry(extras.take(at), Some(value)))
    }.nextOption()
    def ordinalRightAfterBase = extras.headOption.filter(isOrdinal).flatMap(ordinalValue).map(v => Entry(Nil, Some(v)))
    subtitle.orElse(partThenOrdinal).orElse(ordinalRightAfterBase)
  }

  /** [[curatedSiblings]] over two sets of raw title strings: does any title on one
   *  side name a different curated-franchise entry from any title on the other?
   *  The shape both `FilmCanonicalizer`'s id-sharing fold guard and
   *  `MixedFilmDetector.conflicting` ask of cinema-published titles. */
  private[movies] def curatedSiblingTitles(a: Iterable[String], b: Iterable[String]): Boolean = {
    val bTokens = b.iterator.map(TitleContainment.tokens).toSeq
    a.iterator.map(TitleContainment.tokens).exists(at => bTokens.exists(curatedSiblings(at, _)))
  }

  /** The instalment `t` numbers itself as: the words before the number (a part marker
   *  right before it dropped) and its VALUE, read past whatever a venue trails after
   *  it — a rerelease year, "Re-Release", a format tag. `None` when no token names an
   *  instalment, when nothing precedes it, or when the words before it themselves end
   *  in a number ("Ghost 2 (1)": which of the two is the instalment is a guess). */
  private def numberedInstalment(t: Seq[String]): Option[(Seq[String], Int)] = {
    def numbersAt(i: Int): Boolean =
      isOrdinal(t(i)) || (WordOrdinals.contains(t(i)) && i > 0 && PartMarkers.contains(t(i - 1)))
    val at = t.indices.lastIndexWhere(numbersAt)
    if (at <= 0) None
    else {
      val before = t.take(at)
      val words  = if (PartMarkers.contains(before.last)) before.init else before
      ordinalValue(t(at)).filter(_ => words.nonEmpty && !isOrdinal(words.last)).map(words -> _)
    }
  }

  /** Two titles of one series (the words before the number agree, up to the spelling
   *  drift `TitleMatch.close` allows) numbering themselves differently, however each
   *  writes the number and whatever either trails after it. The equal-length rule below
   *  cannot see "Kill Bil: Vol. 2 (2026)" beside "Kill Bill: Vol. 1" or "Mockingjay Pt 6"
   *  beside "Mockingjay 3" — the lengths differ — so a rerelease year a venue stamped on
   *  the title (the Flicks/Odeon shape) put a typo'd sequel back inside the director
   *  walk's fuzzy match with its predecessor, and lowest-id handed it the older film. */
  private def numberedDifferently(a: Seq[String], b: Seq[String]): Boolean =
    (numberedInstalment(a), numberedInstalment(b)) match {
      case (Some((wordsA, valueA)), Some((wordsB, valueB))) =>
        valueA != valueB && services.resolution.TitleMatch.close(wordsA.mkString, wordsB.mkString)
      case _ => false
    }

  /** Symmetric check: do `a` and `b` name two DIFFERENT instalments of the same
   *  series — two curated siblings of one franchise base ([[curatedSiblings]]),
   *  either one's tokens containing the other's plus a trailing ordinal
   *  ([[namesAnotherEntry]], either direction), two titles of one series numbered
   *  differently whatever trails the number ([[numberedDifferently]]), or the two
   *  run the same length and END in ordinals naming different NUMBERS ("Part 1"
   *  vs "Part 2", "Rocky II" vs "Rocky III"). Comparing by VALUE, not by raw token, is what keeps a
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
    else if (curatedSiblings(a, b) || numberedDifferently(a, b)) true
    else if (a.length == b.length)
      (ordinalValue(a.last), ordinalValue(b.last)) match {
        case (Some(va), Some(vb)) => va != vb
        case _                    => false
      }
    else if (a.length < b.length) namesAnotherEntry(a, b)
    else namesAnotherEntry(b, a)
}
