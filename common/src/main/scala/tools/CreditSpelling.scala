package tools

import java.util.Locale

/**
 * Re-spell a cast/crew list from an AUTHORITATIVE list of the same names.
 *
 * WHY: the display cast is the LONGEST list across sources
 * ([[models.MovieRecord.cast]]), and the longest list is very often a scraped
 * one — TMDB deliberately caps `fullDetails.cast` at a top-N, so a nine-name
 * Flicks list beats TMDB's five every time. That is the right choice for WHICH
 * names to list (more names is more useful) and the wrong one for HOW they are
 * SPELLED: we hold TMDB's properly-cased credits and then show the scraper's
 * spelling anyway.
 *
 * [[PersonName]] fixes the easy half of that at the parse boundary — an
 * all-lowercase `christoph waltz` becomes `Christoph Waltz`. It cannot fix the
 * hard half, because no rule derived from the letters alone knows about a
 * capital INSIDE a word: `leonardo dicaprio` title-cases to `Leonardo Dicaprio`,
 * never `Leonardo DiCaprio`, and a `di`/`mac`/`de` prefix rule that got DiCaprio
 * right would wreck Diaz, Macy and Dean. The fix is not a better rule, it is the
 * name we already hold from a source that spells it correctly.
 *
 * ==The match is EXACT, case- and whitespace-insensitive, and nothing else==
 *
 * A name is re-spelled only when the authoritative list carries a name equal to
 * it after trimming, collapsing internal whitespace runs, and lowercasing. No
 * edit distance, no initials expansion, no surname-only matching, no diacritic
 * folding: `Michał Żebrowski` does NOT match TMDB's `Michal Zebrowski`, and that
 * is deliberate. A missed correction leaves a name exactly as good as it is
 * today; a wrong "correction" renames a person, which is far worse. Lowercasing
 * goes through [[Locale.ROOT]] — a default-locale `toLowerCase` maps `I` to
 * dotless `ı` under Turkish, a flake this repo has already been bitten by.
 *
 * ==What it cannot change==
 *
 * The result is `names` mapped one-to-one, so LENGTH and ORDER are preserved and
 * no name is ever added or dropped. Two entries of the input that match the same
 * authoritative name both take that spelling and both stay — de-duplication is
 * not this function's job, and silently shortening a cast list would be a
 * surprising side effect of a spelling pass. A name the authority does not carry
 * is returned byte-identical, so `PersonName`'s casing still stands behind
 * everything TMDB never heard of.
 *
 * Pure: a function of two lists, no fetching, no caching, no source lookup.
 */
object CreditSpelling {

  /**
   * `names` with every entry the `authoritative` list knows re-spelled the way
   * that list spells it. Entries it doesn't know are returned unchanged.
   */
  def alignedTo(names: Seq[String], authoritative: Seq[String]): Seq[String] = {
    if (names.isEmpty || authoritative.isEmpty) return names
    val spellings = authoritative.iterator
      .map(name => matchKey(name) -> name)
      .filter { case (key, _) => key.nonEmpty }
      .toSeq
      // First spelling wins if the authority itself lists one name twice — the
      // alternative is a last-one-wins map whose answer depends on list order
      // for no reason.
      .distinctBy { case (key, _) => key }
      .toMap
    names.map(name => spellings.getOrElse(matchKey(name), name))
  }

  /** The identity two spellings of one name share: case-folded, trimmed, and
   *  with every internal whitespace run collapsed to a single space, so
   *  `"Sandra  Bullock "` and `"sandra bullock"` are the same person. */
  private def matchKey(name: String): String =
    name.trim.replaceAll("\\s+", " ").toLowerCase(Locale.ROOT)
}
