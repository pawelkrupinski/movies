package services.movies

import tools.{EditDistance, TextNormalization}

/**
 * Do two written credits name the same person?
 *
 * The ONE answer to that question. It used to be asked three ways — a
 * substring-or-token-subset test in `MovieService` when a search hit was verified
 * against a cinema's director, a folding-and-edit-distance test in
 * `CinemaCorroboration` when a resolved row was checked against its cinemas, and a
 * name-shortening in `CrewConfirmation` before asking TMDB — and every shape a feed
 * mangles a credit into was learned by one of them and not the others. Thirteen
 * commits on 2026-09-05 taught the second one initials, surname-first credits,
 * truncations, transliterations and familiar forms; the first never heard of any of
 * them, so a search hit could be rejected by a name the sweep would have accepted.
 *
 * The comparison is deliberately LOOSE. A missed match costs one row a re-check or a
 * search fallback; a false "different person" force-re-resolves a film that was
 * already right, or rejects the one candidate that was correct. Every tolerance
 * below exists for a shape a real feed produced, and each needs LENGTH to earn it so
 * "Bong Joon Ho" and "Bong Joon Il" stay two people.
 *
 * Names that share no letters at all — a pseudonym, two dialects' romanisations —
 * are beyond any comparison of the strings and live in [[DirectorAliases]], which
 * this consults last.
 */
object SamePerson {

  /** True when `a` and `b` name the same person. False when either folds away to
   *  nothing (a CJK credit): the caller that needs to ABSTAIN rather than deny
   *  should test [[tokens]] for emptiness itself, as `CinemaCorroboration` does. */
  def apply(a: String, b: String): Boolean = {
    val (ta, tb) = (tokens(a), tokens(b))
    // Two credits the fold cannot read at all ("王家衛" twice) are still the same
    // credit when they are the same string; only a COMPARISON of unreadable
    // credits has to abstain.
    a.trim.equalsIgnoreCase(b.trim) && a.trim.nonEmpty ||
      ta.nonEmpty && tb.nonEmpty && sameTokens(ta, tb)
  }

  /** A credit as its SEQUENCE of name tokens — case- and diacritic-folded,
   *  punctuation dropped. Compared as a set in [[sameTokens]] because the two sides
   *  do not agree on ORDER: TMDB writes Hungarian and Japanese credits surname-first
   *  ("Enyedi Ildikó", "Szabó István") where the cinemas write them given-name-first.
   *  Comparing folded strings made every one of those a contradiction — 191 of the
   *  202 rows the first sweep flagged were correctly resolved films whose director
   *  had simply been written the other way round.
   *
   *  Empty for a name that folds away entirely, which is how a CJK credit behaves:
   *  "王家衛" and "Wong Kar Wai" are the same person and nothing here can know it. */
  def tokens(name: String): Seq[String] =
    foldUndecomposed(TextNormalization.deburr(name)).toLowerCase.split("[^a-z0-9]+").filter(_.nonEmpty).toSeq

  /** Whether two already-tokenised credits name the same person. */
  def sameTokens(a: Seq[String], b: Seq[String]): Boolean =
    // Whole-string first: the two sides may split a name differently — a hyphenated
    // surname ("Amrou Al-Kadhi" / "Amrou Alkadhi"), or a Tamil name written as one
    // word with a given name TMDB omits ("Mathi Maran" / "Pugazhendhi Mathimaran").
    // Token-wise both look like an extra word; written out one contains the other.
    joinedMatch(a.mkString, b.mkString) || covers(a, b) || covers(b, a) ||
      sameFamiliarForm(a, b) || DirectorAliases.sameDirector(a.mkString, b.mkString)

  /** Honorific / generational suffixes, dropped before a credit is compared or
   *  shortened so they cannot pose as the surname. */
  val Suffixes: Set[String] = Set("jr", "sr", "ii", "iii", "iv")

  /** Nobiliary and patronymic particles: a middle word that belongs to the SURNAME
   *  rather than being a middle name, so shortening across it renames the person. */
  val Particles: Set[String] = Set(
    "von", "van", "de", "del", "della", "der", "den", "di", "da", "dos", "das",
    "du", "la", "le", "el", "al", "bin", "ibn", "ben", "af", "av", "ter", "te", "zu")

  /** "David Kerrick Hand" as "David Hand" — the first and last of three or more
   *  SUFFIX-FREE tokens, the working form TMDB tends to hold where a venue writes
   *  the name in full. Fewer than three has no middle name to drop, and MUST not be
   *  shortened: "Robert Downey Jr." minus its suffix is "Robert Downey", his FATHER,
   *  whom TMDB ranks first because he is a director. Never across a nobiliary
   *  particle either: "Lars von Trier" shortened to "Lars Trier" is a different
   *  person if TMDB has one at all. None when there is nothing safe to drop. */
  def withoutMiddleNames(name: String): Option[String] = {
    val words = name.split("\\s+").filter(_.nonEmpty)
    val core  = words.filterNot(w => Suffixes.contains(foldWord(w)))
    Option.when(core.length >= 3)(s"${core.head} ${core.last}")
      .filterNot(_ => core.tail.dropRight(1).exists(w => Particles.contains(foldWord(w))))
  }

  private def foldWord(word: String): String = word.toLowerCase.stripSuffix(".")

  /** Letters NFD leaves alone because they are distinct letters rather than an
   *  accented base, so `deburr` passes them through and the ASCII split would
   *  simply DELETE them: "Fatih Akın" became "fatih ak" and read as a different
   *  person from "Fatih Akin". Folded here rather than in `deburr`, which is frozen —
   *  `TitleRuleKey` derives stored rule keys from it, and widening it re-keys every
   *  title rule in prod. */
  private def foldUndecomposed(s: String): String =
    s.replace('ı', 'i').replace('İ', 'i')
      .replace('ø', 'o').replace('Ø', 'o')
      .replace('đ', 'd').replace('Đ', 'd')
      .replace("ß", "ss")

  /** One written-out credit standing for the other. Containment rather than
   *  equality catches a name the venue joins and prefixes; the length floor keeps a
   *  short token from being swallowed by an unrelated longer one. */
  private def joinedMatch(a: String, b: String): Boolean =
    a == b || (a.length >= 8 && b.contains(a)) || (b.length >= 8 && a.contains(b))

  /** Same SURNAME and a compatible first initial — the shape a familiar form
   *  takes: "Tom Donnelly" for "Thomas Michael Donnelly", "Dave Derrick Jr." for
   *  "David G. Derrick Jr.". Nicknames are not derivable from the formal name, so
   *  no prefix or edit distance reaches them; the surname carries the identity and
   *  the initial guards it. "Andrzej Wajda" and "Andrzej Żuławski" share a first
   *  name and NOT a surname, so they stay two people. */
  private def sameFamiliarForm(a: Seq[String], b: Seq[String]): Boolean = {
    val an = a.filterNot(Suffixes.contains)
    val bn = b.filterNot(Suffixes.contains)
    an.length >= 2 && bn.length >= 2 &&
      sameToken(an.last, bn.last) &&
      an.head.headOption == bn.head.headOption
  }

  /** Every token of `narrow` accounted for by some token of `wide` — so a middle
   *  name present on one side only ("Neele Leana Vollmar" / "Neele Vollmar") is not
   *  a different director, and an initial matches the name it abbreviates
   *  ("Alejandro G. Iñárritu" / "Alejandro González Iñárritu"). An initial only ever
   *  matches ALONGSIDE the rest of the credit, so "A. Wajda" cannot become "Louisa
   *  Proske" on the strength of a shared letter. */
  private def covers(narrow: Seq[String], wide: Seq[String]): Boolean =
    narrow.forall(t => wide.exists(sameToken(t, _)))

  /** One name token standing for another. Beyond equality this forgives the ways
   *  upstream feeds mangle a credit, none of which says a different person:
   *
   *    - an INITIAL for the name it abbreviates ("Alejandro G." / "González");
   *    - a TRUNCATION, which arrives identically from every venue on a feed —
   *      "Michael Gottli" from five Arc cinemas, "Pedro Almod" cut at the accent;
   *    - a ONE-LETTER misspelling — "Paul Verhoven" for Verhoeven, from six
   *      unrelated UK venues, so a feed's error rather than a venue's;
   *    - two transliterations of one long surname ("Tarkowski" / "Tarkovsky"),
   *      two edits apart, which two DIFFERENT surnames that long rarely are.
   *
   *  Every tolerance needs length to earn it — a prefix 5+, a one-letter miss 4+,
   *  two edits 7+. */
  private def sameToken(a: String, b: String): Boolean =
    a == b ||
      (a.length == 1 && b.startsWith(a)) || (b.length == 1 && a.startsWith(b)) ||
      (a.length >= 5 && b.startsWith(a)) || (b.length >= 5 && a.startsWith(b)) ||
      (a.length >= 4 && b.length >= 4 && EditDistance.within(a, b, 1)) ||
      (a.length >= 7 && b.length >= 7 && EditDistance.within(a, b, 2))
}
