package services.movies

import models.{MovieRecord, Tmdb}
import services.resolution.TmdbBasis
import tools.TextNormalization

/**
 * Does a row's own resolution survive contact with what its CINEMAS published?
 *
 * `resolveTmdbId` never re-runs once a `tmdbId` is set, so a wrong answer stands
 * for ever. Prod, 2026-09-05, carried five of them: "Vivaldi i ja" on an
 * 18-minute STABAT MATER concert short while 46 venues advertised 110 minutes and
 * named Damiano Michieletto; "Das Phantom der Oper" on the 1925 silent while ten
 * venues published 2004, 140 minutes and Joel Schumacher. Every one needed a hand
 * repair, and the evidence to catch every one was on the row the whole time.
 *
 * They arise because a deferred-detail cinema's FIRST scrape carries a title and
 * nothing else — year, director and runtime all arrive later with the detail — so
 * the first resolve is a title-only search, and `MovieCache.settleResolved` then
 * stamps that guess's year into the row's key, promoting it from guess to
 * identity. This is the other half of the loop: once the detail HAS landed, ask
 * again whether the cinemas agree.
 *
 * Both signals demand POSITIVE contradiction and abstain otherwise. A venue that
 * published nothing is not disagreeing, and read the other way this would
 * force-re-resolve the corpus.
 */
object CinemaCorroboration {

  /** True when the row's conclusion is weaker than the evidence it now holds — a
   *  bare-title guess on a row that has SINCE acquired a director or a year.
   *
   *  Resolution is a one-shot, and nothing makes it look again:
   *  `needsTmdbResolution` re-verifies only when the triggering event carries a
   *  director, and the later detail refresh that finally supplies one publishes no
   *  event at all. So the row keeps a guess it could now improve on — which is how
   *  every one of prod's five mis-resolved films stayed wrong while sitting on the
   *  very hints that would have corrected them.
   *
   *  Converges rather than churning: a re-resolve concludes on the stronger basis
   *  (or fails and leaves the row unresolved, which the sweep's first predicate
   *  owns), so a given row satisfies this at most once. */
  def resolvedOnWeakerEvidenceThanAvailable(record: MovieRecord): Boolean =
    record.tmdbId.isDefined &&
      record.tmdbBasis.flatMap(TmdbBasis.parse).contains(TmdbBasis.TitleOnly) &&
      (record.cinemaDirector.nonEmpty || record.cinemaYears.nonEmpty)

  /** True when the cinemas positively contradict the film this row resolved to. */
  def contradicts(record: MovieRecord): Boolean =
    record.tmdbId.isDefined && record.data.get(Tmdb).exists { film =>
      runtimeDenies(record, film.runtimeMinutes) || directorDenies(record, film.director)
    }

  /** The runtime band is deliberately wide (see [[RuntimeCorroboration.plausible]]):
   *  cinemas pad, round and shave, and Multikino advertises the 162-minute "Lalka"
   *  at 147. Only a category error trips it. */
  private def runtimeDenies(record: MovieRecord, filmRuntime: Option[Int]): Boolean =
    !RuntimeCorroboration.plausible(record.cinemaRuntimesMinutes, filmRuntime)

  /** Directors speak only when BOTH sides name someone: a film TMDB credits to
   *  nobody, or one no venue credits, says nothing either way. A contradiction
   *  needs EVERY cinema credit to match no film credit at all. */
  private def directorDenies(record: MovieRecord, filmDirectors: Seq[String]): Boolean = {
    val cinemaNames = record.cinemaDirector.map(nameTokens).filter(_.nonEmpty)
    val filmNames   = filmDirectors.map(nameTokens).filter(_.nonEmpty)
    cinemaNames.nonEmpty && filmNames.nonEmpty &&
      !cinemaNames.exists(c => filmNames.exists(f => samePerson(c, f)))
  }

  /** A credit as its SET of name tokens — case- and diacritic-folded, punctuation
   *  dropped. A set rather than a string because the two sides do not agree on
   *  ORDER: TMDB writes Hungarian and Japanese credits surname-first ("Enyedi
   *  Ildikó", "Szabó István", "Pálfi György") where the cinemas write them
   *  given-name-first. Comparing folded strings made every one of those a
   *  contradiction — 191 of the 202 rows the first version of this flagged in prod
   *  were correctly resolved films whose director had simply been written the other
   *  way round.
   *
   *  Empty for a name that folds away entirely, which is how a CJK credit behaves:
   *  "王家衛" and "Wong Kar Wai" are the same person and nothing here can know it,
   *  so that comparison must abstain rather than guess. */
  private def nameTokens(name: String): Seq[String] =
    foldUndecomposed(TextNormalization.deburr(name)).toLowerCase.split("[^a-z0-9]+").filter(_.nonEmpty).toSeq

  /** Letters NFD leaves alone because they are distinct letters rather than an
   *  accented base, so `deburr` passes them through and the ASCII split below
   *  simply DELETES them: "Fatih Akın" became "fatih ak" and read as a different
   *  person from "Fatih Akin". Folded here rather than in `deburr`, which is
   *  frozen — `TitleRuleKey` derives stored rule keys from it, and widening it
   *  re-keys every title rule in prod. */
  private def foldUndecomposed(s: String): String =
    s.replace('ı', 'i').replace('İ', 'i')
      .replace('ø', 'o').replace('Ø', 'o')
      .replace('đ', 'd').replace('Đ', 'd')
      .replace("ß", "ss")

  /** Honorific suffixes, dropped before a credit is compared so they cannot pose
   *  as the surname. */
  private val Suffixes = Set("jr", "sr", "ii", "iii", "iv")

  /** Same SURNAME and a compatible first initial — the shape a familiar form
   *  takes: "Tom Donnelly" for "Thomas Michael Donnelly", "Dave Derrick Jr." for
   *  "David G. Derrick Jr.". Nicknames are not derivable from the formal name, so
   *  no amount of prefix or edit distance reaches them; the surname carries the
   *  identity and the initial guards it. "Andrzej Wajda" and "Andrzej Żuławski"
   *  share a first name and NOT a surname, so they stay two people. */
  private def sameFamiliarForm(a: Seq[String], b: Seq[String]): Boolean = {
    val an = a.filterNot(Suffixes.contains)
    val bn = b.filterNot(Suffixes.contains)
    an.length >= 2 && bn.length >= 2 &&
      sameToken(an.last, bn.last) &&
      an.head.headOption == bn.head.headOption
  }

  /** One credit naming the same person as the other. Subset rather than equality
   *  so a middle name present on one side only ("Neele Leana Vollmar" against
   *  TMDB's "Neele Vollmar") is not a different director, and a single-letter token
   *  matches the name it abbreviates so "Alejandro G. Iñárritu" and "Alejandro
   *  González Iñárritu" are one person.
   *
   *  An initial only ever matches ALONGSIDE the rest of the credit — every other
   *  token must still be accounted for — so "A. Wajda" cannot become "Louisa
   *  Proske" on the strength of a shared letter. */
  private def samePerson(a: Seq[String], b: Seq[String]): Boolean =
    // Whole-string first: the two sides may split a hyphenated surname differently
    // ("Amrou Al-Kadhi" / "Amrou Alkadhi"), which token-wise looks like an extra
    // word and written out is the same name.
    a.mkString == b.mkString || covers(a, b) || covers(b, a) || sameFamiliarForm(a, b)

  /** Every token of `narrow` accounted for by some token of `wide`. */
  private def covers(narrow: Seq[String], wide: Seq[String]): Boolean =
    narrow.forall(t => wide.exists(sameToken(t, _)))

  /** One name token standing for another. Beyond equality this forgives the three
   *  ways upstream feeds mangle a credit, none of which says a different person:
   *
   *    - an INITIAL for the name it abbreviates ("Alejandro G." / "González");
   *    - a TRUNCATION, which arrives identically from every venue on a feed —
   *      "Michael Gottli" from five Arc cinemas, "Pedro Almod" cut at the accent;
   *    - a ONE-LETTER misspelling — "Paul Verhoven" for Verhoeven, from six
   *      unrelated UK venues, so a feed's error rather than a venue's.
   *
   *  Both tolerances need length to earn them: a prefix must be 5+ characters and a
   *  near-miss 6+, so "Bong Joon Ho" and "Bong Joon Il" stay two people. The whole
   *  point of this comparison is to abstain unless the names genuinely differ. */
  private def sameToken(a: String, b: String): Boolean =
    a == b ||
      (a.length == 1 && b.startsWith(a)) || (b.length == 1 && a.startsWith(b)) ||
      (a.length >= 5 && b.startsWith(a)) || (b.length >= 5 && a.startsWith(b)) ||
      (a.length >= 6 && b.length >= 6 && withinOneEdit(a, b)) ||
      // Two transliterations of one long surname ("Tarkowski" / "Tarkovsky") sit two
      // edits apart, and two DIFFERENT surnames that long rarely do.
      (a.length >= 8 && b.length >= 8 && withinEdits(a, b, 2))

  /** True when `a` and `b` are at most one insertion, deletion or substitution
   *  apart. Bounded and allocation-free: the only distance that matters here is
   *  "one", so a longer walk is abandoned as soon as a second difference shows. */
  private def withinOneEdit(a: String, b: String): Boolean = withinEdits(a, b, 1)

  /** True when `a` and `b` are at most `max` edits apart. Bounded: the walk is
   *  abandoned as soon as the budget is spent. */
  private def withinEdits(a: String, b: String, max: Int): Boolean = {
    if (math.abs(a.length - b.length) > max) return false
    var i = 0
    var j = 0
    var edits = 0
    while (i < a.length && j < b.length) {
      if (a.charAt(i) == b.charAt(j)) { i += 1; j += 1 }
      else {
        edits += 1
        if (edits > max) return false
        if (a.length == b.length) { i += 1; j += 1 }        // substitution
        else if (a.length > b.length) i += 1                 // deletion from a
        else j += 1                                          // insertion into a
      }
    }
    edits + (a.length - i) + (b.length - j) <= max
  }
}
