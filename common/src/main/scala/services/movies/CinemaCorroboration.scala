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
  private def nameTokens(name: String): Set[String] =
    TextNormalization.deburr(name).toLowerCase.split("[^a-z0-9]+").filter(_.nonEmpty).toSet

  /** One credit naming the same person as the other. Subset rather than equality
   *  so a middle name present on one side only ("Neele Leana Vollmar" against
   *  TMDB's "Neele Vollmar") is not a different director. */
  private def samePerson(a: Set[String], b: Set[String]): Boolean =
    a.subsetOf(b) || b.subsetOf(a)
}
