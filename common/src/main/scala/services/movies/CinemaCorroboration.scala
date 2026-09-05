package services.movies

import models.{MovieRecord, Tmdb}
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
   *  nobody, or one no venue credits, says nothing either way. */
  private def directorDenies(record: MovieRecord, filmDirectors: Seq[String]): Boolean = {
    val cinemaNames = record.cinemaDirector.map(nameKey).filter(_.nonEmpty).toSet
    val filmNames   = filmDirectors.map(nameKey).filter(_.nonEmpty).toSet
    cinemaNames.nonEmpty && filmNames.nonEmpty && !cinemaNames.exists(filmNames.contains)
  }

  /** Fold a credit the way the rating clients already compare names — case- and
   *  diacritic-insensitive, punctuation dropped — so "Gastón Duprat" and "Gaston
   *  Duprat" are one person and a stray comma can't split a credit. */
  private def nameKey(name: String): String =
    TextNormalization.deburr(name).toLowerCase.replaceAll("[^a-z0-9]", "")
}
