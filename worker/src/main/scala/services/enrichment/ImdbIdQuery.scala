package services.enrichment

import models.{EmbeddedYear, MovieRecord}
import services.movies.LatestTitleYear

/**
 * What an IMDb id lookup for one film asks with, read off its row once: the search title and the key's year; every
 * year the search is anchored on ([[ImdbIdQuery.of]]); the film's directors; the titles its venues list it under; and
 * the cross-references its row holds (a Filmweb page, a TMDB id).
 */
final case class ImdbIdQuery(
  searchTitle: String,
  year:        Option[Int],
  years:       Seq[Option[Int]],
  directors:   Set[String],
  venueTitles: Seq[String],
  filmwebUrl:  Option[String],
  tmdbId:      Option[Int]
) {
  /** The titles the other databases are asked under: the search title first, then the venues' own. */
  def crossReferenceTitles: Seq[String] = (searchTitle +: venueTitles).distinct

  /** The film's other venue titles, sorted: the search title is ONE venue's, kept from the first draft that took the
   *  film (`ScopeDrafting`), so which it is depends on history. */
  def otherTitles: Seq[String] = venueTitles.sorted.filterNot(_ == searchTitle)
}

object ImdbIdQuery {

  /** The lookup for `searchTitle` and the key's `year` of the film `record` holds. */
  def of(searchTitle: String, year: Option[Int], record: MovieRecord)(using LatestTitleYear): ImdbIdQuery = {
    val venueTitles = record.evidence.titles.toSeq
    ImdbIdQuery(searchTitle, year, searchYears(searchTitle, year, record), record.director.toSet, venueTitles,
      record.filmwebUrl, record.tmdbId)
  }

  /** The years the IMDb search is anchored on, each its own rung; one yearless search when none is known. A row
   *  no source dated still has a year when its title brackets one ("It (1990)"): without it the search is
   *  yearless, and the lone IMDb film of the bare name binds (It, 2017).
   *
   *  Every year the film's cinemas report (plus the key year), sorted. IMDb's release year can sit at any cinema's
   *  reported (production) year, not the canonical TMDB one ("Chłopiec na krańcach świata": TMDB 2026, IMDb + the
   *  cinemas 2025), so a single-key-year lookup left the id flickering present/absent with arrival order. The sorted
   *  year set is order-independent; the per-year EXACT match still refuses a same-series sibling ("Kicia Kocia w
   *  przedszkolu" 2024) at no reported year. */
  private def searchYears(searchTitle: String, year: Option[Int], record: MovieRecord)(using LatestTitleYear): Seq[Option[Int]] = {
    val reported = record.cinemaData.values.flatMap(_.releaseYear).toSet ++ year
    val years = (if (reported.nonEmpty) reported
                 else EmbeddedYear.ofAll(searchTitle +: record.evidence.titles.toSeq, LatestTitleYear.current).toSet).toSeq.sorted
    if (years.isEmpty) Seq(year) else years.map(Option(_))
  }
}
