package services.movies

import models.{Cinema, CinemaMovie}

/**
 * A venue's LISTING, identified by what the venue itself published — never by a title the
 * pipeline normalised or a year it derived. The identity resolver's unit of evidence
 * (docs/design/identity-resolver.md, "Listing identity"): slots and showtimes are to be keyed
 * by it, so a resolution that moves a listing between films can never re-key, move or delete
 * its showtimes.
 *
 * Two shapes, by what the venue gives:
 *
 *  - [[ListingKey.Native]] — the venue publishes a page for the listing (`filmUrl`). The page
 *    alone is NOT enough: KinoPort, Kino Studio Opole and DK Łapy link every film of a month
 *    to one repertoire page (the recorded PL corpus holds "Opętanie" 1981 and "Zawieście
 *    czerwone latarnie" 1991 under one URL), so the key is the page AND the venue's raw title.
 *  - [[ListingKey.Published]] — no page. The raw title is not enough either: Cinema-Arthouse
 *    and Schauburg Karlsruhe list "Sinn und Sinnlichkeit" twice (Ang Lee 1995 and Georgia
 *    Oakley 2026), Club Manufaktur "Bad Apples" twice (2018 and 2025), with no page and one
 *    spelling each. What tells them apart is the year and director the VENUE printed, so they
 *    are part of the key. That is a published year, not a derived one: a TMDB answer, a
 *    bracket the pipeline parsed, or a fold never reaches it.
 *
 * The price of the second shape: when a page-less venue corrects its own year or director the
 * listing gets a new key, and its film keeps its id only through its other listings (the
 * resolver's overlap rule). A page-bearing venue's listing survives any such correction.
 *
 * `ListingKeyCorpusSpec` proves the key unique per distinct listing over every recorded corpus,
 * and that each naive key (the production slot key, venue + raw title, venue + page) is not.
 */
sealed trait ListingKey {
  def venue: String
  def rawTitle: String
}

object ListingKey {

  final case class Native(venue: String, nativeId: String, rawTitle: String) extends ListingKey

  final case class Published(venue: String, rawTitle: String, year: Option[Int], directors: Seq[String]) extends ListingKey

  /** The key of `cm` as `cinema` lists it. Directors are sorted, because a venue's credit
   *  order is presentation, not identity; blank page and names count as absent. */
  def of(cinema: Cinema, cm: CinemaMovie): ListingKey = {
    val raw = cm.movie.rawTitle.getOrElse(cm.movie.title)
    cm.filmUrl.map(_.trim).filter(_.nonEmpty) match {
      case Some(page) => Native(cinema.displayName, page, raw)
      case None       => Published(cinema.displayName, raw, cm.movie.releaseYear,
        cm.director.map(_.trim).filter(_.nonEmpty).distinct.sorted)
    }
  }

  implicit val ordering: Ordering[ListingKey] = Ordering.by {
    case Native(v, id, raw)          => (v, 0, id, raw, "", "")
    case Published(v, raw, year, ds) => (v, 1, "", raw, year.fold("")(_.toString), ds.mkString("\u0000"))
  }
}
