package services.cinemas.common

import models.{Cinema, CinemaMovie}

/**
 * One venue that its source lists more than once — Końskie's culture centre has
 * a biletyna page per hall ("sala kinowa", "sala widowiskowa"). Each listing is
 * scraped by its own client, stamped for this ONE cinema, and a film screened in
 * both halls becomes one film with both halls' showtimes. Two roster entries
 * would show one venue twice under two names, which the roster audit refuses.
 *
 * Every listing must answer: a failing one fails the fetch (and is retried), so
 * a hall is never silently pruned for the other hall's programme.
 */
class MultiListingScraper(override val cinema: Cinema, val listings: Seq[CinemaScraper]) extends CinemaScraper {
  require(listings.nonEmpty, s"${cinema.displayName}: no listings")
  require(listings.forall(_.cinema == cinema), s"${cinema.displayName}: every listing must scrape this cinema")

  def scrapeHosts: Set[String] = listings.flatMap(_.scrapeHosts).toSet
  override def maxFetchAttempts: Int = listings.map(_.maxFetchAttempts).max
  override def chain: Boolean = listings.exists(_.chain)
  override def listingIsComplete: Boolean = listings.forall(_.listingIsComplete)
  override def sourceUrl: Option[String] = listings.flatMap(_.sourceUrl).headOption
  /** The listings' keys together. The roster audit reads [[listings]] to hold
   *  each one unique on its own. */
  override def sourceKey: Option[String] = Some(listings.flatMap(_.sourceKey).mkString(" + ")).filter(_.nonEmpty)

  def fetch(): Seq[CinemaMovie] =
    listings.flatMap(_.fetch()).groupBy(_.movie.title).values.toSeq.map { same =>
      same.head.copy(showtimes = same.flatMap(_.showtimes).distinct.sortBy(_.dateTime))
    }.sortBy(_.movie.title)
}
