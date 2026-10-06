package services.review

import services.identity.ResolverDecision

import java.time.Instant

/** What a venue said about a listing, in one place it said it (its listing row, or its own film page). */
final case class VenueFacts(title: Option[String] = None, originalTitle: Option[String] = None, year: Option[Int] = None,
                            directors: Seq[String] = Nil, runtime: Option[Int] = None, cast: Seq[String] = Nil,
                            countries: Seq[String] = Nil, poster: Option[String] = None, synopsis: Option[String] = None) {
  def isEmpty: Boolean = this == VenueFacts()
}

/** A listing's slot row (`movie_slots`): the venue's facts as its scrape landed them, and when the row was written. */
final case class SlotFacts(facts: VenueFacts, updatedAt: Instant)

/** A listing as its venue's last scrape (`identity_listings`) holds it: the catalogue ids the venue's
 *  feed names it by (a chain's or aggregator's own id — NOT the venue's facts) and its screenings. */
final case class ListingFeed(catalogueIds: Seq[(String, String)], screenings: Int, first: Option[String], last: Option[String]) {
  def catalogue: String = catalogueIds.map { case (source, id) => s"$source=$id" }.mkString(", ")
}

/** A film as the corpus knows it (`movies` + `web_movies`, else its TMDB slot) — never a live TMDB call. */
final case class FilmCard(tmdb: Int, imdb: Option[String], title: Option[String], originalTitle: Option[String],
                          year: Option[Int], directors: Seq[String], runtime: Option[Int], poster: Option[String],
                          overview: Option[String]) {
  def facts: FilmFacts = FilmFacts(FilmRef.tmdb(tmdb), title, year, directors)
}

/**
 * The storage seam of ONE country's review pages: the reads, and nothing about what they mean.
 * Every read is bounded by the keys it is given, or paged.
 */
trait ReviewSource {
  /** The model's decisions — only those of families holding an unmatched one when `unmatchedOnly`. */
  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision]
  /** The slot rows of these listings (by [[services.movies.ListingKey.serialised]]). */
  def slots(listingKeys: Seq[String]): Map[String, SlotFacts]
  /** When each listing slot row written since `since` was written, by its serialised listing key. */
  def updatedSince(since: Instant): Map[String, Instant]
  /** The venue film pages read at these URLs (`venue_pages`). */
  def venuePages(urls: Seq[String]): Map[String, VenueFacts]
  /** Each `(venue, raw title)` as the venue's last scrape lists it. */
  def feeds(listings: Seq[(String, String)]): Map[(String, String), ListingFeed]
  /** The corpus's record of each of these TMDB films that it holds. */
  def films(tmdbIds: Seq[Int]): Map[Int, FilmCard]
}

object ReviewSource {
  /** A country with no mirror to read: every read is empty. */
  val empty: ReviewSource = new ReviewSource {
    def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] = Nil
    def slots(listingKeys: Seq[String]): Map[String, SlotFacts] = Map.empty
    def updatedSince(since: Instant): Map[String, Instant] = Map.empty
    def venuePages(urls: Seq[String]): Map[String, VenueFacts] = Map.empty
    def feeds(listings: Seq[(String, String)]): Map[(String, String), ListingFeed] = Map.empty
    def films(tmdbIds: Seq[Int]): Map[Int, FilmCard] = Map.empty
  }
}
