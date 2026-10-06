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
final case class ListingFeed(catalogueIds: Seq[services.identity.CatalogueId], screenings: Int, first: Option[String], last: Option[String]) {
  def catalogue: String = catalogueIds.map(id => s"${id.source}=${id.id}").mkString(", ")
}

object ListingFeed {
  /** Catalogue ids however a document spells them: a map (`{"flicks": "29423"}`, how the scrape archive stores
   *  `externalIds`), one `{source, id}` object, a list of those or of `source:id` strings, or one such string. A value
   *  of any other shape is no id, never a failed card. */
  def catalogueIdsOf(value: org.bson.BsonValue): Seq[services.identity.CatalogueId] = {
    import scala.jdk.CollectionConverters._
    def text(v: org.bson.BsonValue): Option[String] =
      if (v.isString) Some(v.asString.getValue.trim).filter(_.nonEmpty)
      else if (v.isNumber) Some(v.asNumber.longValue.toString) else None
    def fromString(s: String) = s.split(":", 2) match {
      case Array(source, id) if source.nonEmpty && id.nonEmpty => Some(services.identity.CatalogueId(source, id))
      case _                                                   => None
    }
    Option(value).toSeq.flatMap { v =>
      if (v.isArray) v.asArray.getValues.asScala.toSeq.flatMap(catalogueIdsOf)
      else if (v.isDocument) {
        val d = v.asDocument
        if (d.containsKey("source") && d.containsKey("id"))
          (for { s <- text(d.get("source")); i <- text(d.get("id")) } yield services.identity.CatalogueId(s, i)).toSeq
        else d.asScala.toSeq.flatMap { case (source, id) => text(id).map(services.identity.CatalogueId(source, _)) }
      }
      else text(v).flatMap(fromString).toSeq
    }.distinct.sortBy(id => (id.source, id.id))
  }
}

/** A film as the review card shows it: the resolver's stored TMDB record (`tmdb_films`) and the corpus's (`movies` + `web_movies`, else
 *  its TMDB slot), merged by [[ReviewCards.withRecords]] — never a live TMDB call. */
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
  /** The resolver's own stored TMDB record of each of these films (`tmdb_films`) — its title, original title, year,
   *  directors, running time and IMDb id; no poster or overview — the films it weighed whether or not the corpus has them. */
  def filmRecords(tmdbIds: Seq[Int]): Map[Int, FilmCard]
  /** For each corpus record naming one of `refs` (by its TMDB, IMDb, Filmweb, RT or Metacritic id), every ref it names. */
  def filmLinks(refs: Seq[FilmRef]): Seq[Set[FilmRef]]
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
    def filmRecords(tmdbIds: Seq[Int]): Map[Int, FilmCard] = Map.empty
    def filmLinks(refs: Seq[FilmRef]): Seq[Set[FilmRef]] = Nil
  }
}
