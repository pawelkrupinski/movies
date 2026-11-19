package services.review

import models.ListingKey

import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

/** What the cards of the clusters on screen read from their [[ReviewSource]] — and only theirs: their members' slot
 *  rows, venue pages and feeds, and the films they weigh. */
final case class ReviewCardFacts(slots: Map[String, SlotFacts], pages: Map[String, VenueFacts],
                                 feeds: Map[(String, String), ListingFeed], films: Map[Int, FilmCard]) {

  /** One member listing as its card shows it. */
  def member(k: ListingKey): MemberView = MemberView(k, slots.get(ListingKey.serialised(k)),
    k match { case ListingKey.Native(_, page, _) => pages.get(page); case _ => None }, feeds.get(k.venue -> k.rawTitle))
}

object ReviewCardFacts {

  /** The facts of `clusters`' cards, the five reads side by side: a page of cards waits on the slowest, not on their sum. */
  def read(source: ReviewSource, clusters: Seq[(ReviewCluster, Option[Instant])]): ReviewCardFacts = {
    val keys   = clusters.flatMap(_._1.members)
    val ids    = clusters.flatMap { case (c, _) => c.film.toSeq ++ c.candidates.map(_.film) }.distinct
    def reading[A](read: => A): Future[A] = Future(blocking(read))(using ExecutionContext.global)
    val slotsRead   = reading(source.slots(keys.map(ListingKey.serialised)))
    val pagesRead   = reading(source.venuePages(keys.collect { case ListingKey.Native(_, page, _) => page }))
    val feedsRead   = reading(source.feeds(keys.map(k => k.venue -> k.rawTitle)))
    val filmsRead   = reading(source.films(ids))
    val recordsRead = reading(source.filmRecords(ids))
    def read[A](f: Future[A]): A = Await.result(f, Duration.Inf)
    ReviewCardFacts(read(slotsRead), read(pagesRead), read(feedsRead), withRecords(read(filmsRead), read(recordsRead)))
  }

  /** Each film as the resolver weighed it — its stored TMDB record's title, year, directors, running time and cast — with the
   *  corpus's poster (else the record's) and overview (never a live TMDB call); the corpus's facts only where no record
   *  states them. */
  def withRecords(corpus: Map[Int, FilmCard], records: Map[Int, FilmCard]): Map[Int, FilmCard] =
    (corpus.keySet ++ records.keySet).toSeq.flatMap { tmdb =>
      ((records.get(tmdb), corpus.get(tmdb)) match {
        case (Some(r), Some(c)) => Some(FilmCard(tmdb, c.imdb.orElse(r.imdb), r.title.orElse(c.title), r.originalTitle.orElse(c.originalTitle),
          r.year.orElse(c.year), if (r.directors.nonEmpty) r.directors else c.directors, r.runtime.orElse(c.runtime), c.poster.orElse(r.poster), c.overview,
          if (r.cast.nonEmpty) r.cast else c.cast))
        case (r, c) => r.orElse(c)
      }).map(tmdb -> _)
    }.toMap
}
