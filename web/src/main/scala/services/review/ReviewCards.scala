package services.review

import models.ListingKey

import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

/** Builds the cards of the clusters on screen — and reads only theirs. */
object ReviewCards {
  /** How many of a cast's names a card shows before folding the rest behind "+N more". */
  val CastShown = 5

  def build(source: ReviewSource, clusters: Seq[(ReviewCluster, Option[Instant])], labels: Seq[LabelRow],
            answers: ReviewAnswers.Index): Seq[ReviewCard] = {
    val keys   = clusters.flatMap(_._1.members)
    val ids    = clusters.flatMap { case (c, _) => c.film.toSeq ++ c.candidates.map(_.film) }.distinct
    // the five reads side by side: a page of cards waits on the slowest, not on their sum
    def reading[A](read: => A): Future[A] = Future(blocking(read))(using ExecutionContext.global)
    val slotsRead   = reading(source.slots(keys.map(ListingKey.serialised)))
    val pagesRead   = reading(source.venuePages(keys.collect { case ListingKey.Native(_, page, _) => page }))
    val feedsRead   = reading(source.feeds(keys.map(k => k.venue -> k.rawTitle)))
    val filmsRead   = reading(source.films(ids))
    val recordsRead = reading(source.filmRecords(ids))
    def read[A](f: Future[A]): A = Await.result(f, Duration.Inf)
    val (slots, pages, feeds) = (read(slotsRead), read(pagesRead), read(feedsRead))
    val films  = withRecords(read(filmsRead), read(recordsRead))
    clusters.map { case (cluster, at) =>
      val members = cluster.members.map(k => MemberView(k, slots.get(ListingKey.serialised(k)),
        k match { case ListingKey.Native(_, page, _) => pages.get(page); case _ => None }, feeds.get(k.venue -> k.rawTitle)))
      val code = cluster.country.code
      val overlay = labels.filter(l => l.country == code &&
        cluster.members.exists(m => m.rawTitle == l.rawTitle && (l.venue == "*" || l.venue == m.venue)))
      ReviewCard(cluster, members, films, overlay, answers.answerFor(cluster), at)
    }
  }

  /** Each film as the resolver weighed it — its stored TMDB record's title, year, directors, running time and cast — with the
   *  corpus's poster (else the record's) and overview (never a live TMDB call); the corpus's facts only where no record
   *  states them. */
  private[review] def withRecords(corpus: Map[Int, FilmCard], records: Map[Int, FilmCard]): Map[Int, FilmCard] =
    (corpus.keySet ++ records.keySet).toSeq.flatMap { tmdb =>
      ((records.get(tmdb), corpus.get(tmdb)) match {
        case (Some(r), Some(c)) => Some(FilmCard(tmdb, c.imdb.orElse(r.imdb), r.title.orElse(c.title), r.originalTitle.orElse(c.originalTitle),
          r.year.orElse(c.year), if (r.directors.nonEmpty) r.directors else c.directors, r.runtime.orElse(c.runtime), c.poster.orElse(r.poster), c.overview,
          if (r.cast.nonEmpty) r.cast else c.cast))
        case (r, c) => r.orElse(c)
      }).map(tmdb -> _)
    }.toMap
}
