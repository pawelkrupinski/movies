package tools

import models.{Cinema, CinemaMovie, CinemaShowing}
import services.identity.{IdSeeding, Listing, PipelineFilms, ProjectionTick}
import services.movies.{ListingKey, StoredMovieRecord, TitleNormalizer}

/**
 * The identity projection's guaranteed properties (docs/design/identity-resolver.md §7), checked
 * over what a cut-over country's worker actually STORED — the specs that boot a corpus with the
 * switch on share them:
 *
 *  - P3 [[cannotLinked]]: no stored film holds two listings the resolution cannot-linked;
 *  - P4 [[lostShowtimes]]: every showtime of every published listing is on the stored film that
 *    holds the listing, at the listing's venue;
 *  - [[misassigned]]: every film keeps the id `IdAssigner` says it keeps over the films before.
 *
 * P1 (order independence) and P2 (fixpoint) compare two boots or two projections; the specs do that
 * with [[films]].
 */
object CutoverProperties {

  /** The stored films as the order-independence check compares them: listings, TMDB film, key and
   *  title, with the film id when `withIds` (a boot from nothing numbers films by their listings). */
  def films(tick: ProjectionTick, withIds: Boolean): Set[String] =
    tick.plan.toSeq.flatMap(_.films).map { f =>
      (if (withIds) s"${f.id} " else "") + s"${f.record.tmdbId.getOrElse("—")} ${f.key} '${f.title}' " +
        f.members.map(ListingKey.serialised).sorted.mkString("[", ", ", "]")
    }.toSet

  /** P3: each pair of listings the resolution cannot-linked that one planned film holds. */
  def cannotLinked(tick: ProjectionTick, listings: Seq[Listing]): Seq[String] = {
    val bySortKey = listings.map(l => l.sortKey -> l.key).toMap
    val filmOf    = tick.plan.toSeq.flatMap(_.films).flatMap(f => f.members.map(_ -> f.id)).toMap
    tick.resolution.toSeq.flatMap(_.edges).filterNot(_.must).flatMap { e =>
      for {
        a <- bySortKey.get(e.a); b <- bySortKey.get(e.b)
        fa <- filmOf.get(a) if filmOf.get(b).contains(fa)
      } yield s"$fa holds ${ListingKey.serialised(a)} and ${ListingKey.serialised(b)}, cannot-linked (${e.reason})"
    }
  }

  /** P4: each published showtime missing from the stored film its listing is on. */
  def lostShowtimes(published: Seq[(Cinema, Seq[CinemaMovie])], tick: ProjectionTick, stored: Seq[StoredMovieRecord]): Seq[String] = {
    val filmOf = tick.plan.toSeq.flatMap(_.films).flatMap(f => f.members.map(_ -> f.id)).toMap
    val byId   = stored.map(r => r.id -> r).toMap
    published.flatMap { case (cinema, films) =>
      films.flatMap { cm =>
        val key  = ListingKey.of(cinema, cm)
        val held = filmOf.get(key).flatMap(byId.get).toSeq.flatMap(_.record.data.collect {
          case (CinemaShowing(c, _), sd) if c == cinema => sd.showtimes.map(_.dateTime)
        }.flatten).toSet
        cm.showtimes.map(_.dateTime).filterNot(held).map(t => s"${cinema.displayName} '${cm.movie.title}' $t (film ${filmOf.get(key)})")
      }
    }
  }

  /** Every showtime of `published` that NO stored film carries at its venue — P4 read without the
   *  projection's own listing-to-film map, for a store another path wrote (a rollback). */
  def unservedShowtimes(published: Seq[(Cinema, Seq[CinemaMovie])], stored: Seq[StoredMovieRecord]): Seq[String] = {
    val held = stored.flatMap(_.record.data.collect { case (CinemaShowing(c, _), sd) => sd.showtimes.map(c -> _.dateTime) }.flatten).toSet
    published.flatMap { case (cinema, films) =>
      films.flatMap(cm => cm.showtimes.map(_.dateTime).filterNot(t => held((cinema, t))).map(t => s"${cinema.displayName} '${cm.movie.title}' $t"))
    }
  }

  /** The films `IdAssigner` keeps over `before` (the films stored before the projection, as listing
   *  sets) whose stored film does not hold exactly the cluster the review gives it. */
  def misassigned(before: Seq[StoredMovieRecord], listings: Seq[Listing], tick: ProjectionTick, after: Seq[StoredMovieRecord],
                  counters: services.identity.FilmIdCounters, normalizer: TitleNormalizer): Seq[String] = {
    val previous = PipelineFilms.of(listings, before, normalizer).toSeq.groupMap(_._2.id)(_._1).toSeq
      .map { case (id, ls) => IdSeeding.Film(id, ls.toSet) }
    val clusters = tick.plan.toSeq.flatMap(_.films).map(_.members.toSet)
    val review   = IdSeeding.review(previous, clusters, counters)
    val members  = tick.plan.toSeq.flatMap(_.films).map(f => f.id.value -> f.members.toSet).toMap
    val storedIds = after.map(_.id.value).toSet
    review.keeps.toSeq.flatMap { case (id, cluster) =>
      if (!storedIds(id)) Some(s"$id should hold ${cluster.size} listing(s) and is not stored")
      else if (!members.get(id).contains(cluster)) Some(s"$id holds ${members.get(id).fold(0)(_.size)} listing(s), IdAssigner gives it ${cluster.size}")
      else None
    }
  }
}
