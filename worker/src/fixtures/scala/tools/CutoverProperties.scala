package tools

import models.{Cinema, CinemaMovie, CinemaShowing}
import services.identity.{Listing, ProjectionTick}
import services.movies.{ListingKey, StoredMovieRecord}

/**
 * The identity projection's guaranteed properties (docs/design/identity-resolver.md §7), checked
 * over what a country's worker actually STORED — the specs that boot a corpus share them:
 *
 *  - P3 [[cannotLinked]]: no stored film holds two listings the resolution cannot-linked;
 *  - P4 [[lostShowtimes]]: every showtime of every published listing is on the stored film that
 *    holds the listing, at the listing's venue.
 *
 * P1 (order independence) and P2 (fixpoint) compare two boots or two projections; the specs do that
 * with [[films]].
 */
object CutoverProperties {

  /** The stored films as the order-independence check compares them: listings, TMDB film, key and
   *  title, with the film id when `withIds` (a boot from nothing numbers films by their listings).
   *  Without ids, a key's twin disambiguator — the film's counter, so its history — is left out too. */
  def films(tick: ProjectionTick, withIds: Boolean): Set[String] =
    tick.plan.toSeq.flatMap(_.films).map { f =>
      val key = if (withIds) f.key else f.key.replaceFirst("~\\d+\\|", "~|")
      (if (withIds) s"${f.id} " else "") + s"${f.record.tmdbId.getOrElse("—")} $key '${f.title}' " +
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
}
