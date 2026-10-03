package services.identity

import models.{Cinema, CinemaMovie}
import services.scrapes.{ContentStamp, ScrapeArchiveRepository}

import java.time.Instant

/**
 * Each venue's last successful listing in one scrape archive, held between reads, so a read fetches
 * only the venues whose listing changed since the last one: their content stamps first (`_id` and
 * `scrapedAt`, a few hundred kilobytes), then those venues' rows by key. Read whole every projection,
 * the two archives were ~1.35 MB/s out of Mongo across the five countries (the US 278 MB a tick), while
 * about one venue in twelve re-scrapes between ticks.
 *
 * A venue whose scrape the intake lands is forgotten at once ([[forget]]), so its next read fetches it
 * whatever its stamp says — two scrapes stamped the same instant included.
 *
 * Only the venues a read asks for are held, so nothing beyond one read's listings stays in memory. A
 * read that cannot be completed — no stamps, or a keyed read cut short — falls back to reading the
 * archive whole, and an archive that cannot be read whole is empty, as it always was: a partial read
 * is not a smaller archive.
 */
private[identity] final class HeldListings(repository: ScrapeArchiveRepository) {
  private final case class Held(at: Instant, cinema: Cinema, films: Seq[CinemaMovie])
  private var held = Map.empty[String, Held]

  /** The last successful listing of each venue `keep` names (by display name). */
  def apply(keep: String => Boolean): Map[Cinema, Seq[CinemaMovie]] = synchronized {
    val stamps = repository.contentStamps()
    if (stamps.isEmpty) whole(keep)
    else {
      val needed = stamps.collect { case (name, ContentStamp(Some(at), _)) if keep(name) => name -> at }
      val stale  = needed.collect { case (name, at) if !held.get(name).exists(_.at == at) => name }.toSeq.sorted
      val fresh  = Map.newBuilder[String, Held]
      val read   = stale.isEmpty || repository.scanKeys(stale, rows => rows.foreach { row =>
        if (keep(row.cinema.displayName)) row.lastSuccess.foreach(s => fresh += row.cinema.displayName -> Held(s.at, row.cinema, s.films))
      })
      if (!read) whole(keep)
      else {
        held = held.filter { case (name, _) => needed.contains(name) } ++ fresh.result()
        current
      }
    }
  }

  /** Read `venue` again on the next read, whatever its stamp. */
  def forget(venue: String): Unit = synchronized { held -= venue }

  private def whole(keep: String => Boolean): Map[Cinema, Seq[CinemaMovie]] = {
    val all  = Map.newBuilder[String, Held]
    val read = repository.scan(_.foreach { row =>
      if (keep(row.cinema.displayName)) row.lastSuccess.foreach(s => all += row.cinema.displayName -> Held(s.at, row.cinema, s.films))
    })
    held = if (read) all.result() else Map.empty
    current
  }

  private def current: Map[Cinema, Seq[CinemaMovie]] = held.valuesIterator.map(h => h.cinema -> h.films).toMap
}
