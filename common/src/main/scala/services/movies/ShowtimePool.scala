package services.movies

import java.time.LocalDateTime

import com.github.benmanes.caffeine.cache.Interner
import models.{CityScreening, Showtime}

/** What a city's showtimes repeat, held once: one `LocalDateTime` per distinct instant, and one
 *  String per booking-URL prefix a row's showtimes share.
 *
 *  INSTANTS. A decoded showtime carries its own `LocalDateTime` — and with it a `LocalDate` and a
 *  `LocalTime`, ~72 bytes — yet a city's showtimes repeat few instants: New York's 50,289 hold
 *  6,793 distinct ones. Shared, a showtime costs ~211 bytes instead of ~311 (measured on New York's
 *  rows, 2026-10-03), ~165 MB across the US corpus web-us holds.
 *
 *  BOOKING URLS. A row's URLs — one cinema's, for one film — differ only in their last few
 *  characters. Each is held split at the prefix they all share ([[Showtime.withUrlPrefix]]): the
 *  prefix once, each showtime only its remainder. The prefix is recomputed from the row's own
 *  URLs every time a row enters, so a cinema that moves its booking pages — another path, another
 *  domain — simply splits at the new prefix; nothing is keyed by a cinema or a domain to go stale.
 *  Rows with the same prefix (a cinema's other films, often) share one String.
 *
 *  WEAK, not bounded like [[StringPool]]: both are per-screening values — tens of thousands of
 *  distinct instants per country, new ones every day — which would evict a bounded vocabulary. A
 *  weak interner keeps a value exactly as long as some showtime refers to it, so what has lapsed
 *  out of every listing goes with its showtimes.
 *
 *  An instance, never a global, for [[StringPool]]'s reason. */
final class ShowtimePool {
  private val instants: Interner[LocalDateTime] = Interner.newWeakInterner[LocalDateTime]()
  private val prefixes: Interner[String]        = Interner.newWeakInterner[String]()
  private val rooms: Interner[Some[String]]      = Interner.newWeakInterner[Some[String]]()
  private val formats: Interner[List[String]]    = Interner.newWeakInterner[List[String]]()

  def canonical(at: LocalDateTime): LocalDateTime = instants.intern(at)

  /** `read` with each showtime's instant, room and format the pool's: a scan holding the corpus decoded every one
   *  afresh (worker-us boot: 1.2M showtimes, each its own LocalDateTime, LocalDate, LocalTime and format list, for a
   *  few thousand distinct values). A showtime already sharing them is handed back as it is. */
  def showtimes(read: Seq[Showtime]): Seq[Showtime] = read.map { st =>
    val at     = canonical(st.dateTime)
    val room   = st.room match { case some: Some[String] => rooms.intern(some); case None => None }
    val format = if (st.format.isEmpty) st.format else formats.intern(st.format)
    if ((at eq st.dateTime) && (room eq st.room) && (format eq st.format)) st else st.copy(dateTime = at, room = room, format = format)
  }

  /** `row` with every showtime's instant the shared one and its booking URL split at the row's
   *  shared prefix — the row itself when they all already are. */
  def share(row: CityScreening): CityScreening = {
    val common = Showtime.commonUrlPrefix(row.showtimes)
    val prefix = if (common.isEmpty) null else prefixes.intern(common)
    var changed = false
    val shared = row.showtimes.map { st =>
      val at     = canonical(st.dateTime)
      val dated  = if (at eq st.dateTime) st else st.copy(dateTime = at)
      val held   = if (prefix == null) dated else dated.withUrlPrefix(prefix)
      if (!(held eq st)) changed = true
      held
    }
    if (changed) row.copy(showtimes = shared) else row
  }
}
