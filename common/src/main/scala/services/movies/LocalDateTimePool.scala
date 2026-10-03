package services.movies

import java.time.LocalDateTime

import com.github.benmanes.caffeine.cache.Interner
import models.CityScreening

/** One `LocalDateTime` per distinct showtime instant, shared by every showtime at it.
 *
 *  A decoded showtime carries its own `LocalDateTime` — and with it a `LocalDate` and a
 *  `LocalTime`, ~72 bytes — yet a city's showtimes repeat few instants: New York's 50,289
 *  hold 6,793 distinct ones. Shared, a showtime costs ~211 bytes instead of ~311 (measured
 *  on New York's rows, 2026-10-03), ~165 MB across the US corpus web-us holds.
 *
 *  WEAK, not bounded like [[StringPool]]: showtime instants are per-screening values — tens
 *  of thousands distinct per country, and new ones every day — which would evict a bounded
 *  vocabulary. A weak interner keeps an instant exactly as long as some showtime refers to
 *  it, so instants that have lapsed out of every listing go with their showtimes.
 *
 *  An instance, never a global, for [[StringPool]]'s reason. */
final class LocalDateTimePool {
  private val instants: Interner[LocalDateTime] = Interner.newWeakInterner[LocalDateTime]()

  def canonical(at: LocalDateTime): LocalDateTime = instants.intern(at)

  /** `row` with every showtime's instant the shared one — the row itself when they all
   *  already are. */
  def showtimes(row: CityScreening): CityScreening = {
    var changed = false
    val shared = row.showtimes.map { st =>
      val at = canonical(st.dateTime)
      if (at eq st.dateTime) st else { changed = true; st.copy(dateTime = at) }
    }
    if (changed) row.copy(showtimes = shared) else row
  }
}
