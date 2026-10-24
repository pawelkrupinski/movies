package pl.kinowo.filter

import java.time.Instant
import java.time.ZoneId

/**
 * A search's day. A search looks through EVERY day: the first letter typed moves the listing to
 * "All" and remembers the day it was on; clearing the search goes back there -- unless a day was
 * picked while it ran (a tap, a swipe), which then stays. While the search shows every day, the
 * days it brought in are drawn lighter ([outsideDays]). The web's twin is `onSearchInput` in
 * shared.js, iOS's `SearchDay.swift`. Immutable, so a Compose state holding it recomposes.
 */
data class SearchDay(
    /** The day the listing was on when the search began; null while no search runs. */
    val origin: DateFilter? = null,
    /** A day was picked while the search ran. */
    val picked: Boolean = false,
) {
    /** The next state, and the day to move the listing to (null: leave it where it is). */
    data class Change(val next: SearchDay, val moveTo: DateFilter?)

    /** The search box now reads [query] while the listing shows [showing]. */
    fun queryChanged(query: String, showing: DateFilter): Change {
        val searching = query.isNotBlank()
        if (searching && origin == null) {
            return Change(SearchDay(origin = showing), if (showing == DateFilter.Anytime) null else DateFilter.Anytime)
        }
        if (!searching && origin != null) {
            val back = if (picked) null else origin
            return Change(SearchDay(), if (back == showing) null else back)
        }
        return Change(this, null)
    }

    /** The person moved the day (not [queryChanged]'s answer): mid-search, that day stays. */
    fun dayPicked(): SearchDay = if (origin != null) copy(picked = true) else this

    /** The day the search began from, while nothing has been picked since; else null. */
    val outsideOf: DateFilter? get() = if (picked) null else origin

    /** On the page for [page]: whether a day ("YYYY-MM-DD") is one the search brought in, to draw
     *  lighter -- only on the "All" page, measured on the city's clock. Null when none is. */
    fun outsideDays(page: DateFilter, now: Instant = Instant.now(), zone: ZoneId = WARSAW): ((String) -> Boolean)? {
        val from = outsideOf ?: return null
        if (page != DateFilter.Anytime) return null
        return { date -> !from.matches(date, now, zone) }
    }
}
