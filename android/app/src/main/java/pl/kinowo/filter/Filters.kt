package pl.kinowo.filter

import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter

/** The only timezone the app (and the server it talks to) reasons about. */
internal val WARSAW: ZoneId = ZoneId.of("Europe/Warsaw")

private val ISO_DATE: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
private val WARSAW_DATE_TIME: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

/**
 * Date axis of the Filtry bar. Dated options first (Dziś / Jutro / 7 dni),
 * the catch-all `Wszystkie` pushed rightmost so the row reads narrow → broad.
 *
 * The fixed options are an [enum][Kind] rather than parameterless `object`
 * subclasses: a sealed class whose companion (and the ViewModel's default)
 * reference its own nested `object`s is a circular class-init hazard (outer ⇄
 * companion ⇄ objects). On the Android runtime that initialised the singletons
 * inconsistently — once as a `null` in `presets` (launch crash), and
 * intermittently leaving the default filter behaving as a different option than
 * `Today`. Enum constants initialise eagerly in one well-defined static block
 * with no cycle, so `Kind.TODAY` is always the same value. The public API
 * (`DateFilter.Today`, `.Anytime`, `.Specific(...)`, `.presets`, `.label`,
 * `.matches`) is unchanged.
 */
sealed interface DateFilter {
    val label: String
    fun matches(date: String, now: Instant = Instant.now()): Boolean

    enum class Kind(override val label: String) : DateFilter {
        TODAY("Dziś"),
        TOMORROW("Jutro"),
        WEEK("7 dni"),
        ANYTIME("Wszystkie");

        override fun matches(date: String, now: Instant): Boolean {
            val todayDate = LocalDate.ofInstant(now, WARSAW)
            return when (this) {
                ANYTIME -> true
                TODAY -> date == todayDate.format(ISO_DATE)
                TOMORROW -> date == todayDate.plusDays(1).format(ISO_DATE)
                WEEK -> {
                    val today = todayDate.format(ISO_DATE)
                    val in7 = todayDate.plusDays(7).format(ISO_DATE)
                    date in today..in7
                }
            }
        }
    }

    data class Specific(val date: String) : DateFilter {
        override val label: String get() = date
        override fun matches(date: String, now: Instant): Boolean = date == this.date
    }

    companion object {
        val Anytime: DateFilter get() = Kind.ANYTIME
        val Today: DateFilter get() = Kind.TODAY
        val Tomorrow: DateFilter get() = Kind.TOMORROW
        val Week: DateFilter get() = Kind.WEEK

        // Dated options first, catch-all rightmost.
        val presets: List<DateFilter> get() = listOf(Kind.TODAY, Kind.TOMORROW, Kind.WEEK, Kind.ANYTIME)
        fun iso(now: Instant): String = LocalDate.ofInstant(now, WARSAW).format(ISO_DATE)
    }
}

/**
 * Format axis of the Filtry dropdown: Wymiar / Wersja / IMAX format tokens
 * combined with a from-hour lower bound. Each axis independent — empty
 * radio strings mean "no constraint", `fromHour < 0` means "Dowolna". A
 * showtime passes when EVERY non-empty constraint matches.
 */
data class FormatFilter(
    val dimension: String = "",   // "" | "2D" | "3D"
    val language: String = "",    // "" | "NAP" | "DUB"
    val imax: Boolean = false,
    val fromHour: Int = -1,       // -1 = Dowolna
    val fromMinute: Int = 0,
) {
    val isEmpty: Boolean
        get() = dimension.isEmpty() && language.isEmpty() && !imax && fromHour < 0

    private val requiredTokens: List<String>
        get() = buildList {
            if (dimension.isNotEmpty()) add(dimension)
            if (language.isNotEmpty()) add(language)
            if (imax) add("IMAX")
        }

    /** `null` when the user picked "Dowolna" (any time). */
    val fromMinutes: Int?
        get() = if (fromHour >= 0) fromHour * 60 + fromMinute else null

    fun matches(showtime: Showtime): Boolean {
        val tokens = requiredTokens
        if (tokens.isNotEmpty()) {
            val badge = showtime.format.split(" ").filter { it.isNotEmpty() }.toSet()
            for (t in tokens) if (t !in badge) return false
        }
        val from = fromMinutes
        if (from != null) {
            val parts = showtime.time.split(":").mapNotNull { it.toIntOrNull() }
            // Unparseable time → never filtered out (matches the web guard).
            if (parts.size == 2 && (parts[0] * 60 + parts[1]) < from) return false
        }
        return true
    }

    companion object {
        val EMPTY = FormatFilter()
    }
}

/**
 * Combines a screening's date + time into a wall-clock moment in Warsaw, so
 * `prunedPastShowings` and any future caller share one notion of "is this
 * slot still future" and "what minute does this cinema's earliest slot start".
 */
object ShowtimeClock {
    /** A slot is "future" if its wall-clock dateTime is strictly after
     *  `now - 30min` (a screening that started 25 min ago is still live).
     *  Unparseable times are kept — never silently drop a badge we can't
     *  reason about. */
    fun isFuture(slot: Showtime, date: String, now: Instant = Instant.now()): Boolean {
        val dt = warsawInstant(date, slot.time) ?: return true
        return dt.isAfter(now.minusSeconds(30 * 60))
    }

    /** Minute-of-day of the earliest slot in a cinema-group; sentinel
     *  `Int.MAX_VALUE` for an empty list to keep the comparator total. */
    fun earliestMinutes(cg: CinemaShowings): Int =
        cg.showtimes.mapNotNull(::minutesOfDay).minOrNull() ?: Int.MAX_VALUE

    private fun minutesOfDay(slot: Showtime): Int? {
        val parts = slot.time.split(":").mapNotNull { it.toIntOrNull() }
        return if (parts.size == 2) parts[0] * 60 + parts[1] else null
    }

    private fun warsawInstant(date: String, time: String): Instant? = try {
        LocalDateTime.parse("$date $time", WARSAW_DATE_TIME).atZone(WARSAW).toInstant()
    } catch (_: Exception) {
        null
    }
}
