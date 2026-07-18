package pl.kinowo.filter

import pl.kinowo.model.CinemaArea
import pl.kinowo.model.CinemaCatalog

/** Tri-state for a checkbox covering one or more cinemas. */
enum class CinemaCheck { ON, OFF, MIXED }

/**
 * The Filtry sheet's "Kina" section as pure, Compose-free data: which boxes read
 * ticked, and what `disabledCinemas` becomes when one is toggled.
 *
 * There is ONE cinema filter axis — the [disabled] exclusion set, shared with
 * the web's `localStorage` (it round-trips through StateSyncService and the
 * `?cinema=` deep link). A **flat** city renders [cityCinemas] as a single
 * checkbox list; a **split** city (e.g. London) renders that same universe
 * grouped into [CinemaCatalog.areas]. Both mutate the one set, mirroring the
 * web's Filtry cinema panel (`buildCinemaPanel` in `shared.js`).
 *
 * Pure Kotlin → unit-tested by CinemaFilterSectionTest; `FiltersSheet` is a thin
 * renderer over it, and iOS `CinemaFilterSection` is the parallel implementation.
 */
data class CinemaFilterSection(
    /** The current city's cinema universe + its optional area grouping. */
    val catalog: CinemaCatalog,
    /** The excluded set, GLOBAL across cities (see UserPreferences). */
    val disabled: Set<String>,
) {
    val cityCinemas: List<String> get() = catalog.cinemas
    val isSplit: Boolean get() = catalog.isSplit

    /**
     * How many of THIS city's cinemas are still shown — the count the section
     * header reads out. Names belonging to other cities are ignored.
     */
    val enabledCount: Int get() = cityCinemas.count { it !in disabled }

    /** The "all cinemas" master checkbox. */
    val allCheck: CinemaCheck get() = checkOf(cityCinemas)

    fun checkOfCinema(cinema: String): CinemaCheck =
        if (cinema in disabled) CinemaCheck.OFF else CinemaCheck.ON

    fun checkOfArea(area: CinemaArea): CinemaCheck = checkOf(area.cinemas)

    fun checkOf(cinemas: List<String>): CinemaCheck {
        val off = cinemas.count { it in disabled }
        return when (off) {
            0 -> CinemaCheck.ON
            cinemas.size -> CinemaCheck.OFF
            else -> CinemaCheck.MIXED
        }
    }

    /** The new excluded set after ticking/unticking one cinema. */
    fun settingCinema(cinema: String, enabled: Boolean): Set<String> =
        setting(listOf(cinema), enabled)

    /** The new excluded set after ticking/unticking a whole area. */
    fun settingArea(area: CinemaArea, enabled: Boolean): Set<String> =
        setting(area.cinemas, enabled)

    /**
     * The new excluded set after the "all cinemas" master. Scoped to this city's
     * universe so other cities' entries survive a select-all.
     */
    fun settingAll(enabled: Boolean): Set<String> = setting(cityCinemas, enabled)

    /**
     * Every mutator funnels here: add/remove exactly the named cinemas and leave
     * the rest of the (cross-city) set alone.
     */
    private fun setting(cinemas: List<String>, enabled: Boolean): Set<String> =
        if (enabled) disabled - cinemas.toSet() else disabled + cinemas.toSet()
}
