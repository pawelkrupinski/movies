package pl.kinowo.filter

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.CinemaArea
import pl.kinowo.model.CinemaCatalog

/**
 * The Filtry sheet's "Kina" section, as pure data. Since the top-bar cinema bars
 * were retired there is ONE cinema filter axis — the `disabledCinemas` exclusion
 * set — rendered as a flat checkbox list on a flat city and as area groups on a
 * split city. Parallel to iOS `CinemaFilterSectionTests`.
 */
class CinemaFilterSectionTest {

    private val flat = CinemaCatalog(
        cinemas = listOf("Kino Muza", "Multikino Poznań", "Cinema City Plaza"),
        areas = emptyList(),
    )

    private val split = CinemaCatalog(
        cinemas = listOf("Odeon Leicester Square", "Odeon Camden", "Curzon Soho", "Vue Westfield"),
        areas = listOf(
            CinemaArea("Central", "central", listOf("Odeon Leicester Square", "Curzon Soho")),
            CinemaArea("North", "north", listOf("Odeon Camden")),
            CinemaArea("West", "west", listOf("Vue Westfield")),
        ),
    )

    // ── shape ──────────────────────────────────────────────────────────────

    @Test
    fun `flat city exposes every cinema and no areas`() {
        val section = CinemaFilterSection(flat, emptySet())
        assertFalse(section.isSplit)
        assertEquals(3, section.cityCinemas.size)
        assertTrue(section.catalog.areas.isEmpty())
    }

    @Test
    fun `split city groups the same universe into areas`() {
        val section = CinemaFilterSection(split, emptySet())
        assertTrue(section.isSplit)
        assertEquals(
            section.cityCinemas.toSet(),
            section.catalog.areas.flatMap { it.cinemas }.toSet(),
        )
    }

    // ── tri-state checks ───────────────────────────────────────────────────

    @Test
    fun `nothing excluded reads as fully on`() {
        val section = CinemaFilterSection(flat, emptySet())
        assertEquals(CinemaCheck.ON, section.allCheck)
        assertEquals(3, section.enabledCount)
    }

    @Test
    fun `every cinema excluded reads as off`() {
        val section = CinemaFilterSection(flat, flat.cinemas.toSet())
        assertEquals(CinemaCheck.OFF, section.allCheck)
        assertEquals(0, section.enabledCount)
    }

    @Test
    fun `some excluded reads as mixed`() {
        val section = CinemaFilterSection(flat, setOf("Kino Muza"))
        assertEquals(CinemaCheck.MIXED, section.allCheck)
        assertEquals(2, section.enabledCount)
        assertEquals(CinemaCheck.OFF, section.checkOfCinema("Kino Muza"))
        assertEquals(CinemaCheck.ON, section.checkOfCinema("Multikino Poznań"))
    }

    @Test
    fun `area check is mixed when only part of it is excluded`() {
        val section = CinemaFilterSection(split, setOf("Curzon Soho"))
        assertEquals(CinemaCheck.MIXED, section.checkOfArea(split.areas[0]))
        assertEquals(CinemaCheck.ON, section.checkOfArea(split.areas[1]))
        assertEquals(CinemaCheck.MIXED, section.allCheck)
    }

    // ── toggling ───────────────────────────────────────────────────────────

    @Test
    fun `unchecking one cinema excludes only it`() {
        val section = CinemaFilterSection(flat, emptySet())
        assertEquals(setOf("Kino Muza"), section.settingCinema("Kino Muza", enabled = false))
    }

    @Test
    fun `checking one cinema clears its exclusion`() {
        val section = CinemaFilterSection(flat, setOf("Kino Muza", "Cinema City Plaza"))
        assertEquals(
            setOf("Cinema City Plaza"),
            section.settingCinema("Kino Muza", enabled = true),
        )
    }

    @Test
    fun `unchecking an area excludes every cinema in it`() {
        val section = CinemaFilterSection(split, emptySet())
        assertEquals(
            setOf("Odeon Leicester Square", "Curzon Soho"),
            section.settingArea(split.areas[0], enabled = false),
        )
    }

    @Test
    fun `checking an area clears only its cinemas`() {
        val section = CinemaFilterSection(
            split,
            setOf("Odeon Leicester Square", "Curzon Soho", "Odeon Camden"),
        )
        assertEquals(setOf("Odeon Camden"), section.settingArea(split.areas[0], enabled = true))
    }

    @Test
    fun `the all master clears and fills the whole city`() {
        assertEquals(
            flat.cinemas.toSet(),
            CinemaFilterSection(flat, emptySet()).settingAll(enabled = false),
        )
        assertEquals(
            emptySet<String>(),
            CinemaFilterSection(flat, flat.cinemas.toSet()).settingAll(enabled = true),
        )
    }

    // ── cross-city safety ──────────────────────────────────────────────────

    // `disabledCinemas` is global across cities (it mirrors the web's
    // localStorage set and round-trips through StateSyncService), so every
    // mutator must leave other cities' names untouched — otherwise switching
    // city would silently re-enable everything the user hid there.
    @Test
    fun `mutators preserve other cities entries`() {
        val foreign = "Kino Pod Baranami" // Kraków — not in `flat`
        val section = CinemaFilterSection(flat, setOf(foreign))

        assertTrue(foreign in section.settingCinema("Kino Muza", enabled = false))
        assertTrue(foreign in section.settingAll(enabled = false))
        assertTrue(foreign in section.settingAll(enabled = true))

        val splitSection = CinemaFilterSection(split, setOf(foreign))
        assertTrue(foreign in splitSection.settingArea(split.areas[0], enabled = false))
        assertTrue(foreign in splitSection.settingArea(split.areas[0], enabled = true))
    }

    @Test
    fun `foreign entries dont affect the current citys checks`() {
        val section = CinemaFilterSection(flat, setOf("Kino Pod Baranami"))
        assertEquals(CinemaCheck.ON, section.allCheck)
        assertEquals(3, section.enabledCount)
    }
}
