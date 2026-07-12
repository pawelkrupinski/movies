package pl.kinowo.ui.list

import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.performClick
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.CinemaArea
import pl.kinowo.model.CinemaCatalog

/**
 * Off-device (Robolectric) Compose test for the split-city area picker: the
 * handle unfolds, an area checkbox (de)selects its whole area, a per-cinema
 * checkbox toggles one venue, and the master (de)selects everything — each
 * emitting the right callback for the ViewModel to write `disabledCinemas`.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CinemaAreaBarTest {

    @get:Rule
    val compose = createComposeRule()

    private val catalog = CinemaCatalog(
        cinemas = listOf("A Cinema", "B Cinema", "C Cinema"),
        areas = listOf(
            CinemaArea("Central", "central", listOf("A Cinema")),
            CinemaArea("North", "north", listOf("B Cinema", "C Cinema")),
        ),
    )

    @Test
    fun areaToggleDeselectsTheWholeArea() {
        var call: Pair<List<String>, Boolean>? = null
        compose.setContent {
            CinemaAreaBar(catalog, emptySet(),
                onSetCinema = { _, _ -> }, onSetArea = { c, e -> call = c to e }, onSetAll = { _, _ -> })
        }
        compose.onNodeWithTag("areabar.handle").performClick()          // expand
        compose.onNodeWithTag("areabar.area.north").performClick()      // all-on → turn off
        assertEquals(listOf("B Cinema", "C Cinema") to false, call)
    }

    @Test
    fun cinemaCheckboxTogglesOneVenue() {
        var call: Pair<String, Boolean>? = null
        compose.setContent {
            CinemaAreaBar(catalog, emptySet(),
                onSetCinema = { c, e -> call = c to e }, onSetArea = { _, _ -> }, onSetAll = { _, _ -> })
        }
        compose.onNodeWithTag("areabar.handle").performClick()
        compose.onNodeWithTag("areabar.header.central").performClick()  // open the fold
        compose.onNodeWithTag("areabar.cinema.A Cinema").performClick() // enabled → turn off
        assertEquals("A Cinema" to false, call)
    }

    @Test
    fun masterDeselectsEveryCinema() {
        var call: Pair<List<String>, Boolean>? = null
        compose.setContent {
            CinemaAreaBar(catalog, emptySet(),
                onSetCinema = { _, _ -> }, onSetArea = { _, _ -> }, onSetAll = { a, e -> call = a to e })
        }
        compose.onNodeWithTag("areabar.handle").performClick()
        compose.onNodeWithTag("areabar.all").performClick()
        assertEquals(catalog.cinemas to false, call)
    }
}
