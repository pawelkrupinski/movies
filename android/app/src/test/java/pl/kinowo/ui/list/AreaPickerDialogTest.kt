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
 * Off-device (Robolectric) Compose test for the first-visit area picker: all
 * areas start checked, so confirming immediately keeps everything; unchecking an
 * area drops it from the kept set the caller uses to exclude its cinemas.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class AreaPickerDialogTest {

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
    fun confirmingWithAllPreselectedKeepsEveryArea() {
        var kept: Set<String>? = null
        compose.setContent { AreaPickerDialog(catalog) { kept = it } }
        compose.onNodeWithTag("areapicker.confirm").performClick()
        assertEquals(setOf("central", "north"), kept)
    }

    @Test
    fun uncheckingAnAreaDropsItFromKept() {
        var kept: Set<String>? = null
        compose.setContent { AreaPickerDialog(catalog) { kept = it } }
        compose.onNodeWithTag("areapicker.area.north").performClick()   // uncheck North
        compose.onNodeWithTag("areapicker.confirm").performClick()
        assertEquals(setOf("central"), kept)
    }
}
