package pl.kinowo.ui.list

import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.filter.CinemaSection
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.CinemaSeason

/**
 * A cinema row in the Filtry sheet: an out-of-season venue (open-air, drive-in)
 * carries a muted "back in season · May–Sept" line under its name, in the UI
 * language; an in-season one, seasonal or not, is just its name.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CinemaRowTest {

    @get:Rule
    val compose = createComposeRule()

    private val catalog = CinemaCatalog(
        cinemas = listOf("Freiluftkino Kreuzberg", "Freiluftkino Rehberge"),
        seasons = listOf(
            CinemaSeason("Freiluftkino Kreuzberg", opens = 5, closes = 9, offSeason = true),
            CinemaSeason("Freiluftkino Rehberge", opens = 5, closes = 9, offSeason = false),
        ),
    )

    @Test
    fun outOfSeasonRowSaysWhenItIsBack() {
        compose.setContent { CinemaRow("Freiluftkino Kreuzberg", catalog, checked = true) {} }

        compose.onNodeWithText(CinemaSection.pillName("Freiluftkino Kreuzberg")).assertExists()
        compose.onNodeWithText("wraca w sezonie · maj–wrz").assertExists()
    }

    @Test
    fun inSeasonRowIsJustItsName() {
        compose.setContent { CinemaRow("Freiluftkino Rehberge", catalog, checked = true) {} }

        compose.onNodeWithText(CinemaSection.pillName("Freiluftkino Rehberge")).assertExists()
        compose.onNodeWithText("wraca w sezonie", substring = true).assertDoesNotExist()
    }
}
