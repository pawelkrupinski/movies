package pl.kinowo.ui.common

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.ui.theme.KinowoTheme

/**
 * Off-device (Robolectric) Compose test for the optional age-rating badge in
 * [MetaPills]: it renders as its own pill only when the film carries a rating,
 * and is absent when the field is null (the common non-UK case).
 * `./gradlew app:testDebugUnitTest`.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class MetaPillsAgeRatingTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun ageBadgeRendersWhenPresent() {
        compose.setContent {
            KinowoTheme {
                MetaPills(runtimeMinutes = 130, releaseYear = 2026, ageRating = "15")
            }
        }
        compose.onNodeWithTag(AgeRatingBadgeTag).assertExists()
        compose.onNodeWithText("15").assertIsDisplayed()
    }

    @Test
    fun ageBadgeAbsentWhenNull() {
        compose.setContent {
            KinowoTheme {
                MetaPills(runtimeMinutes = 130, releaseYear = 2026, ageRating = null)
            }
        }
        // The rest of the meta row still renders...
        compose.onNodeWithText("2026").assertExists()
        // ...but no age-rating pill is emitted.
        compose.onNodeWithTag(AgeRatingBadgeTag).assertDoesNotExist()
    }
}
