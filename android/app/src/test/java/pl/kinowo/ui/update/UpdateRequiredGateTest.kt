package pl.kinowo.ui.update

import android.content.Intent
import androidx.activity.ComponentActivity
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.data.ClientSupportGate
import pl.kinowo.offlineClientSupportGate
import pl.kinowo.ui.KinowoApp
import pl.kinowo.ui.KinowoViewModel

/**
 * The whole app, launched as [KinowoApp] launches it: when `/api/client-support`
 * says this version is too old, the update screen replaces everything (here, a
 * chosen city's repertoire) and its button opens the store; when the check
 * fails, the app carries on as if there were no gate.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class UpdateRequiredGateTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun anUnsupportedVersionSeesOnlyTheUpdateScreen() {
        val vm = harness.viewModel(clientSupportGate = ClientSupportGate({ BODY_MIN_2_0 }, appVersion = "1.0"))
        chooseWarszawa(vm)
        compose.setContent { KinowoApp(vm) }
        harness.pumpUntil("the client-support check") { vm.updateStoreUrl.value != null }
        compose.waitForIdle()

        compose.onNodeWithText("Ta wersja aplikacji nie jest już obsługiwana.").assertExists()
        compose.onNodeWithText(REPERTOIRE_SEARCH).assertDoesNotExist()

        compose.onNodeWithText("Zaktualizuj w Google Play").performClick()
        val opened = shadowOf(compose.activity).nextStartedActivity
        assertEquals(Intent.ACTION_VIEW, opened.action)
        assertEquals(STORE, opened.dataString)
    }

    @Test
    fun aFailedCheckLeavesTheAppUsable() {
        val vm = harness.viewModel(clientSupportGate = offlineClientSupportGate())
        chooseWarszawa(vm)
        compose.setContent { KinowoApp(vm) }
        harness.pumpFor(200)
        compose.waitForIdle()

        compose.onNodeWithText(REPERTOIRE_SEARCH).assertExists()
        compose.onNodeWithText("Zaktualizuj w Google Play").assertDoesNotExist()
    }

    private fun chooseWarszawa(vm: KinowoViewModel) {
        harness.settle(vm.chooseCityAtGate("warszawa", nearestSlug = null))
        harness.pumpUntil("the pick to reach selectedCity") { vm.selectedCity.value == "warszawa" }
    }

    private companion object {
        /** The repertoire screen's film-search placeholder — present iff the app is usable. */
        const val REPERTOIRE_SEARCH = "Szukaj filmu"
        const val STORE = "https://play.google.com/store/apps/details?id=net.pawel.kinowo"
        const val BODY_MIN_2_0 =
            """{"android":{"minimumVersion":"2.0","storeUrl":"$STORE"},"ios":{"minimumVersion":"1.0","storeUrl":"https://apps.apple.com/app/id6792566321"}}"""
    }
}
