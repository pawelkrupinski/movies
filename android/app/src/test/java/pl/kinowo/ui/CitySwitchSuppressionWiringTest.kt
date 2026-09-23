package pl.kinowo.ui

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness

/**
 * [KinowoViewModel.chooseCityAtGate] must not let [KinowoViewModel.checkCitySwitch]
 * immediately re-offer the "you're nearer …" prompt for a city the user just
 * picked on purpose — first-launch or a later re-pick alike.
 *
 * The first-launch path (a real detected [nearestSlug]) already had a precise
 * fix: it seeds [pl.kinowo.data.UserPreferences.citySwitchPromptKey] with the
 * exact `chosen→nearest` pair, which [pl.kinowo.model.switchSuggestion] then
 * recognises and stays quiet for. A later re-pick — Filtry's "Pick another
 * city", or a country switch — has no detected nearest to build that pair
 * from ([nearestSlug] is null), so it falls back to the SAME one-shot
 * suppressor a web sign-in's Custom Tab resume already uses: skip the very
 * next [checkCitySwitch], not check-by-key.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class CitySwitchSuppressionWiringTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun rePickingWithNoDetectedNearestArmsTheBlanketSuppressor() {
        val vm = harness.viewModel()

        harness.settle(vm.chooseCityAtGate("warszawa", nearestSlug = null))

        assertTrue(
            "chooseCityAtGate(nearestSlug = null) should skip the next checkCitySwitch",
            vm.citySwitchSuppressor.consumeShouldSkip(),
        )
    }

    @Test
    fun firstLaunchWithADetectedNearestUsesThePreciseKeyInstead() {
        val vm = harness.viewModel()

        harness.settle(vm.chooseCityAtGate("warszawa", nearestSlug = "poznan"))

        // The exact chosen→nearest pair is handled by the persisted prompt key
        // (see UserPreferencesCityTests-equivalent coverage of setCitySwitchPromptKey);
        // the blanket suppressor has nothing to do here and stays disarmed.
        assertFalse(
            "A real detected nearest should be handled by the precise key, not the blanket suppressor",
            vm.citySwitchSuppressor.consumeShouldSkip(),
        )
    }

    @Test
    fun rePickingTheAlreadyNearestCityAlsoLeavesTheBlanketSuppressorDisarmed() {
        val vm = harness.viewModel()

        // chosen == nearest: initialChoiceSuppressKey returns null (nothing to
        // suppress — switchSuggestion already stays quiet when nearest equals
        // chosen), and nearestSlug is non-null, so the blanket fallback must not
        // fire either.
        harness.settle(vm.chooseCityAtGate("warszawa", nearestSlug = "warszawa"))

        assertFalse(vm.citySwitchSuppressor.consumeShouldSkip())
    }
}
