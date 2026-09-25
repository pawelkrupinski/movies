package pl.kinowo.ui

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.location.GrantedLocationSource

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
 *
 * Each test drives the real check against an injected location fix.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class CitySwitchSuppressionWiringTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun rePickingWithNoDetectedNearestSkipsExactlyTheNextCheck() {
        val vm = pickWarszawa(nearestSlug = null, fix = POZNAN)

        check(vm)
        assertNull("chooseCityAtGate(nearestSlug = null) should skip the next checkCitySwitch", vm.citySwitchSuggestion)

        // One-shot: the check after that is a genuine one and offers the switch.
        check(vm)
        assertEquals("poznan", vm.citySwitchSuggestion?.target?.slug)
    }

    @Test
    fun firstLaunchWithADetectedNearestUsesThePreciseKeyNotTheBlanketSuppressor() {
        // The chosen→nearest pair itself stays quiet (the persisted key)…
        val quiet = pickWarszawa(nearestSlug = "poznan", fix = POZNAN)
        check(quiet)
        assertNull(quiet.citySwitchSuggestion)
    }

    @Test
    fun firstLaunchWithADetectedNearestLeavesOtherPairsPrompting() {
        // …but the blanket suppressor stays disarmed, so the very next check
        // still offers a DIFFERENT nearer city.
        val vm = pickWarszawa(nearestSlug = "poznan", fix = KRAKOW)
        check(vm)
        assertEquals("krakow", vm.citySwitchSuggestion?.target?.slug)
    }

    @Test
    fun rePickingTheAlreadyNearestCityLeavesTheBlanketSuppressorDisarmed() {
        // chosen == nearest: initialChoiceSuppressKey returns null (nothing to
        // suppress), and nearestSlug is non-null, so the blanket fallback must
        // not fire either — the next check still prompts.
        val vm = pickWarszawa(nearestSlug = "warszawa", fix = POZNAN)
        check(vm)
        assertEquals("poznan", vm.citySwitchSuggestion?.target?.slug)
    }

    private fun pickWarszawa(nearestSlug: String?, fix: Pair<Double, Double>): KinowoViewModel {
        val vm = harness.viewModel(location = GrantedLocationSource { fix })
        harness.settle(vm.chooseCityAtGate("warszawa", nearestSlug = nearestSlug))
        harness.pumpUntil("the pick to reach selectedCity") { vm.selectedCity.value == "warszawa" }
        return vm
    }

    private fun check(vm: KinowoViewModel) {
        vm.citySwitchSuggestion?.let { vm.dismissCitySwitch() }
        harness.settle(vm.checkCitySwitch())
    }

    private companion object {
        val POZNAN = 52.4064 to 16.9252
        val KRAKOW = 50.0647 to 19.9450
    }
}
