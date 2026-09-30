package pl.kinowo.ui

import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.StaticRepertoireApi
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.model.Film

/**
 * [KinowoApp] calls [KinowoViewModel.start] from a `LaunchedEffect(Unit)`,
 * which runs again every time the activity is recreated around the RETAINED
 * ViewModel — a rotation, or the language switch's deliberate recreate()
 * without clearing it. Each extra call used to launch another `selectedCity`
 * collector, so after N recreations every city change fetched N+1 times.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class StartIsIdempotentTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun aSecondStartDoesNotDoubleTheRepertoireFetch() {
        val api = StaticRepertoireApi()
        val repository = RepertoireRepository(api, JsonListCache(harness.context.cacheDir, "start_idempotent", Film.serializer()))
        val vm = harness.viewModel(repository = repository)
        harness.settle(vm.setCity("start-idempotent-city"))

        vm.start()
        vm.start() // the recreated activity's LaunchedEffect
        harness.pumpUntil("the first fetch") { api.fetchCount >= 1 }
        harness.pumpFor(millis = 200)

        assertEquals(1, api.fetchCount)
    }
}
