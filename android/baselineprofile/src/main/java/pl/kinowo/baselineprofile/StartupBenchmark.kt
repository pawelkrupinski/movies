package pl.kinowo.baselineprofile

import androidx.benchmark.macro.BaselineProfileMode
import androidx.benchmark.macro.CompilationMode
import androidx.benchmark.macro.StartupMode
import androidx.benchmark.macro.StartupTimingMetric
import androidx.benchmark.macro.junit4.MacrobenchmarkRule
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

/**
 * Cold start to a loaded poster grid, without any AOT compilation vs with the
 * committed Baseline Profile — the gain the profile buys. Runs on the
 * benchmarkRelease build (R8-minified like release):
 * `./gradlew :baselineprofile:connectedBenchmarkReleaseAndroidTest
 *  -Pandroid.testInstrumentationRunnerArguments.class=pl.kinowo.baselineprofile.StartupBenchmark`.
 * Emulator numbers are indicative only; measure on a physical device for real ones.
 */
@RunWith(AndroidJUnit4::class)
class StartupBenchmark {
    @get:Rule
    val rule = MacrobenchmarkRule()

    @Test
    fun startupWithoutCompilation() = startup(CompilationMode.None())

    @Test
    fun startupWithBaselineProfile() = startup(CompilationMode.Partial(BaselineProfileMode.Require))

    private fun startup(compilationMode: CompilationMode) = FixtureBackend().start().use {
        var cityChosen = false
        rule.measureRepeated(
            packageName = KINOWO_PACKAGE,
            metrics = listOf(StartupTimingMetric()),
            compilationMode = compilationMode,
            startupMode = StartupMode.COLD,
            iterations = 10,
            setupBlock = {
                if (!cityChosen) chooseFixtureCity().also { cityChosen = true }
                pressHome()
            },
        ) {
            startActivityAndWait()
            awaitPosterGrid()
        }
    }
}
