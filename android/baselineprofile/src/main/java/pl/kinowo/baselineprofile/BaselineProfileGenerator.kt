package pl.kinowo.baselineprofile

import androidx.benchmark.macro.junit4.BaselineProfileRule
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

/**
 * Generates `app/src/main/generated/baselineProfiles/baseline-prof.txt`:
 * `./gradlew :app:generateBaselineProfile` with an emulator or device connected
 * (API 33+, or rooted 28+). The critical journey: cold start → poster grid
 * loaded → scroll it → open a film → back.
 */
@RunWith(AndroidJUnit4::class)
class BaselineProfileGenerator {
    @get:Rule
    val rule = BaselineProfileRule()

    @Test
    fun coldStartBrowseAndOpenFilm() = FixtureBackend().start().use {
        var cityChosen = false
        rule.collect(packageName = KINOWO_PACKAGE) {
            if (!cityChosen) chooseFixtureCity().also { cityChosen = true }
            pressHome()
            startActivityAndWait()
            awaitPosterGrid()
            scrollPosterGrid()
            openFilmDetailAndBack()
        }
    }
}
