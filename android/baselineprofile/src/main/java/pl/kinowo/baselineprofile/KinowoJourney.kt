package pl.kinowo.baselineprofile

import androidx.benchmark.macro.MacrobenchmarkScope
import androidx.test.uiautomator.By
import androidx.test.uiautomator.Direction
import androidx.test.uiautomator.Until

/** The installed app under test (`applicationId`, not the `pl.kinowo` namespace). */
const val KINOWO_PACKAGE = "net.pawel.kinowo"
private const val MAIN_ACTIVITY = "$KINOWO_PACKAGE/pl.kinowo.MainActivity"

// testTags in :app (exposed as resource ids by MainActivity's testTagsAsResourceId).
private const val FILM_CARD = "film-card"
// Tapped rather than the card: the card's middle holds showtime chips, whose
// tap opens the cinema's booking page in the browser.
private const val FILM_CARD_POSTER = "film-card-poster"
private const val DETAIL_POSTER = "detailPoster"
private const val TIMEOUT_MS = 20_000L

/**
 * A fresh install opens on the city gate, which first asks for location. Pick Poznań — the fixture corpus's
 * city — the way a shared link would, so every later cold start lands straight
 * on the poster grid. Not part of the profiled / measured journey.
 */
fun MacrobenchmarkScope.chooseFixtureCity() {
    // Deny location as "don't ask again", so the gate's permission dialog never
    // covers the grid (and no emulator fix suggests a city of its own).
    device.executeShellCommand("pm revoke $KINOWO_PACKAGE android.permission.ACCESS_COARSE_LOCATION")
    device.executeShellCommand("pm set-permission-flags $KINOWO_PACKAGE android.permission.ACCESS_COARSE_LOCATION user-set user-fixed")
    device.executeShellCommand("am start -W -a android.intent.action.VIEW -d https://kinowo.net/poznan/ -n $MAIN_ACTIVITY")
    awaitPosterGrid()
    killProcess()
}

fun MacrobenchmarkScope.awaitPosterGrid() {
    check(device.wait(Until.hasObject(By.res(FILM_CARD)), TIMEOUT_MS)) { "the poster grid never showed a film card${onScreen()}" }
}

/** What the screen showed instead, for a failed wait's message. */
private fun MacrobenchmarkScope.onScreen(): String {
    val focus = device.executeShellCommand("dumpsys window").lineSequence()
        .filter { "mCurrentFocus" in it || "mFocusedApp" in it }.joinToString(" | ") { it.trim() }
    val texts = device.findObjects(By.textContains("")).mapNotNull { it.text }.take(20)
    return " — focus: $focus; texts: $texts"
}

/** Fling the grid down and back up, as a first browse of the listing does. */
fun MacrobenchmarkScope.scrollPosterGrid() {
    val grid = device.findObject(By.res(FILM_CARD)).parent
    grid.setGestureMarginPercentage(0.2f)
    repeat(2) { grid.fling(Direction.DOWN); device.waitForIdle() }
    grid.fling(Direction.UP)
    device.waitForIdle()
}

/** Open the first film's detail screen, then go back to the grid. */
fun MacrobenchmarkScope.openFilmDetailAndBack() {
    device.findObject(By.res(FILM_CARD_POSTER)).click()
    check(device.wait(Until.hasObject(By.res(DETAIL_POSTER)), TIMEOUT_MS)) { "the film detail never opened${onScreen()}" }
    device.waitForIdle()
    device.pressBack()
    awaitPosterGrid()
}
