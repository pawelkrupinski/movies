package pl.kinowo

import androidx.compose.ui.test.junit4.ComposeTestRule

/** Keep the app idling for [millis] — for asserting something did NOT happen
 *  (a recreate, a replayed link) that would only show up a few frames later. */
fun ComposeTestRule.idleFor(millis: Long) {
    val end = System.currentTimeMillis() + millis
    while (System.currentTimeMillis() < end) { waitForIdle(); Thread.sleep(20) }
}
