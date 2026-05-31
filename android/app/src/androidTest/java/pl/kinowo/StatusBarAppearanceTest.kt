package pl.kinowo

import androidx.core.view.WindowCompat
import androidx.test.core.app.ActivityScenario
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.Assert.assertFalse
import org.junit.Test
import org.junit.runner.RunWith

/**
 * Regression for the "invisible status bar / empty dark band at the top" bug.
 *
 * Kinowo renders a fixed dark UI (see [pl.kinowo.ui.theme.KinowoTheme]). The
 * original `enableEdgeToEdge()` call took no arguments, so it used the default
 * `SystemBarStyle.auto`, which derives the status-bar icon colour from the
 * *device's* night-mode setting rather than the app's. On a light-mode device
 * that produced dark icons — invisible against our near-black background — so
 * the status bar read as an empty dark band and the app looked like it didn't
 * reach the top of the screen.
 *
 * The fix forces the dark system-bar style (light icons) regardless of the
 * system night mode, so `isAppearanceLightStatusBars` must be false even when
 * the device is in light mode.
 */
@RunWith(AndroidJUnit4::class)
class StatusBarAppearanceTest {

    @Test
    fun statusBarIconsStayLightInSystemLightMode() {
        val automation = InstrumentationRegistry.getInstrumentation().uiAutomation
        // Reproduce the failing condition: device in light mode.
        automation.executeShellCommand("cmd uimode night no").close()
        try {
            ActivityScenario.launch(MainActivity::class.java).use { scenario ->
                scenario.onActivity { activity ->
                    val controller = WindowCompat.getInsetsController(
                        activity.window,
                        activity.window.decorView,
                    )
                    assertFalse(
                        "Dark-only app must keep light status-bar icons even when the " +
                            "system is in light mode; otherwise they vanish against the " +
                            "dark background and the status bar looks like an empty gap.",
                        controller.isAppearanceLightStatusBars,
                    )
                }
            }
        } finally {
            automation.executeShellCommand("cmd uimode night no").close()
        }
    }
}
