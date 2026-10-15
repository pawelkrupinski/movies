package pl.kinowo

import android.app.Activity
import android.app.Application
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.runtime.Composable
import androidx.compose.ui.test.junit4.AndroidComposeTestRule
import androidx.compose.ui.test.junit4.ComposeContentTestRule
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.test.core.app.ActivityScenario
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.rules.ActivityScenarioRule
import org.junit.runner.Description
import org.junit.runners.model.Statement

/**
 * A compose rule whose content survives the host activity being recreated.
 *
 * `createAndroidComposeRule<ComponentActivity>()` sets the test's content on the
 * activity instance it launched, from the test — not from that activity's
 * `onCreate`. When the system recreates the activity (a configuration change it
 * decides on its own — on CI's freshly-booted emulator a burst of keyboard /
 * input-device changes did, while PackageManager was still digesting the
 * just-installed APKs), the new instance comes back EMPTY and every finder fails
 * with "No compose hierarchies found" (DeepLinkInstrumentedTest, Android run
 * 38781236755).
 *
 * This rule re-hosts the same content on every [ComponentActivity] created while
 * the test runs, from that activity's own `onCreate` — exactly what a real
 * activity does — so a recreation shows the same screen again. State the test
 * holds (a ViewModel it built) survives; state the composable `remember`s is
 * rebuilt, as it would be in the app.
 */
class RecreationProofComposeRule private constructor(
    private val inner: AndroidComposeTestRule<ActivityScenarioRule<ComponentActivity>, ComponentActivity>,
) : ComposeContentTestRule by inner {

    constructor() : this(createAndroidComposeRule<ComponentActivity>())

    /** The CURRENT host activity — after a recreation, the new instance. */
    val activity: ComponentActivity get() = inner.activity

    val scenario: ActivityScenario<ComponentActivity> get() = inner.activityRule.scenario

    private val application: Application get() = ApplicationProvider.getApplicationContext()

    private var rehost: Application.ActivityLifecycleCallbacks? = null

    override fun setContent(composable: @Composable () -> Unit) {
        inner.setContent(composable)
        val host = activity
        val callbacks = object : Application.ActivityLifecycleCallbacks {
            override fun onActivityCreated(activity: Activity, savedInstanceState: Bundle?) {
                // Dispatched from Activity.onCreate, after ComponentActivity restored its
                // saved state — the point a real activity calls setContent from.
                if (activity !== host && activity.javaClass == ComponentActivity::class.java) {
                    (activity as ComponentActivity).setContent(content = composable)
                }
            }
            override fun onActivityStarted(activity: Activity) = Unit
            override fun onActivityResumed(activity: Activity) = Unit
            override fun onActivityPaused(activity: Activity) = Unit
            override fun onActivityStopped(activity: Activity) = Unit
            override fun onActivitySaveInstanceState(activity: Activity, outState: Bundle) = Unit
            override fun onActivityDestroyed(activity: Activity) = Unit
        }
        unregister()
        application.registerActivityLifecycleCallbacks(callbacks)
        rehost = callbacks
    }

    private fun unregister() {
        rehost?.let(application::unregisterActivityLifecycleCallbacks)
        rehost = null
    }

    override fun apply(base: Statement, description: Description): Statement =
        inner.apply(object : Statement() {
            override fun evaluate() {
                try {
                    base.evaluate()
                } finally {
                    // Never leak this test's content into a later test's activity.
                    unregister()
                }
            }
        }, description)
}
