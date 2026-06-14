package pl.kinowo.ui.detail

import android.graphics.Color
import android.os.SystemClock
import android.util.Log
import android.view.View
import android.view.ViewGroup
import android.view.WindowManager
import android.webkit.WebChromeClient
import android.webkit.WebView
import androidx.activity.ComponentActivity
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

/**
 * On-device proof that inline trailers actually PLAY — the layer the off-device
 * [TrailerPlayerTest] (Robolectric) can't reach.
 *
 * Why JavaScript and not pixels: PixelCopy / screencap read YouTube's video back
 * as a blank surface (it renders on a hardware overlay), so pixel-motion sampling
 * measures page chrome, not the film — it can't tell a playing trailer from an
 * error screen. Instead we read playback state directly:
 *
 *  - [productionEmbedStrategyPlays] loads the embed exactly as production does —
 *    an iframe under our real site origin via loadDataWithBaseURL — but wired
 *    through the YouTube IFrame Player API, whose onStateChange/onError fire in
 *    the parent doc (same-origin), so the player's true state is readable across
 *    the cross-origin iframe boundary. Asserts it reaches PLAYING with no error.
 *  - [topLevelNavigationIsRejected] is the negative control: the pre-fix approach
 *    (a top-level loadUrl to the embed URL) returns YouTube Error 153 and never
 *    plays. Without it, the positive test couldn't prove it catches the bug.
 *
 * Embed URLs are built for iframes; a top-level load has no embedding referer, so
 * YouTube rejects it (Error 153). The fix — and the proven iOS approach — is to
 * embed under our own origin, the referer YouTube already allows on the web.
 *
 * Run on a connected, unlocked, online device:
 *   `./gradlew app:connectedDebugAndroidTest \
 *     -Pandroid.testInstrumentationRunnerArguments.class=pl.kinowo.ui.detail.TrailerPlaybackTest`
 * Not in CI (needs a device + live YouTube). Uses a public, embeddable clip;
 * swap [VIDEO_ID] if it ever goes private.
 */
@RunWith(AndroidJUnit4::class)
class TrailerPlaybackTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    @Test
    fun productionEmbedStrategyPlays() {
        val web = render { it.loadDataWithBaseURL(SITE_ORIGIN, iframeApiPage(), "text/html", "utf-8", null) }

        // PlayerState: -1 unstarted, 0 ended, 1 playing, 2 paused, 3 buffering, 5 cued.
        var state = -99
        var error: String? = null
        val deadline = SystemClock.uptimeMillis() + PLAY_WAIT_MS
        while (SystemClock.uptimeMillis() < deadline) {
            val report = evalJson(web, "window.report?report():'na|na'").split("|")
            state = report.getOrNull(0)?.toIntOrNull() ?: state
            error = report.getOrNull(1)?.takeIf { it != "null" && it != "na" } ?: error
            Log.i(TAG, "embed state=$state error=$error")
            if (state == STATE_PLAYING || error != null) break
            SystemClock.sleep(1000)
        }
        assertTrue("Embed reported error $error — the player was rejected", error == null)
        assertEquals("Trailer never reached PLAYING (final state=$state)", STATE_PLAYING, state)
    }

    @Test
    fun topLevelNavigationIsRejected() {
        val web = render { it.loadUrl("$EMBED?autoplay=1&playsinline=1") }

        // Same-origin youtube.com document: read the <video> clock and the error
        // screen. A top-level embed load yields Error 153 and a clock stuck at 0.
        SystemClock.sleep(REJECT_WAIT_MS)
        val currentTime = evalJson(web, CURRENT_TIME_JS).toDoubleOrNull() ?: -1.0
        val errorScreen = evalJson(web, "(''+!!document.querySelector('.ytp-error'))")
        val body = evalJson(web, "document.body.innerText.replace(/\\s+/g,' ').slice(0,120)")
        Log.i(TAG, "topLevel currentTime=$currentTime errorScreen=$errorScreen body=$body")

        assertTrue(
            "A top-level embed navigation unexpectedly played (currentTime=$currentTime, body=$body). " +
                "If YouTube now allows this, the embed strategy can be revisited.",
            currentTime <= 0.0,
        )
    }

    /** Render a fresh full-screen WebView, force the screen on, apply [load]. */
    private fun render(load: (WebView) -> Unit): WebView {
        compose.setContent {
            AndroidView(
                modifier = Modifier.fillMaxSize(),
                factory = { context ->
                    WebView(context).apply {
                        settings.javaScriptEnabled = true
                        settings.mediaPlaybackRequiresUserGesture = false
                        settings.domStorageEnabled = true
                        webChromeClient = WebChromeClient()
                        setBackgroundColor(Color.BLACK)
                    }
                },
                update = { web -> if (web.tag == null) { web.tag = "x"; load(web) } },
            )
        }
        compose.runOnUiThread {
            // A WebView won't play video unless its surface is on-screen.
            compose.activity.setTurnScreenOn(true)
            compose.activity.setShowWhenLocked(true)
            compose.activity.window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)
        }
        compose.waitForIdle()
        return compose.runOnUiThread { findWebView(compose.activity.window.decorView) }
            ?: error("WebView never composed")
    }

    /**
     * The production embedding strategy (iframe under our origin) wired through the
     * IFrame Player API so its state is observable. [report] returns
     * "<state>|<error>" — plain pipe-joined to dodge nested-JSON quote escaping.
     */
    private fun iframeApiPage() = """
        <!DOCTYPE html><html><head>
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <style>*{margin:0;padding:0}body{background:#000}#p{width:100%;height:100%}</style>
        </head><body><div id="p"></div>
        <script>
          window.__s=-99; window.__e=null;
          var t=document.createElement('script'); t.src='https://www.youtube.com/iframe_api';
          document.head.appendChild(t);
          function onYouTubeIframeAPIReady(){
            new YT.Player('p',{videoId:'$VIDEO_ID',
              playerVars:{autoplay:1,playsinline:1,mute:1,origin:'$SITE_ORIGIN'},
              events:{onStateChange:function(e){window.__s=e.data;},
                      onError:function(e){window.__e=e.data;}}});
          }
          window.report=function(){return window.__s+'|'+window.__e;};
        </script></body></html>
    """.trimIndent()

    private fun evalJson(web: WebView, js: String): String {
        val latch = CountDownLatch(1)
        var result = "null"
        compose.runOnUiThread { web.evaluateJavascript(js) { v -> result = v; latch.countDown() } }
        latch.await(3, TimeUnit.SECONDS)
        // evaluateJavascript returns a JSON literal: strip surrounding quotes and
        // unescape the inner ones so callers see the raw string value.
        return result.trim('"').replace("\\\"", "\"").replace("\\\\", "\\")
    }

    private companion object {
        const val TAG = "TrailerExp"
        const val SITE_ORIGIN = "https://kinowo.fly.dev"
        const val VIDEO_ID = "aqz-KE-bpKQ"
        const val EMBED = "https://www.youtube.com/embed/$VIDEO_ID"
        const val CURRENT_TIME_JS =
            "(function(){var v=document.querySelector('video');return v?v.currentTime:-1;})()"
        const val STATE_PLAYING = 1
        const val PLAY_WAIT_MS = 18_000L
        const val REJECT_WAIT_MS = 8_000L
    }
}

private fun findWebView(view: View): WebView? = when (view) {
    is WebView -> view
    is ViewGroup -> (0 until view.childCount).firstNotNullOfOrNull { findWebView(view.getChildAt(it)) }
    else -> null
}
