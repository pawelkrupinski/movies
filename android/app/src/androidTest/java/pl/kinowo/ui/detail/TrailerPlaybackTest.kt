package pl.kinowo.ui.detail

import android.app.Activity
import android.graphics.Bitmap
import android.graphics.Color
import android.graphics.Rect
import android.os.Handler
import android.os.Looper
import android.os.SystemClock
import android.view.PixelCopy
import android.view.View
import android.view.ViewGroup
import android.webkit.WebChromeClient
import android.webkit.WebView
import androidx.activity.ComponentActivity
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performScrollTo
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import kotlin.math.abs

/**
 * On-device **behavioural** regression: the inline trailer must actually *play*.
 *
 * This is the layer the off-device [TrailerPlayerTest] (Robolectric) cannot
 * reach. Robolectric's WebView is a shadow that loads nothing, so that test can
 * only pin the *mechanism* — that the player loads an iframe document under our
 * real origin via `loadDataWithBaseURL`, not a dead top-level `loadUrl`. Whether
 * a real YouTube video renders moving pixels can only be observed on a real
 * WebView with real network and a real video decoder — i.e. a connected phone.
 *
 * How it observes "playing" without an API key or cross-origin access into the
 * YouTube iframe: it captures the trailer's pixels twice, a beat apart, with
 * [PixelCopy] (the one capture API that reads hardware/video surfaces from the
 * window — `captureToImage` returns black for an accelerated WebView). A playing
 * video makes consecutive frames differ; a static error page ("Video
 * unavailable", the failure mode of a top-level embed navigation) does not.
 *
 * - [trailerActuallyPlaysOnDevice] renders the real [DetailScreen] and asserts
 *   the trailer animates — the bug this guards is "renders but never plays".
 * - [topLevelEmbedNavigationStaysDead] is the discriminating negative control:
 *   it loads the same video the *old broken way* (top-level `loadUrl` to the
 *   embed URL) and asserts it does NOT animate. Without it, a test that always
 *   passed would be indistinguishable from one that actually catches the bug.
 *
 * Run on a connected, unlocked, network-connected device/emulator:
 *   `./gradlew app:connectedDebugAndroidTest`
 * Not part of CI (no device layer there, and it needs live YouTube).
 *
 * The thresholds below ([MOTION_THRESHOLD] etc.) are deliberately conservative —
 * a playing video moves a large fraction of pixels, a still frame ~none — but
 * they are the one thing to re-check on the first real-device run, per the
 * project's "tune visuals on the device, don't guess" rule.
 */
@RunWith(AndroidJUnit4::class)
class TrailerPlaybackTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    private val film = Film(title = "Test Trailer", posterURL = "https://example.com/x.jpg")

    @Test
    fun trailerActuallyPlaysOnDevice() {
        val details = FilmDetails(title = film.title, trailerURLs = listOf(TRAILER_EMBED))
        compose.setContent { DetailScreen(film, details, onBack = {}) }
        // Bring the trailer into view so PixelCopy reads on-screen pixels.
        compose.onNodeWithText("Zwiastuny").performScrollTo()
        compose.waitForIdle()

        val web = compose.runOnUiThread { findWebView(compose.activity.window.decorView) }
        requireNotNull(web) { "trailer WebView never composed" }

        val best = bestMotionWithin(web, MAX_WAIT_MS, stopAt = MOTION_THRESHOLD)
        if (best < MOTION_THRESHOLD) {
            fail(
                "Trailer never played: best inter-frame motion ${"%.3f".format(best)} stayed " +
                    "below $MOTION_THRESHOLD over ${MAX_WAIT_MS}ms. The embed rendered but the " +
                    "video never moved — the player is dead (see the iframe-under-our-origin fix).",
            )
        }
    }

    @Test
    fun topLevelEmbedNavigationStaysDead() {
        // The pre-fix mechanism: navigate the WebView straight to the embed URL.
        // YouTube rejects this (embed URLs are built for iframes, a top-level load
        // carries no embedding referer) and shows a static error — so the control
        // must observe NO motion. If this ever starts animating, YouTube changed
        // its behaviour and the discriminator (and the fix's rationale) needs a
        // fresh look.
        compose.setContent {
            AndroidView(
                modifier = Modifier.fillMaxSize(),
                factory = { ctx ->
                    WebView(ctx).apply {
                        settings.javaScriptEnabled = true
                        settings.mediaPlaybackRequiresUserGesture = false
                        settings.domStorageEnabled = true
                        webChromeClient = WebChromeClient()
                        setBackgroundColor(Color.BLACK)
                    }
                },
                update = { it.loadUrl(TRAILER_TOP_LEVEL) },
            )
        }
        compose.waitForIdle()

        val web = compose.runOnUiThread { findWebView(compose.activity.window.decorView) }
        requireNotNull(web) { "control WebView never composed" }

        val best = bestMotionWithin(web, CONTROL_WAIT_MS, stopAt = null)
        assertTrue(
            "A top-level navigation to the embed URL animated (motion ${"%.3f".format(best)} ≥ " +
                "$MOTION_THRESHOLD) — it was expected to stay a dead/static error frame. The test " +
                "can no longer tell a working trailer from a broken one; revisit the embed fix.",
            best < MOTION_THRESHOLD,
        )
    }

    /**
     * Captures the [web] view region twice, [FRAME_GAP_MS] apart, repeatedly until
     * [maxWaitMs] elapses, returning the largest fraction of sampled pixels that
     * changed between a pair. If [stopAt] is non-null, returns early once a pair
     * reaches it (so a clearly-playing video doesn't burn the full budget).
     */
    private fun bestMotionWithin(web: WebView, maxWaitMs: Long, stopAt: Double?): Double {
        val deadline = SystemClock.uptimeMillis() + maxWaitMs
        var best = 0.0
        while (SystemClock.uptimeMillis() < deadline) {
            val rect = compose.runOnUiThread { webViewWindowRect(web) }
            if (rect.width() < 2 || rect.height() < 2) {
                SystemClock.sleep(400)
                continue
            }
            val first = capture(compose.activity, rect)
            SystemClock.sleep(FRAME_GAP_MS)
            val second = capture(compose.activity, rect)
            best = maxOf(best, movingFraction(first, second, PIXEL_DELTA, SAMPLE_STEP))
            if (stopAt != null && best >= stopAt) return best
            SystemClock.sleep(SETTLE_GAP_MS)
        }
        return best
    }

    private companion object {
        // Big Buck Bunny — a public, embeddable, motion-rich clip. The video only
        // needs to be playable-when-embedded; swap the id if it ever goes private.
        const val VIDEO_ID = "aqz-KE-bpKQ"
        const val TRAILER_EMBED = "https://www.youtube.com/embed/$VIDEO_ID"
        const val TRAILER_TOP_LEVEL = "https://www.youtube.com/embed/$VIDEO_ID?autoplay=1&playsinline=1"

        /** Per-pixel channel-sum change that counts as "moved" (ignores AA noise). */
        const val PIXEL_DELTA = 24

        /** Sample every Nth pixel in both axes — enough signal, far cheaper. */
        const val SAMPLE_STEP = 4

        /** Fraction of sampled pixels that must move between a frame pair to call
         *  it "playing". A still frame ≈ 0; a playing video moves a large share. */
        const val MOTION_THRESHOLD = 0.06

        const val FRAME_GAP_MS = 900L
        const val SETTLE_GAP_MS = 400L
        const val MAX_WAIT_MS = 30_000L
        const val CONTROL_WAIT_MS = 12_000L
    }
}

private fun findWebView(view: View): WebView? = when (view) {
    is WebView -> view
    is ViewGroup -> (0 until view.childCount).firstNotNullOfOrNull { findWebView(view.getChildAt(it)) }
    else -> null
}

/** The view's bounds in its window — the source rect [PixelCopy] copies from. */
private fun webViewWindowRect(web: WebView): Rect {
    val loc = IntArray(2)
    web.getLocationInWindow(loc)
    return Rect(loc[0], loc[1], loc[0] + web.width, loc[1] + web.height)
}

/** Copies [rect] (window coordinates) out of the live window into a Bitmap. */
private fun capture(activity: Activity, rect: Rect): Bitmap {
    val bmp = Bitmap.createBitmap(rect.width(), rect.height(), Bitmap.Config.ARGB_8888)
    val latch = CountDownLatch(1)
    val result = AtomicInteger(PixelCopy.ERROR_UNKNOWN)
    PixelCopy.request(
        activity.window,
        rect,
        bmp,
        { code -> result.set(code); latch.countDown() },
        Handler(Looper.getMainLooper()),
    )
    check(latch.await(5, TimeUnit.SECONDS)) { "PixelCopy timed out" }
    check(result.get() == PixelCopy.SUCCESS) { "PixelCopy failed with code ${result.get()}" }
    return bmp
}

/**
 * Fraction of pixels (sampled every [step] in both axes) whose colour changed by
 * more than [pixelDelta] channel-sum between the two frames.
 */
private fun movingFraction(a: Bitmap, b: Bitmap, pixelDelta: Int, step: Int): Double {
    require(a.width == b.width && a.height == b.height) { "frame sizes differ" }
    var changed = 0
    var total = 0
    var y = 0
    while (y < a.height) {
        var x = 0
        while (x < a.width) {
            total++
            val pa = a.getPixel(x, y)
            val pb = b.getPixel(x, y)
            val delta = abs(Color.red(pa) - Color.red(pb)) +
                abs(Color.green(pa) - Color.green(pb)) +
                abs(Color.blue(pa) - Color.blue(pb))
            if (delta > pixelDelta) changed++
            x += step
        }
        y += step
    }
    return if (total == 0) 0.0 else changed.toDouble() / total
}
