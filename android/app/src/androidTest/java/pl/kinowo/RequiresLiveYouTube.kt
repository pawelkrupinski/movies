package pl.kinowo

/**
 * Marks an instrumented test that plays a real video from youtube.com, so its
 * verdict depends on how YouTube treats the device's network, not only on our
 * code. On a GitHub Actions runner the production embed got error 150
 * ("playback not allowed") before a frame loaded (android.yml run 35915030310),
 * while the same API-34 google_apis image, with the same WebView 113, played
 * it locally. The image and the code match, so the difference is the network
 * YouTube sees: it refuses the embed to a datacenter address.
 *
 * The CI emulator lane leaves these out (`devtest.sh --skip-live-youtube`);
 * run them on a phone or local emulator whenever the trailer embed changes.
 */
@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class RequiresLiveYouTube
