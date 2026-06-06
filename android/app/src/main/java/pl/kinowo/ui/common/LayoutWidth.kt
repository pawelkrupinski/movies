package pl.kinowo.ui.common

import androidx.compose.runtime.Composable
import androidx.compose.runtime.ReadOnlyComposable
import androidx.compose.ui.platform.LocalConfiguration

/**
 * The width (dp) the UI sizes itself against — the device's PORTRAIT width,
 * held constant across orientation.
 *
 * This is [android.content.res.Configuration.smallestScreenWidthDp]: the
 * smallest `screenWidthDp` the device ever reports, which by definition is its
 * portrait (narrow-dimension) width and does NOT grow when the phone is rotated
 * to landscape. Feeding this — rather than the live `screenWidthDp`, which
 * balloons in landscape — into the width-driven metrics ([ShowtimeChipMetrics],
 * [RatingBadgeMetrics]) keeps fonts, paddings and styles byte-for-byte identical
 * between portrait and landscape on a given phone. In portrait
 * `screenWidthDp == smallestScreenWidthDp`, so portrait rendering is unchanged;
 * only landscape stops scaling up.
 *
 * Two things deliberately do NOT lock to this and read the live width
 * ([viewportWidthDp]) instead: the poster grid (so landscape can fit more
 * columns — see `posterGridColumns`, each still at least a portrait card wide so
 * two-per-row holds) and the search-placement decision (so a landscape phone
 * hosts search inline on the top bar, matching iOS — see `TopBarLayout`).
 */
@Composable
@ReadOnlyComposable
fun layoutWidthDp(): Int = LocalConfiguration.current.smallestScreenWidthDp

/**
 * The LIVE viewport width (dp) — the current `screenWidthDp`, which grows when
 * the phone is rotated to landscape (and shrinks back in portrait).
 *
 * This is what the SEARCH-placement decision keys off, so a landscape phone
 * (wide live width, narrow portrait width) crosses [TopBarLayout.WideThresholdDp]
 * and hosts search inline on the top bar — matching the iOS app, which keys the
 * same decision off its live viewport width. Contrast [layoutWidthDp], which the
 * width-driven metrics lock to so fonts/paddings stay identical across rotation.
 */
@Composable
@ReadOnlyComposable
fun viewportWidthDp(): Int = LocalConfiguration.current.screenWidthDp
