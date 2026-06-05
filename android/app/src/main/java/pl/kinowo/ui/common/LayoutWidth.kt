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
 * [RatingBadgeMetrics]) and the search-placement decision keeps fonts, paddings
 * and styles byte-for-byte identical between portrait and landscape on a given
 * phone. In portrait `screenWidthDp == smallestScreenWidthDp`, so portrait
 * rendering is unchanged; only landscape stops scaling up.
 *
 * The poster grid deliberately does NOT lock to this — it reads the live width
 * so landscape can fit more columns (see `posterGridColumns`), each still at
 * least a portrait card wide so two-per-row holds.
 */
@Composable
@ReadOnlyComposable
fun layoutWidthDp(): Int = LocalConfiguration.current.smallestScreenWidthDp
