package pl.kinowo.ui.common

import androidx.compose.animation.core.animate
import androidx.compose.animation.core.spring
import androidx.compose.animation.core.tween
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.gestures.detectVerticalDragGestures
import androidx.compose.foundation.gestures.rememberTransformableState
import androidx.compose.foundation.gestures.transformable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableFloatStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import kotlinx.coroutines.launch
import kotlin.math.abs

/** testTag the UI/unit tests use to find the open viewer. */
const val FullScreenPosterTag = "fullScreenPoster"

/**
 * Full-screen poster viewer: a black-backdrop dialog showing the whole poster
 * (Fit, letterboxed), pinch-to-zoom and pan up to 5×, dismissed by a tap on the
 * backdrop, the close button, a swipe up or down (while not zoomed in), or the
 * system back gesture. Opened from the detail screen by tapping or long-pressing
 * the header poster. Reuses [PosterImage] so the fallback-chain logic stays in
 * one place. Mirrors the iOS full-screen cover.
 */
@Composable
fun FullScreenPoster(
    chain: List<String>,
    contentDescription: String?,
    onDismiss: () -> Unit,
) {
    Dialog(
        onDismissRequest = onDismiss,
        // The poster fills the whole screen, not the centred platform dialog box.
        properties = DialogProperties(usePlatformDefaultWidth = false),
    ) {
        val scope = rememberCoroutineScope()
        val density = LocalDensity.current
        val screenHeightPx = with(density) { LocalConfiguration.current.screenHeightDp.dp.toPx() }
        val dismissPx = with(density) { 80.dp.toPx() }

        var scale by remember { mutableFloatStateOf(1f) }
        var offset by remember { mutableStateOf(Offset.Zero) }
        // Vertical translation of an in-progress swipe-to-dismiss (only while not
        // zoomed). The poster follows the finger; on release it either flies the
        // rest of the way off-screen (dismiss) or springs back to centre.
        var dismissY by remember { mutableFloatStateOf(0f) }
        val transform = rememberTransformableState { zoomChange, panChange, _ ->
            scale = (scale * zoomChange).coerceIn(1f, 5f)
            // Only pan while zoomed in; snap back to centre at 1×.
            offset = if (scale > 1f) offset + panChange else Offset.Zero
        }
        Box(
            modifier = Modifier
                .fillMaxSize()
                // Backdrop dims toward transparent in step with the swipe — fully
                // clear by the time the poster has travelled a screen-height off,
                // so the fly-off reads as a dismissal and the screen behind shows
                // through. drawBehind reads dismissY at draw time, so the drag
                // repaints without recomposing.
                .drawBehind {
                    val progress = (abs(dismissY) / screenHeightPx).coerceIn(0f, 1f)
                    drawRect(Color.Black, alpha = 1f - progress)
                }
                .testTag(FullScreenPosterTag)
                // Tap anywhere on the backdrop to dismiss.
                .pointerInput(Unit) { detectTapGestures(onTap = { onDismiss() }) }
                .transformable(transform)
                // Swipe up or down to flick the poster away — but only at rest
                // (scale == 1×). While zoomed in, a vertical drag pans instead, so
                // the detector is keyed off the zoom state and steps aside above 1×.
                // Placed AFTER transformable so it wins the single-finger vertical
                // drag in the main pass (transformable would otherwise consume it
                // as a pan before this detector could claim it for dismissal).
                .pointerInput(scale <= 1f) {
                    if (scale > 1f) return@pointerInput
                    detectVerticalDragGestures(
                        onDragEnd = {
                            if (abs(dismissY) > dismissPx) {
                                // Fly the poster the rest of the way off-screen in
                                // the thrown direction, then dismiss — so an up-swipe
                                // exits up and a down-swipe exits down. Mirrors the
                                // iOS fly-off.
                                val target = if (dismissY < 0f) -screenHeightPx else screenHeightPx
                                scope.launch {
                                    animate(dismissY, target, animationSpec = tween(220)) { v, _ ->
                                        dismissY = v
                                    }
                                    onDismiss()
                                }
                            } else {
                                // Not far enough: settle back to centre.
                                scope.launch {
                                    animate(dismissY, 0f, animationSpec = spring()) { v, _ ->
                                        dismissY = v
                                    }
                                }
                            }
                        },
                        onVerticalDrag = { change, amount ->
                            dismissY += amount
                            change.consume()
                        },
                    )
                },
            contentAlignment = Alignment.Center,
        ) {
            PosterImage(
                chain = chain,
                contentDescription = contentDescription,
                contentScale = ContentScale.Fit,
                background = Color.Transparent,
                modifier = Modifier
                    .fillMaxSize()
                    .graphicsLayer {
                        scaleX = scale
                        scaleY = scale
                        translationX = offset.x
                        translationY = offset.y + dismissY
                    },
            )
            IconButton(
                onClick = onDismiss,
                modifier = Modifier.align(Alignment.TopEnd).padding(8.dp),
            ) {
                Icon(Icons.Filled.Close, contentDescription = "Zamknij", tint = Color.White)
            }
        }
    }
}
