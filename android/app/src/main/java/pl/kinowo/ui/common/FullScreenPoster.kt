package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.gestures.detectTapGestures
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
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties

/** testTag the UI/unit tests use to find the open viewer. */
const val FullScreenPosterTag = "fullScreenPoster"

/**
 * Full-screen poster viewer: a black-backdrop dialog showing the whole poster
 * (Fit, letterboxed), pinch-to-zoom and pan up to 5×, dismissed by a tap on the
 * backdrop, the close button, or the system back gesture. Opened from the detail
 * screen by tapping or long-pressing the header poster. Reuses [PosterImage] so
 * the fallback-chain logic stays in one place. Mirrors the iOS full-screen cover.
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
        var scale by remember { mutableFloatStateOf(1f) }
        var offset by remember { mutableStateOf(Offset.Zero) }
        val transform = rememberTransformableState { zoomChange, panChange, _ ->
            scale = (scale * zoomChange).coerceIn(1f, 5f)
            // Only pan while zoomed in; snap back to centre at 1×.
            offset = if (scale > 1f) offset + panChange else Offset.Zero
        }
        Box(
            modifier = Modifier
                .fillMaxSize()
                .background(Color.Black)
                .testTag(FullScreenPosterTag)
                // Tap anywhere on the backdrop to dismiss.
                .pointerInput(Unit) { detectTapGestures(onTap = { onDismiss() }) }
                .transformable(transform),
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
                        translationY = offset.y
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
