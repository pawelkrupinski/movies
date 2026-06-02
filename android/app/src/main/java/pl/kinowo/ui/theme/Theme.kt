package pl.kinowo.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color

/** Brand tint — RGB(0.42, 0.67, 0.87), the iOS accent. */
val Brand = Color(0xFF6BABDE)
val Background = Color(0xFF0B0B12)
val CardSurface = Color(0xFF1E1E2E)
val CardElevated = Color(0xFF2A2A3E)
val CinemaBlue = Color(0xFFAAD4FF)
val Divider = Color(0xFF3A3A6E)
val TextSecondary = Color(0xB3FFFFFF)

// Long-press room tooltip on a showtime pill (mirrors iOS ShowtimeBadge).
val RoomTooltipBackground = Color(0xFF0E0E1E)
val RoomTooltipBorder = Color(0xFF3B3B6E)
val RoomTooltipText = Color(0xFFD9D9D9)

// Rating-badge palette (mirrors iOS RatingBadgesView).
val ImdbYellow = Color(0xFFF5C518)
val MetaGood = Color(0xFF66CC66)
val MetaMid = Color(0xFFE0C040)
val MetaBad = Color(0xFFE05050)
val RtFresh = Color(0xFFFA320A)
val RtRotten = Color(0xFF1A8F1A)
val FwOrange = Color(0xFFFF6C00)
val FwOrangeLight = Color(0xFFFF9C4A)

private val KinowoColors = darkColorScheme(
    primary = Brand,
    onPrimary = Color(0xFF06121C),
    secondary = CinemaBlue,
    background = Background,
    onBackground = Color.White,
    surface = Background,
    onSurface = Color.White,
    surfaceVariant = CardSurface,
    onSurfaceVariant = TextSecondary,
    outline = Divider,
)

@Composable
fun KinowoTheme(content: @Composable () -> Unit) {
    // The app is dark-only (matches iOS `preferredColorScheme(.dark)`).
    @Suppress("UNUSED_EXPRESSION") isSystemInDarkTheme()
    MaterialTheme(
        colorScheme = KinowoColors,
        typography = MaterialTheme.typography,
        content = content,
    )
}
