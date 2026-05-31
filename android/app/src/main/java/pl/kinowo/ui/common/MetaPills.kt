package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.ui.theme.CardElevated

/**
 * Runtime / year / genre row for a film's title block — the Android
 * counterpart of the web `_movieCard` / `/film` `.pill.runtime`,
 * `.pill.year`, `.pill.genre` row. The year renders as plain text
 * (matching the web's `.year { background: transparent; border: none }`),
 * runtime and genres as rounded pills. Genres default to none — the
 * listing card omits them; the detail screen passes them all. Renders
 * nothing when there's no runtime, year, or genre.
 */
@OptIn(ExperimentalLayoutApi::class)
@Composable
fun MetaPills(
    runtimeMinutes: Int?,
    releaseYear: Int?,
    genres: List<String> = emptyList(),
    modifier: Modifier = Modifier,
) {
    val runtime = runtimeMinutes?.takeIf { it > 0 }?.let { formatRuntime(it) }
    val year = releaseYear?.toString()
    if (runtime == null && year == null && genres.isEmpty()) return
    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy(6.dp),
        verticalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        // Bottom-align so the larger plain-text year shares a bottom
        // edge with the smaller pills.
        val itemAlign = Modifier.align(Alignment.Bottom)
        runtime?.let { Pill(it, itemAlign) }
        year?.let { YearText(it, itemAlign) }
        for (genre in genres) Pill(genre, itemAlign)
    }
}

@Composable
private fun Pill(label: String, modifier: Modifier = Modifier) {
    Text(
        label,
        color = Color(0xFFCCCCCC),
        fontSize = 11.sp,
        fontWeight = FontWeight.Medium,
        modifier = modifier
            .clip(RoundedCornerShape(50))
            .background(CardElevated)
            .padding(horizontal = 8.dp, vertical = 3.dp),
    )
}

/**
 * Year as plain text, mirroring the web's `.year` (no pill background,
 * dimmer `#888` ink). Rendered a touch larger than the pills; the row
 * bottom-aligns items and the matching vertical padding keeps the
 * year's text bottom flush with the pilled text bottom.
 */
@Composable
private fun YearText(label: String, modifier: Modifier = Modifier) {
    Text(
        label,
        color = Color(0xFF888888),
        fontSize = 13.sp,
        fontWeight = FontWeight.Medium,
        modifier = modifier.padding(vertical = 3.dp),
    )
}

/** "157" → "2h 37min", "45" → "45min". Matches the web/iOS runtime label. */
fun formatRuntime(mins: Int): String {
    val h = mins / 60
    val m = mins % 60
    return when {
        h > 0 && m > 0 -> "${h}h ${m}min"
        h > 0 -> "${h}h"
        else -> "${m}min"
    }
}
