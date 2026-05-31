package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.ui.theme.CardElevated

/**
 * Runtime / year / genre pills for a film's title block — the Android
 * counterpart of the web `_movieCard` / `/film` `.pill.runtime`,
 * `.pill.year`, `.pill.genre` row. The listing card caps genres at three
 * (`maxGenres = 3`); the detail screen passes `null` to show them all.
 * Renders nothing when there's no runtime, year, or genre.
 */
@OptIn(ExperimentalLayoutApi::class)
@Composable
fun MetaPills(
    runtimeMinutes: Int?,
    releaseYear: Int?,
    genres: List<String>,
    modifier: Modifier = Modifier,
    maxGenres: Int? = null,
) {
    val labels = buildList {
        runtimeMinutes?.takeIf { it > 0 }?.let { add(formatRuntime(it)) }
        releaseYear?.let { add(it.toString()) }
        addAll(if (maxGenres != null) genres.take(maxGenres) else genres)
    }
    if (labels.isEmpty()) return
    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy(6.dp),
        verticalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        for (label in labels) {
            Text(
                label,
                color = Color(0xFFCCCCCC),
                fontSize = 11.sp,
                fontWeight = FontWeight.Medium,
                modifier = Modifier
                    .clip(RoundedCornerShape(50))
                    .background(CardElevated)
                    .padding(horizontal = 8.dp, vertical = 3.dp),
            )
        }
    }
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
