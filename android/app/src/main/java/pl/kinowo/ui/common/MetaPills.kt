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
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.MetaPillText
import pl.kinowo.ui.theme.MetaYearText

/**
 * Runtime / year / genre row for a film's title block — the Android
 * counterpart of the web `_movieCard` / `/film` `.pill.runtime`,
 * `.pill.year`, `.pill.genre` row. The year renders as plain text
 * (matching the web's `.year { background: transparent; border: none }`),
 * runtime and genres as rounded pills. Genres default to none — the
 * listing card omits them; the detail screen passes them all. The
 * age-rating certificate ([ageRating], e.g. BBFC "15") renders as its
 * own pill immediately after the year when present, and is dropped when
 * null — most non-UK films carry no rating. Renders nothing when there's
 * no runtime, year, genre, or age rating.
 */
@OptIn(ExperimentalLayoutApi::class)
@Composable
fun MetaPills(
    runtimeMinutes: Int?,
    releaseYear: Int?,
    genres: List<String> = emptyList(),
    ageRating: String? = null,
    scale: Float = 1f,
    modifier: Modifier = Modifier,
) {
    val runtime = runtimeMinutes?.takeIf { it > 0 }?.let { formatRuntime(it) }
    val year = releaseYear?.toString()
    val age = ageRating?.takeIf { it.isNotBlank() }
    if (runtime == null && year == null && genres.isEmpty() && age == null) return
    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy((6 * scale).dp),
        verticalArrangement = Arrangement.spacedBy((6 * scale).dp),
    ) {
        // Bottom-align so the larger plain-text year shares a bottom
        // edge with the smaller pills.
        val itemAlign = Modifier.align(Alignment.Bottom)
        runtime?.let { Pill(it, scale, itemAlign) }
        year?.let { YearText(it, scale, itemAlign) }
        age?.let { Pill(it, scale, itemAlign.testTag(AgeRatingBadgeTag)) }
        for (genre in genres) Pill(genre, scale, itemAlign)
    }
}

/** Test tag on the age-rating pill so a Compose test can assert its
 *  presence/absence without matching the certificate string itself. */
const val AgeRatingBadgeTag = "ageRatingBadge"

@Composable
private fun Pill(label: String, scale: Float, modifier: Modifier = Modifier) {
    Text(
        label,
        color = MetaPillText,
        fontSize = (11 * scale).sp,
        fontWeight = FontWeight.Medium,
        modifier = modifier
            .clip(RoundedCornerShape(50))
            .background(CardElevated)
            .padding(horizontal = (8 * scale).dp, vertical = (3 * scale).dp),
    )
}

/**
 * Year as plain text, mirroring the web's `.year` (no pill background,
 * dimmer `#888` ink). Rendered a touch larger than the pills; the row
 * bottom-aligns items and the matching vertical padding keeps the
 * year's text bottom flush with the pilled text bottom.
 */
@Composable
private fun YearText(label: String, scale: Float, modifier: Modifier = Modifier) {
    Text(
        label,
        color = MetaYearText,
        fontSize = (13 * scale).sp,
        fontWeight = FontWeight.Medium,
        modifier = modifier.padding(vertical = (3 * scale).dp),
    )
}

/** "157" → "2h 37min", "45" → "45min". Matches the web/iOS runtime label. */
fun formatRuntime(totalMinutes: Int): String {
    val hours = totalMinutes / 60
    val minutes = totalMinutes % 60
    return when {
        hours > 0 && minutes > 0 -> "${hours}h ${minutes}min"
        hours > 0 -> "${hours}h"
        else -> "${minutes}min"
    }
}
