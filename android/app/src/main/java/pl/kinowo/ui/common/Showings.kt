package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.zIndex
import pl.kinowo.filter.FormatTokenFilter
import pl.kinowo.model.Film
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.RoomTooltipBackground
import pl.kinowo.ui.theme.RoomTooltipBorder
import pl.kinowo.ui.theme.RoomTooltipText
import pl.kinowo.ui.theme.ShowtimeChipBackground
import pl.kinowo.ui.theme.ShowtimeChipBackgroundPressed
import pl.kinowo.ui.theme.TextSecondary

/** Test tag on the showtime pill, so the padding/visual tests can find the
 *  rendered chip and measure its height. */
internal const val ShowtimeChipTestTag = "showtime-chip"

/**
 * The day-by-day showtimes tree for a film: day label → (optional cinema
 * label) → a wrapping row of showtime chips. Mirrors iOS `ShowingsView`.
 *
 * @param showCinemaHeaders drop the cinema label when the parent already
 *   names the cinema (Kina-tab sections).
 * @param maxChips truncate to roughly this many chips on cards, appending a
 *   "+N seansów" hint; `null` shows everything (detail screen).
 */
@OptIn(ExperimentalLayoutApi::class)
@Composable
fun Showings(
    film: Film,
    showCinemaHeaders: Boolean = true,
    maxChips: Int? = null,
    modifier: Modifier = Modifier,
) {
    val context = LocalContext.current
    val total = film.showings.sumOf { d -> d.cinemas.sumOf { it.showtimes.size } }
    var budget = maxChips ?: Int.MAX_VALUE
    var shown = 0

    Column(modifier = modifier, verticalArrangement = Arrangement.spacedBy(8.dp)) {
        for (day in film.showings) {
            if (budget <= 0) break
            Text(
                text = day.label.uppercase(),
                color = TextSecondary,
                fontSize = 11.sp,
                fontWeight = FontWeight.SemiBold,
            )
            for (cg in day.cinemas) {
                if (budget <= 0) break
                if (showCinemaHeaders) {
                    Text(
                        text = cg.cinema,
                        color = CinemaBlue,
                        fontSize = 12.sp,
                        fontWeight = FontWeight.Medium,
                        modifier = if (cg.cinemaURL != null) {
                            Modifier.clickable { openUrl(context, cg.cinemaURL) }
                        } else Modifier,
                    )
                }
                val common = FormatTokenFilter.commonTokens(cg)
                val slots = cg.showtimes.take(budget)
                budget -= slots.size
                shown += slots.size
                FlowRow(
                    horizontalArrangement = Arrangement.spacedBy(4.dp),
                    verticalArrangement = Arrangement.spacedBy(4.dp),
                ) {
                    for (st in slots) {
                        ShowtimeChip(
                            time = st.time,
                            format = FormatTokenFilter.filter(st.format, common),
                            room = st.displayRoom,
                            onClick = st.bookingURL?.let { url -> { openUrl(context, url) } },
                        )
                    }
                }
            }
        }
        val hidden = total - shown
        if (hidden > 0) {
            Text(
                text = "+$hidden ${seansForm(hidden)}",
                color = TextSecondary,
                fontSize = 11.sp,
                fontWeight = FontWeight.Medium,
            )
        }
    }
}

/**
 * A single showtime pill. A quick tap opens the booking link (when present);
 * pressing and holding reveals the room name in a tooltip above the pill for
 * as long as the finger stays down, mirroring the web's long-press tooltip and
 * iOS `ShowtimeBadge`. The room is only shown when the scraper provided one.
 */
@Composable
private fun ShowtimeChip(time: String, format: String, room: String?, onClick: (() -> Unit)?) {
    var holding by remember { mutableStateOf(false) }
    val base = Modifier
        // Outermost, so the tagged node's bounds include the padding (a tag
        // placed after .padding() would wrap only the inner text).
        .testTag(ShowtimeChipTestTag)
        .clip(RoundedCornerShape(6.dp))
        .pointerInput(room, onClick) {
            detectTapGestures(
                onTap = { onClick?.invoke() },
                onLongPress = { if (room != null) holding = true },
                onPress = {
                    tryAwaitRelease()
                    holding = false
                },
            )
        }
        .background(if (holding) ShowtimeChipBackgroundPressed else ShowtimeChipBackground)
        // The time uses the trimmed `pillTextStyle` (no includeFontPadding), so it
        // doesn't read tall on its own; the 4dp inset then adds the breathing room
        // around it. Inset + trim pinned by ShowtimeChipPaddingTest.
        .padding(4.dp)
    Box(contentAlignment = Alignment.TopCenter) {
        Row(base, horizontalArrangement = Arrangement.spacedBy(2.dp)) {
            Text(time, color = CinemaBlue, style = pillTextStyle(11.sp, FontWeight.SemiBold))
            if (format.isNotEmpty()) {
                Text(format, color = CinemaBlue.copy(alpha = 0.7f), style = pillTextStyle(9.sp, FontWeight.Medium))
            }
        }
        if (holding && room != null) {
            RoomTooltip(room, Modifier.align(Alignment.TopCenter).offset(y = (-30).dp).zIndex(1f))
        }
    }
}

@Composable
private fun RoomTooltip(room: String, modifier: Modifier = Modifier) {
    Text(
        text = room,
        color = RoomTooltipText,
        fontSize = 10.sp,
        fontWeight = FontWeight.Medium,
        modifier = modifier
            .clip(RoundedCornerShape(4.dp))
            .background(RoomTooltipBackground)
            .border(1.dp, RoomTooltipBorder, RoundedCornerShape(4.dp))
            .padding(horizontal = 7.dp, vertical = 3.dp),
    )
}

/** Polish plural: 1 seans, 2–4 seanse, else seansów. */
private fun seansForm(n: Int): String {
    val mod10 = n % 10
    val mod100 = n % 100
    return when {
        n == 1 -> "seans"
        mod10 in 2..4 && mod100 !in 12..14 -> "seanse"
        else -> "seansów"
    }
}
