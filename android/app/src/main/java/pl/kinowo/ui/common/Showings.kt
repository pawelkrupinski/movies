package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.filter.FormatTokenFilter
import pl.kinowo.model.Film
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.TextSecondary

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
                    horizontalArrangement = Arrangement.spacedBy(6.dp),
                    verticalArrangement = Arrangement.spacedBy(6.dp),
                ) {
                    for (st in slots) {
                        ShowtimeChip(
                            time = st.time,
                            format = FormatTokenFilter.filter(st.format, common),
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

@Composable
private fun ShowtimeChip(time: String, format: String, onClick: (() -> Unit)?) {
    val base = Modifier
        .clip(RoundedCornerShape(6.dp))
        .let { if (onClick != null) it.clickable(onClick = onClick) else it }
        .background(CardElevated)
        .padding(horizontal = 8.dp, vertical = 4.dp)
    Row(base, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
        Text(time, color = androidx.compose.ui.graphics.Color.White, fontSize = 13.sp, fontWeight = FontWeight.SemiBold)
        if (format.isNotEmpty()) {
            Text(format, color = TextSecondary, fontSize = 10.sp, fontWeight = FontWeight.Medium)
        }
    }
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
