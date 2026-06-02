package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import java.util.Locale
import pl.kinowo.model.Ratings
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.FwOrange
import pl.kinowo.ui.theme.FwOrangeLight
import pl.kinowo.ui.theme.ImdbYellow
import pl.kinowo.ui.theme.MetaBad
import pl.kinowo.ui.theme.MetaGood
import pl.kinowo.ui.theme.MetaMid
import pl.kinowo.ui.theme.RtFresh
import pl.kinowo.ui.theme.RtRotten

/**
 * The IMDb / Metascore / RT / Filmweb pill row. Each pill links to its
 * external rating page. Mirrors iOS `RatingBadgesView` colours.
 */
@OptIn(ExperimentalLayoutApi::class)
@Composable
fun RatingBadges(ratings: Ratings, modifier: Modifier = Modifier) {
    if (ratings.isEmpty) return
    val context = LocalContext.current
    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy(6.dp),
        verticalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        ratings.imdb?.let { v ->
            LabelValuePill(
                label = "IMDb", labelBg = ImdbYellow, labelFg = Color.Black,
                value = oneDecimal(v), valueFg = ImdbYellow,
                onClick = { openUrl(context, ratings.imdbURL) },
            )
        }
        ratings.metascore?.let { v ->
            val c = if (v >= 61) MetaGood else if (v >= 40) MetaMid else MetaBad
            SinglePill(text = v.toString(), fg = c, onClick = { openUrl(context, ratings.metacriticURL) })
        }
        ratings.rottenTomatoes?.let { v ->
            val c = if (v >= 60) RtFresh else RtRotten
            LabelValuePill(
                label = "RT", labelBg = c, labelFg = Color.White,
                value = "$v%", valueFg = c,
                onClick = { openUrl(context, ratings.rottenTomatoesURL) },
            )
        }
        ratings.filmweb?.let { v ->
            LabelValuePill(
                label = "FW", labelBg = FwOrange, labelFg = Color.White,
                value = oneDecimal(v), valueFg = FwOrangeLight,
                onClick = { openUrl(context, ratings.filmwebURL) },
            )
        }
    }
}

/** One-decimal score for the IMDb / Filmweb pills. Always shows the tenths
 *  place — a whole-number score like 7.0 renders "7.0", not "7" — and pins
 *  [Locale.US] so the separator is a dot, never a Polish-locale comma. Matches
 *  the web's `f"$r%.1f"` and iOS's `Film.Ratings.scoreText`. */
internal fun oneDecimal(v: Double): String = String.format(Locale.US, "%.1f", v)

@Composable
private fun LabelValuePill(
    label: String, labelBg: Color, labelFg: Color,
    value: String, valueFg: Color, onClick: () -> Unit,
) {
    Row(modifier = Modifier.clip(RoundedCornerShape(5.dp)).clickable(onClick = onClick)) {
        Text(
            label, color = labelFg, fontSize = 11.sp, fontWeight = FontWeight.Bold,
            modifier = Modifier.background(labelBg).padding(horizontal = 6.dp, vertical = 2.dp),
        )
        Text(
            value, color = valueFg, fontSize = 11.sp, fontWeight = FontWeight.SemiBold,
            modifier = Modifier.background(CardElevated).padding(horizontal = 6.dp, vertical = 2.dp),
        )
    }
}

@Composable
private fun SinglePill(text: String, fg: Color, onClick: () -> Unit) {
    Text(
        text, color = fg, fontSize = 11.sp, fontWeight = FontWeight.Bold,
        modifier = Modifier
            .clip(RoundedCornerShape(5.dp))
            .clickable(onClick = onClick)
            .background(CardElevated)
            .padding(horizontal = 6.dp, vertical = 2.dp),
    )
}
