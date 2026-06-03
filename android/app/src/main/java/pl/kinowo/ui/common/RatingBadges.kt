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
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.PlatformTextStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.Dp
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
    // Pills scale with viewport width, anchored at the Pixel 9a's ~411dp where
    // the base sizes were tuned (scale 1.0). See RatingBadgeMetrics.
    val scale = RatingBadgeMetrics.scale(LocalConfiguration.current.screenWidthDp)
    val fontSize = (RatingBadgeMetrics.BaseFontSp * scale).sp
    val hPad = (6f * scale).dp
    // No extra vertical padding: the trimmed font box already carries ~4px above
    // the caps and the descent below the baseline (the pill text has no
    // descenders), which is the whole pill height. Any vPad here just stacks on
    // that and makes the pill read tall — see RatingPillVisualPaddingTest.
    val vPad = 0.dp
    val corner = (5f * scale).dp
    val gap = (6f * scale).dp
    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy(gap),
        verticalArrangement = Arrangement.spacedBy(gap),
    ) {
        ratings.imdb?.let { v ->
            LabelValuePill(
                label = "IMDb", labelBg = ImdbYellow, labelFg = Color.Black,
                value = oneDecimal(v), valueFg = ImdbYellow,
                fontSize = fontSize, hPad = hPad, vPad = vPad, corner = corner,
                onClick = { openUrl(context, ratings.imdbURL) },
            )
        }
        ratings.metascore?.let { v ->
            val c = if (v >= 61) MetaGood else if (v >= 40) MetaMid else MetaBad
            SinglePill(
                text = v.toString(), fg = c,
                fontSize = fontSize, hPad = hPad, vPad = vPad, corner = corner,
                onClick = { openUrl(context, ratings.metacriticURL) },
            )
        }
        ratings.rottenTomatoes?.let { v ->
            val c = if (v >= 60) RtFresh else RtRotten
            LabelValuePill(
                label = "RT", labelBg = c, labelFg = Color.White,
                value = "$v%", valueFg = c,
                fontSize = fontSize, hPad = hPad, vPad = vPad, corner = corner,
                onClick = { openUrl(context, ratings.rottenTomatoesURL) },
            )
        }
        ratings.filmweb?.let { v ->
            LabelValuePill(
                label = "FW", labelBg = FwOrange, labelFg = Color.White,
                value = oneDecimal(v), valueFg = FwOrangeLight,
                fontSize = fontSize, hPad = hPad, vPad = vPad, corner = corner,
                onClick = { openUrl(context, ratings.filmwebURL) },
            )
        }
    }
}

/** The trimmed text style shared by every pill — rating badges *and* the
 *  showtime time chips ([Showings]). A bare `Text` adds `includeFontPadding`
 *  leading on top of the font's own ascent/descent, making the pill read taller
 *  still. Turning it off drops that extra leading (the Compose cousin of the
 *  web's `text-box-trim`); the line-box trim/centre is kept as a defensive
 *  no-op so any future lineHeight change stays centred. What it can *not* remove
 *  is the font's intrinsic descent box below the baseline — pill text has no
 *  descenders, so that space sits empty and sets the pill's height floor (see
 *  RatingPillVisualPaddingTest). */
internal fun pillTextStyle(fontSize: TextUnit, weight: FontWeight) = TextStyle(
    fontSize = fontSize,
    fontWeight = weight,
    platformStyle = PlatformTextStyle(includeFontPadding = false),
    lineHeightStyle = LineHeightStyle(
        alignment = LineHeightStyle.Alignment.Center,
        trim = LineHeightStyle.Trim.Both,
    ),
)

/** One-decimal score for the IMDb / Filmweb pills. Always shows the tenths
 *  place — a whole-number score like 7.0 renders "7.0", not "7" — and pins
 *  [Locale.US] so the separator is a dot, never a Polish-locale comma. Matches
 *  the web's `f"$r%.1f"` and iOS's `Film.Ratings.scoreText`. */
internal fun oneDecimal(v: Double): String = String.format(Locale.US, "%.1f", v)

@Composable
private fun LabelValuePill(
    label: String, labelBg: Color, labelFg: Color,
    value: String, valueFg: Color,
    fontSize: TextUnit, hPad: Dp, vPad: Dp, corner: Dp,
    onClick: () -> Unit,
) {
    Row(modifier = Modifier.clip(RoundedCornerShape(corner)).clickable(onClick = onClick)) {
        Text(
            label, color = labelFg, style = pillTextStyle(fontSize, FontWeight.Bold),
            modifier = Modifier.background(labelBg).padding(horizontal = hPad, vertical = vPad),
        )
        Text(
            value, color = valueFg, style = pillTextStyle(fontSize, FontWeight.SemiBold),
            modifier = Modifier.background(CardElevated).padding(horizontal = hPad, vertical = vPad),
        )
    }
}

@Composable
private fun SinglePill(
    text: String, fg: Color,
    fontSize: TextUnit, hPad: Dp, vPad: Dp, corner: Dp,
    onClick: () -> Unit,
) {
    Text(
        text, color = fg, style = pillTextStyle(fontSize, FontWeight.Bold),
        modifier = Modifier
            .clip(RoundedCornerShape(corner))
            .clickable(onClick = onClick)
            .background(CardElevated)
            .padding(horizontal = hPad, vertical = vPad),
    )
}
