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
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.compositionLocalOf
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntRect
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Popup
import androidx.compose.ui.window.PopupPositionProvider
import androidx.compose.ui.window.PopupProperties
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
 * Live-tunable rendering parameters for a showtime chip, read by [ShowtimeChip]
 * and the [Showings] flow row from [LocalShowtimeChipStyle].
 *
 * The defaults are the SHIPPING chip values, dialled in on the tuning screen at
 * the 360dp floor — time 10.5sp Normal, format 8.5sp Medium, 4.5dp padding each
 * side, 2.5dp time↔format gap, 4dp between chips — so production (which never
 * provides its own value) renders them. The slightly smaller 10.5sp time is
 * what buys room for the fuller 4.5dp padding + 4dp gap while two-per-row at
 * 360dp still holds (pinned by `ShowtimeChipFitTest`). The non-prod
 * `ShowtimeTuningScreen` injects an edited copy via `CompositionLocalProvider`
 * to preview changes against the real [FilmCard]. Mirrors iOS `ShowtimePillStyle`.
 */
data class ShowtimeChipStyle(
    val timeFontSize: TextUnit = 10.5.sp,
    val timeWeight: FontWeight = FontWeight.Normal,
    val formatFontSize: TextUnit = 8.5.sp,
    val formatWeight: FontWeight = FontWeight.Medium,
    /** Per-side horizontal padding inside the chip. */
    val horizontalInset: Dp = 4.5.dp,
    /** Per-side vertical padding inside the chip. */
    val verticalInset: Dp = 4.5.dp,
    /** Gap between the time and the format tag. */
    val internalGap: Dp = 2.5.dp,
    /** Gap between adjacent chips in the flow row. */
    val interPillGap: Dp = 4.dp,
)

/** Style driving every showtime chip. The default equals today's shipping
 *  values, so production renders exactly as before; the tuning screen overrides
 *  it through `CompositionLocalProvider`. */
val LocalShowtimeChipStyle = compositionLocalOf { ShowtimeChipStyle() }

/** A copy with every dimension multiplied by [scale] — the viewport-width factor
 *  from [ShowtimeChipMetrics]. Weights are untouched. `scale == 1f` (the 360 dp
 *  baseline) returns this unchanged, so a chip at the floor renders the dialled
 *  values exactly. */
fun ShowtimeChipStyle.scaledBy(scale: Float): ShowtimeChipStyle =
    if (scale == 1f) this else copy(
        timeFontSize = timeFontSize * scale,
        formatFontSize = formatFontSize * scale,
        horizontalInset = horizontalInset * scale,
        verticalInset = verticalInset * scale,
        internalGap = internalGap * scale,
        interPillGap = interPillGap * scale,
    )

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
    // Chips (and the showings-internal gaps) scale with the device's PORTRAIT
    // width off the 360 dp baseline (layoutWidthDp, not the live width — so a
    // chip is the same size in landscape as in portrait); two-per-row stays safe
    // because the card column grows faster than the scaled chips (see
    // ShowtimeChipMetrics). scale == 1f at the 360 dp floor.
    val scale = ShowtimeChipMetrics.scale(layoutWidthDp())
    val chipStyle = LocalShowtimeChipStyle.current.scaledBy(scale)
    val cardSpacing = LocalCardSpacingStyle.current
    val showingsBlock = cardSpacing.showingsBlock * scale
    val dayToCinema = cardSpacing.dayToCinema * scale
    val total = film.showings.sumOf { d -> d.cinemas.sumOf { it.showtimes.size } }
    var budget = maxChips ?: Int.MAX_VALUE
    var shown = 0

    // Provide the scaled chip style so the leaf ShowtimeChip reads it directly.
    CompositionLocalProvider(LocalShowtimeChipStyle provides chipStyle) {
    Column(modifier = modifier, verticalArrangement = Arrangement.spacedBy(showingsBlock)) {
        for (day in film.showings) {
            if (budget <= 0) break
            // Each day is its own block so the gap below the day label
            // (`dayToCinema`) is independent of `showingsBlock`, which spaces the
            // cinema/pills rows and the day blocks from one another.
            Column {
                Text(
                    text = day.label.uppercase(),
                    color = TextSecondary,
                    fontSize = (11 * scale).sp,
                    fontWeight = FontWeight.SemiBold,
                )
                Column(
                    modifier = Modifier.padding(top = dayToCinema),
                    verticalArrangement = Arrangement.spacedBy(showingsBlock),
                ) {
                    for (cg in day.cinemas) {
                        if (budget <= 0) break
                        if (showCinemaHeaders) {
                            Text(
                                text = cg.cinema,
                                color = CinemaBlue,
                                fontSize = (12 * scale).sp,
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
                            horizontalArrangement = Arrangement.spacedBy(chipStyle.interPillGap),
                            verticalArrangement = Arrangement.spacedBy(chipStyle.interPillGap),
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
            }
        }
        val hidden = total - shown
        if (hidden > 0) {
            Text(
                text = "+$hidden ${seansForm(hidden)}",
                color = TextSecondary,
                fontSize = (11 * scale).sp,
                fontWeight = FontWeight.Medium,
            )
        }
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
    val style = LocalShowtimeChipStyle.current
    // The room tooltip grows with the viewport in lockstep with the chip it pops
    // from — same factor `Showings` scales `chipStyle` by. scale == 1f at 360 dp.
    val scale = ShowtimeChipMetrics.scale(layoutWidthDp())
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
        // doesn't read tall on its own; the inset (4dp by default) then adds the
        // breathing room around it. Inset + trim pinned by ShowtimeChipPaddingTest.
        .padding(horizontal = style.horizontalInset, vertical = style.verticalInset)
    Box(contentAlignment = Alignment.TopCenter) {
        // Center the smaller format tag vertically against the time (matching
        // iOS's `.center` HStack) rather than letting it ride the top — Row's
        // default is Alignment.Top. Pinned by ShowtimeChipAlignmentTest.
        Row(
            base,
            horizontalArrangement = Arrangement.spacedBy(style.internalGap),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(time, color = CinemaBlue, style = pillTextStyle(style.timeFontSize, style.timeWeight))
            if (format.isNotEmpty()) {
                // 7sp default, not larger: the time's 11sp is the most that still lets
                // two chips share a row on the narrowest card; an 8sp+ tag pushes
                // the second chip onto a new line. See ShowtimeChipFitTest.
                Text(format, color = CinemaBlue.copy(alpha = 0.7f), style = pillTextStyle(style.formatFontSize, style.formatWeight))
            }
        }
        if (holding && room != null) {
            // The tooltip rides in a Popup, NOT as a child of this Box: a Box
            // sizes itself to its largest child, so an in-layout bubble would
            // grow the Box the instant it appeared and shove the pill (and its
            // FlowRow neighbours) sideways. A Popup is its own window — it never
            // resizes the pill, and with clipping disabled it floats free of the
            // card's rounded-Surface clip. It's anchored just above the pill.
            val gapPx = with(LocalDensity.current) { (RoomTooltipGap * scale).roundToPx() }
            Popup(
                popupPositionProvider = remember(gapPx) { RoomTooltipPositionProvider(gapPx) },
                properties = PopupProperties(focusable = false, clippingEnabled = false),
            ) {
                RoomTooltip(room, scale)
            }
        }
    }
}

/** Gap between the tooltip's bottom edge and the pill's top at the 360 dp
 *  baseline — small so the bubble hugs the finger that popped it, but clear
 *  enough to stay readable. Scaled by the viewport factor at the call site. */
private val RoomTooltipGap = 20.dp

@Composable
private fun RoomTooltip(room: String, scale: Float) {
    // The tooltip pops on a press-and-hold, so the thumb is parked right on the
    // pill. It's sized to stay legible around the finger — the 24sp text (1.5×
    // the pill) + padding carry it — without dominating the card; half the size
    // of the original oversized bubble. Every dimension scales with the viewport
    // (factor `scale`) so the bubble tracks the chip. Size pinned by RoomTooltipSizeTest.
    val corner = RoundedCornerShape((6 * scale).dp)
    Text(
        text = room,
        color = RoomTooltipText,
        fontSize = (24 * scale).sp,
        fontWeight = FontWeight.SemiBold,
        modifier = Modifier
            .clip(corner)
            .background(RoomTooltipBackground)
            .border((1 * scale).dp, RoomTooltipBorder, corner)
            .padding(horizontal = (10 * scale).dp, vertical = (6 * scale).dp),
    )
}

/**
 * Top-left of the room tooltip in window pixels: centred horizontally over the
 * pill ([anchorBounds]) and lifted so its bottom sits [gapPx] above the pill's
 * top. The x is clamped to `[0, windowWidth − bubbleWidth]` so a chip in the
 * right-hand column can't push the bubble off the screen edge.
 */
internal fun roomTooltipPopupOffset(
    anchorBounds: IntRect,
    popupContentSize: IntSize,
    gapPx: Int,
    windowWidth: Int,
): IntOffset {
    val centredX = anchorBounds.left + (anchorBounds.width - popupContentSize.width) / 2
    val maxX = (windowWidth - popupContentSize.width).coerceAtLeast(0)
    return IntOffset(
        x = centredX.coerceIn(0, maxX),
        y = anchorBounds.top - popupContentSize.height - gapPx,
    )
}

private class RoomTooltipPositionProvider(private val gapPx: Int) : PopupPositionProvider {
    override fun calculatePosition(
        anchorBounds: IntRect,
        windowSize: IntSize,
        layoutDirection: LayoutDirection,
        popupContentSize: IntSize,
    ): IntOffset = roomTooltipPopupOffset(anchorBounds, popupContentSize, gapPx, windowSize.width)
}

/** Polish plural: 1 seans, 2–4 seanse, else seansów. */
private fun seansForm(count: Int): String {
    val mod10 = count % 10
    val mod100 = count % 100
    return when {
        count == 1 -> "seans"
        mod10 in 2..4 && mod100 !in 12..14 -> "seanse"
        else -> "seansów"
    }
}
