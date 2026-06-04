package pl.kinowo.ui.dev

import android.graphics.Paint
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.Orientation
import androidx.compose.foundation.gestures.draggable
import androidx.compose.foundation.gestures.rememberDraggableState
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.Button
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Slider
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.sp
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Ratings
import pl.kinowo.model.Showtime
import pl.kinowo.ui.common.CardSpacingStyle
import pl.kinowo.ui.common.LocalCardSpacingStyle
import pl.kinowo.ui.common.LocalRatingPillStyle
import pl.kinowo.ui.common.LocalShowtimeChipStyle
import pl.kinowo.ui.common.RatingBadgeMetrics
import pl.kinowo.ui.common.RatingPillStyle
import pl.kinowo.ui.common.ShowtimeChipStyle
import pl.kinowo.ui.list.FilmCard
import pl.kinowo.ui.theme.Background
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.CardSurface
import pl.kinowo.ui.theme.TextSecondary

/**
 * Non-prod developer screen for dialling in the showtime-chip look. It renders
 * six real [FilmCard]s in the production two-column grid and drives their chips
 * from a live [ShowtimeChipStyle] injected through [LocalShowtimeChipStyle], so
 * every slider / segmented control redraws the cards instantly. Nothing here
 * ships in a normal run — it's reached only via the `kinowo_tuning` launch
 * extra (DEBUG) or the [ShowtimeTuningScreenPreview] at the bottom.
 *
 * The first two films exercise every showtime-rendering case on purpose:
 * empty / short / long / multi-token formats, a chip with a room (long-press),
 * common-token stripping (a cinema whose every showtime shares "2D NAP" renders
 * bare times), and enough showtimes to wrap across several rows. The Android
 * twin of iOS `ShowtimeTuningScreen`.
 */
@Composable
fun ShowtimeTuningScreen() {
    var style by remember { mutableStateOf(ShowtimeChipStyle()) }
    var ratingStyle by remember { mutableStateOf(RatingPillStyle()) }
    var cardSpacing by remember { mutableStateOf(CardSpacingStyle()) }

    Box(
        Modifier
            .fillMaxSize()
            .background(Background),
    ) {
        // Cards scroll behind the sheet, driven by the edited chip + rating styles.
        CompositionLocalProvider(
            LocalShowtimeChipStyle provides style,
            LocalRatingPillStyle provides ratingStyle,
            LocalCardSpacingStyle provides cardSpacing,
        ) {
            LazyVerticalGrid(
                columns = GridCells.Fixed(2),
                contentPadding = PaddingValues(12.dp),
                horizontalArrangement = Arrangement.spacedBy(12.dp),
                verticalArrangement = Arrangement.spacedBy(12.dp),
                modifier = Modifier.fillMaxSize(),
            ) {
                items(ShowtimeTuningData.films, key = { it.title }) { film ->
                    FilmCard(
                        film = film,
                        showCinemaHeaders = true,
                        onOpen = {},
                        onHide = {},
                    )
                }
            }
        }

        ControlsSheet(
            style = style,
            onStyleChange = { style = it },
            ratingStyle = ratingStyle,
            onRatingStyleChange = { ratingStyle = it },
            cardSpacing = cardSpacing,
            onCardSpacingChange = { cardSpacing = it },
            onReset = {
                style = ShowtimeChipStyle()
                ratingStyle = RatingPillStyle()
                cardSpacing = CardSpacingStyle()
            },
            modifier = Modifier.align(Alignment.BottomCenter),
        )
    }
}

// ── draggable bottom sheet ──────────────────────────────────────────────────

private val SheetMinHeight = 56.dp

/**
 * The controls live in a sheet the user can drag up (covering the screen) or
 * down (collapsing to its drag handle). Its height is a draggable state clamped
 * between [SheetMinHeight] and the full screen height — dragging the handle up
 * grows it, down shrinks it. The cards scroll behind it. [BoxWithConstraints]
 * gives us the full screen height for the drag ceiling.
 */
@Composable
private fun ControlsSheet(
    style: ShowtimeChipStyle,
    onStyleChange: (ShowtimeChipStyle) -> Unit,
    ratingStyle: RatingPillStyle,
    onRatingStyleChange: (RatingPillStyle) -> Unit,
    cardSpacing: CardSpacingStyle,
    onCardSpacingChange: (CardSpacingStyle) -> Unit,
    onReset: () -> Unit,
    modifier: Modifier = Modifier,
) {
    val density = LocalDensity.current
    BoxWithConstraints(modifier.fillMaxWidth()) {
        val fullHeight = maxHeight
        // Start at ~55% of the screen; clamp drags between the handle-only
        // minimum and the full screen height (covers everything in portrait).
        var height by remember { mutableStateOf(fullHeight * 0.55f) }

        Column(
            Modifier
                .fillMaxWidth()
                .height(height)
                .clip(RoundedCornerShape(topStart = 16.dp, topEnd = 16.dp))
                .background(CardSurface),
        ) {
            // Drag handle. Dragging up (negative delta) grows the sheet.
            Box(
                Modifier
                    .fillMaxWidth()
                    .height(28.dp)
                    .draggable(
                        orientation = Orientation.Vertical,
                        state = rememberDraggableState { delta ->
                            val newDp = height - with(density) { delta.toDp() }
                            height = newDp.coerceIn(SheetMinHeight, fullHeight)
                        },
                    ),
                contentAlignment = Alignment.Center,
            ) {
                Box(
                    Modifier
                        .width(40.dp)
                        .height(5.dp)
                        .clip(RoundedCornerShape(3.dp))
                        .background(TextSecondary),
                )
            }

            SheetHeader(style = style, ratingStyle = ratingStyle, cardSpacing = cardSpacing, onReset = onReset)
            SheetControls(
                style = style,
                onStyleChange = onStyleChange,
                ratingStyle = ratingStyle,
                onRatingStyleChange = onRatingStyleChange,
                cardSpacing = cardSpacing,
                onCardSpacingChange = onCardSpacingChange,
                modifier = Modifier
                    .fillMaxWidth()
                    .verticalScroll(rememberScrollState())
                    .padding(horizontal = 16.dp, vertical = 12.dp),
            )
        }
    }
}

// ── sheet header: fit readout + copy + reset ────────────────────────────────

@Composable
private fun SheetHeader(style: ShowtimeChipStyle, ratingStyle: RatingPillStyle, cardSpacing: CardSpacingStyle, onReset: () -> Unit) {
    val clipboard = LocalClipboardManager.current
    val density = LocalDensity.current
    val screenWidthDp = LocalConfiguration.current.screenWidthDp
    Row(
        Modifier
            .fillMaxWidth()
            .background(CardElevated)
            .padding(horizontal = 16.dp, vertical = 10.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Column(Modifier.weight(1f)) {
            Text("Showtime tuning", color = Color.White, fontSize = 13.sp, fontWeight = FontWeight.SemiBold)
            val fit = ChipFit.evaluate(style, density)
            Text(
                text = fit.readout,
                color = if (fit.ok) Color(0xFF66CC66) else Color(0xFFE0A040),
                fontSize = 11.sp,
            )
        }
        OutlinedButton(onClick = { clipboard.setText(AnnotatedString(copyText(style, ratingStyle, cardSpacing, screenWidthDp))) }) {
            Text("Kopiuj", fontSize = 13.sp)
        }
        Spacer(Modifier.width(8.dp))
        Button(onClick = onReset) { Text("Reset", fontSize = 13.sp) }
    }
}

// ── sheet controls ──────────────────────────────────────────────────────────

@Composable
private fun SheetControls(
    style: ShowtimeChipStyle,
    onStyleChange: (ShowtimeChipStyle) -> Unit,
    ratingStyle: RatingPillStyle,
    onRatingStyleChange: (RatingPillStyle) -> Unit,
    cardSpacing: CardSpacingStyle,
    onCardSpacingChange: (CardSpacingStyle) -> Unit,
    modifier: Modifier = Modifier,
) {
    Column(modifier, verticalArrangement = Arrangement.spacedBy(14.dp)) {
        Group("Czas (time)") {
            WeightRow("Grubość", style.timeWeight) { onStyleChange(style.copy(timeWeight = it)) }
            SpSlider("Rozmiar", style.timeFontSize.value, 7f..20f) {
                onStyleChange(style.copy(timeFontSize = it.sp))
            }
        }
        Group("Format") {
            WeightRow("Grubość", style.formatWeight) { onStyleChange(style.copy(formatWeight = it)) }
            SpSlider("Rozmiar", style.formatFontSize.value, 5f..16f) {
                onStyleChange(style.copy(formatFontSize = it.sp))
            }
        }
        Group("Padding (pigułki)") {
            DpSlider("Poziomy", style.horizontalInset, 0f..14f) { onStyleChange(style.copy(horizontalInset = it)) }
            DpSlider("Pionowy", style.verticalInset, 0f..14f) { onStyleChange(style.copy(verticalInset = it)) }
        }
        Group("Odstępy") {
            DpSlider("Czas ↔ format", style.internalGap, 0f..12f) { onStyleChange(style.copy(internalGap = it)) }
            DpSlider("Między pigułkami", style.interPillGap, 0f..16f) { onStyleChange(style.copy(interPillGap = it)) }
        }
        Group("Odstępy karty") {
            DpSlider("Tytuł ↔ meta", cardSpacing.titleToMeta, 0f..24f) { onCardSpacingChange(cardSpacing.copy(titleToMeta = it)) }
            DpSlider("Meta ↔ oceny", cardSpacing.metaToRatings, 0f..24f) { onCardSpacingChange(cardSpacing.copy(metaToRatings = it)) }
            DpSlider("Oceny ↔ seanse", cardSpacing.ratingsToShowings, 0f..24f) { onCardSpacingChange(cardSpacing.copy(ratingsToShowings = it)) }
            DpSlider("Dni / kina", cardSpacing.showingsBlock, 0f..24f) { onCardSpacingChange(cardSpacing.copy(showingsBlock = it)) }
        }

        Group("Ocena: rozmiar bazowy") {
            // Android scales rating pills by device width (unlike iOS's flat
            // sizing), so the slider sets the BASE sp and the readout shows what
            // this device actually renders after RatingBadgeMetrics.scale().
            RatingScaleReadout(ratingStyle.baseFontSize)
            SpSlider("Rozmiar bazowy", ratingStyle.baseFontSize.value, 6f..18f) {
                onRatingStyleChange(ratingStyle.copy(baseFontSize = it.sp))
            }
        }
        Group("Ocena: grubości") {
            WeightRow("Etykieta", ratingStyle.labelWeight) { onRatingStyleChange(ratingStyle.copy(labelWeight = it)) }
            WeightRow("Wartość", ratingStyle.valueWeight) { onRatingStyleChange(ratingStyle.copy(valueWeight = it)) }
            WeightRow("Solid (MC)", ratingStyle.solidWeight) { onRatingStyleChange(ratingStyle.copy(solidWeight = it)) }
        }
        Group("Ocena: padding") {
            DpSlider("Poziomy", ratingStyle.hPad, 0f..12f) { onRatingStyleChange(ratingStyle.copy(hPad = it)) }
            DpSlider("Pionowy", ratingStyle.vPad, 0f..10f) { onRatingStyleChange(ratingStyle.copy(vPad = it)) }
        }
        Group("Ocena: kształt") {
            DpSlider("Zaokrąglenie", ratingStyle.corner, 0f..14f) { onRatingStyleChange(ratingStyle.copy(corner = it)) }
            DpSlider("Między pigułkami", ratingStyle.interPillGap, 0f..16f) { onRatingStyleChange(ratingStyle.copy(interPillGap = it)) }
        }
    }
}

/** Shows this device's pill scale and the resulting rendered font, the key
 *  difference from iOS — Android sizes rating pills relative to viewport width,
 *  so the same base sp renders differently per phone. */
@Composable
private fun RatingScaleReadout(baseFontSize: TextUnit) {
    val screenWidthDp = LocalConfiguration.current.screenWidthDp
    val scale = RatingBadgeMetrics.scale(screenWidthDp)
    val rendered = baseFontSize.value * scale
    Text(
        "skala ×${twoDecimal(scale)} @${RatingBadgeMetrics.ReferenceWidthDp.toInt()}dp → ${oneDecimal(rendered)}sp",
        color = TextSecondary,
        fontSize = 11.sp,
    )
}

@Composable
private fun Group(title: String, content: @Composable () -> Unit) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(
            title.uppercase(),
            color = TextSecondary,
            fontSize = 10.sp,
            fontWeight = FontWeight.SemiBold,
        )
        content()
    }
}

/** The weight choices the picker offers, in ascending order, with their copy
 *  keyword (`copyText`) and short label for the segmented control. */
private enum class WeightOption(val short: String, val key: String, val weight: FontWeight) {
    Regular("Reg", "regular", FontWeight.Normal),
    Medium("Med", "medium", FontWeight.Medium),
    SemiBold("Semi", "semibold", FontWeight.SemiBold),
    Bold("Bold", "bold", FontWeight.Bold),
    Black("Black", "black", FontWeight.Black);

    companion object {
        fun of(w: FontWeight): WeightOption = entries.firstOrNull { it.weight == w } ?: Medium
    }
}

@Composable
private fun WeightRow(label: String, weight: FontWeight, onSelect: (FontWeight) -> Unit) {
    Row(verticalAlignment = Alignment.CenterVertically) {
        Text(label, color = Color.White, fontSize = 12.sp, modifier = Modifier.width(110.dp))
        val current = WeightOption.of(weight)
        Row(
            Modifier
                .weight(1f)
                .clip(RoundedCornerShape(8.dp))
                .background(Color.White.copy(alpha = 0.06f)),
        ) {
            for (opt in WeightOption.entries) {
                val selected = opt == current
                Box(
                    Modifier
                        .weight(1f)
                        .clip(RoundedCornerShape(8.dp))
                        .background(if (selected) Brand.copy(alpha = 0.85f) else Color.Transparent)
                        .clickable(
                            interactionSource = remember { MutableInteractionSource() },
                            indication = null,
                        ) { onSelect(opt.weight) }
                        .padding(vertical = 7.dp),
                    contentAlignment = Alignment.Center,
                ) {
                    Text(opt.short, color = Color.White, fontSize = 11.sp, fontWeight = if (selected) FontWeight.SemiBold else FontWeight.Normal)
                }
            }
        }
    }
}

@Composable
private fun SpSlider(label: String, value: Float, range: ClosedFloatingPointRange<Float>, onChange: (Float) -> Unit) {
    LabeledSlider(label, value, range) { onChange(it) }
}

@Composable
private fun DpSlider(label: String, value: Dp, range: ClosedFloatingPointRange<Float>, onChange: (Dp) -> Unit) {
    LabeledSlider(label, value.value, range) { onChange(it.dp) }
}

@Composable
private fun LabeledSlider(
    label: String,
    value: Float,
    range: ClosedFloatingPointRange<Float>,
    onChange: (Float) -> Unit,
) {
    val step = 0.5f
    // Snap to 0.5 steps to match the iOS slider granularity.
    val steps = ((range.endInclusive - range.start) / step).toInt() - 1
    Row(verticalAlignment = Alignment.CenterVertically) {
        Text(label, color = Color.White, fontSize = 12.sp, modifier = Modifier.width(96.dp))
        StepButton("−") { onChange((value - step).coerceIn(range.start, range.endInclusive)) }
        Slider(
            value = value,
            onValueChange = { onChange((Math.round(it * 2f) / 2f)) },
            valueRange = range,
            steps = steps.coerceAtLeast(0),
            modifier = Modifier.weight(1f),
        )
        StepButton("+") { onChange((value + step).coerceIn(range.start, range.endInclusive)) }
        Text(
            oneDecimal(value),
            color = Color.White,
            fontSize = 12.sp,
            fontWeight = FontWeight.Medium,
            modifier = Modifier.width(36.dp),
        )
    }
}

/** Small −/+ button flanking a slider, nudging it by one 0.5 step (clamped to
 *  the slider's range) for precise dialling without dragging. */
@Composable
private fun StepButton(symbol: String, onClick: () -> Unit) {
    Box(
        Modifier
            .size(26.dp)
            .clip(RoundedCornerShape(5.dp))
            .background(Color.White.copy(alpha = 0.08f))
            .clickable(onClick = onClick),
        contentAlignment = Alignment.Center,
    ) {
        Text(symbol, color = Color.White, fontSize = 15.sp, fontWeight = FontWeight.Bold)
    }
}

// ── copy format + fit estimate ──────────────────────────────────────────────

private fun oneDecimal(v: Float): String = String.format(java.util.Locale.US, "%.1f", v)
private fun twoDecimal(v: Float): String = String.format(java.util.Locale.US, "%.2f", v)

/** Serialises the current styles to the exact paste format shared with iOS: one
 *  space-separated `key=value` per line. The rating lines append after the
 *  showtime ones and carry the device-scale readout (Android-specific, since the
 *  pills size relative to viewport width). */
internal fun copyText(s: ShowtimeChipStyle, r: RatingPillStyle, c: CardSpacingStyle, screenWidthDp: Int): String {
    val scale = RatingBadgeMetrics.scale(screenWidthDp)
    val rendered = r.baseFontSize.value * scale
    return buildString {
        append("time: size=${oneDecimal(s.timeFontSize.value)} weight=${WeightOption.of(s.timeWeight).key}\n")
        append("format: size=${oneDecimal(s.formatFontSize.value)} weight=${WeightOption.of(s.formatWeight).key}\n")
        append("padding: h=${oneDecimal(s.horizontalInset.value)} v=${oneDecimal(s.verticalInset.value)}\n")
        append("gaps: internal=${oneDecimal(s.internalGap.value)} interPill=${oneDecimal(s.interPillGap.value)}\n")
        append("card-gaps: titleToMeta=${oneDecimal(c.titleToMeta.value)} metaToRatings=${oneDecimal(c.metaToRatings.value)} ratingsToShowings=${oneDecimal(c.ratingsToShowings.value)} showingsBlock=${oneDecimal(c.showingsBlock.value)}\n")
        append("rating-base: size=${oneDecimal(r.baseFontSize.value)} labelW=${WeightOption.of(r.labelWeight).key} valueW=${WeightOption.of(r.valueWeight).key} solidW=${WeightOption.of(r.solidWeight).key}\n")
        append("rating-pad: h=${oneDecimal(r.hPad.value)} v=${oneDecimal(r.vPad.value)}\n")
        append("rating-shape: corner=${oneDecimal(r.corner.value)} interPill=${oneDecimal(r.interPillGap.value)}\n")
        append("rating-scale: x${twoDecimal(scale)} rendered=${oneDecimal(rendered)}")
    }
}

/**
 * Rough live estimate of whether two canonical chips still share one row in the
 * narrowest card column. APPROXIMATE — it measures text with an Android [Paint]
 * at the chosen sizes/weights; the rendered cards (and `ShowtimeChipFitTest`)
 * are the source of truth. Mirrors iOS `ShowtimeTuningScreen`'s `fitReadout`.
 */
private object ChipFit {
    /** Narrowest Android card showings column: 360dp screen, `(360-36)/2 - 24`. */
    private const val CardWidthDp = (360 - 36) / 2 - 24 // = 138

    data class Result(val ok: Boolean, val readout: String)

    fun evaluate(s: ShowtimeChipStyle, density: androidx.compose.ui.unit.Density): Result {
        val a = chipWidthDp(s, "12:55", "2D DUB", density)
        val b = chipWidthDp(s, "22:55", "3D NAP", density)
        val two = a + s.interPillGap.value + b
        val margin = CardWidthDp - two
        return if (margin >= 0f) {
            Result(true, "✓ dwie pigułki w rzędzie (zapas ${oneDecimal(margin)} dp)")
        } else {
            Result(false, "✗ druga pigułka się zawija (brakuje ${oneDecimal(-margin)} dp)")
        }
    }

    private fun chipWidthDp(s: ShowtimeChipStyle, time: String, format: String, density: androidx.compose.ui.unit.Density): Float {
        val tw = textWidthDp(time, s.timeFontSize.value, s.timeWeight, density)
        val fw = if (format.isEmpty()) 0f
        else s.internalGap.value + textWidthDp(format, s.formatFontSize.value, s.formatWeight, density)
        return 2 * s.horizontalInset.value + tw + fw
    }

    private fun textWidthDp(text: String, sizeSp: Float, weight: FontWeight, density: androidx.compose.ui.unit.Density): Float {
        val paint = Paint().apply {
            textSize = with(density) { sizeSp.sp.toPx() }
            isFakeBoldText = weight.weight >= FontWeight.Bold.weight
        }
        val px = paint.measureText(text)
        return with(density) { px.toDp().value }
    }
}

@Preview(widthDp = 380, heightDp = 800)
@Composable
private fun ShowtimeTuningScreenPreview() {
    ShowtimeTuningScreen()
}

/**
 * Deterministic sample repertoire — identical every launch. Films 1 & 2
 * exercise every showtime-rendering case; films 3–6 are ordinary so the grid
 * reads like a real screen. Mirrors iOS `ShowtimeTuningData`.
 */
object ShowtimeTuningData {
    val films: List<Film> = listOf(edgeCases(), manyShowtimes(), drama(), kids(), lateNight(), festival())

    private fun ratings(imdb: Double?, mc: Int?, rt: Int?, fw: Double?) =
        Ratings(imdb = imdb, metascore = mc, rottenTomatoes = rt, filmweb = fw)

    private fun st(time: String, format: String = "", room: String? = null) =
        Showtime(time = time, format = format, room = room, bookingURL = null)

    // 1 — every case: empty / short / long / multi-token formats, a room
    // (long-press tooltip), common-token stripping (Multikino's showtimes all
    // share "2D NAP" → rendered bare), two days, two cinemas.
    private fun edgeCases() = Film(
        title = "1 — Wszystkie przypadki",
        runtimeMinutes = 128,
        releaseYear = 2025,
        ratings = ratings(7.8, 81, 92, 7.4),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino Pod Baranami", cinemaURL = "https://example.com",
                        showtimes = listOf(
                            st("10:00"),
                            st("12:30", "2D"),
                            st("14:45", "2D DUB", room = "Sala 1"),
                            st("18:00", "IMAX 3D NAP", room = "IMAX"),
                            st("20:15", "3D NAP"),
                            st("22:30", "2D NAP"),
                            st("23:59", "4DX 3D DUB"),
                        ),
                    ),
                    CinemaShowings(
                        cinema = "Multikino",
                        showtimes = listOf(st("11:00", "2D NAP"), st("13:00", "2D NAP"), st("15:00", "2D NAP")),
                    ),
                ),
            ),
            DayShowings(
                date = "2026-06-09", label = "Wtorek 9 czerwca",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino Pod Baranami",
                        showtimes = listOf(st("16:00", "2D"), st("19:30", "VOSE")),
                    ),
                ),
            ),
        ),
    )

    // 2 — many showtimes → wraps across several rows.
    private fun manyShowtimes() = Film(
        title = "2 — Dużo seansów",
        runtimeMinutes = 95,
        releaseYear = 2026,
        ratings = ratings(6.2, null, 55, null),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Cinema City",
                        showtimes = listOf(
                            st("09:15", "2D"), st("10:30", "2D DUB"), st("11:45", "3D"), st("13:00", "2D NAP"),
                            st("14:15", "2D"), st("15:30", "3D DUB"), st("16:45", "2D"), st("18:00", "IMAX 2D"),
                            st("19:15", "2D NAP"), st("20:30", "3D NAP"), st("21:45", "2D"), st("23:00", "2D DUB"),
                        ),
                    ),
                ),
            ),
        ),
    )

    private fun drama() = Film(
        title = "3 — Dramat", runtimeMinutes = 142, releaseYear = 2024,
        ratings = ratings(8.1, 88, null, 7.9),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Kino Pod Baranami", showtimes = listOf(st("17:00", "2D NAP"), st("20:00", "2D NAP"))),
                ),
            ),
        ),
    )

    private fun kids() = Film(
        title = "4 — Dla dzieci", runtimeMinutes = 88, releaseYear = 2026,
        ratings = ratings(null, null, 78, 6.8),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Multikino", showtimes = listOf(st("10:00", "2D DUB"), st("12:00", "2D DUB"), st("14:00", "3D DUB"))),
                ),
            ),
        ),
    )

    private fun lateNight() = Film(
        title = "5 — Nocny seans", runtimeMinutes = 116, releaseYear = 2025,
        ratings = ratings(7.0, 64, 71, null),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Cinema City", showtimes = listOf(st("22:00", "2D NAP"), st("23:30", "2D NAP", room = "Sala 7"))),
                ),
            ),
        ),
    )

    private fun festival() = Film(
        title = "6 — Festiwal", runtimeMinutes = 101, releaseYear = 2023,
        ratings = ratings(7.5, null, null, 8.2),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Kino Pod Baranami", showtimes = listOf(st("15:00", "VOSE"), st("18:30", "OV"), st("21:00", "VOSE"))),
                ),
            ),
        ),
    )
}
