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
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
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
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.sp
import kotlinx.coroutines.launch
import pl.kinowo.filter.CinemaSection
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.model.Ratings
import pl.kinowo.model.Showtime
import pl.kinowo.ui.common.CardSpacingStyle
import pl.kinowo.ui.common.CinemaHeaderStyle
import pl.kinowo.ui.common.FilmDetailStyle
import pl.kinowo.ui.common.LocalCardSpacingStyle
import pl.kinowo.ui.common.LocalCinemaHeaderStyle
import pl.kinowo.ui.common.LocalFilmDetailStyle
import pl.kinowo.ui.common.LocalRatingPillStyle
import pl.kinowo.ui.common.LocalShowtimeChipStyle
import pl.kinowo.ui.common.RatingBadgeMetrics
import pl.kinowo.ui.common.RatingPillStyle
import pl.kinowo.ui.common.ShowtimeChipStyle
import pl.kinowo.ui.detail.DetailScreen
import pl.kinowo.ui.list.CinemaGrid
import pl.kinowo.ui.list.FilmCard
import pl.kinowo.ui.theme.Background
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.CardSurface
import pl.kinowo.ui.theme.TextSecondary

/**
 * Non-prod developer screen for dialling in the look of the three main views
 * against their real renderers. A full-screen [HorizontalPager] swaps between
 * pages — swipe left/right, or tap a label in the page bar:
 *
 *  - **Karta** — six real [FilmCard]s in the production grid, driven by live
 *    [ShowtimeChipStyle] / [RatingPillStyle] / [CardSpacingStyle].
 *  - **Kina**  — the real [CinemaGrid], driven by a live [CinemaHeaderStyle].
 *  - **Film**  — the real [DetailScreen], driven by a live [FilmDetailStyle].
 *
 * The sliders live in a draggable bottom sheet that is a SIBLING of the pager,
 * not a child of it — a horizontal slider drag must not be stolen by the
 * pager's swipe gesture (the same lesson as keeping interactive controls out of
 * a paged scroll view). The sheet's content switches to whichever page shows.
 *
 * Nothing here ships in a normal run — it's reached only via the `kinowo_tuning`
 * launch extra (DEBUG / tuneRelease) or [ShowtimeTuningScreenPreview]. The
 * Android twin of iOS `ShowtimeTuningScreen`.
 */
@Composable
fun ShowtimeTuningScreen() {
    var style by remember { mutableStateOf(ShowtimeChipStyle()) }
    var ratingStyle by remember { mutableStateOf(RatingPillStyle()) }
    var cardSpacing by remember { mutableStateOf(CardSpacingStyle()) }
    var cinemaHeader by remember { mutableStateOf(CinemaHeaderStyle()) }
    var filmStyle by remember { mutableStateOf(FilmDetailStyle()) }
    val pager = rememberPagerState(pageCount = { TuningPage.entries.size })
    val scope = rememberCoroutineScope()

    Box(
        Modifier
            .fillMaxSize()
            .background(Background),
    ) {
        // Only the PREVIEWS go inside the pager; the controls sheet is a sibling
        // overlay below, so a horizontal slider drag can't trip the page swipe.
        CompositionLocalProvider(
            LocalShowtimeChipStyle provides style,
            LocalRatingPillStyle provides ratingStyle,
            LocalCardSpacingStyle provides cardSpacing,
            LocalCinemaHeaderStyle provides cinemaHeader,
            LocalFilmDetailStyle provides filmStyle,
        ) {
            HorizontalPager(state = pager, modifier = Modifier.fillMaxSize()) { page ->
                when (TuningPage.entries[page]) {
                    TuningPage.Card -> CardPreview()
                    TuningPage.Kina -> CinemaGrid(
                        sections = ShowtimeTuningData.sections,
                        showHeaders = true,
                        bottomInset = 120.dp,
                        scrollResetKey = null,
                        onOpen = {},
                        onHide = {},
                    )
                    TuningPage.Film -> DetailScreen(
                        film = ShowtimeTuningData.detailFilm,
                        details = ShowtimeTuningData.detailDetails,
                        onBack = {},
                    )
                }
            }
        }

        ControlsSheet(
            page = TuningPage.entries[pager.currentPage],
            onSelectPage = { p -> scope.launch { pager.animateScrollToPage(p.ordinal) } },
            style = style, onStyleChange = { style = it },
            ratingStyle = ratingStyle, onRatingStyleChange = { ratingStyle = it },
            cardSpacing = cardSpacing, onCardSpacingChange = { cardSpacing = it },
            cinemaHeader = cinemaHeader, onCinemaHeaderChange = { cinemaHeader = it },
            filmStyle = filmStyle, onFilmStyleChange = { filmStyle = it },
            onReset = {
                when (TuningPage.entries[pager.currentPage]) {
                    TuningPage.Card -> {
                        style = ShowtimeChipStyle(); ratingStyle = RatingPillStyle(); cardSpacing = CardSpacingStyle()
                    }
                    TuningPage.Kina -> cinemaHeader = CinemaHeaderStyle()
                    TuningPage.Film -> filmStyle = FilmDetailStyle()
                }
            },
            modifier = Modifier.align(Alignment.BottomCenter),
        )
    }
}

/** The pages the tuning pager swipes between, in display order. */
enum class TuningPage(val title: String) {
    Card("Karta"),
    Kina("Kina"),
    Film("Film"),
}

/** Compose test tags on the tuning controls, so `ShowtimeTuningScreenTest` can
 *  switch pages and drive specific sliders — the Android counterpart of iOS's
 *  `A11y.Tuning` identifiers. */
internal object TuningTags {
    fun pageTab(page: TuningPage) = "tuning.tab.${page.name}"
    const val TimeSizeSlider = "tuning.slider.timeSize"
    const val CinemaHeaderFontSlider = "tuning.slider.cinemaHeaderFont"
    const val DetailTitleFontSlider = "tuning.slider.detailTitleFont"
    const val ResolutionReadout = "tuning.resolution"
}

/** Page 0 — the showtime card grid, the original tuning surface. */
@Composable
private fun CardPreview() {
    LazyVerticalGrid(
        columns = GridCells.Fixed(2),
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = 120.dp),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
        modifier = Modifier.fillMaxSize(),
    ) {
        items(ShowtimeTuningData.films, key = { it.title }) { film ->
            FilmCard(film = film, showCinemaHeaders = true, onOpen = {}, onHide = {})
        }
    }
}

// ── draggable bottom sheet ──────────────────────────────────────────────────

private val SheetMinHeight = 56.dp

/**
 * The controls live in a sheet the user can drag up (covering the screen) or
 * down (collapsing to its drag handle). Its height is a draggable state clamped
 * between [SheetMinHeight] and the full screen height. The previews scroll/swipe
 * behind it. The page-tab row + the sheet's controls switch with [page].
 */
@Composable
private fun ControlsSheet(
    page: TuningPage,
    onSelectPage: (TuningPage) -> Unit,
    style: ShowtimeChipStyle,
    onStyleChange: (ShowtimeChipStyle) -> Unit,
    ratingStyle: RatingPillStyle,
    onRatingStyleChange: (RatingPillStyle) -> Unit,
    cardSpacing: CardSpacingStyle,
    onCardSpacingChange: (CardSpacingStyle) -> Unit,
    cinemaHeader: CinemaHeaderStyle,
    onCinemaHeaderChange: (CinemaHeaderStyle) -> Unit,
    filmStyle: FilmDetailStyle,
    onFilmStyleChange: (FilmDetailStyle) -> Unit,
    onReset: () -> Unit,
    modifier: Modifier = Modifier,
) {
    BoxWithConstraints(modifier.fillMaxWidth()) {
        val fullHeight = maxHeight
        val density = LocalDensity.current
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

            PageTabs(current = page, onSelect = onSelectPage)
            SheetHeader(
                page = page, style = style, ratingStyle = ratingStyle, cardSpacing = cardSpacing,
                cinemaHeader = cinemaHeader, filmStyle = filmStyle, onReset = onReset,
            )
            Column(
                Modifier
                    .fillMaxWidth()
                    .verticalScroll(rememberScrollState())
                    .padding(horizontal = 16.dp, vertical = 12.dp),
                verticalArrangement = Arrangement.spacedBy(14.dp),
            ) {
                ResolutionReadout()
                when (page) {
                    TuningPage.Card -> CardControls(style, onStyleChange, ratingStyle, onRatingStyleChange, cardSpacing, onCardSpacingChange)
                    TuningPage.Kina -> KinaControls(cinemaHeader, onCinemaHeaderChange)
                    TuningPage.Film -> FilmControls(filmStyle, onFilmStyleChange)
                }
            }
        }
    }
}

/** Tappable page indicator doubling as the page switcher — swipe is primary,
 *  but a tap jumps straight there (and gives the tests a stable page hook). */
@Composable
private fun PageTabs(current: TuningPage, onSelect: (TuningPage) -> Unit) {
    Row(
        Modifier
            .fillMaxWidth()
            .padding(horizontal = 16.dp, vertical = 6.dp),
        horizontalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        for (p in TuningPage.entries) {
            val selected = p == current
            Box(
                Modifier
                    .weight(1f)
                    .testTag(TuningTags.pageTab(p))
                    .clip(RoundedCornerShape(7.dp))
                    .background(if (selected) Brand.copy(alpha = 0.85f) else Color.White.copy(alpha = 0.08f))
                    .clickable(
                        interactionSource = remember { MutableInteractionSource() },
                        indication = null,
                    ) { onSelect(p) }
                    .padding(vertical = 7.dp),
                contentAlignment = Alignment.Center,
            ) {
                Text(p.title, color = Color.White, fontSize = 12.sp, fontWeight = if (selected) FontWeight.SemiBold else FontWeight.Normal)
            }
        }
    }
}

// ── sheet header: title + fit readout (card only) + copy + reset ─────────────

@Composable
private fun SheetHeader(
    page: TuningPage,
    style: ShowtimeChipStyle,
    ratingStyle: RatingPillStyle,
    cardSpacing: CardSpacingStyle,
    cinemaHeader: CinemaHeaderStyle,
    filmStyle: FilmDetailStyle,
    onReset: () -> Unit,
) {
    val clipboard = LocalClipboardManager.current
    val density = LocalDensity.current
    val configuration = LocalConfiguration.current
    val screenWidthDp = configuration.screenWidthDp
    Row(
        Modifier
            .fillMaxWidth()
            .background(CardElevated)
            .padding(horizontal = 16.dp, vertical = 10.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Column(Modifier.weight(1f)) {
            Text("${page.title} tuning", color = Color.White, fontSize = 13.sp, fontWeight = FontWeight.SemiBold)
            // The two-per-row fit readout only makes sense for the card page.
            if (page == TuningPage.Card) {
                val fit = ChipFit.evaluate(style, density)
                Text(
                    text = fit.readout,
                    color = if (fit.ok) Color(0xFF66CC66) else Color(0xFFE0A040),
                    fontSize = 11.sp,
                )
            }
        }
        OutlinedButton(onClick = {
            clipboard.setText(AnnotatedString(copyTextFor(page, style, ratingStyle, cardSpacing, cinemaHeader, filmStyle, screenWidthDp)))
        }) {
            Text("Kopiuj", fontSize = 13.sp)
        }
        Spacer(Modifier.width(8.dp))
        Button(onClick = onReset) { Text("Reset", fontSize = 13.sp) }
    }
}

/** Live readout of the area the layout actually gets (dp), plus the density and
 *  the pixels it works out to. Parked at the top of the scrolling controls
 *  (not the pinned header) so it scrolls away once read instead of permanently
 *  eating header height. */
@Composable
private fun ResolutionReadout() {
    val density = LocalDensity.current
    val configuration = LocalConfiguration.current
    Text(
        DisplayInfo.tuningReadout(configuration.screenWidthDp, configuration.screenHeightDp, density.density),
        color = Color.White.copy(alpha = 0.55f),
        fontSize = 10.sp,
        modifier = Modifier.testTag(TuningTags.ResolutionReadout),
    )
}

// ── per-page controls ────────────────────────────────────────────────────────

@Composable
private fun CardControls(
    style: ShowtimeChipStyle,
    onStyleChange: (ShowtimeChipStyle) -> Unit,
    ratingStyle: RatingPillStyle,
    onRatingStyleChange: (RatingPillStyle) -> Unit,
    cardSpacing: CardSpacingStyle,
    onCardSpacingChange: (CardSpacingStyle) -> Unit,
) {
    Group("Czas (time)") {
        WeightRow("Grubość", style.timeWeight) { onStyleChange(style.copy(timeWeight = it)) }
        SpSlider("Rozmiar", style.timeFontSize.value, 7f..20f, TuningTags.TimeSizeSlider) { onStyleChange(style.copy(timeFontSize = it.sp)) }
    }
    Group("Format") {
        WeightRow("Grubość", style.formatWeight) { onStyleChange(style.copy(formatWeight = it)) }
        SpSlider("Rozmiar", style.formatFontSize.value, 5f..16f) { onStyleChange(style.copy(formatFontSize = it.sp)) }
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
        DpSlider("Dzień → kino", cardSpacing.dayToCinema, 0f..24f) { onCardSpacingChange(cardSpacing.copy(dayToCinema = it)) }
        DpSlider("Blok seansów", cardSpacing.showingsBlock, 0f..24f) { onCardSpacingChange(cardSpacing.copy(showingsBlock = it)) }
    }

    Group("Ocena: rozmiar bazowy") {
        // Android scales rating pills by device width (unlike iOS's flat
        // sizing), so the slider sets the BASE sp and the readout shows what
        // this device actually renders after RatingBadgeMetrics.scale().
        RatingScaleReadout(ratingStyle.baseFontSize)
        SpSlider("Rozmiar bazowy", ratingStyle.baseFontSize.value, 6f..18f) { onRatingStyleChange(ratingStyle.copy(baseFontSize = it.sp)) }
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

@Composable
private fun KinaControls(cinemaHeader: CinemaHeaderStyle, onChange: (CinemaHeaderStyle) -> Unit) {
    Group("Nagłówek kina") {
        WeightRow("Grubość", cinemaHeader.fontWeight) { onChange(cinemaHeader.copy(fontWeight = it)) }
        SpSlider("Rozmiar", cinemaHeader.fontSize.value, 10f..24f, TuningTags.CinemaHeaderFontSlider) { onChange(cinemaHeader.copy(fontSize = it.sp)) }
    }
    Group("Odstępy") {
        DpSlider("Nagłówek górny", cinemaHeader.headerTopGap, 0f..24f) { onChange(cinemaHeader.copy(headerTopGap = it)) }
        DpSlider("Nagłówek → siatka", cinemaHeader.headerToGrid, 0f..16f) { onChange(cinemaHeader.copy(headerToGrid = it)) }
        DpSlider("Między rzędami", cinemaHeader.sectionSpacing, 0f..32f) { onChange(cinemaHeader.copy(sectionSpacing = it)) }
    }
}

@Composable
private fun FilmControls(filmStyle: FilmDetailStyle, onChange: (FilmDetailStyle) -> Unit) {
    Group("Układ") {
        DpSlider("Sekcje", filmStyle.outerSpacing, 0f..32f) { onChange(filmStyle.copy(outerSpacing = it)) }
        DpSlider("Nagłówek (kolumna)", filmStyle.headerSpacing, 0f..20f) { onChange(filmStyle.copy(headerSpacing = it)) }
    }
    Group("Tytuł") {
        WeightRow("Grubość", filmStyle.titleWeight) { onChange(filmStyle.copy(titleWeight = it)) }
        SpSlider("Rozmiar", filmStyle.titleFontSize.value, 14f..34f, TuningTags.DetailTitleFontSlider) { onChange(filmStyle.copy(titleFontSize = it.sp)) }
        SpSlider("Tytuł oryg.", filmStyle.originalTitleFontSize.value, 10f..22f) { onChange(filmStyle.copy(originalTitleFontSize = it.sp)) }
    }
    Group("Bloki meta") {
        SpSlider("Etykieta", filmStyle.sectionLabelFontSize.value, 8f..16f) { onChange(filmStyle.copy(sectionLabelFontSize = it.sp)) }
        SpSlider("Wartość", filmStyle.sectionBodyFontSize.value, 10f..20f) { onChange(filmStyle.copy(sectionBodyFontSize = it.sp)) }
    }
    Group("Seanse") {
        SpSlider("Nagłówek", filmStyle.showingsHeaderFontSize.value, 12f..26f) { onChange(filmStyle.copy(showingsHeaderFontSize = it.sp)) }
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
private fun SpSlider(label: String, value: Float, range: ClosedFloatingPointRange<Float>, tag: String? = null, onChange: (Float) -> Unit) {
    LabeledSlider(label, value, range, tag) { onChange(it) }
}

@Composable
private fun DpSlider(label: String, value: Dp, range: ClosedFloatingPointRange<Float>, tag: String? = null, onChange: (Dp) -> Unit) {
    LabeledSlider(label, value.value, range, tag) { onChange(it.dp) }
}

@Composable
private fun LabeledSlider(
    label: String,
    value: Float,
    range: ClosedFloatingPointRange<Float>,
    tag: String? = null,
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
            modifier = Modifier.weight(1f).then(if (tag != null) Modifier.testTag(tag) else Modifier),
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

/** Serialises the current page's values to the `key=value`-per-line paste
 *  format shared with iOS. */
internal fun copyTextFor(
    page: TuningPage,
    s: ShowtimeChipStyle,
    r: RatingPillStyle,
    c: CardSpacingStyle,
    h: CinemaHeaderStyle,
    f: FilmDetailStyle,
    screenWidthDp: Int,
): String = when (page) {
    TuningPage.Card -> copyText(s, r, c, screenWidthDp)
    TuningPage.Kina -> buildString {
        append("cinema-header: size=${oneDecimal(h.fontSize.value)} weight=${WeightOption.of(h.fontWeight).key}\n")
        append("cinema-gaps: top=${oneDecimal(h.headerTopGap.value)} headerToGrid=${oneDecimal(h.headerToGrid.value)} section=${oneDecimal(h.sectionSpacing.value)}")
    }
    TuningPage.Film -> buildString {
        append("film-layout: outer=${oneDecimal(f.outerSpacing.value)} header=${oneDecimal(f.headerSpacing.value)}\n")
        append("film-title: size=${oneDecimal(f.titleFontSize.value)} weight=${WeightOption.of(f.titleWeight).key} original=${oneDecimal(f.originalTitleFontSize.value)}\n")
        append("film-meta: label=${oneDecimal(f.sectionLabelFontSize.value)} body=${oneDecimal(f.sectionBodyFontSize.value)}\n")
        append("film-showings: header=${oneDecimal(f.showingsHeaderFontSize.value)}")
    }
}

/** The card page's serialisation: showtime + rating + card-gap lines. The
 *  rating lines carry the device-scale readout (Android-specific, since the
 *  pills size relative to viewport width). */
internal fun copyText(s: ShowtimeChipStyle, r: RatingPillStyle, c: CardSpacingStyle, screenWidthDp: Int): String {
    val scale = RatingBadgeMetrics.scale(screenWidthDp)
    val rendered = r.baseFontSize.value * scale
    return buildString {
        append("time: size=${oneDecimal(s.timeFontSize.value)} weight=${WeightOption.of(s.timeWeight).key}\n")
        append("format: size=${oneDecimal(s.formatFontSize.value)} weight=${WeightOption.of(s.formatWeight).key}\n")
        append("padding: h=${oneDecimal(s.horizontalInset.value)} v=${oneDecimal(s.verticalInset.value)}\n")
        append("gaps: internal=${oneDecimal(s.internalGap.value)} interPill=${oneDecimal(s.interPillGap.value)}\n")
        append("card-gaps: titleToMeta=${oneDecimal(c.titleToMeta.value)} metaToRatings=${oneDecimal(c.metaToRatings.value)} ratingsToShowings=${oneDecimal(c.ratingsToShowings.value)} dayToCinema=${oneDecimal(c.dayToCinema.value)} showingsBlock=${oneDecimal(c.showingsBlock.value)}\n")
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
 * reads like a real screen. [sections] regroups them by cinema for the Kina
 * page; [detailFilm] (+ [detailDetails]) feeds the Film page. Mirrors iOS
 * `TuningSampleData`.
 */
object ShowtimeTuningData {
    val films: List<Film> = listOf(edgeCases(), manyShowtimes(), drama(), kids(), lateNight(), festival())

    /** Cinema-grouped view of the sample films for the Kina page. Uses real
     *  cinema display names so `CinemaSection.pillName` shortens them the way
     *  the live Kina tab does. */
    val sections: List<CinemaSection> = listOf(
        CinemaSection("Cinema City Kinepolis", listOf(films[0], films[2], films[5])),
        CinemaSection("Multikino Stary Browar", listOf(films[1], films[3])),
        CinemaSection("Helios Posnania", listOf(films[4])),
    )

    /** One rich film for the Film-detail page — genres / directors / cast /
     *  countries populated (the listing samples leave them empty) so every meta
     *  block renders and its fonts are tunable. */
    val detailFilm: Film = Film(
        title = "Wszystkie przypadki",
        runtimeMinutes = 128,
        releaseYear = 2025,
        genres = listOf("Dramat", "Thriller"),
        ratings = ratings(7.8, 81, 92, 7.4),
        countries = listOf("Polska", "Francja"),
        directors = listOf("Jan Kowalski"),
        cast = listOf("Anna Nowak", "Piotr Wiśniewski", "Maria Lewandowska"),
        showings = edgeCases().showings,
    )

    /** Synopsis for [detailFilm] (matched by title). No trailers — the tuning
     *  preview keeps the embedded-player WebView out of the picture. */
    val detailDetails: FilmDetails = FilmDetails(
        title = "Wszystkie przypadki",
        originalTitle = "All The Cases",
        synopsis = "Wciągający dramat o splątanych losach kilku rodzin w powojennej Europie. " +
            "Reżyser prowadzi widza przez kolejne odsłony tajemnicy, stopniowo odsłaniając, " +
            "jak jeden wybór potrafi zaważyć na życiu wszystkich bohaterów. Tekst celowo długi, " +
            "by dało się dostroić rozmiar i odstępy opisu na ekranie szczegółów.",
    )

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
