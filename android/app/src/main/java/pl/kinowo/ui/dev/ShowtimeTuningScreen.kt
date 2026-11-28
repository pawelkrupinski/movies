package pl.kinowo.ui.dev

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
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Button
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.foundation.pager.PagerState
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
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import kotlinx.coroutines.launch
import pl.kinowo.model.Film
import pl.kinowo.ui.common.CardSpacingStyle
import pl.kinowo.ui.common.CinemaHeaderStyle
import pl.kinowo.ui.common.FilmDetailStyle
import pl.kinowo.ui.common.LocalCardSpacingStyle
import pl.kinowo.ui.common.LocalCinemaHeaderStyle
import pl.kinowo.ui.common.LocalFilmDetailStyle
import pl.kinowo.ui.common.LocalRatingPillStyle
import pl.kinowo.ui.common.LocalShowtimeChipStyle
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
    val styles = remember { TuningStyles() }
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
            LocalShowtimeChipStyle provides styles.chip,
            LocalRatingPillStyle provides styles.rating,
            LocalCardSpacingStyle provides styles.spacing,
            LocalCinemaHeaderStyle provides styles.cinemaHeader,
            LocalFilmDetailStyle provides styles.film,
        ) {
            TuningPager(pager)
        }

        ControlsSheet(
            page = TuningPage.entries[pager.currentPage],
            onSelectPage = { p -> scope.launch { pager.animateScrollToPage(p.ordinal) } },
            styles = styles,
            modifier = Modifier.align(Alignment.BottomCenter),
        )
    }
}

/** The live styles the tuning controls edit, one per previewed view. */
internal class TuningStyles {
    var chip by mutableStateOf(ShowtimeChipStyle())
    var rating by mutableStateOf(RatingPillStyle())
    var spacing by mutableStateOf(CardSpacingStyle())
    var cinemaHeader by mutableStateOf(CinemaHeaderStyle())
    var film by mutableStateOf(FilmDetailStyle())

    /** Back to the shipping values of what [page] previews. */
    fun reset(page: TuningPage) {
        when (page) {
            TuningPage.Card -> {
                chip = ShowtimeChipStyle(); rating = RatingPillStyle(); spacing = CardSpacingStyle()
            }
            TuningPage.Kina -> cinemaHeader = CinemaHeaderStyle()
            TuningPage.Film -> film = FilmDetailStyle()
        }
    }
}

@Composable
private fun TuningPager(pager: PagerState) {
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
    styles: TuningStyles,
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
            // Dragging up (negative delta) grows the sheet.
            SheetHandle { delta ->
                val newDp = height - with(density) { delta.toDp() }
                height = newDp.coerceIn(SheetMinHeight, fullHeight)
            }

            PageTabs(current = page, onSelect = onSelectPage)
            SheetHeader(
                page = page, style = styles.chip, ratingStyle = styles.rating, cardSpacing = styles.spacing,
                cinemaHeader = styles.cinemaHeader, filmStyle = styles.film, onReset = { styles.reset(page) },
            )
            PageControls(page, styles)
        }
    }
}

@Composable
private fun SheetHandle(onDragBy: (Float) -> Unit) {
    Box(
        Modifier
            .fillMaxWidth()
            .height(28.dp)
            .draggable(orientation = Orientation.Vertical, state = rememberDraggableState(onDragBy)),
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
internal fun ResolutionReadout() {
    val density = LocalDensity.current
    val configuration = LocalConfiguration.current
    Text(
        DisplayInfo.tuningReadout(configuration.screenWidthDp, configuration.screenHeightDp, density.density),
        color = Color.White.copy(alpha = 0.55f),
        fontSize = 10.sp,
        modifier = Modifier.testTag(TuningTags.ResolutionReadout),
    )
}

@Preview(widthDp = 380, heightDp = 800)
@Composable
private fun ShowtimeTuningScreenPreview() {
    ShowtimeTuningScreen()
}
