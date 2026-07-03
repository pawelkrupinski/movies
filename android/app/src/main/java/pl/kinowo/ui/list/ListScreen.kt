package pl.kinowo.ui.list

import androidx.activity.compose.BackHandler
import androidx.compose.animation.animateContentSize
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.RowScope
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.imePadding
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.GridItemSpan
import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.ExpandMore
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.Movie
import androidx.compose.material.icons.outlined.Swipe
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.LocalTextStyle
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.material3.rememberModalBottomSheetState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import android.content.res.Configuration
import java.time.LocalDate
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.DateFilter
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.TopBarLayout
import pl.kinowo.ui.common.LocalCinemaHeaderStyle
import pl.kinowo.ui.common.PosterGridMetrics
import pl.kinowo.ui.common.PosterPrefetch
import pl.kinowo.ui.common.viewportWidthDp
import dev.chrisbanes.haze.HazeState
import dev.chrisbanes.haze.HazeStyle
import dev.chrisbanes.haze.HazeTint
import dev.chrisbanes.haze.haze
import dev.chrisbanes.haze.hazeChild
import pl.kinowo.ui.theme.Background
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.TextSecondary

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ListScreen(viewModel: KinowoViewModel, onOpenFilm: (String) -> Unit) {
    val films by viewModel.films.collectAsState()
    val isLoading by viewModel.isLoading.collectAsState()
    val error by viewModel.error.collectAsState()
    // Observed here (not read off the StateFlow inside the VM) so hiding a film
    // or picking a cinema recomposes the grid — see filmsFor's comment.
    val hidden by viewModel.hiddenFilms.collectAsState()
    val selectedCinema by viewModel.selectedCinema.collectAsState()
    // The cinema pill row's universe is the current city's cinemas; a persisted
    // pick absent from them (a leftover from another city) reads as null
    // ("Wszystkie") so the grid never blanks on a stale cross-city name.
    val cityCinemas = remember(films) { viewModel.allCinemas(films) }
    val effectiveCinema = selectedCinema?.takeIf { it in cityCinemas }

    // Wide viewports (tablets, and phones rotated to landscape) host search
    // inline on the top bar; narrow ones (portrait phones) keep it as the
    // floating bottom pill. Keyed off the LIVE viewport width (viewportWidthDp),
    // so a phone moves search into the bar — and spreads the date pills evenly —
    // the moment it's rotated to landscape, matching iOS. The width-driven chip
    // and rating metrics deliberately stay locked to the portrait width. See
    // TopBarLayout and LayoutWidth.
    val wide = TopBarLayout.searchInline(viewportWidthDp())

    // Pull-to-refresh indicator state. Driven ONLY by a user pull — NOT by the
    // background `isLoading`. Binding the indicator to `isLoading` left it stuck:
    // on a cold start the cache paints films instantly, so the PullToRefreshBox
    // first composes while the automatic reload already has `isLoading == true`,
    // and Material3's indicator, shown without a preceding pull gesture, never
    // retracts. Here a pull flips `refreshing` true and the reload job's
    // completion (join) flips it back — deterministic, no flow-conflation race.
    val refreshScope = rememberCoroutineScope()
    var refreshing by remember { mutableStateOf(false) }

    // Backdrop captured by `Modifier.haze` (the grid) and sampled by the
    // floating search pill's `hazeChild` to blur whatever scrolls under it.
    val hazeState = remember { HazeState() }
    var showFilters by remember { mutableStateOf(false) }
    val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)

    // The grid has no day tabs — the selected day is changed by swiping the
    // grid horizontally (left = next day, right = previous, wrapping the
    // ordered day list). Two affordances stand in for an explicit control
    // (mirroring the iOS app):
    //  • a momentary centre label naming the day on each swipe, and
    //  • a once-a-day swipe hint until the user's first-ever swipe.
    var tabLabel by remember { mutableStateOf<String?>(null) }
    var showSwipeHint by remember { mutableStateOf(false) }

    // Flash the newly-selected day's label on each swipe, then fade it after
    // 0.7 s. Re-keying on dateFilter cancels the previous delay, so back-to-back
    // swipes don't leave a stale label stuck. Skips the very first composition
    // (no swipe happened — just don't flash on arrival).
    var seenFirstDay by remember { mutableStateOf(false) }
    LaunchedEffect(viewModel.dateFilter) {
        if (!seenFirstDay) {
            seenFirstDay = true
            return@LaunchedEffect
        }
        tabLabel = viewModel.dateFilter.label
        delay(700)
        tabLabel = null
    }

    // The centre column's vertical scroll, hoisted so the revealed neighbours can
    // mirror it during a drag — the new day slides in showing the same rows, and
    // the committed day keeps that position (lands where the user was, not the
    // top), so swiping between days continues browsing the same region.
    val sharedScroll = rememberLazyGridState()

    // Commit a swiped-to day as the new selection. Wraparound is already applied
    // by the carousel (it hands back the actual neighbour preset). The first
    // committed swipe also retires the once-a-day hint.
    val onCommitDay: (DateFilter) -> Unit = { day ->
        selectDayKeepingTopFlat(sharedScroll) { viewModel.dateFilter = day }
        showSwipeHint = false
        viewModel.markSwiped()
    }

    // Which day pill is highlighted. At rest it tracks viewModel.dateFilter; during a
    // drag the carousel flips it (via onPreviewDay) to the day a release would
    // commit to, the moment the drag crosses the commit threshold — a highlight-
    // only PREVIEW that leaves the selection, grid, and scroll untouched. Kept
    // in sync with viewModel.dateFilter so a pill TAP (which sets viewModel.dateFilter) and a
    // committed swipe both land the highlight on the new day with no flicker.
    var previewDay by remember { mutableStateOf(viewModel.dateFilter) }
    LaunchedEffect(viewModel.dateFilter) { previewDay = viewModel.dateFilter }

    // Surface the swipe hint the moment the first repertoire load lands, gated
    // to once per calendar day until the first swipe. The decision reads
    // DataStore directly (see KinowoViewModel.shouldShowSwipeHint).
    val moviesLoaded = films.isNotEmpty()
    LaunchedEffect(moviesLoaded) {
        if (!moviesLoaded) return@LaunchedEffect
        val today = LocalDate.now().toString()
        if (viewModel.shouldShowSwipeHint(today)) {
            showSwipeHint = true
            viewModel.markSwipeHintShown(today)
            delay(2500)
            showSwipeHint = false
        }
    }

    // Once a day, after the repertoire lands, drop cached posters for films
    // that have left it (no future screening). See KinowoViewModel.
    val context = LocalContext.current
    LaunchedEffect(moviesLoaded) {
        if (moviesLoaded) viewModel.purgePostersIfNeeded(context, LocalDate.now().toString())
    }

    Box(Modifier.fillMaxSize()) {
        // Edge-to-edge: keep the custom top chrome below the status bar and the
        // grid above the nav bar. (DetailScreen's Scaffold handles its own insets.)
        Column(Modifier.fillMaxSize().windowInsetsPadding(WindowInsets.systemBars)) {
            // ── top chrome ────────────────────────────────────────────────────
            DateBar(
                wide = wide,
                highlighted = previewDay,
                filtersActive = viewModel.filtersActive(),
                search = viewModel.search,
                onSearch = { viewModel.search = it },
                onSelect = { day -> selectDayKeepingTopFlat(sharedScroll) { viewModel.dateFilter = day } },
                onOpenFilters = { showFilters = true },
            )

            // A slim, low-emphasis handle directly under the date bar that
            // unfolds a horizontally-scrolling row of cinema pills — the
            // single-select cinema filter. Mirrors iOS's parallel CinemaPillBar.
            CinemaPillBar(
                cinemas = cityCinemas,
                selected = effectiveCinema,
                onSelect = { viewModel.selectCinema(it) },
            )

            // ── content ───────────────────────────────────────────────────────
            // The search field floats over the grid as a bottom capsule
            // (mirrors the iOS `SearchBar`) rather than taking its own row.
            // The grid is the haze source; the pill samples it through
            // `hazeChild`, so the cards blur and shift as they scroll under it.
            // Bottom content padding keeps the last row clear of the pill.
            Box(Modifier.fillMaxSize()) {
                // No floating pill on wide screens (search is inline up top), so the
                // grid only needs to clear it on narrow ones.
                val gridBottomInset = if (wide) 12.dp else SearchBarBottomInset
                Box(Modifier.fillMaxSize().haze(hazeState)) {
                    when {
                        isLoading && films.isEmpty() -> CenteredMessage("Ładowanie repertuaru…")
                        error != null && films.isEmpty() -> ErrorState(error!!) { viewModel.reload() }
                        else -> PullToRefreshBox(
                            isRefreshing = refreshing,
                            onRefresh = {
                                refreshing = true
                                refreshScope.launch {
                                    viewModel.reload().join()
                                    refreshing = false
                                }
                            },
                        ) {
                            // A finger-following carousel: the selected day's grid
                            // is centred; dragging horizontally reveals the wrap-
                            // around previous/next day and commits it on release. The
                            // revealed neighbour mirrors the centre's vertical
                            // scroll during the drag.
                            DayCarousel(
                                current = viewModel.dateFilter,
                                sharedScroll = sharedScroll,
                                onCommitDay = onCommitDay,
                                onPreviewDay = { previewDay = it },
                            ) { day, state, columnModifier ->
                                val visible = viewModel.filmsFor(day, films, hidden, effectiveCinema)
                                // Suppress the per-card cinema label when the
                                // repertoire on show narrows to a single cinema —
                                // it's the same name on every card.
                                val showCinemaHeaders = distinctCinemaCount(visible) > 1
                                // The mirror lands the new day at the SAME scroll the
                                // user was at; ScrollToTopOnChange then eases that up to
                                // the top — so a swipe reads as "land where I was, then
                                // roll to the top". An at-top switch is pinned flat by
                                // selectDayKeepingTopFlat, so this is a no-op there and
                                // doesn't jump.
                                if (day == viewModel.dateFilter) ScrollToTopOnChange(state, viewModel.dateFilter)
                                FilmsGrid(
                                    films = visible,
                                    state = state,
                                    bottomInset = gridBottomInset,
                                    showCinemaHeaders = showCinemaHeaders,
                                    onOpen = onOpenFilm,
                                    onHide = { viewModel.hide(it) },
                                    modifier = columnModifier,
                                )
                            }
                        }
                    }
                }

                if (!wide) {
                    FloatingSearchBar(
                        value = viewModel.search,
                        onValueChange = { viewModel.search = it },
                        hazeState = hazeState,
                        modifier = Modifier.align(Alignment.BottomCenter).imePadding(),
                    )
                }
            }
        }

        // Centre overlays. The hint takes precedence over the momentary label
        // so the two pills never stack on top of each other.
        if (showSwipeHint) {
            SwipeHintOverlay(Modifier.align(Alignment.Center))
        } else {
            tabLabel?.let { TabLabelOverlay(it, Modifier.align(Alignment.Center)) }
        }
    }

    // System back closes the Filtry sheet (the user's "wyjść" gesture). The bare
    // ModalBottomSheet doesn't intercept back in this window config, so layer an
    // explicit handler over showFilters — see FilterSheetBackDismissTest.
    BackHandler(enabled = showFilters) { showFilters = false }

    if (showFilters) {
        FiltersSheet(
            viewModel = viewModel,
            films = films,
            sheetState = sheetState,
            onDismiss = { showFilters = false },
        )
    }
}

// Momentary screen-name pill (Filmy / Kina), flashed centre-screen on arrival.
// The iOS counterpart is `TabLabelOverlay` in FiltersBar.swift.
@Composable
private fun TabLabelOverlay(text: String, modifier: Modifier = Modifier) {
    Surface(modifier = modifier, shape = CircleShape, color = Color.Black.copy(alpha = 0.55f)) {
        Text(
            text,
            color = Color.White,
            fontSize = 26.sp,
            fontWeight = FontWeight.SemiBold,
            modifier = Modifier.padding(horizontal = 28.dp, vertical = 12.dp),
        )
    }
}

// Once-a-day onboarding hint: a swipe icon over one line of copy, telling
// first-time users they can swipe to change the selected day.
@Composable
private fun SwipeHintOverlay(modifier: Modifier = Modifier) {
    Surface(modifier = modifier, shape = RoundedCornerShape(20.dp), color = Color.Black.copy(alpha = 0.6f)) {
        Column(
            Modifier.padding(horizontal = 24.dp, vertical = 16.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
        ) {
            Icon(
                Icons.Outlined.Swipe,
                contentDescription = null,
                tint = Color.White,
                modifier = Modifier.size(34.dp),
            )
            Text(
                "Przesuń, aby zmienić dzień",
                color = Color.White,
                fontSize = 15.sp,
                fontWeight = FontWeight.Medium,
                modifier = Modifier.padding(top = 8.dp),
            )
        }
    }
}

// Floating search pill, pinned bottom-centre and overlaid on the grid —
// the Android counterpart to the iOS `SearchBar` (FiltersBar.swift). A
// translucent capsule with a leading magnifier, an inline text field, and
// a trailing clear glyph that appears once there's a query. `imePadding`
// (applied by the caller) lifts it above the soft keyboard on focus; the
// grids reserve `SearchBarBottomInset` so their last row clears the pill.
@Composable
private fun FloatingSearchBar(
    value: String,
    onValueChange: (String) -> Unit,
    hazeState: HazeState,
    modifier: Modifier = Modifier,
) {
    val shape = RoundedCornerShape(28.dp)
    Row(
        modifier
            .padding(horizontal = 24.dp, vertical = 14.dp)
            .fillMaxWidth()
            .clip(shape)
            // Real frosted glass: blur the grid captured by `haze` and lay a
            // whisper-thin light tint over it, so the pill reads as clear glass
            // (like the iOS `.ultraThinMaterial`) rather than a solid fill. The
            // hairline white border catches the edge against busy posters.
            .hazeChild(
                state = hazeState,
                style = HazeStyle(
                    // The opaque base Haze composites the blur over (the app
                    // background) — required, else it throws at draw time.
                    backgroundColor = Background,
                    // Keep enough blur to visibly distort the cards underneath,
                    // but barely any tint and no noise grain — so it reads as
                    // clear, see-through glass rather than a frosty pane.
                    blurRadius = 1.dp,
                    tint = HazeTint(Color.White.copy(alpha = 0.0f)),
                    noiseFactor = 0f,
                ),
            )
            .border(1.dp, Color.White.copy(alpha = 0.22f), shape)
            .padding(horizontal = 18.dp, vertical = 13.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        SearchFieldContent(value, onValueChange)
    }
}

// Inline search on the top bar (wide screens): the shared field in a
// translucent capsule sized to the date pills, sitting between the pills and
// the Filtry button. Capped at a fixed width so it stays a comfortable
// type-into size and the date pills get the rest of the row. The Android
// counterpart to the iOS InlineSearchField.
@Composable
private fun InlineSearchField(value: String, onValueChange: (String) -> Unit) {
    val shape = RoundedCornerShape(20.dp)
    Row(
        Modifier
            .width(240.dp)
            .clip(shape)
            .background(Color.White.copy(alpha = 0.08f))
            .padding(horizontal = 12.dp, vertical = 7.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        SearchFieldContent(value, onValueChange, compact = true)
    }
}

// The magnifier + text field + clear glyph shared by both search placements:
// the floating bottom FloatingSearchBar and the inline InlineSearchField. In
// compact mode (inline) the glyphs and text shrink to match the date-pill height.
@Composable
private fun RowScope.SearchFieldContent(value: String, onValueChange: (String) -> Unit, compact: Boolean = false) {
    val iconSize = if (compact) 16.dp else 20.dp
    val baseStyle = LocalTextStyle.current
    val textStyle = baseStyle.copy(
        color = Color.White,
        fontSize = if (compact) 14.sp else baseStyle.fontSize,
    )
    Icon(Icons.Filled.Search, contentDescription = null, tint = TextSecondary, modifier = Modifier.size(iconSize))
    Spacer(Modifier.width(if (compact) 8.dp else 10.dp))
    Box(Modifier.weight(1f)) {
        if (value.isEmpty()) {
            Text("Szukaj filmu", color = TextSecondary, fontSize = textStyle.fontSize)
        }
        BasicTextField(
            value = value,
            onValueChange = onValueChange,
            singleLine = true,
            textStyle = textStyle,
            cursorBrush = SolidColor(Brand),
            keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
            modifier = Modifier.fillMaxWidth(),
        )
    }
    if (value.isNotEmpty()) {
        Spacer(Modifier.width(8.dp))
        Icon(
            Icons.Filled.Close,
            contentDescription = "Wyczyść",
            tint = TextSecondary,
            modifier = Modifier.size(iconSize).clickable { onValueChange("") },
        )
    }
}

// The top-bar chrome: the 🎬 mark, the four date pills, the inline search
// field (wide screens only), and the Filtry button — all on one row. Extracted
// so DayPillFitTest can render it at a fixed viewport width and assert the day
// labels never clip. [filtersActive] tints the Filtry icon; [highlighted] drives
// which date pill reads as selected (see DatePills).
@Composable
internal fun DateBar(
    wide: Boolean,
    highlighted: DateFilter,
    filtersActive: Boolean,
    search: String,
    onSearch: (String) -> Unit,
    onSelect: (DateFilter) -> Unit,
    onOpenFilters: () -> Unit,
) {
    Row(
        Modifier.fillMaxWidth().padding(start = 10.dp, end = 4.dp, top = 8.dp),
        // Narrow phones tighten the inter-item gap so the weighted dated pills get
        // more of the row to fill, clearing their labels at 14sp (see DatePill).
        horizontalArrangement = Arrangement.spacedBy(if (wide) 6.dp else 4.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Text("🎬", fontSize = 22.sp)
        // The date pills always spread to fill the row; on wide screens
        // the inline search field sits between them and Filtry, capped
        // at a fixed width rather than eating the leftover space.
        DatePills(wide, highlighted = highlighted, onSelect = onSelect)
        if (wide) {
            InlineSearchField(value = search, onValueChange = onSearch)
        }
        IconButton(onClick = onOpenFilters) {
            Icon(
                Icons.Outlined.FilterList,
                contentDescription = "Filtry",
                tint = if (filtersActive) Brand else TextSecondary,
            )
        }
    }
}

// The four date presets, laid out inline in the top bar (mirroring iOS
// DatePillsRow). On wide screens all four pills share the row width equally
// via `weight` — including "Wszystkie" — so they read as one evenly-spaced
// segmented control. On narrow screens (portrait phones) only the three short
// pills (Dziś / Jutro / 7 dni) get weight while "Wszystkie" keeps its intrinsic
// width, so the dated pills fill all the width left between the 🎬 mark and the
// Filtry icon with no trailing slack. See TopBarLayout.datePillFillsRow.
// [highlighted] drives which pill reads as selected — it tracks viewModel.dateFilter at
// rest but flips to the swipe-preview day mid-drag (see ListScreen.previewDay).
// A TAP still sets viewModel.dateFilter directly, which becomes the new highlight.
@Composable
private fun RowScope.DatePills(wide: Boolean, highlighted: DateFilter, onSelect: (DateFilter) -> Unit) {
    for (preset in DateFilter.presets) {
        val fills = TopBarLayout.datePillFillsRow(preset == DateFilter.Anytime, wide)
        DatePill(
            label = preset.label,
            selected = highlighted == preset,
            wide = wide,
            modifier = if (fills) Modifier.weight(1f) else Modifier,
        ) { onSelect(preset) }
    }
}

@Composable
private fun DatePill(label: String, selected: Boolean, wide: Boolean, modifier: Modifier = Modifier, onClick: () -> Unit) {
    Box(
        modifier
            .clip(CircleShape)
            .background(if (selected) Brand.copy(alpha = 0.85f) else Color.Transparent)
            .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null,
            ) { onClick() }
            // Roomy 12dp inset on wide screens; narrow phones tighten to 6dp so
            // the three weighted dated pills, after filling the leftover width
            // between 🎬 and Filtry, still clear "Jutro" / "7 dni" at 14sp on a
            // 360dp phone (Galaxy S24) instead of clipping. Guarded by
            // DayPillFitTest.
            .padding(horizontal = if (wide) 12.dp else 6.dp, vertical = 7.dp),
        contentAlignment = Alignment.Center,
    ) {
        Text(
            label,
            fontSize = 14.sp,
            fontWeight = FontWeight.Medium,
            maxLines = 1,
            color = Color.White,
        )
    }
}

// A slim expand-handle under the DateBar that unfolds a horizontally-scrolling
// row of cinema pills — the single-select cinema filter. Collapsed it shows the
// current pick ("Wszystkie kina" or the cinema's short name) beside a chevron
// that rotates as the row unfolds; expanded it reveals a leading "Wszystkie"
// pill and one pill per cinema in the current city, styled like the date pills
// (capsule, Brand tint when active). Deliberately low emphasis — a secondary
// control, not a second app bar. Renders nothing until the city has cinemas.
// [selected] is the EFFECTIVE pick (a cross-city leftover already resolved to
// null upstream). Mirrors iOS's parallel CinemaPillBar. Exercised by
// CinemaPillBarTest.
@Composable
internal fun CinemaPillBar(
    cinemas: List<String>,
    selected: String?,
    onSelect: (String?) -> Unit,
) {
    if (cinemas.isEmpty()) return
    // rememberSaveable so the unfolded state survives config changes; the bar
    // sits above the grid and is never disposed by scrolling.
    var expanded by rememberSaveable { mutableStateOf(false) }
    val chevronRotation by animateFloatAsState(if (expanded) 180f else 0f, label = "cinemaChevron")
    Column(Modifier.fillMaxWidth().animateContentSize()) {
        Row(
            Modifier
                .fillMaxWidth()
                .clickable(
                    interactionSource = remember { MutableInteractionSource() },
                    indication = null,
                ) { expanded = !expanded }
                .padding(start = 14.dp, end = 8.dp, top = 4.dp, bottom = 4.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(
                text = selected?.let { CinemaSection.pillName(it) } ?: "Wszystkie kina",
                color = TextSecondary,
                fontSize = 13.sp,
                fontWeight = FontWeight.Medium,
                maxLines = 1,
                modifier = Modifier.weight(1f),
            )
            Icon(
                Icons.Filled.ExpandMore,
                contentDescription = if (expanded) "Zwiń kina" else "Rozwiń kina",
                tint = TextSecondary,
                modifier = Modifier.size(20.dp).rotate(chevronRotation),
            )
        }
        if (expanded) {
            LazyRow(
                Modifier.fillMaxWidth(),
                contentPadding = PaddingValues(start = 12.dp, end = 12.dp, bottom = 4.dp),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                item(key = "cinema_all") {
                    CinemaPill(label = "Wszystkie", selected = selected == null) { onSelect(null) }
                }
                items(cinemas, key = { it }) { cinema ->
                    CinemaPill(
                        label = CinemaSection.pillName(cinema),
                        selected = cinema == selected,
                    ) { onSelect(cinema) }
                }
            }
        }
    }
}

// One cinema capsule in [CinemaPillBar], sharing the date pill's visual style:
// a CircleShape capsule, Brand-tinted when active, a faint fill otherwise.
@Composable
private fun CinemaPill(label: String, selected: Boolean, onClick: () -> Unit) {
    Box(
        Modifier
            .clip(CircleShape)
            .background(if (selected) Brand.copy(alpha = 0.85f) else Color.White.copy(alpha = 0.08f))
            .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null,
            ) { onClick() }
            .padding(horizontal = 14.dp, vertical = 7.dp),
        contentAlignment = Alignment.Center,
    ) {
        Text(
            label,
            fontSize = 14.sp,
            fontWeight = FontWeight.Medium,
            maxLines = 1,
            color = Color.White,
        )
    }
}

// Distinct cinema names anywhere in the currently-shown films. Drives whether
// the per-card cinema label is worth showing: with a single cinema on screen
// it's the same name on every card, so it's suppressed as redundant.
private fun distinctCinemaCount(films: List<Film>): Int {
    val seen = HashSet<String>()
    for (film in films) for (day in film.showings) for (c in day.cinemas) seen.add(c.cinema)
    return seen.size
}

// Vertical room the floating search pill occupies at the bottom of the
// grid — capsule height (~46dp) plus its 14dp margin plus breathing space.
// Grids carry it as bottom content padding so the last card row can scroll
// clear of the pill instead of sitting permanently behind it.
private val SearchBarBottomInset = 84.dp

// Poster grid columns. Portrait always shows exactly two columns regardless of
// device width; landscape fits as many as the live width allows, each no
// narrower than a portrait card — that floor is the width the two-chips-per-row
// rule is proven against, and chips are locked to portrait scale (layoutWidthDp),
// so every landscape column is wide enough to keep two chips on a row.
@Composable
private fun posterGridCells(): GridCells {
    val config = LocalConfiguration.current
    val landscape = config.orientation == Configuration.ORIENTATION_LANDSCAPE
    return posterGridColumns(landscape, config.smallestScreenWidthDp)
}

internal fun posterGridColumns(landscape: Boolean, layoutWidthDp: Int): GridCells =
    if (!landscape) GridCells.Fixed(2)
    else GridCells.Adaptive(minSize = PosterGridMetrics.cardColumnDp(layoutWidthDp).dp)

/**
 * Switch the selected day via [setDay], keeping an at-top view flat across the
 * change.
 *
 * The centre column shares ONE [LazyGridState] across days, and a day change
 * restores the grid position BY KEY (films are keyed by title). If you're at the
 * top of one day on a film that the next day also lists — but lower down — the
 * state jumps to that film's index in the new day, and [ScrollToTopOnChange] then
 * animates back up: a "synthetic scroll" for someone who was already at the top.
 *
 * When the outgoing day is already at the very top we pin the incoming day to
 * index 0 in the SAME layout pass ([LazyGridState.requestScrollToItem]), which
 * overrides the key restore so the new day lays out flat at the top — no jump to
 * the film's index, so the follow-up roll-to-top is a genuine no-op. When NOT at
 * the top we leave the scroll untouched: the new day keeps the position and
 * [ScrollToTopOnChange] eases it up ("land where I was, then roll to the top").
 */
internal inline fun selectDayKeepingTopFlat(state: LazyGridState, setDay: () -> Unit) {
    val wasAtTop = state.firstVisibleItemIndex == 0 && state.firstVisibleItemScrollOffset == 0
    setDay()
    if (wasAtTop) state.requestScrollToItem(0)
}

/**
 * Animate [state] to the first item whenever [key] changes — so a day swipe from
 * a scrolled position eases up to the top, and a date-pill tap / Kina section
 * change starts the new content at the top instead of stranding the user mid-list.
 *
 * An at-top day switch is kept flat upstream by [selectDayKeepingTopFlat], so the
 * grid is already at index 0 here and this animate is a true no-op — no visible
 * roll. The first composition is a no-op too, which also preserves a scroll
 * position restored across a config change.
 */
@Composable
internal fun ScrollToTopOnChange(state: LazyGridState, key: Any?) {
    var seenFirst by remember { mutableStateOf(false) }
    LaunchedEffect(key) {
        if (!seenFirst) seenFirst = true else state.animateScrollToItem(0)
    }
}

@Composable
internal fun FilmsGrid(
    films: List<Film>,
    state: LazyGridState,
    bottomInset: Dp,
    showCinemaHeaders: Boolean,
    onOpen: (String) -> Unit,
    onHide: (String) -> Unit,
    modifier: Modifier = Modifier,
) {
    if (films.isEmpty()) {
        // Apply the column modifier so the empty state fills exactly its own
        // carousel column and centres within it. Without it the bare
        // fillMaxSize() has no width box and its placement leaks from the
        // neighbour columns' content — jamming the message against the screen
        // edge on days whose neighbour is full.
        EmptyState("Brak repertuaru.", modifier)
        return
    }
    // Grid items map 1:1 to films, so the URL list lines up with item indices.
    PosterPrefetch(remember(films) { films.map { it.posterChain.firstOrNull().orEmpty() } }, state)
    LazyVerticalGrid(
        columns = posterGridCells(),
        state = state,
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = bottomInset),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
        modifier = modifier.fillMaxSize(),
    ) {
        items(films, key = { it.title }) { film ->
            FilmCard(film, showCinemaHeaders = showCinemaHeaders, onOpen = { onOpen(film.title) }, onHide = { onHide(film.title) })
        }
    }
}

@Composable
internal fun CinemaGrid(sections: List<CinemaSection>, showHeaders: Boolean, bottomInset: Dp, scrollResetKey: Any?, onOpen: (String) -> Unit, onHide: (String) -> Unit) {
    if (sections.isEmpty()) {
        EmptyState("Brak repertuaru.")
        return
    }
    val gridState = rememberLazyGridState()
    ScrollToTopOnChange(gridState, scrollResetKey)
    // Mirror the grid's item order: a blank slot for each section header,
    // then one poster URL per film, so indices line up with the grid.
    val posterUrls = remember(sections) {
        buildList {
            for (section in sections) {
                add("")
                for (film in section.films) add(film.posterChain.firstOrNull().orEmpty())
            }
        }
    }
    PosterPrefetch(posterUrls, gridState)
    val headerStyle = LocalCinemaHeaderStyle.current
    LazyVerticalGrid(
        columns = posterGridCells(),
        state = gridState,
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = bottomInset),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
        verticalArrangement = Arrangement.spacedBy(headerStyle.sectionSpacing),
        modifier = Modifier.fillMaxSize(),
    ) {
        for (section in sections) {
            if (showHeaders) {
                item(span = { GridItemSpan(maxLineSpan) }, key = "h_${section.cinema}") {
                    Text(
                        text = CinemaSection.pillName(section.cinema),
                        color = CinemaBlue,
                        fontSize = headerStyle.fontSize,
                        fontWeight = headerStyle.fontWeight,
                        modifier = Modifier.padding(top = headerStyle.headerTopGap, bottom = headerStyle.headerToGrid),
                    )
                }
            }
            items(section.films, key = { "${section.cinema}_${it.title}" }) { film ->
                FilmCard(film, showCinemaHeaders = false, onOpen = { onOpen(film.title) }, onHide = { onHide(film.title) })
            }
        }
    }
}

@Composable
private fun CenteredMessage(text: String) {
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
        Text(text, color = TextSecondary)
    }
}

@Composable
private fun EmptyState(text: String, modifier: Modifier = Modifier) {
    // A bare Column dispatches no nested-scroll deltas, so the enclosing
    // PullToRefreshBox never sees a drag — pull-to-refresh dies on a "brak
    // repertuaru" day. verticalScroll makes the column over-draggable (the
    // content fits, so it never actually scrolls, but a top over-drag is
    // forwarded up to the refresh box), re-enabling refresh on an empty day.
    // fillMaxSize keeps the minHeight, so Arrangement.Center still centres.
    Column(
        modifier
            .fillMaxSize()
            .verticalScroll(rememberScrollState()),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center,
    ) {
        Icon(Icons.Outlined.Movie, contentDescription = null, tint = TextSecondary)
        Text(text, color = TextSecondary, modifier = Modifier.padding(top = 8.dp))
    }
}

@Composable
private fun ErrorState(message: String, onRetry: () -> Unit) {
    Column(
        Modifier.fillMaxSize().padding(24.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center,
    ) {
        Text("Nie udało się pobrać repertuaru.", fontWeight = FontWeight.SemiBold)
        Text(message, color = TextSecondary, fontSize = 12.sp, modifier = Modifier.padding(top = 4.dp))
        androidx.compose.material3.Button(onClick = onRetry, modifier = Modifier.padding(top = 12.dp)) {
            Text("Spróbuj ponownie")
        }
    }
}
