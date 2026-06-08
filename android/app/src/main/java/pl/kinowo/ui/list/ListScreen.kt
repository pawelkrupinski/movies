package pl.kinowo.ui.list

import androidx.activity.compose.BackHandler
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
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.Close
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
import androidx.compose.runtime.setValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
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
fun ListScreen(vm: KinowoViewModel, onOpenFilm: (String) -> Unit) {
    val films by vm.films.collectAsState()
    val isLoading by vm.isLoading.collectAsState()
    val error by vm.error.collectAsState()
    // Observed here (not read off the StateFlow inside the VM) so hiding a film
    // or toggling a cinema recomposes the grid — see filmsForFilmsTab's comment.
    val hidden by vm.hiddenFilms.collectAsState()
    val disabled by vm.disabledCinemas.collectAsState()

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
    LaunchedEffect(vm.dateFilter) {
        if (!seenFirstDay) {
            seenFirstDay = true
            return@LaunchedEffect
        }
        tabLabel = vm.dateFilter.label
        delay(700)
        tabLabel = null
    }

    // Commit a swiped-to day as the new selection. Wraparound is already applied
    // by the carousel (it hands back the actual neighbour preset). The first
    // committed swipe also retires the once-a-day hint.
    val onCommitDay: (DateFilter) -> Unit = { day ->
        vm.dateFilter = day
        showSwipeHint = false
        vm.markSwiped()
    }

    // Which day pill is highlighted. At rest it tracks vm.dateFilter; during a
    // drag the carousel flips it (via onPreviewDay) to the day a release would
    // commit to, the moment the drag crosses the commit threshold — a highlight-
    // only PREVIEW that leaves the selection, grid, and scroll untouched. Kept
    // in sync with vm.dateFilter so a pill TAP (which sets vm.dateFilter) and a
    // committed swipe both land the highlight on the new day with no flicker.
    var previewDay by remember { mutableStateOf(vm.dateFilter) }
    LaunchedEffect(vm.dateFilter) { previewDay = vm.dateFilter }

    // The centre column's vertical scroll, hoisted so the revealed neighbours can
    // mirror it during a drag — the new day slides in showing the same rows, and
    // the committed day keeps that position (lands where the user was, not the
    // top), so swiping between days continues browsing the same region.
    val sharedScroll = rememberLazyGridState()

    // Surface the swipe hint the moment the first repertoire load lands, gated
    // to once per calendar day until the first swipe. The decision reads
    // DataStore directly (see KinowoViewModel.shouldShowSwipeHint).
    val moviesLoaded = films.isNotEmpty()
    LaunchedEffect(moviesLoaded) {
        if (!moviesLoaded) return@LaunchedEffect
        val today = LocalDate.now().toString()
        if (vm.shouldShowSwipeHint(today)) {
            showSwipeHint = true
            vm.markSwipeHintShown(today)
            delay(2500)
            showSwipeHint = false
        }
    }

    // Once a day, after the repertoire lands, drop cached posters for films
    // that have left it (no future screening). See KinowoViewModel.
    val context = LocalContext.current
    LaunchedEffect(moviesLoaded) {
        if (moviesLoaded) vm.purgePostersIfNeeded(context, LocalDate.now().toString())
    }

    Box(Modifier.fillMaxSize()) {
        // Edge-to-edge: keep the custom top chrome below the status bar and the
        // grid above the nav bar. (DetailScreen's Scaffold handles its own insets.)
        Column(Modifier.fillMaxSize().windowInsetsPadding(WindowInsets.systemBars)) {
            // ── top chrome ────────────────────────────────────────────────────
            Row(
                Modifier.fillMaxWidth().padding(start = 10.dp, end = 4.dp, top = 8.dp),
                horizontalArrangement = Arrangement.spacedBy(6.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Text("🎬", fontSize = 22.sp)
                // The date pills always spread to fill the row; on wide screens
                // the inline search field sits between them and Filtry, capped
                // at a fixed width rather than eating the leftover space.
                DatePills(wide, highlighted = previewDay, onSelect = { vm.dateFilter = it })
                if (wide) {
                    InlineSearchField(value = vm.search, onValueChange = { vm.search = it })
                }
                IconButton(onClick = { showFilters = true }) {
                    Icon(
                        Icons.Outlined.FilterList,
                        contentDescription = "Filtry",
                        tint = if (vm.filtersActive(vm.allCinemas(films))) Brand else TextSecondary,
                    )
                }
            }

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
                        error != null && films.isEmpty() -> ErrorState(error!!) { vm.reload() }
                        else -> PullToRefreshBox(
                            isRefreshing = refreshing,
                            onRefresh = {
                                refreshing = true
                                refreshScope.launch {
                                    vm.reload().join()
                                    refreshing = false
                                }
                            },
                        ) {
                            // A finger-following carousel: the selected day's grid
                            // is centred; dragging horizontally reveals the wrap-
                            // around prev/next day and commits it on release. The
                            // revealed neighbour mirrors the centre's vertical
                            // scroll during the drag.
                            DayCarousel(
                                current = vm.dateFilter,
                                sharedScroll = sharedScroll,
                                onCommitDay = onCommitDay,
                                onPreviewDay = { previewDay = it },
                            ) { day, state, columnModifier ->
                                val visible = vm.filmsFor(day, films, hidden, disabled)
                                // Suppress the per-card cinema label when the
                                // repertoire on show narrows to a single cinema —
                                // it's the same name on every card.
                                val showCinemaHeaders = distinctCinemaCount(visible) > 1
                                // The mirror lands the new day at the SAME scroll the
                                // user was at; ScrollToTopOnChange then eases that up to
                                // the top — so a swipe reads as "land where I was, then
                                // roll to the top". A no-op when already at the exact
                                // top, so an at-top swipe doesn't jump.
                                if (day == vm.dateFilter) ScrollToTopOnChange(state, vm.dateFilter)
                                FilmsGrid(
                                    films = visible,
                                    state = state,
                                    bottomInset = gridBottomInset,
                                    showCinemaHeaders = showCinemaHeaders,
                                    onOpen = onOpenFilm,
                                    onHide = { vm.hide(it) },
                                    modifier = columnModifier,
                                )
                            }
                        }
                    }
                }

                if (!wide) {
                    FloatingSearchBar(
                        value = vm.search,
                        onValueChange = { vm.search = it },
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
            vm = vm,
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

// The four date presets, laid out inline in the top bar (mirroring iOS
// DatePillsRow). On wide screens all four pills share the row width equally
// via `weight` — including "Wszystkie" — so they read as one evenly-spaced
// segmented control. On narrow screens only the three short pills (Dziś /
// Jutro / 7 dni) get weight while "Wszystkie" keeps its intrinsic width, so
// the row still fits beside the 🎬 mark and Filtry icon without a separate
// strip or horizontal scrolling. See TopBarLayout.datePillFillsRow.
// [highlighted] drives which pill reads as selected — it tracks vm.dateFilter at
// rest but flips to the swipe-preview day mid-drag (see ListScreen.previewDay).
// A TAP still sets vm.dateFilter directly, which becomes the new highlight.
@Composable
private fun RowScope.DatePills(wide: Boolean, highlighted: DateFilter, onSelect: (DateFilter) -> Unit) {
    for (preset in DateFilter.presets) {
        val fills = TopBarLayout.datePillFillsRow(preset == DateFilter.Anytime, wide)
        DatePill(
            label = preset.label,
            selected = highlighted == preset,
            modifier = if (fills) Modifier.weight(1f) else Modifier,
        ) { onSelect(preset) }
    }
}

@Composable
private fun DatePill(label: String, selected: Boolean, modifier: Modifier = Modifier, onClick: () -> Unit) {
    Box(
        modifier
            .clip(CircleShape)
            .background(if (selected) Brand.copy(alpha = 0.85f) else Color.Transparent)
            .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null,
            ) { onClick() }
            .padding(horizontal = 12.dp, vertical = 7.dp),
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
    val cfg = LocalConfiguration.current
    val landscape = cfg.orientation == Configuration.ORIENTATION_LANDSCAPE
    return posterGridColumns(landscape, cfg.smallestScreenWidthDp)
}

internal fun posterGridColumns(landscape: Boolean, layoutWidthDp: Int): GridCells =
    if (!landscape) GridCells.Fixed(2)
    else GridCells.Adaptive(minSize = PosterGridMetrics.cardColumnDp(layoutWidthDp).dp)

/**
 * Animate [state] to the first item whenever [key] changes — so a day swipe (the
 * mirror lands the new day where the user was) then eases up to the top, and a
 * date-pill tap / Kina section change starts the new content at the top instead
 * of stranding the user mid-list.
 *
 * `animateScrollToItem(0)` is a visible roll-to-top — "land where I was, then go
 * to the top" — and a no-op from the exact top, so an at-top swipe doesn't jump.
 * The first composition is a no-op, which also preserves a scroll position
 * restored across a config change.
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
