package pl.kinowo.ui.list

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.rememberScrollState
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
import androidx.compose.runtime.snapshotFlow
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
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.take
import android.content.res.Configuration
import java.time.LocalDate
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.DateFilter
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.TopBarLayout
import pl.kinowo.ui.common.PosterPrefetch
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

    // Wide screens (tablets, landscape phones) host search inline on the top
    // bar; narrow ones keep it as the floating bottom pill. screenWidthDp
    // recomposes on rotation / resize, so the placement follows. See TopBarLayout.
    val wide = TopBarLayout.searchInline(LocalConfiguration.current.screenWidthDp)

    // Pull-to-refresh indicator state. Driven ONLY by a user pull — NOT by the
    // background `isLoading`. Binding the indicator to `isLoading` left it stuck:
    // on a cold start the cache paints films instantly, so the PullToRefreshBox
    // first composes while the automatic reload already has `isLoading == true`,
    // and Material3's indicator, shown without a preceding pull gesture, never
    // retracts. Here a pull flips `refreshing` true and the reload job's
    // completion (join) flips it back — deterministic, no flow-conflation race.
    val refreshScope = rememberCoroutineScope()
    var refreshing by remember { mutableStateOf(false) }

    val pager = rememberPagerState(pageCount = { 2 })
    // Backdrop captured by `Modifier.haze` (the grid) and sampled by the
    // floating search pill's `hazeChild` to blur whatever scrolls under it.
    val hazeState = remember { HazeState() }
    var showFilters by remember { mutableStateOf(false) }
    val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)

    // There is no tab bar — Filmy / Kina are reached only by swiping the
    // pager. Two affordances stand in for it (mirroring the iOS app):
    //  • a momentary centre label naming the screen on arrival, and
    //  • a once-a-day swipe hint until the user's first-ever swipe.
    var tabLabel by remember { mutableStateOf<String?>(null) }
    var showSwipeHint by remember { mutableStateOf(false) }

    // Flash the destination screen's name on first appear and on each swipe,
    // then fade it after 0.7 s. Re-keying on currentPage cancels the previous
    // delay, so back-to-back swipes don't leave a stale label stuck.
    LaunchedEffect(pager.currentPage) {
        tabLabel = if (pager.currentPage == 0) "Filmy" else "Kina"
        delay(700)
        tabLabel = null
    }

    // The first-ever settled page change is, by definition, a real swipe (no
    // tabs left to drive it programmatically): retire the hint for good.
    LaunchedEffect(Unit) {
        snapshotFlow { pager.currentPage }.drop(1).take(1).collect {
            showSwipeHint = false
            vm.markSwiped()
        }
    }

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
                DatePills(vm, wide)
                if (wide) {
                    InlineSearchField(value = vm.search, onValueChange = { vm.search = it })
                }
                IconButton(onClick = { showFilters = true }) {
                    Icon(
                        Icons.Outlined.FilterList,
                        contentDescription = "Filtry",
                        tint = if (vm.filtersActive) Brand else TextSecondary,
                    )
                }
            }

            if (pager.currentPage == 1) {
                CinemaChips(vm, films)
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
                        else -> HorizontalPager(state = pager, modifier = Modifier.fillMaxSize()) { page ->
                            PullToRefreshBox(
                                isRefreshing = refreshing,
                                onRefresh = {
                                    refreshing = true
                                    refreshScope.launch {
                                        vm.reload().join()
                                        refreshing = false
                                    }
                                },
                            ) {
                                if (page == 0) {
                                    FilmsGrid(vm.filmsForFilmsTab(films, hidden, disabled), gridBottomInset, onOpenFilm) { vm.hide(it) }
                                } else {
                                    CinemaGrid(
                                        vm.cinemaSections(films, hidden),
                                        // A pinned cinema shows only its own section, so the
                                        // per-cinema header is redundant — the pill already names it.
                                        showHeaders = vm.pinnedCinema == null,
                                        bottomInset = gridBottomInset,
                                        onOpen = onOpenFilm,
                                    ) { vm.hide(it) }
                                }
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

    if (showFilters) {
        FiltersSheet(
            vm = vm,
            films = films,
            // The /kina tab pins one cinema via its pill row, so the multi-select
            // cinema filter is redundant there — only the Filmy tab shows it.
            showCinemaFilter = pager.currentPage == 0,
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
// first-time users they can swipe to the Kina screen.
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
                "Przesuń, aby zobaczyć kina",
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
@Composable
private fun RowScope.DatePills(vm: KinowoViewModel, wide: Boolean) {
    for (preset in DateFilter.presets) {
        val fills = TopBarLayout.datePillFillsRow(preset == DateFilter.Anytime, wide)
        DatePill(
            label = preset.label,
            selected = vm.dateFilter == preset,
            modifier = if (fills) Modifier.weight(1f) else Modifier,
        ) { vm.dateFilter = preset }
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

// Single-cinema selector for the /kina tab: a horizontally scrollable row of
// pills — "Wszystkie" (no pin → all cinemas) first, then one per cinema.
// Mirrors iOS `CinemaPillsRow`: pills tile edge-to-edge, their rectangular hit
// areas touching exactly (zero Row spacing + a tapMargin inside each pill), so
// there's no dead gap to miss-tap into while the visible capsules keep a gap.
@Composable
private fun CinemaChips(vm: KinowoViewModel, films: List<Film>) {
    val cinemas = remember(films) { vm.allCinemas(films) }
    Row(
        Modifier
            .fillMaxWidth()
            .horizontalScroll(rememberScrollState())
            .padding(horizontal = 11.dp, vertical = 5.dp),
    ) {
        FilterPill("Wszystkie", selected = vm.pinnedCinema == null) { vm.pinnedCinema = null }
        for (cinema in cinemas) {
            FilterPill(
                title = CinemaSection.pillName(cinema),
                selected = vm.pinnedCinema == cinema,
            ) { vm.pinnedCinema = cinema }
        }
    }
}

// Half the gap between neighbouring pills. It lives as padding *inside* each
// pill's tap target (with zero Row spacing), so the rectangular hit areas of
// adjacent pills meet exactly while the visible capsules keep a 2*tapMargin gap.
private val FilterPillTapMargin = 3.dp

// Shared pill for the date and cinema selector rows: an edge-to-edge capsule
// with a rectangular tap area, mirroring iOS `CinemaPillsRow`.
@Composable
private fun FilterPill(title: String, selected: Boolean, onClick: () -> Unit) {
    Box(
        Modifier
            .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null,
            ) { onClick() }
            .padding(FilterPillTapMargin),
    ) {
        Text(
            title,
            fontSize = 13.sp,
            fontWeight = FontWeight.Medium,
            maxLines = 1,
            color = Color.White,
            modifier = Modifier
                .clip(CircleShape)
                .background(if (selected) Brand.copy(alpha = 0.85f) else Color.White.copy(alpha = 0.08f))
                .padding(horizontal = 10.dp, vertical = 5.dp),
        )
    }
}

// Vertical room the floating search pill occupies at the bottom of the
// grid — capsule height (~46dp) plus its 14dp margin plus breathing space.
// Grids carry it as bottom content padding so the last card row can scroll
// clear of the pill instead of sitting permanently behind it.
private val SearchBarBottomInset = 84.dp

// Poster grid columns. Portrait always shows exactly two columns regardless of
// device width; landscape stays adaptive so wider screens fill with more.
@Composable
private fun posterGridCells(): GridCells {
    val portrait = LocalConfiguration.current.orientation != Configuration.ORIENTATION_LANDSCAPE
    return if (portrait) GridCells.Fixed(2) else GridCells.Adaptive(minSize = 170.dp)
}

@Composable
private fun FilmsGrid(films: List<Film>, bottomInset: Dp, onOpen: (String) -> Unit, onHide: (String) -> Unit) {
    if (films.isEmpty()) {
        EmptyState("Brak repertuaru.")
        return
    }
    val gridState = rememberLazyGridState()
    // Grid items map 1:1 to films, so the URL list lines up with item indices.
    PosterPrefetch(remember(films) { films.map { it.posterChain.firstOrNull().orEmpty() } }, gridState)
    LazyVerticalGrid(
        columns = posterGridCells(),
        state = gridState,
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = bottomInset),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
        modifier = Modifier.fillMaxSize(),
    ) {
        items(films, key = { it.title }) { film ->
            FilmCard(film, showCinemaHeaders = true, onOpen = { onOpen(film.title) }, onHide = { onHide(film.title) })
        }
    }
}

@Composable
private fun CinemaGrid(sections: List<CinemaSection>, showHeaders: Boolean, bottomInset: Dp, onOpen: (String) -> Unit, onHide: (String) -> Unit) {
    if (sections.isEmpty()) {
        EmptyState("Brak repertuaru.")
        return
    }
    val gridState = rememberLazyGridState()
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
    LazyVerticalGrid(
        columns = posterGridCells(),
        state = gridState,
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = bottomInset),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
        modifier = Modifier.fillMaxSize(),
    ) {
        for (section in sections) {
            if (showHeaders) {
                item(span = { GridItemSpan(maxLineSpan) }, key = "h_${section.cinema}") {
                    Text(
                        text = CinemaSection.pillName(section.cinema),
                        color = CinemaBlue,
                        fontSize = 15.sp,
                        fontWeight = FontWeight.SemiBold,
                        modifier = Modifier.padding(top = 8.dp, bottom = 2.dp),
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
private fun EmptyState(text: String) {
    Column(
        Modifier.fillMaxSize(),
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
