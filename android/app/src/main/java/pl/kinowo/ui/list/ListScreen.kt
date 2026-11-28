package pl.kinowo.ui.list

import androidx.compose.ui.semantics.selected
import androidx.compose.runtime.CompositionLocalProvider
import pl.kinowo.ui.common.LocalOutsideDays
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.imePadding
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBars
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Surface
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
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.time.LocalDate
import pl.kinowo.R
import pl.kinowo.filter.FilterFacets
import pl.kinowo.filter.DateFilter
import androidx.compose.ui.unit.Dp
import androidx.compose.foundation.lazy.grid.LazyGridState
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.TopBarLayout
import pl.kinowo.ui.common.labelText
import pl.kinowo.ui.common.viewportWidthDp
import dev.chrisbanes.haze.HazeState
import dev.chrisbanes.haze.blur.hazeBlur
import dev.chrisbanes.haze.hazeSource

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ListScreen(viewModel: KinowoViewModel, onOpenFilm: (String) -> Unit) {
    val films by viewModel.films.collectAsState()
    val isLoading by viewModel.isLoading.collectAsState()
    val error by viewModel.error.collectAsState()
    // Observed here (not read off the StateFlow inside the VM) so hiding a film
    // or unticking a cinema recomposes the grid — see filmsFor's comment.
    val hidden by viewModel.hiddenFilms.collectAsState()
    // The one cinema axis: an exclusion set the Filtry sheet's "Kina" section
    // writes. `catalog` carries the city's area grouping, which that section
    // renders as groups on a split city (e.g. London) and the first-visit area
    // dialog below reads.
    val catalog by viewModel.catalog.collectAsState()
    val disabledCinemas by viewModel.disabledCinemas.collectAsState()
    val selectedCity by viewModel.selectedCity.collectAsState()
    val areaPickerSeen by viewModel.areaPickerSeenCities.collectAsState()
    // Scopes the Filtry icon's "any filter on?" check to this city — the
    // excluded set is global across cities.
    val cityCinemas = remember(films) { FilterFacets.allCinemas(films) }

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

    // Backdrop captured by `Modifier.hazeSource` (the grid) and sampled by the
    // floating search pill's `hazeBlur` to blur whatever scrolls under it.
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
    val dateLabel = viewModel.dateFilter.labelText()
    LaunchedEffect(viewModel.dateFilter) {
        if (!seenFirstDay) {
            seenFirstDay = true
            return@LaunchedEffect
        }
        tabLabel = dateLabel
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
                filtersActive = viewModel.filtersActive(cityCinemas),
                search = viewModel.search,
                onSearch = { viewModel.search = it },
                onSelect = { day -> selectDayKeepingTopFlat(sharedScroll) { viewModel.dateFilter = day } },
                onOpenFilters = { showFilters = true },
            )

            // Nothing sits between the date bar and the grid: the cinema filter
            // lives in the Filtry sheet ("Kina"), not in a pulldown bar here.

            // First visit to a split city: ask which areas to show (all
            // pre-selected). Shown once per city; no-op on flat cities.
            val city = selectedCity
            if (catalog.isSplit && city != null && city !in areaPickerSeen) {
                AreaPickerDialog(
                    catalog = catalog,
                    onConfirm = { kept -> viewModel.completeAreaPicker(city, kept) },
                )
            }

            // ── content ───────────────────────────────────────────────────────
            // The search field floats over the grid as a bottom capsule
            // (mirrors the iOS `SearchBar`) rather than taking its own row.
            // The grid is the haze source; the pill samples it through
            // `hazeBlur`, so the cards blur and shift as they scroll under it.
            // Bottom content padding keeps the last row clear of the pill.
            Box(Modifier.fillMaxSize()) {
                // No floating pill on wide screens (search is inline up top), so the
                // grid only needs to clear it on narrow ones.
                val gridBottomInset = if (wide) 12.dp else SearchBarBottomInset
                Box(Modifier.fillMaxSize().hazeSource(hazeState)) {
                    when {
                        isLoading && films.isEmpty() -> LoadingState(stringResource(R.string.loading_repertoire))
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
                            DayPages(
                                viewModel = viewModel, films = films, hidden = hidden,
                                disabledCinemas = disabledCinemas, sharedScroll = sharedScroll,
                                onCommitDay = onCommitDay, onPreviewDay = { previewDay = it },
                                gridBottomInset = gridBottomInset, onOpenFilm = onOpenFilm,
                            )
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

/** A finger-following carousel: the selected day's grid is centred; dragging horizontally
 *  reveals the wrap-around previous/next day and commits it on release. The revealed neighbour
 *  mirrors the centre's vertical scroll during the drag. */
@Composable
private fun DayPages(
    viewModel: KinowoViewModel,
    films: List<Film>,
    hidden: Set<String>,
    disabledCinemas: Set<String>,
    sharedScroll: LazyGridState,
    onCommitDay: (DateFilter) -> Unit,
    onPreviewDay: (DateFilter) -> Unit,
    gridBottomInset: Dp,
    onOpenFilm: (String) -> Unit,
) {
    DayCarousel(
        current = viewModel.dateFilter,
        sharedScroll = sharedScroll,
        onCommitDay = onCommitDay,
        onPreviewDay = onPreviewDay,
    ) { day, state, columnModifier ->
        val visible = viewModel.filmsFor(day, films, hidden, disabledCinemas)
        // The mirror lands the new day at the SAME scroll the user was at; ScrollToTopOnChange
        // then eases that up to the top — so a swipe reads as "land where I was, then roll to
        // the top". An at-top switch is pinned flat by selectDayKeepingTopFlat, so this is a
        // no-op there and doesn't jump.
        if (day == viewModel.dateFilter) ScrollToTopOnChange(state, viewModel.dateFilter)
        // The per-card cinema label is always shown on the main listing — even when the filter
        // narrows to one cinema. Only the grouped-by-cinema section view (CinemaGrid) suppresses
        // it, since its section header already names the cinema. While a search shows every day,
        // the "All" page draws the days the search brought in lighter.
        CompositionLocalProvider(LocalOutsideDays provides viewModel.outsideDays(day)) {
            FilmsGrid(
                films = visible,
                state = state,
                bottomInset = gridBottomInset,
                showCinemaHeaders = true,
                onOpen = onOpenFilm,
                onHide = { viewModel.hide(it) },
                modifier = columnModifier,
            )
        }
    }
}
