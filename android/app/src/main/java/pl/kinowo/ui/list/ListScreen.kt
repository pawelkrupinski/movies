package pl.kinowo.ui.list

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
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
import androidx.compose.material3.FilterChip
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
import androidx.compose.runtime.setValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.snapshotFlow
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.take
import java.time.LocalDate
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.DateFilter
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.Divider
import pl.kinowo.ui.theme.TextSecondary

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ListScreen(vm: KinowoViewModel, onOpenFilm: (String) -> Unit) {
    val films by vm.films.collectAsState()
    val isLoading by vm.isLoading.collectAsState()
    val error by vm.error.collectAsState()

    val pager = rememberPagerState(pageCount = { 2 })
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

    Box(Modifier.fillMaxSize()) {
        // Edge-to-edge: keep the custom top chrome below the status bar and the
        // grid above the nav bar. (DetailScreen's Scaffold handles its own insets.)
        Column(Modifier.fillMaxSize().windowInsetsPadding(WindowInsets.systemBars)) {
            // ── top chrome ────────────────────────────────────────────────────
            Row(
                Modifier.fillMaxWidth().padding(start = 16.dp, end = 4.dp, top = 8.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Text("🎬 Kinowo", fontSize = 18.sp, fontWeight = FontWeight.Bold, modifier = Modifier.weight(1f))
                IconButton(onClick = { showFilters = true }) {
                    Icon(
                        Icons.Outlined.FilterList,
                        contentDescription = "Filtry",
                        tint = if (vm.filtersActive) Brand else TextSecondary,
                    )
                }
            }

            DateChips(vm)

            if (pager.currentPage == 1) {
                CinemaChips(vm, films)
            }

            // ── content ───────────────────────────────────────────────────────
            // The search field floats over the grid as a bottom capsule
            // (mirrors the iOS `SearchBar`) rather than taking its own row —
            // the grid scrolls visibly behind it, with bottom content padding
            // so the last row still clears the pill.
            Box(Modifier.fillMaxSize()) {
                when {
                    isLoading && films.isEmpty() -> CenteredMessage("Ładowanie repertuaru…")
                    error != null && films.isEmpty() -> ErrorState(error!!) { vm.reload() }
                    else -> HorizontalPager(state = pager, modifier = Modifier.fillMaxSize()) { page ->
                        PullToRefreshBox(isRefreshing = isLoading, onRefresh = { vm.reload() }) {
                            if (page == 0) {
                                FilmsGrid(vm.filmsForFilmsTab(films), onOpenFilm) { vm.hide(it) }
                            } else {
                                CinemaGrid(vm.cinemaSections(films), onOpenFilm) { vm.hide(it) }
                            }
                        }
                    }
                }

                FloatingSearchBar(
                    value = vm.search,
                    onValueChange = { vm.search = it },
                    modifier = Modifier.align(Alignment.BottomCenter).imePadding(),
                )
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
    modifier: Modifier = Modifier,
) {
    Surface(
        shape = RoundedCornerShape(28.dp),
        // Translucent fill so the grid scrolls visibly behind the pill —
        // the closest Android gets to the iOS frosted-glass look without a
        // backdrop-blur RenderEffect. Low alpha on the dark card tint, with
        // a slightly stronger border so the capsule edge still reads against
        // busy posters.
        color = CardElevated.copy(alpha = 0.55f),
        border = BorderStroke(1.dp, Divider.copy(alpha = 0.85f)),
        shadowElevation = 8.dp,
        modifier = modifier
            .padding(horizontal = 24.dp, vertical = 14.dp)
            .fillMaxWidth(),
    ) {
        Row(
            Modifier.padding(horizontal = 18.dp, vertical = 13.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Icon(Icons.Filled.Search, contentDescription = null, tint = TextSecondary, modifier = Modifier.size(20.dp))
            Spacer(Modifier.width(10.dp))
            Box(Modifier.weight(1f)) {
                if (value.isEmpty()) {
                    Text("Szukaj filmu", color = TextSecondary)
                }
                BasicTextField(
                    value = value,
                    onValueChange = onValueChange,
                    singleLine = true,
                    textStyle = LocalTextStyle.current.copy(color = Color.White),
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
                    modifier = Modifier.size(20.dp).clickable { onValueChange("") },
                )
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun DateChips(vm: KinowoViewModel) {
    Row(
        Modifier.fillMaxWidth().padding(horizontal = 12.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        for (preset in DateFilter.presets) {
            val selected = vm.dateFilter == preset
            FilterChip(
                selected = selected,
                onClick = { vm.dateFilter = preset },
                label = { Text(preset.label) },
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class, androidx.compose.foundation.layout.ExperimentalLayoutApi::class)
@Composable
private fun CinemaChips(vm: KinowoViewModel, films: List<Film>) {
    val cinemas = remember(films) { vm.allCinemas(films) }
    androidx.compose.foundation.layout.FlowRow(
        Modifier.fillMaxWidth().padding(horizontal = 12.dp, vertical = 4.dp),
        horizontalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        for (cinema in cinemas) {
            FilterChip(
                selected = vm.pinnedCinema == cinema,
                onClick = { vm.pinnedCinema = if (vm.pinnedCinema == cinema) null else cinema },
                label = { Text(CinemaSection.pillName(cinema)) },
            )
        }
    }
}

// Vertical room the floating search pill occupies at the bottom of the
// grid — capsule height (~46dp) plus its 14dp margin plus breathing space.
// Grids carry it as bottom content padding so the last card row can scroll
// clear of the pill instead of sitting permanently behind it.
private val SearchBarBottomInset = 84.dp

@Composable
private fun FilmsGrid(films: List<Film>, onOpen: (String) -> Unit, onHide: (String) -> Unit) {
    if (films.isEmpty()) {
        EmptyState("Brak repertuaru.")
        return
    }
    LazyVerticalGrid(
        columns = GridCells.Adaptive(minSize = 170.dp),
        state = rememberLazyGridState(),
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = SearchBarBottomInset),
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
private fun CinemaGrid(sections: List<CinemaSection>, onOpen: (String) -> Unit, onHide: (String) -> Unit) {
    if (sections.isEmpty()) {
        EmptyState("Brak repertuaru.")
        return
    }
    LazyVerticalGrid(
        columns = GridCells.Adaptive(minSize = 170.dp),
        contentPadding = PaddingValues(start = 12.dp, top = 12.dp, end = 12.dp, bottom = SearchBarBottomInset),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
        modifier = Modifier.fillMaxSize(),
    ) {
        for (section in sections) {
            item(span = { GridItemSpan(maxLineSpan) }, key = "h_${section.cinema}") {
                Text(
                    text = CinemaSection.pillName(section.cinema),
                    color = CinemaBlue,
                    fontSize = 15.sp,
                    fontWeight = FontWeight.SemiBold,
                    modifier = Modifier.padding(top = 8.dp, bottom = 2.dp),
                )
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
