package pl.kinowo.ui.list

import android.content.res.Configuration
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.GridItemSpan
import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.selected
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.filter.CinemaSection
import pl.kinowo.model.Film
import pl.kinowo.ui.common.LocalCinemaHeaderStyle
import pl.kinowo.ui.common.PosterGridMetrics
import pl.kinowo.ui.common.PosterPrefetch
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.TextSecondary

// Vertical room the floating search pill occupies at the bottom of the
// grid — capsule height (~46dp) plus its 14dp margin plus breathing space.
// Grids carry it as bottom content padding so the last card row can scroll
// clear of the pill instead of sitting permanently behind it.
internal val SearchBarBottomInset = 84.dp

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
        EmptyState(stringResource(R.string.no_showings), modifier)
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
        EmptyState(stringResource(R.string.no_showings))
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

/** Test tag on the initial-load spinner so Robolectric can assert it renders. */
internal const val LoadingSpinnerTag = "loading-spinner"
internal const val FloatingSearchBarTag = "floating-search-bar"

// Centred spinner + label shown on the first load of a city, before any films
// arrive (mirrors the iOS `ListingLoadingView`). On later launches cached films render
// immediately, so this only surfaces when there's nothing to show yet.
@Composable
internal fun LoadingState(text: String) {
    Column(
        Modifier.fillMaxSize(),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center,
    ) {
        CircularProgressIndicator(color = Brand, modifier = Modifier.testTag(LoadingSpinnerTag))
        Text(text, color = TextSecondary, modifier = Modifier.padding(top = 12.dp))
    }
}

@Composable
internal fun EmptyState(text: String, modifier: Modifier = Modifier) {
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
        Icon(painterResource(R.drawable.ic_movie), contentDescription = null, tint = TextSecondary)
        Text(text, color = TextSecondary, modifier = Modifier.padding(top = 8.dp))
    }
}

@Composable
internal fun ErrorState(message: String, onRetry: () -> Unit) {
    Column(
        Modifier.fillMaxSize().padding(24.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center,
    ) {
        Text(stringResource(R.string.error_repertoire), fontWeight = FontWeight.SemiBold)
        Text(message, color = TextSecondary, fontSize = 12.sp, modifier = Modifier.padding(top = 4.dp))
        androidx.compose.material3.Button(onClick = onRetry, modifier = Modifier.padding(top = 12.dp)) {
            Text(stringResource(R.string.retry))
        }
    }
}
