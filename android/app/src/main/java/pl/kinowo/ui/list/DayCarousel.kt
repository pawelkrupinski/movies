package pl.kinowo.ui.list

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.tween
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.runtime.snapshotFlow
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntOffset
import kotlinx.coroutines.launch
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.wrappedDayIndex

// Fraction of the container width a drag must cross to commit a day change.
// Below it the gesture snaps back to the current day. Hand-tunable later.
private const val CommitFraction = 0.25f

// How quickly a committed swipe finishes sliding to the neighbour, and an
// aborted drag eases back to centre. Short so the day flips snappily.
private const val SettleDurationMs = 220

/**
 * A finger-following horizontal carousel over the four [DateFilter.presets]:
 * the centre column shows the selected day, the columns on either side reveal
 * the wrap-around previous / next day as the strip is dragged. Releasing past
 * [CommitFraction] of the width commits that neighbour as the new selected day
 * (via [onCommitDay]); a shorter drag eases back to the current day.
 *
 * Why hand-rolled and not a `HorizontalPager`: the revealed neighbour mirrors
 * the centre's VERTICAL scroll position during the drag, so the user lands on
 * the same place in the new day. A pager gives each page an independent
 * `LazyGridState` with no way to keep them in lock-step; here the centre owns
 * [sharedScroll] and the neighbours are slaved to it through a `snapshotFlow`.
 *
 * @param current the selected day; the strip is centred on it at rest.
 * @param sharedScroll the centre column's grid state — hoisted by the caller so
 *   [ScrollToTopOnChange] can animate it to the top after a committed day flip.
 * @param onCommitDay called once, with the newly-selected preset, the instant a
 *   swipe commits (after the slide animation, before the strip recentres).
 * @param dayColumn renders one day's grid: given its preset, the [LazyGridState]
 *   to drive it, and a [Modifier] to apply, it lays out a full-width column.
 */
@Composable
fun DayCarousel(
    current: DateFilter,
    sharedScroll: LazyGridState,
    onCommitDay: (DateFilter) -> Unit,
    modifier: Modifier = Modifier,
    dayColumn: @Composable (day: DateFilter, state: LazyGridState, modifier: Modifier) -> Unit,
) {
    val presets = DateFilter.presets
    val currentIdx = presets.indexOf(current).coerceAtLeast(0)
    val prev = presets[wrappedDayIndex(currentIdx, -1, presets.size)]
    val next = presets[wrappedDayIndex(currentIdx, +1, presets.size)]

    // Independent scroll states for the revealed neighbours; mirrored off the
    // centre's [sharedScroll] during a drag so all three sit at the same offset.
    val prevScroll = rememberLazyGridState()
    val nextScroll = rememberLazyGridState()

    // Drag offset in px: 0 = centred, negative = dragging left (next revealed
    // from the right edge), positive = dragging right (prev revealed).
    val dragOffset = remember { Animatable(0f) }
    var containerWidthPx by remember { mutableStateOf(0) }
    // Guards against starting a fresh drag while a commit/recentre is mid-flight
    // — interrupting a swap leaves the strip and the selected day out of sync.
    var isSettling by remember { mutableStateOf(false) }
    // True only while a finger drag is in progress; gates the scroll mirror so it
    // does NOT chase the post-commit scroll-to-top animation (which would fire a
    // forceRemeasure from inside that animation's layout pass and crash).
    var dragging by remember { mutableStateOf(false) }
    val scope = rememberCoroutineScope()

    // Slave each neighbour to the centre's vertical scroll while the finger is
    // down, so the revealed day shows the same rows the user was looking at.
    // INSTANT (scrollToItem, not animate) to track the finger frame-for-frame;
    // differing column heights are clamped silently by scrollToItem. Restricting
    // to an active drag keeps it off the centre's own programmatic scrolls.
    LaunchedEffect(sharedScroll, prevScroll, nextScroll) {
        snapshotFlow {
            Triple(dragging, sharedScroll.firstVisibleItemIndex, sharedScroll.firstVisibleItemScrollOffset)
        }.collect { (active, index, offset) ->
            if (!active) return@collect
            prevScroll.scrollToItem(index, offset)
            nextScroll.scrollToItem(index, offset)
        }
    }

    // Slide the strip to reveal [day] fully ([target] = ∓width), flip the
    // selected day, then recentre WITHOUT animating (the new current is now the
    // centre column). Guarded by [isSettling] so no drag can race the swap.
    fun commit(day: DateFilter, target: Float) {
        isSettling = true
        scope.launch {
            dragOffset.animateTo(target, tween(SettleDurationMs))
            onCommitDay(day)
            dragOffset.snapTo(0f)
            isSettling = false
        }
    }

    Box(
        modifier
            .fillMaxSize()
            .clipToBounds()
            .onSizeChanged { containerWidthPx = it.width }
            .pointerInput(currentIdx, containerWidthPx) {
                if (containerWidthPx == 0) return@pointerInput
                detectHorizontalDragGestures(
                    onDragStart = {
                        if (!isSettling) {
                            dragging = true
                            scope.launch { dragOffset.stop() }
                        }
                    },
                    onHorizontalDrag = { change, amount ->
                        if (isSettling) return@detectHorizontalDragGestures
                        change.consume()
                        scope.launch { dragOffset.snapTo(dragOffset.value + amount) }
                    },
                    onDragEnd = {
                        if (isSettling) return@detectHorizontalDragGestures
                        dragging = false
                        val width = containerWidthPx.toFloat()
                        when {
                            dragOffset.value <= -width * CommitFraction -> commit(next, -width)
                            dragOffset.value >= width * CommitFraction -> commit(prev, width)
                            // Abort: ease back to the current day, scroll untouched.
                            else -> scope.launch { dragOffset.animateTo(0f, tween(SettleDurationMs)) }
                        }
                    },
                )
            },
    ) {
        // Render the three-column strip (current centred at rest) only once the
        // width is known, so each column can be pinned to the container width.
        if (containerWidthPx > 0) {
            val widthDp: Dp = with(LocalDensity.current) { containerWidthPx.toDp() }
            Row(
                Modifier
                    .fillMaxHeight()
                    .offset { IntOffset(x = (-containerWidthPx + dragOffset.value).toInt(), y = 0) },
            ) {
                dayColumn(prev, prevScroll, Modifier.width(widthDp).fillMaxHeight())
                dayColumn(current, sharedScroll, Modifier.width(widthDp).fillMaxHeight())
                dayColumn(next, nextScroll, Modifier.width(widthDp).fillMaxHeight())
            }
        }
    }
}
