package pl.kinowo.ui.list

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.tween
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.requiredWidth
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.wrapContentWidth
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
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.input.pointer.PointerEventPass
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.input.pointer.positionChange
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntOffset
import kotlin.math.abs
import kotlinx.coroutines.launch
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.previewDayIndex
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
 * @param onPreviewDay called continuously during a drag with the day a release
 *   would commit to RIGHT NOW — the current day until the drag crosses
 *   [CommitFraction], the neighbour past it, flipping back if the drag retreats.
 *   Lets the caller live-highlight the day pill without touching the selection,
 *   grid, or scroll. Also fired on drag end/abort with the resolved day so the
 *   highlight never stays stale. Defaults to a no-op for callers that don't
 *   surface a preview.
 * @param dayColumn renders one day's grid: given its preset, the [LazyGridState]
 *   to drive it, and a [Modifier] to apply, it lays out a full-width column.
 */
@Composable
fun DayCarousel(
    current: DateFilter,
    sharedScroll: LazyGridState,
    onCommitDay: (DateFilter) -> Unit,
    modifier: Modifier = Modifier,
    onPreviewDay: (DateFilter) -> Unit = {},
    dayColumn: @Composable (day: DateFilter, state: LazyGridState, modifier: Modifier) -> Unit,
) {
    val presets = DateFilter.presets
    val currentIndex = presets.indexOf(current).coerceAtLeast(0)
    val previous = presets[wrappedDayIndex(currentIndex, -1, presets.size)]
    val next = presets[wrappedDayIndex(currentIndex, +1, presets.size)]

    // Independent scroll states for the revealed neighbours; mirrored off the
    // centre's [sharedScroll] during a drag so all three sit at the same offset.
    val previousScroll = rememberLazyGridState()
    val nextScroll = rememberLazyGridState()

    // Drag offset in px: 0 = centred, negative = dragging left (next revealed
    // from the right edge), positive = dragging right (previous revealed).
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
    LaunchedEffect(sharedScroll, previousScroll, nextScroll) {
        snapshotFlow {
            Triple(dragging, sharedScroll.firstVisibleItemIndex, sharedScroll.firstVisibleItemScrollOffset)
        }.collect { (active, index, offset) ->
            if (!active) return@collect
            previousScroll.scrollToItem(index, offset)
            nextScroll.scrollToItem(index, offset)
        }
    }

    // Slide the strip to reveal [day] fully ([target] = ∓width), flip the
    // selected day, then recentre WITHOUT animating (the new current is now the
    // centre column). Guarded by [isSettling] so no drag can race the swap.
    fun commit(day: DateFilter, target: Float) {
        isSettling = true
        // Land the pill highlight on the committed day immediately, so it never
        // lingers on a stale value while the slide animation plays out and so it
        // matches the selection the upstream onCommitDay sets (no flicker).
        onPreviewDay(day)
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
            .pointerInput(currentIndex, containerWidthPx) {
                if (containerWidthPx == 0) return@pointerInput
                val width = containerWidthPx.toFloat()
                // Axis-lock the gesture. The day grid is an inner vertical scroller;
                // by default it wins the gesture in the main pass, so a horizontal
                // day-swipe with any vertical drift scrolled the list instead of
                // changing the day (worst at the top, where an up-drift reveals
                // rows). We arbitrate in the INITIAL pass, which the parent sees
                // before the grid: once the drag crosses the touch slop, a
                // horizontal-dominant gesture is CLAIMED for the day-swipe
                // (consumed, so the grid never scrolls) and a vertical-dominant one
                // is left untouched for the grid to scroll.
                awaitEachGesture {
                    val down = awaitFirstDown(requireUnconsumed = false)
                    if (isSettling) return@awaitEachGesture
                    val slop = viewConfiguration.touchSlop
                    var dx = 0f
                    var dy = 0f
                    var locked = false

                    // Slide the strip by [amount] px and live-preview the day a
                    // release would land on (current until the drag crosses
                    // CommitFraction, the neighbour past it).
                    fun drag(amount: Float) {
                        val offset = dragOffset.value + amount
                        scope.launch { dragOffset.snapTo(offset) }
                        onPreviewDay(
                            presets[
                                previewDayIndex(
                                    dragOffsetPx = offset,
                                    widthPx = width,
                                    currentIndex = currentIndex,
                                    count = presets.size,
                                    commitFraction = CommitFraction,
                                )
                            ],
                        )
                    }

                    while (true) {
                        val event = awaitPointerEvent(PointerEventPass.Initial)
                        val change = event.changes.firstOrNull { it.id == down.id } ?: break
                        if (!change.pressed) break
                        if (!locked) {
                            dx += change.positionChange().x
                            dy += change.positionChange().y
                            if (abs(dx) < slop && abs(dy) < slop) continue
                            // Vertical-dominant → leave it for the grid to scroll.
                            if (abs(dx) <= abs(dy)) return@awaitEachGesture
                            // Horizontal-dominant → claim the day-swipe.
                            locked = true
                            dragging = true
                            scope.launch { dragOffset.stop() }
                            change.consume()
                            drag(dx)
                        } else {
                            val amount = change.positionChange().x
                            change.consume()
                            if (amount != 0f) drag(amount)
                        }
                    }

                    if (locked) {
                        dragging = false
                        when {
                            dragOffset.value <= -width * CommitFraction -> commit(next, -width)
                            dragOffset.value >= width * CommitFraction -> commit(previous, width)
                            // Abort: ease back to the current day, scroll untouched,
                            // and snap the pill highlight back to the current day.
                            else -> {
                                onPreviewDay(current)
                                scope.launch { dragOffset.animateTo(0f, tween(SettleDurationMs)) }
                            }
                        }
                    }
                }
            },
    ) {
        // Render the three-column strip (current centred at rest) only once the
        // width is known, so each column can be pinned to the container width.
        if (containerWidthPx > 0) {
            val widthDp: Dp = with(LocalDensity.current) { containerWidthPx.toDp() }
            // The strip is three container-widths wide and slid one width left so
            // the centre column sits in view. Pin the Row's width to 3× explicitly:
            // the parent Box is only ONE width wide, so without this the Row inherits
            // a single-width max constraint and hands the first (previous) column the
            // whole budget, leaving the centre + next columns measured at WIDTH 0 —
            // the centre grid then composes nothing at rest and cards only appear
            // mid-drag when the offset drags a neighbour in. The Box's clipToBounds
            // hides the overflow; the offset places the centre at [0, width].
            Row(
                Modifier
                    // Let the strip grow to three container-widths even though the
                    // parent Box is only one wide, and ANCHOR the overflow at the
                    // start edge (the default would CENTRE the overflow, shifting the
                    // whole strip left by one width). Without unbounded width the Row
                    // inherits a single-width max and the first column eats the whole
                    // budget, leaving the centre + next columns measured at WIDTH 0 —
                    // the centre grid then composes nothing at rest and cards only
                    // appear mid-drag as a neighbour slides in.
                    .wrapContentWidth(align = Alignment.Start, unbounded = true)
                    .requiredWidth(widthDp * 3)
                    .fillMaxHeight()
                    .offset { IntOffset(x = (-containerWidthPx + dragOffset.value).toInt(), y = 0) },
            ) {
                dayColumn(previous, previousScroll, Modifier.width(widthDp).fillMaxHeight())
                dayColumn(current, sharedScroll, Modifier.width(widthDp).fillMaxHeight())
                dayColumn(next, nextScroll, Modifier.width(widthDp).fillMaxHeight())
            }
        }
    }
}
