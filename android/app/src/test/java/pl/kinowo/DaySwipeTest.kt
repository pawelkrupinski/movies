package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.filter.previewDayIndex
import pl.kinowo.filter.wrappedDayIndex

class DaySwipeTest {

    // The day list is [Dziś, Jutro, 7 dni, Wszystkie] → four stops.
    private val count = 4

    @Test
    fun nextStepsForward() {
        assertEquals(1, wrappedDayIndex(current = 0, delta = +1, count = count))
        assertEquals(2, wrappedDayIndex(current = 1, delta = +1, count = count))
        assertEquals(3, wrappedDayIndex(current = 2, delta = +1, count = count))
    }

    @Test
    fun previousStepsBackward() {
        assertEquals(2, wrappedDayIndex(current = 3, delta = -1, count = count))
        assertEquals(0, wrappedDayIndex(current = 1, delta = -1, count = count))
    }

    @Test
    fun wrapsPastTheEnd() {
        // Swiping next from the last stop (Wszystkie) lands back on the first.
        assertEquals(0, wrappedDayIndex(current = 3, delta = +1, count = count))
    }

    @Test
    fun wrapsBeforeTheStart() {
        // Swiping previous from the first stop (Dziś) lands on the last.
        assertEquals(3, wrappedDayIndex(current = 0, delta = -1, count = count))
    }

    @Test
    fun degenerateCounts() {
        // Single stop: every swipe stays put. Empty/negative: no movement.
        assertEquals(0, wrappedDayIndex(current = 0, delta = +1, count = 1))
        assertEquals(0, wrappedDayIndex(current = 0, delta = -1, count = 1))
        assertEquals(5, wrappedDayIndex(current = 5, delta = +1, count = 0))
    }

    // The pill highlight commits at the same fraction of the width as a release.
    private val commitFraction = 0.25f
    private val width = 1000f

    @Test
    fun previewStaysOnCurrentBelowThreshold() {
        // A drag short of 25% of the width must not flip the highlighted pill,
        // in either direction.
        assertEquals(1, previewDayIndex(dragOffsetPx = -200f, widthPx = width, currentIndex = 1, count = count, commitFraction = commitFraction))
        assertEquals(1, previewDayIndex(dragOffsetPx = 200f, widthPx = width, currentIndex = 1, count = count, commitFraction = commitFraction))
        // Exactly at the threshold still counts as "not yet committed".
        assertEquals(1, previewDayIndex(dragOffsetPx = -250f, widthPx = width, currentIndex = 1, count = count, commitFraction = commitFraction))
    }

    @Test
    fun previewFlipsToNextPastThresholdDraggingLeft() {
        // dragOffset is NEGATIVE when dragging left = next day → +1 step.
        assertEquals(2, previewDayIndex(dragOffsetPx = -300f, widthPx = width, currentIndex = 1, count = count, commitFraction = commitFraction))
    }

    @Test
    fun previewFlipsToPreviousPastThresholdDraggingRight() {
        // dragOffset is POSITIVE when dragging right = previous day → -1 step.
        assertEquals(0, previewDayIndex(dragOffsetPx = 300f, widthPx = width, currentIndex = 1, count = count, commitFraction = commitFraction))
    }

    @Test
    fun previewWrapsDraggingLeftFromLastStop() {
        // From the last stop (Wszystkie), a committing drag-left wraps to the first.
        assertEquals(0, previewDayIndex(dragOffsetPx = -300f, widthPx = width, currentIndex = 3, count = count, commitFraction = commitFraction))
    }

    @Test
    fun previewWrapsDraggingRightFromFirstStop() {
        // From the first stop (Dziś), a committing drag-right wraps to the last.
        assertEquals(3, previewDayIndex(dragOffsetPx = 300f, widthPx = width, currentIndex = 0, count = count, commitFraction = commitFraction))
    }

    @Test
    fun previewStaysOnCurrentWhenWidthUnknown() {
        // Before the carousel has measured a width, there's nothing to commit
        // against — the highlight stays on the current day.
        assertEquals(1, previewDayIndex(dragOffsetPx = -900f, widthPx = 0f, currentIndex = 1, count = count, commitFraction = commitFraction))
    }
}
