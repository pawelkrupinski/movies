package pl.kinowo.filter

/**
 * Steps a position within the day list (the ordered [DateFilter.presets]) by
 * [delta], wrapping around both ends so the cycle is endless. A swipe LEFT maps
 * to `delta = +1` (next day), a swipe RIGHT to `delta = -1` (previous day); the
 * "Wszystkie" entry is just another stop in the cycle.
 *
 * Pure index math, kept separate from the Compose gesture so it can be unit
 * tested on the JVM. Returns [current] unchanged when [count] is non-positive.
 */
fun wrappedDayIndex(current: Int, delta: Int, count: Int): Int {
    if (count <= 0) return current
    return ((current + delta) % count + count) % count
}

/**
 * The day index the carousel WOULD commit to if the finger were released right
 * now, given the live [dragOffsetPx] (negative = dragging left = next day,
 * positive = dragging right = previous day — matching DayCarousel's commit
 * logic). Used to drive a live highlight PREVIEW on the day pills: the pill
 * flips the moment the drag passes the same distance threshold a release would
 * commit at, and flips back if the drag returns under it.
 *
 * Returns [currentIndex] unchanged while the drag is under
 * `widthPx * commitFraction` (or when [widthPx] is non-positive — nothing to
 * measure against); past the threshold it steps to the wrap-around neighbour the
 * drag direction points at. Pure index math, kept JVM-unit-testable alongside
 * [wrappedDayIndex].
 *
 * NOTE: this tracks the DISTANCE threshold only. A flick can commit a shorter
 * drag, but flick velocity is unknown mid-drag, so the preview deliberately
 * follows distance — see DayCarousel's CommitFraction.
 */
fun previewDayIndex(
    dragOffsetPx: Float,
    widthPx: Float,
    currentIndex: Int,
    count: Int,
    commitFraction: Float,
): Int {
    if (widthPx <= 0f) return currentIndex
    if (kotlin.math.abs(dragOffsetPx) <= widthPx * commitFraction) return currentIndex
    return wrappedDayIndex(currentIndex, if (dragOffsetPx < 0) +1 else -1, count)
}
