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
