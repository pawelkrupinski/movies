import Foundation

/// Horizontal-swipe day navigation math, factored out of the view so it's
/// unit-testable. A swipe left advances to the next day (`delta = +1`), a
/// swipe right steps back (`delta = -1`); both wrap around the ordered list
/// the date pills show (with "Wszystkie"/all included as one of the entries).
///
/// `current` is the index of the selected pill in the ordered list, `delta`
/// the signed step, `count` the number of entries. The result is always a
/// valid index in `0..<count` thanks to the Euclidean modulo, so the cycle
/// is seamless in both directions (stepping right off index 0 lands on the
/// last entry, stepping left off the last lands on 0). Returns `current`
/// unchanged when the list is empty.
func wrappedDayIndex(current: Int, delta: Int, count: Int) -> Int {
    guard count > 0 else { return current }
    let raw = (current + delta) % count
    return raw >= 0 ? raw : raw + count
}
