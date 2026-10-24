import Foundation

/// A search's day. A search looks through EVERY day: the first letter typed moves the listing to
/// "All" and remembers the day it was on; clearing the search goes back there -- unless a day was
/// picked while it ran (a tap, a swipe), which then stays. While the search shows every day, the
/// days it brought in are drawn lighter (`outsideDays`). The web's twin is `onSearchInput` in
/// shared.js, Android's `SearchDay.kt`.
struct SearchDay: Equatable, Sendable {
    /// The day the listing was on when the search began; nil while no search runs.
    private(set) var origin: DateFilter?
    /// A day was picked while the search ran.
    private(set) var picked = false

    /// The search box now reads `query` while the listing shows `day`: the day to move the
    /// listing to, or nil to leave it where it is.
    mutating func queryChanged(to query: String, showing day: DateFilter) -> DateFilter? {
        let searching = !query.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
        if searching, origin == nil {
            origin = day
            picked = false
            return day == .anytime ? nil : .anytime
        }
        if !searching, let from = origin {
            let back = picked ? nil : from
            origin = nil
            picked = false
            return back == day ? nil : back
        }
        return nil
    }

    /// The person moved the day (not `queryChanged`'s answer): mid-search, that day stays.
    mutating func dayPicked() {
        if origin != nil { picked = true }
    }

    /// The day the search began from, while nothing has been picked since; else nil.
    var outsideOf: DateFilter? { picked ? nil : origin }

    /// The days to draw lighter while the listing shows every day: those outside the searched-from
    /// day, measured on the city's clock. Nil when none is.
    func outsideDays(now: Date, zone: TimeZone) -> OutsideDays? {
        outsideOf.map { OutsideDays(searchedFrom: $0, now: now, zone: zone) }
    }
}

/// The days a search brought in that the listing was not on ("YYYY-MM-DD", as `DayShowings.date`).
struct OutsideDays: Equatable, Sendable {
    let searchedFrom: DateFilter
    let now: Date
    let zone: TimeZone

    func contains(_ isoDate: String) -> Bool {
        !searchedFrom.matches(date: isoDate, now: now, zone: zone)
    }
}
