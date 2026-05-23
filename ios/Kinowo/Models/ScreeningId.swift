import Foundation

/// Canonical id for one screening — same shape the web app uses
/// (`title|cinema|YYYY-MM-DDTHH:MM`). Lives in the model layer so
/// pure-logic callers (`Sequence<Film>.filteredForFavourites`) can
/// reach it without importing the SwiftUI-flavoured Storage layer.
///
/// `UserPreferences.screeningId(...)` re-exposes this from the
/// Storage layer for view-side code that already imports it; both
/// call sites build the exact same string.
enum ScreeningId {
    static func make(title: String, cinema: String, date: String, time: String) -> String {
        "\(title)|\(cinema)|\(date)T\(time)"
    }
}
