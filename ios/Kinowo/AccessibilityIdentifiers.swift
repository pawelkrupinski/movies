import Foundation

/// Canonical catalogue of accessibility identifiers the UI tests look up.
/// Production views currently do not set these — XCUITests fall back to
/// Polish label text via the per-test `*Element(_:)` helpers. As the
/// wiring agent (or app dev) attaches `.accessibilityIdentifier(...)`
/// to the matching views, the identifier path becomes the preferred
/// lookup and the label fallback can be retired.
enum A11y {
    enum TopBar {
        static let filtryButton     = "topbar.filtry"
        static let datePillToday    = "topbar.date.today"
        static let datePillTomorrow = "topbar.date.tomorrow"
        static let datePillWeek     = "topbar.date.week"
        static let datePillAnytime  = "topbar.date.anytime"
    }

    enum Search {
        static let field = "search.field"
    }

    enum FilmGrid {
        static let cell = "filmgrid.cell"
    }

    enum FiltersSheet {
        static let root            = "filters.sheet"
        static let cinemaSection   = "filters.cinema"
        static let dimensionSection = "filters.dimension"
        static let versionSection  = "filters.version"
        static let imaxToggle      = "filters.imax"
        static let fromHourSection = "filters.fromHour"
        static let clearButton     = "filters.clear"
        static let doneButton      = "filters.done"
    }

    enum CinemaPage {
        static let sectionHeader = "cinema.section.header"
        static let pill          = "cinema.pill"
    }

    enum TabOverlay {
        static let filmy     = "tab.label.filmy"
        static let kina      = "tab.label.kina"
    }

    enum SwipeHint {
        static let overlay = "swipe.hint"
    }

    enum EmptyState {
        static let repertoire = "empty.repertoire"
        static let error      = "error.repertoire"
    }
}
