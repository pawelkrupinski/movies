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
        /// 1pt automation anchor pinned to the frosted bar's bottom edge, so
        /// tests can read the true bar bottom — the pills / Filtry button sit
        /// on the row *above* it, inside the bar's bottom padding.
        static let bottomEdge       = "topbar.bottom"
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

    /// Per-film detail screen.
    enum FilmDetail {
        /// The header poster — tap or long-press opens the full-screen viewer.
        static let poster      = "filmdetail.poster"
        /// The full-screen poster cover, present only while it's shown.
        static let fullScreen  = "filmdetail.poster.fullscreen"
        /// The close button on the full-screen cover (always an accessibility
        /// element, so it's the reliable existence probe for the cover).
        static let closeButton = "filmdetail.poster.close"
    }

    /// Slim expand-handle row + horizontally-scrolling cinema pills, sitting
    /// directly under the top bar.
    enum CinemaBar {
        /// The collapsed handle row (label + chevron); tap toggles the pills.
        static let handle = "cinemabar.handle"
        /// The leading "Wszystkie" pill (nil selection).
        static let allPill = "cinemabar.pill.all"
        /// Prefix; a per-cinema pill is `"cinemabar.pill.\(cinema)"`.
        static let pillPrefix = "cinemabar.pill"

        // ── Split-city area picker (multi-select) ──
        /// The collapsed handle row of the area picker.
        static let areaHandle = "cinemabar.area.handle"
        /// The "all cinemas" master toggle.
        static let areaAll = "cinemabar.area.all"
        /// Prefix; an area's (de)select toggle is `"cinemabar.area.toggle.\(slug)"`.
        static let areaTogglePrefix = "cinemabar.area.toggle"
        /// Prefix; an area's fold header is `"cinemabar.area.header.\(slug)"`.
        static let areaHeaderPrefix = "cinemabar.area.header"
        /// Prefix; a per-cinema checkbox is `"cinemabar.area.cinema.\(cinema)"`.
        static let areaCinemaPrefix = "cinemabar.area.cinema"
    }

    enum FiltersSheet {
        static let root            = "filters.sheet"
        static let dimensionSection = "filters.dimension"
        static let versionSection  = "filters.version"
        static let imaxToggle      = "filters.imax"
        static let fromHourSection = "filters.fromHour"
        static let clearButton     = "filters.clear"
        static let doneButton      = "filters.done"
    }

    enum CinemaPage {
        static let sectionHeader = "cinema.section.header"
    }

    enum DayOverlay {
        static let label = "day.label"
    }

    enum SwipeHint {
        static let overlay = "swipe.hint"
    }

    /// First-launch city gate.
    enum CityGate {
        /// The primary "show repertoire" button on the location-confirm screen,
        /// so a UITest can measure it renders at the enlarged (`.controlSize`
        /// `.large`) size rather than the compact system default.
        static let confirmButton = "citygate.confirm.button"
        /// The "choose a different city" button on the confirm screen — a UITest
        /// taps it to reach the manual `CityChoiceView` deterministically.
        static let chooseOtherButton = "citygate.chooseOther.button"
    }

    enum EmptyState {
        static let repertoire = "empty.repertoire"
        static let error      = "error.repertoire"
    }

    /// Non-prod tuning pager only — lets the tuning UITests find a card to
    /// measure and the sliders to drag on each page.
    enum Tuning {
        /// Prefix; the nth card is `"tuning.card.\(n)"`.
        static let cardPrefix          = "tuning.card"
        static let sectionSpacingSlider = "tuning.slider.section"
        static let showingsBlockSlider = "tuning.slider.showingsBlock"
        static let dayToCinemaSlider   = "tuning.slider.dayToCinema"
        /// The controls scroll-view, so the test swipes inside the sheet to
        /// reveal a slider rather than scrolling the cards behind it.
        static let controlsScroll      = "tuning.controls"
        /// The viewport/resolution readout, parked at the top of the scroll so
        /// it scrolls away instead of permanently eating header space.
        static let resolutionReadout   = "tuning.resolution"
        /// Kina page: cinema section-header font-size slider + the page itself.
        static let cinemaHeaderFontSlider = "tuning.slider.cinemaHeaderFont"
        /// Film page: detail title font-size slider + the rendered title.
        static let detailTitleFontSlider  = "tuning.slider.detailTitleFont"
        static let detailTitle             = "tuning.detail.title"
        /// Tab labels in the page bar — let a UITest jump to a page by tap
        /// instead of relying on a swipe distance.
        static let pageTabPrefix       = "tuning.page.tab"
    }
}
