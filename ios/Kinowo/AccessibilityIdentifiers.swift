import Foundation

/// Canonical catalogue of accessibility identifiers the UI tests look up.
/// Every identifier below is attached to its view in production, and the
/// tests address elements only this way. The label-text fallbacks they used
/// to carry are gone on purpose: the UI is localized (pl / en / de), so a
/// Polish literal would only ever match a Polish-locale simulator. Anything
/// a test needs to find gets an identifier here rather than a label match.
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

    /// The long-press context menu on a card's poster. Both entries are
    /// translated, so tests must address them by identifier rather than label.
    enum FilmCard {
        static let share    = "filmcard.share"
        static let copyLink = "filmcard.copyLink"
        /// The age-rating certificate badge, present only for a film the
        /// server sent an `ageRating` for.
        static let ageRating = "filmcard.ageRating"
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
        /// One pill in the external-links row under the rating badges. Shared
        /// by every pill so a test can count the row; the cinema names also
        /// appear as links further down in the showings list, which is why the
        /// row can't be addressed by label.
        static let cinemaLink  = "filmdetail.cinemaLink"
    }

    /// The Filtry sheet's "Kina" section — the cinema filter.
    enum CinemaFilter {
        /// The "all cinemas" master toggle.
        static let all = "filters.cinema.all"
        /// Prefix; a per-cinema checkbox is `"filters.cinema.\(cinema)"`.
        static let cinemaPrefix = "filters.cinema"
        /// Prefix; a split city's area (de)select toggle is
        /// `"filters.cinema.area.toggle.\(slug)"`.
        static let areaTogglePrefix = "filters.cinema.area.toggle"
        /// Prefix; a split city's area fold header is
        /// `"filters.cinema.area.header.\(slug)"`.
        static let areaHeaderPrefix = "filters.cinema.area.header"
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
