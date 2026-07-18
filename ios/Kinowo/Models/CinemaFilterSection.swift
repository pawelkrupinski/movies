import Foundation

/// The Filtry sheet's "Kina" section as pure, view-free data: which boxes read
/// ticked, and what `disabledCinemas` becomes when one is toggled.
///
/// There is ONE cinema filter axis — the `disabledCinemas` exclusion set, shared
/// with the web's `localStorage` (it round-trips through `StateSyncService` and
/// the `?cinema=` deep link). A **flat** city renders `cityCinemas` as a single
/// checkbox list; a **split** city (e.g. London) renders that same universe
/// grouped into `catalog.areas`. Both mutate the one set, mirroring the web's
/// Filtry cinema panel (`buildCinemaPanel` in `shared.js`).
///
/// Pure Foundation → lives in `KinowoCore` and is unit-tested under
/// `swift test`; `FiltersSheet` is a thin renderer over it, and Android's
/// `CinemaFilterSection` is the parallel implementation.
struct CinemaFilterSection {
    /// Tri-state for a checkbox covering one or more cinemas.
    enum Check: Equatable { case on, off, mixed }

    /// The current city's cinema universe + its optional area grouping.
    let catalog: CinemaCatalog
    /// The excluded set, GLOBAL across cities (see `UserPreferences`).
    let disabled: Set<String>

    var cityCinemas: [String] { catalog.cinemas }
    var isSplit: Bool { catalog.isSplit }

    /// How many of THIS city's cinemas are still shown — the count the section
    /// header reads out. Names belonging to other cities are ignored.
    var enabledCount: Int { cityCinemas.filter { !disabled.contains($0) }.count }

    /// The "all cinemas" master checkbox.
    var allCheck: Check { check(of: cityCinemas) }

    func check(ofCinema cinema: String) -> Check {
        disabled.contains(cinema) ? .off : .on
    }

    func check(ofArea area: CinemaArea) -> Check {
        check(of: area.cinemas)
    }

    func check(of cinemas: [String]) -> Check {
        let off = cinemas.reduce(0) { $0 + (disabled.contains($1) ? 1 : 0) }
        if off == 0 { return .on }
        if off == cinemas.count { return .off }
        return .mixed
    }

    /// The new excluded set after ticking/unticking one cinema.
    func setting(cinema: String, enabled: Bool) -> Set<String> {
        setting(cinemas: [cinema], enabled: enabled)
    }

    /// The new excluded set after ticking/unticking a whole area.
    func setting(area: CinemaArea, enabled: Bool) -> Set<String> {
        setting(cinemas: area.cinemas, enabled: enabled)
    }

    /// The new excluded set after the "all cinemas" master. Scoped to this
    /// city's universe so other cities' entries survive a select-all.
    func settingAll(enabled: Bool) -> Set<String> {
        setting(cinemas: cityCinemas, enabled: enabled)
    }

    /// Every mutator funnels here: add/remove exactly the named cinemas and
    /// leave the rest of the (cross-city) set alone.
    private func setting(cinemas: [String], enabled: Bool) -> Set<String> {
        var s = disabled
        if enabled { cinemas.forEach { s.remove($0) } }
        else       { cinemas.forEach { s.insert($0) } }
        return s
    }
}
