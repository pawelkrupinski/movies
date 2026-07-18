import XCTest
@testable import KinowoCore

/// The Filtry sheet's "Kina" section, as pure data. This is the single cinema
/// filter axis on both city shapes since the top-bar cinema bars were retired:
/// a flat city renders `cityCinemas` as one checkbox list, a split city renders
/// the same universe grouped into `catalog.areas`. Both mutate the one
/// `disabledCinemas` exclusion set, mirroring the web's Filtry cinema panel.
final class CinemaFilterSectionTests: XCTestCase {

    private let flat = CinemaCatalog(
        cinemas: ["Kino Muza", "Multikino Poznań", "Cinema City Plaza"],
        areas: []
    )

    private let split = CinemaCatalog(
        cinemas: ["Odeon Leicester Square", "Odeon Camden", "Curzon Soho", "Vue Westfield"],
        areas: [
            CinemaArea(name: "Central", slug: "central",
                       cinemas: ["Odeon Leicester Square", "Curzon Soho"]),
            CinemaArea(name: "North", slug: "north", cinemas: ["Odeon Camden"]),
            CinemaArea(name: "West", slug: "west", cinemas: ["Vue Westfield"]),
        ]
    )

    // MARK: - Shape

    func testFlatCityExposesEveryCinemaAndNoAreas() {
        let section = CinemaFilterSection(catalog: flat, disabled: [])
        XCTAssertFalse(section.isSplit)
        XCTAssertEqual(section.cityCinemas.count, 3)
        XCTAssertTrue(section.catalog.areas.isEmpty)
    }

    func testSplitCityGroupsTheSameUniverseIntoAreas() {
        let section = CinemaFilterSection(catalog: split, disabled: [])
        XCTAssertTrue(section.isSplit)
        XCTAssertEqual(
            Set(section.catalog.areas.flatMap(\.cinemas)),
            Set(section.cityCinemas),
            "areas must partition the city's cinema universe"
        )
    }

    // MARK: - Tri-state checks

    func testNothingExcludedReadsAsFullyOn() {
        let section = CinemaFilterSection(catalog: flat, disabled: [])
        XCTAssertEqual(section.allCheck, .on)
        XCTAssertEqual(section.enabledCount, 3)
    }

    func testEveryCinemaExcludedReadsAsOff() {
        let section = CinemaFilterSection(catalog: flat, disabled: Set(flat.cinemas))
        XCTAssertEqual(section.allCheck, .off)
        XCTAssertEqual(section.enabledCount, 0)
    }

    func testSomeExcludedReadsAsMixed() {
        let section = CinemaFilterSection(catalog: flat, disabled: ["Kino Muza"])
        XCTAssertEqual(section.allCheck, .mixed)
        XCTAssertEqual(section.enabledCount, 2)
        XCTAssertEqual(section.check(ofCinema: "Kino Muza"), .off)
        XCTAssertEqual(section.check(ofCinema: "Multikino Poznań"), .on)
    }

    func testAreaCheckIsMixedWhenOnlyPartOfItIsExcluded() {
        let section = CinemaFilterSection(catalog: split, disabled: ["Curzon Soho"])
        let central = split.areas[0]
        let north = split.areas[1]
        XCTAssertEqual(section.check(ofArea: central), .mixed)
        XCTAssertEqual(section.check(ofArea: north), .on)
        XCTAssertEqual(section.allCheck, .mixed)
    }

    // MARK: - Toggling

    func testUncheckingOneCinemaExcludesOnlyIt() {
        let section = CinemaFilterSection(catalog: flat, disabled: [])
        XCTAssertEqual(section.setting(cinema: "Kino Muza", enabled: false), ["Kino Muza"])
    }

    func testCheckingOneCinemaClearsItsExclusion() {
        let section = CinemaFilterSection(catalog: flat, disabled: ["Kino Muza", "Cinema City Plaza"])
        XCTAssertEqual(section.setting(cinema: "Kino Muza", enabled: true), ["Cinema City Plaza"])
    }

    func testUncheckingAnAreaExcludesEveryCinemaInIt() {
        let section = CinemaFilterSection(catalog: split, disabled: [])
        XCTAssertEqual(
            section.setting(area: split.areas[0], enabled: false),
            ["Odeon Leicester Square", "Curzon Soho"]
        )
    }

    func testCheckingAnAreaClearsOnlyItsCinemas() {
        let section = CinemaFilterSection(
            catalog: split,
            disabled: ["Odeon Leicester Square", "Curzon Soho", "Odeon Camden"]
        )
        XCTAssertEqual(section.setting(area: split.areas[0], enabled: true), ["Odeon Camden"])
    }

    func testTheAllMasterClearsAndFillsTheWholeCity() {
        let empty = CinemaFilterSection(catalog: flat, disabled: [])
        XCTAssertEqual(empty.settingAll(enabled: false), Set(flat.cinemas))

        let full = CinemaFilterSection(catalog: flat, disabled: Set(flat.cinemas))
        XCTAssertEqual(full.settingAll(enabled: true), [])
    }

    // MARK: - Cross-city safety

    // `disabledCinemas` is global across cities (it mirrors the web's
    // localStorage set and round-trips through StateSyncService), so every
    // mutator must leave names belonging to OTHER cities untouched — otherwise
    // switching city would silently re-enable everything the user hid there.
    func testMutatorsPreserveOtherCitiesEntries() {
        let foreign = "Kino Pod Baranami"   // Kraków — not in `flat`
        let section = CinemaFilterSection(catalog: flat, disabled: [foreign])

        XCTAssertTrue(section.setting(cinema: "Kino Muza", enabled: false).contains(foreign))
        XCTAssertTrue(section.settingAll(enabled: false).contains(foreign))
        XCTAssertTrue(section.settingAll(enabled: true).contains(foreign))

        let splitSection = CinemaFilterSection(catalog: split, disabled: [foreign])
        XCTAssertTrue(splitSection.setting(area: split.areas[0], enabled: false).contains(foreign))
        XCTAssertTrue(splitSection.setting(area: split.areas[0], enabled: true).contains(foreign))
    }

    // A stale name from another city must not tip the current city's checkboxes.
    func testForeignEntriesDontAffectTheCurrentCitysChecks() {
        let section = CinemaFilterSection(catalog: flat, disabled: ["Kino Pod Baranami"])
        XCTAssertEqual(section.allCheck, .on)
        XCTAssertEqual(section.enabledCount, 3)
    }
}
