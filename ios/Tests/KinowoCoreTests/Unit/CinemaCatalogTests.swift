import XCTest
@testable import KinowoCore

/// Decoding of `GET /:city/api/cinemas` into `CinemaCatalog` — the payload the
/// split-city area picker is built from. A split city carries areas that
/// partition its cinema list; a flat city carries an empty `areas`.
final class CinemaCatalogTests: XCTestCase {

    func testDecodesSplitCityAreas() throws {
        let json = """
        {"cinemas":["A Cinema","B Cinema","C Cinema"],
         "areas":[{"name":"Central","slug":"central","cinemas":["A Cinema"]},
                  {"name":"North","slug":"north","cinemas":["B Cinema","C Cinema"]}]}
        """.data(using: .utf8)!

        let catalog = try JSONDecoder().decode(CinemaCatalog.self, from: json)

        XCTAssertTrue(catalog.isSplit)
        XCTAssertEqual(catalog.cinemas.count, 3)
        XCTAssertEqual(catalog.areas.map(\.name), ["Central", "North"])
        XCTAssertEqual(catalog.areas.map(\.slug), ["central", "north"])
        XCTAssertEqual(catalog.areas[1].cinemas, ["B Cinema", "C Cinema"])
        XCTAssertEqual(catalog.areas[1].id, "north")   // Identifiable by slug
        // The areas partition the cinema universe.
        XCTAssertEqual(Set(catalog.areas.flatMap(\.cinemas)), Set(catalog.cinemas))
    }

    func testDecodesFlatCityWithEmptyAreas() throws {
        let json = """
        {"cinemas":["Only Cinema"],"areas":[]}
        """.data(using: .utf8)!

        let catalog = try JSONDecoder().decode(CinemaCatalog.self, from: json)

        XCTAssertFalse(catalog.isSplit)
        XCTAssertEqual(catalog.cinemas, ["Only Cinema"])
        XCTAssertTrue(catalog.areas.isEmpty)
    }

    /// A city with seasonal venues (open-air, drive-in) carries an extra `seasons` key the
    /// app does not read yet; the catalog must decode exactly as without it.
    func testDecodesACatalogCarryingSeasons() throws {
        let json = """
        {"cinemas":["Kino Central Berlin","Freiluftkino Kreuzberg"],"areas":[],
         "seasons":[{"cinema":"Freiluftkino Kreuzberg","opens":5,"closes":9,"offSeason":true}]}
        """.data(using: .utf8)!

        let catalog = try JSONDecoder().decode(CinemaCatalog.self, from: json)

        XCTAssertEqual(catalog.cinemas, ["Kino Central Berlin", "Freiluftkino Kreuzberg"])
        XCTAssertFalse(catalog.isSplit)
    }

    private let berlin = """
    {"cinemas":["Freiluftkino Kreuzberg","Kino Central Berlin","Freiluftkino Rehberge"],"areas":[],
     "seasons":[{"cinema":"Freiluftkino Kreuzberg","opens":5,"closes":9,"offSeason":true},
                {"cinema":"Freiluftkino Rehberge","opens":5,"closes":9,"offSeason":false}]}
    """.data(using: .utf8)!

    /// The server decides which venues are out of season; the filter sinks them to the bottom.
    func testOutOfSeasonCinemasSinkToTheBottomOnTheServersVerdict() throws {
        let catalog = try JSONDecoder().decode(CinemaCatalog.self, from: berlin)

        XCTAssertTrue(catalog.isOffSeason("Freiluftkino Kreuzberg"))
        XCTAssertFalse(catalog.isOffSeason("Freiluftkino Rehberge"))   // seasonal, in season
        XCTAssertFalse(catalog.isOffSeason("Kino Central Berlin"))
        XCTAssertEqual(catalog.seasonOrdered(catalog.cinemas),
                       ["Kino Central Berlin", "Freiluftkino Rehberge", "Freiluftkino Kreuzberg"])
    }

    /// The muted second line of an out-of-season row, in each UI language.
    func testBackInSeasonLineInEachLanguage() throws {
        let season = try XCTUnwrap(JSONDecoder().decode(CinemaCatalog.self, from: berlin).season(of: "Freiluftkino Kreuzberg"))

        XCTAssertEqual(season.backInSeasonLine(language: "en"), "back in season · May–Sept")
        XCTAssertEqual(season.backInSeasonLine(language: "de"), "öffnet wieder · Mai–Sept.")
        XCTAssertEqual(season.backInSeasonLine(language: "pl"), "wraca w sezonie · maj–wrz")
        XCTAssertEqual(season.backInSeasonLine(language: "es"), "vuelve en temporada · may–sept")
    }

    /// A month outside 1...12 from the server reads as the bare "back in season", never a crash.
    func testBackInSeasonLineWithAMonthOutOfRangeDropsTheWindow() {
        for (opens, closes) in [(0, 9), (5, 13), (-1, 12), (13, 0)] {
            let season = CinemaSeason(cinema: "Kino", opens: opens, closes: closes, offSeason: true)
            XCTAssertEqual(season.backInSeasonLine(language: "en"), "back in season", "\(opens)–\(closes)")
        }
    }

    /// A catalog from a server that predates seasons decodes with none, and nothing sinks.
    func testCatalogWithoutSeasonsHasNoneOutOfSeason() throws {
        let catalog = try JSONDecoder().decode(CinemaCatalog.self, from: #"{"cinemas":["A","B"],"areas":[]}"#.data(using: .utf8)!)
        XCTAssertNil(catalog.seasons)
        XCTAssertEqual(catalog.seasonOrdered(catalog.cinemas), ["A", "B"])
    }

    func testEmptyCatalogIsFlat() {
        XCTAssertFalse(CinemaCatalog.empty.isSplit)
        XCTAssertTrue(CinemaCatalog.empty.cinemas.isEmpty)
    }

    func testCinemasToDisableExcludesUncheckedAreas() {
        let catalog = CinemaCatalog(
            cinemas: ["A Cinema", "B Cinema", "C Cinema"],
            areas: [
                CinemaArea(name: "Central", slug: "central", cinemas: ["A Cinema"]),
                CinemaArea(name: "North", slug: "north", cinemas: ["B Cinema", "C Cinema"]),
            ]
        )
        // Keep only Central → North's cinemas are disabled.
        XCTAssertEqual(catalog.cinemasToDisable(keepingAreas: ["central"]), ["B Cinema", "C Cinema"])
        // Keep all → nothing disabled (the pre-selected default is a no-op).
        XCTAssertTrue(catalog.cinemasToDisable(keepingAreas: ["central", "north"]).isEmpty)
        // Keep none → everything disabled.
        XCTAssertEqual(Set(catalog.cinemasToDisable(keepingAreas: [])), Set(catalog.cinemas))
    }

    // ── AreaSelection — the picker's checkbox state ──────────────────────────

    private var threeAreas: [CinemaArea] {
        [CinemaArea(name: "San Francisco", slug: "san-francisco", cinemas: ["A"]),
         CinemaArea(name: "East Bay", slug: "east-bay", cinemas: ["B", "C"]),
         CinemaArea(name: "South Bay", slug: "south-bay", cinemas: ["D"])]
    }

    func testAreaSelectionStartsWithEveryAreaKept() {
        let selection = AreaSelection(areas: threeAreas)
        XCTAssertEqual(selection.kept, ["san-francisco", "east-bay", "south-bay"])
        XCTAssertTrue(selection.allKept)
        XCTAssertFalse(selection.partial)
    }

    func testAreaSelectionDeselectsAndSelectsEveryArea() {
        var selection = AreaSelection(areas: threeAreas)

        selection.setAll(false)
        XCTAssertTrue(selection.kept.isEmpty)
        XCTAssertFalse(selection.allKept)
        XCTAssertFalse(selection.partial)   // empty is not mixed

        selection.setAll(true)
        XCTAssertEqual(selection.kept, Set(selection.all))
        XCTAssertTrue(selection.allKept)
    }

    func testAreaSelectionIsPartialWhileSomeAreasAreDropped() {
        var selection = AreaSelection(areas: threeAreas)
        selection.toggle("east-bay")

        XCTAssertFalse(selection.isKept("east-bay"))
        XCTAssertFalse(selection.allKept)
        XCTAssertTrue(selection.partial)

        // Re-ticking the last one restores the full check.
        selection.toggle("east-bay")
        XCTAssertTrue(selection.allKept)
        XCTAssertFalse(selection.partial)
    }

    func testDeselectAllThenConfirmDisablesEveryCinema() {
        let catalog = CinemaCatalog(cinemas: ["A", "B", "C", "D"], areas: threeAreas)
        var selection = AreaSelection(areas: catalog.areas)
        selection.setAll(false)

        XCTAssertEqual(Set(catalog.cinemasToDisable(keepingAreas: selection.kept)), Set(catalog.cinemas))
    }
}
