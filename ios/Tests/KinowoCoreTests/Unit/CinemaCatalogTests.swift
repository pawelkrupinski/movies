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
}
