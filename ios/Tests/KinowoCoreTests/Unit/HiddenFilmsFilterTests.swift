import XCTest
@testable import KinowoCore

final class HiddenFilmsFilterTests: XCTestCase {

    private let films: Set<String> = [
        "Diuna: Część druga",
        "Oppenheimer",
        "Barbie",
        "Anatomia upadku",
        "diuna",
    ]

    func testEmptyQueryReturnsEverythingSortedCaseInsensitively() {
        let result = HiddenFilmsFilter.visibleTitles(in: films, query: "")
        XCTAssertEqual(result, [
            "Anatomia upadku",
            "Barbie",
            "diuna",
            "Diuna: Część druga",
            "Oppenheimer",
        ])
    }

    func testWhitespaceOnlyQueryKeepsEverything() {
        let result = HiddenFilmsFilter.visibleTitles(in: films, query: "   ")
        XCTAssertEqual(result.count, films.count)
    }

    func testQueryMatchesAsCaseInsensitiveSubstring() {
        let result = HiddenFilmsFilter.visibleTitles(in: films, query: "diu")
        XCTAssertEqual(result, ["diuna", "Diuna: Część druga"])
    }

    func testQueryIsTrimmedBeforeMatching() {
        let result = HiddenFilmsFilter.visibleTitles(in: films, query: "  barbie  ")
        XCTAssertEqual(result, ["Barbie"])
    }

    func testNoMatchReturnsEmpty() {
        XCTAssertTrue(HiddenFilmsFilter.visibleTitles(in: films, query: "zzz").isEmpty)
    }
}
