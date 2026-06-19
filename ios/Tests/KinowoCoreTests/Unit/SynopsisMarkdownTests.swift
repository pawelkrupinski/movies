import XCTest
@testable import KinowoCore

final class SynopsisMarkdownTests: XCTestCase {

    func testPreservesNewlinesAndStripsMarkers() {
        // .inlineOnlyPreservingWhitespace keeps the \n\n paragraph break and
        // drops the ** / * markers from the visible text.
        let a = SynopsisMarkdown.attributed("a **b** *c*\n\nd")
        XCTAssertEqual(String(a.characters), "a b c\n\nd")
    }

    func testAppliesBoldAndItalic() {
        let a = SynopsisMarkdown.attributed("zwykły **pogrubiony** i *kursywa*")
        var sawBold = false
        var sawItalic = false
        for run in a.runs {
            guard let intent = run.inlinePresentationIntent else { continue }
            if intent.contains(.stronglyEmphasized) { sawBold = true }
            if intent.contains(.emphasized) { sawItalic = true }
        }
        XCTAssertTrue(sawBold, "expected a bold run from **…**")
        XCTAssertTrue(sawItalic, "expected an italic run from *…*")
    }

    func testPlainTextIsUnchanged() {
        XCTAssertEqual(String(SynopsisMarkdown.attributed("zwykły opis bez znaczników").characters),
                       "zwykły opis bez znaczników")
    }
}
