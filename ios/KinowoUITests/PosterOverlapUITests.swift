import XCTest

/// Regression guard for "posters on the Filmy grid overlap each other"
/// (reported 2026-06-01 with screenshots). A wide/landscape poster source
/// image rendered its layout/hit/accessibility frame past its grid column
/// — `.aspectRatio(_, .fit)` let the `.fill` image size the box, and
/// `.clipped()` masked only the pixels, not the frame — so the card spilled
/// over its neighbour. The fix anchors the 2:3 poster box on a
/// zero-intrinsic-size `Color.clear` and reins the bounds back in with
/// `.contentShape` + `.accessibilityHidden` (see `FilmCardView.PosterView`).
///
/// In a `LazyVGrid` two cards in the same row legitimately share a y-range
/// (they sit side by side) but must NEVER share BOTH a y-range and an
/// x-range — a 2D frame intersection means one card is painting on top of
/// another. We scroll a dense grid and assert no two cells ever intersect.
final class PosterOverlapUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launch()
        // Wait on app launch (the date-pill row), NOT on a film card —
        // the default "Dziś" repertoire is empty late at night, so the
        // grid only fills once we switch to "Wszystkie" in the test.
        XCTAssertTrue(anytimePill().waitForExistence(timeout: 30),
                      "App never launched (no date pills)")
    }

    override func tearDownWithError() throws { app = nil }

    func testFilmCardsNeverOverlapWhileScrolling() throws {
        // "Wszystkie" (all dates) guarantees a dense, multi-row grid — the
        // condition the overlap was reported under. The default "Dziś" tab
        // is often a single card late in the evening, which exercises
        // nothing.
        anytimePill().tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30),
                      "Grid never filled after switching to Wszystkie")
        let grid = app.scrollViews.firstMatch
        XCTAssertTrue(grid.waitForExistence(timeout: 5), "No scroll view")

        for _ in 0..<10 {
            if let hit = firstOverlap(cellFrames()) {
                attachFailureDiagnostics(hit)
                let area = Int(hit.intersection.width * hit.intersection.height)
                XCTFail("""
                Two film cards overlap (2D frame intersection of \(area) pt²):
                  A = \(hit.a)
                  B = \(hit.b)
                A poster is painting over its neighbouring card.
                """)
                return
            }
            grid.swipeUp(velocity: .slow)
            Thread.sleep(forTimeInterval: 0.6) // let the lazy grid settle
        }
    }

    // MARK: - helpers

    private func cellFrames() -> [CGRect] {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .allElementsBoundByIndex
            .map { $0.frame }
            .filter { $0.width > 1 && $0.height > 1 }
    }

    /// First pair of visible cells whose frames intersect in BOTH axes,
    /// ignoring hairline (<2pt) touches from rounding.
    private func firstOverlap(_ frames: [CGRect]) -> (a: CGRect, b: CGRect, intersection: CGRect)? {
        for i in 0..<frames.count {
            for j in (i + 1)..<frames.count {
                let inter = frames[i].intersection(frames[j])
                if inter.width > 2 && inter.height > 2 {
                    return (frames[i], frames[j], inter)
                }
            }
        }
        return nil
    }

    /// On failure, capture the screen plus the frames of every text/image
    /// element straddling the offending card — that's how the original
    /// 248pt-wide rogue poster image was pinned down.
    private func attachFailureDiagnostics(_ hit: (a: CGRect, b: CGRect, intersection: CGRect)) {
        let shot = XCTAttachment(screenshot: XCUIScreen.main.screenshot())
        shot.name = "overlap"
        shot.lifetime = .keepAlways
        add(shot)

        let card = hit.a.union(hit.b)
        func dump(_ kind: String, _ q: XCUIElementQuery) {
            for e in q.allElementsBoundByIndex {
                let f = e.frame
                guard f.height > 1, f.midY >= card.minY, f.midY <= card.maxY else { continue }
                print("  [\(kind)] x=\(Int(f.minX)) w=\(Int(f.width)) '\(e.label.prefix(40))'")
            }
        }
        dump("img", app.images)
        dump("text", app.staticTexts)
    }

    private func anytimePill() -> XCUIElement {
        let button = app.buttons["Wszystkie"].firstMatch
        return button.exists ? button : app.staticTexts["Wszystkie"].firstMatch
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
