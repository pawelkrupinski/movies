import XCTest
@testable import KinowoCore

final class TopBarLayoutTests: XCTestCase {

    func testNarrowPortraitPhoneKeepsSearchFloating() {
        // iPhone portrait widths (mini → Pro Max) all stay below the threshold.
        XCTAssertFalse(TopBarLayout.searchInline(width: 375))
        XCTAssertFalse(TopBarLayout.searchInline(width: 393))
        XCTAssertFalse(TopBarLayout.searchInline(width: 440))
    }

    func testWideScreensInlineSearch() {
        // Landscape phones and iPads (either orientation) clear the threshold.
        XCTAssertTrue(TopBarLayout.searchInline(width: 667))  // iPhone SE landscape
        XCTAssertTrue(TopBarLayout.searchInline(width: 744))  // iPad mini portrait
        XCTAssertTrue(TopBarLayout.searchInline(width: 1024)) // iPad landscape
    }

    func testThresholdIsInclusive() {
        XCTAssertFalse(TopBarLayout.searchInline(width: 599))
        XCTAssertTrue(TopBarLayout.searchInline(width: 600))
    }

    // Regression: Wszystkie (.anytime) is the widest, 9-character label. If
    // it expands to an equal share of the row it gets clipped once the inline
    // search field also takes the row — the bug seen on iPad portrait. It
    // must always keep its intrinsic width; only the short pills expand.
    func testWszystkiePillNeverExpands() {
        XCTAssertFalse(TopBarLayout.datePillExpands(.anytime))
    }

    func testShortDatePillsExpandToShareTheRow() {
        XCTAssertTrue(TopBarLayout.datePillExpands(.today))
        XCTAssertTrue(TopBarLayout.datePillExpands(.tomorrow))
        XCTAssertTrue(TopBarLayout.datePillExpands(.week))
    }
}
