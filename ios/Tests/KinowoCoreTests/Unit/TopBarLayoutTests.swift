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

    // MARK: - Equal-width vs fit-the-text decision
    //
    // The pills render at one uniform width only when the row is wide enough
    // to hold `count` copies of the WIDEST pill (equal width forces every pill
    // to at least the widest label's width, or its text clips) plus the gaps
    // between them. When it isn't, the pills fall back to their own intrinsic
    // widths so every label still fits — fitting all the text wins over a
    // uniform row. Widths below model Dziś / Jutro / 7 dni / Wszystkie, with
    // Wszystkie the widest.

    func testEqualWidthWhenRowFitsFourOfTheWidestPill() {
        // 4 × 94 + 3 × 6 = 394 needed; 420 has room to spare.
        XCTAssertTrue(TopBarLayout.datePillsEqualWidth(
            available: 420, intrinsicWidths: [54, 64, 64, 94], spacing: 6))
    }

    func testEqualWidthBoundaryIsInclusive() {
        // Exactly the equal-row width fits…
        XCTAssertTrue(TopBarLayout.datePillsEqualWidth(
            available: 394, intrinsicWidths: [54, 64, 64, 94], spacing: 6))
        // …one point short does not.
        XCTAssertFalse(TopBarLayout.datePillsEqualWidth(
            available: 393, intrinsicWidths: [54, 64, 64, 94], spacing: 6))
    }

    func testFallsBackToIntrinsicWhenTooNarrowForAUniformRow() {
        // ≈ iPhone-portrait leftover. Can't hold 4 × 94 (= 394) but easily
        // holds the intrinsic sum (276 + 18 gaps = 294), so the labels still
        // fit — just not at one shared width.
        XCTAssertFalse(TopBarLayout.datePillsEqualWidth(
            available: 340, intrinsicWidths: [54, 64, 64, 94], spacing: 6))
    }

    func testWidestPillDrivesTheDecisionNotTheSum() {
        // The intrinsic sum (4 × 40 + 18 = 178) fits 300 easily, but one fat
        // pill (140) makes a uniform row need 4 × 140 + 18 = 578 — so equal
        // width is refused even though every label fits at intrinsic width.
        XCTAssertFalse(TopBarLayout.datePillsEqualWidth(
            available: 300, intrinsicWidths: [40, 40, 40, 140], spacing: 6))
    }

    func testEmptyPillSetIsNeverEqualWidth() {
        XCTAssertFalse(TopBarLayout.datePillsEqualWidth(
            available: 10_000, intrinsicWidths: [], spacing: 6))
    }
}
