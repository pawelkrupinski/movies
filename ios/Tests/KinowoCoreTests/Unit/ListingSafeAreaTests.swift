import XCTest
@testable import KinowoCore

final class ListingSafeAreaTests: XCTestCase {

    func testPortraitPhoneListingSpansTheWindow() {
        let area = ListingSafeArea(safeWidth: 402, leading: 0, trailing: 0)
        XCTAssertTrue(area.bleedsIntoHorizontalInsets)
        XCTAssertEqual(area.listingWidth, 402)
    }

    func testLandscapePhoneBleedsUnderBothSensorHousingInsets() {
        // iPhone 17 landscape: 874 pt window, 62 pt each side.
        let area = ListingSafeArea(safeWidth: 750, leading: 62, trailing: 62)
        XCTAssertTrue(area.bleedsIntoHorizontalInsets)
        XCTAssertEqual(area.listingWidth, 874)
    }

    func testDuoStatusColumnIsRespectedNotBledInto() {
        // iPhone Duo portrait: 466 pt window, status bar in an 84 pt trailing column.
        let area = ListingSafeArea(safeWidth: 382, leading: 0, trailing: 84)
        XCTAssertFalse(area.bleedsIntoHorizontalInsets)
        XCTAssertEqual(area.listingWidth, 382)
    }

    func testSubPointDifferenceStillCountsAsSymmetric() {
        let area = ListingSafeArea(safeWidth: 750, leading: 62, trailing: 62.000001)
        XCTAssertTrue(area.bleedsIntoHorizontalInsets)
    }
}
