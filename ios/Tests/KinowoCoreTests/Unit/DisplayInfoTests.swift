import XCTest
@testable import KinowoCore

final class DisplayInfoTests: XCTestCase {
    func testReadoutMultipliesPointsByScale() {
        XCTAssertEqual(
            DisplayInfo.tuningReadout(pointWidth: 390, pointHeight: 844, scale: 3),
            "viewport 390×844 pt · @3× → 1170×2532 px"
        )
    }

    func testFractionalScaleIsKept() {
        XCTAssertEqual(
            DisplayInfo.tuningReadout(pointWidth: 411, pointHeight: 891, scale: 2.625),
            "viewport 411×891 pt · @2.625× → 1079×2339 px"
        )
    }

    func testPointsAreRounded() {
        XCTAssertEqual(
            DisplayInfo.tuningReadout(pointWidth: 389.6, pointHeight: 843.4, scale: 2),
            "viewport 390×843 pt · @2× → 779×1687 px"
        )
    }
}
