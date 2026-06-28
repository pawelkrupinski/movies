import XCTest
@testable import KinowoCore

/// Pins the poster/card layout constants the SwiftUI views render from. The
/// views are excluded from `KinowoCore` (SwiftUI), so here we lock the shared
/// source values that `FilmCardView` / `FilmDetailView` read.
final class PosterMetricsTests: XCTestCase {

    /// Posters are 2:3 — the box the card poster and detail placeholder fit to.
    func testAspectRatioIsTwoThirds() {
        XCTAssertEqual(PosterMetrics.aspectRatio, 2.0 / 3.0, accuracy: 0.0001)
    }

    /// The rounded media surfaces (card, detail poster, trailer embed) share a
    /// 12 pt corner radius.
    func testCornerRadiusIsTwelve() {
        XCTAssertEqual(PosterMetrics.cornerRadius, 12, accuracy: 0.0001)
    }
}
