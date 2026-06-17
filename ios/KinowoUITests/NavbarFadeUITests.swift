import XCTest

/// Regression guard for "remove the shading / fade under the navbar"
/// (requested 2026-06-17). The grid used to carry a `LinearGradient`
/// scrim over its top 56pt — `colors: [systemBackground, .clear]`, top→
/// bottom — so rows dissolved into the bar instead of meeting a hard edge.
///
/// The scrim blends the grid's upper band toward the app background, so a
/// band a few-dozen points below the content top is FLATTENED — its colour
/// variance collapses (posters that would be vivid there are washed to the
/// background colour). That is the discriminating signature, measured ~25–
/// 40pt below the content top (inside the scrim band, below the top inset
/// gap, where posters reliably sit in a dense grid):
///
///   - WITH the fade  → the band is flattened toward background → low
///     colour variance (a few hundred at most), at every scroll offset.
///   - WITHOUT it      → posters show at full colour → high variance
///     (thousands) for at least one offset.
///
/// Variance — not brightness — is the signal, so this holds in light and
/// dark mode alike (the scrim flattens toward whichever background colour).
/// Empirically the two states sit at ~300 vs ~3700+; the 1500 threshold
/// splits them. FAILS while the gradient exists, PASSES once it's gone.
final class NavbarFadeUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        XCUIDevice.shared.orientation = .portrait
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "poznan"
        app.launch()

        // Clear the first-launch city gate deterministically (no CoreLocation).
        let confirm = app.buttons[A11y.CityGate.confirmButton]
        if confirm.waitForExistence(timeout: 10) { confirm.tap() }

        // "Wszystkie" guarantees a dense, multi-row poster grid at any hour.
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30),
                      "Grid never filled after switching to Wszystkie")
    }

    override func tearDownWithError() throws { app = nil }

    func testNoFadeScrimUnderNavbar() throws {
        let grid = app.scrollViews.firstMatch
        XCTAssertTrue(grid.waitForExistence(timeout: 5), "No grid scroll view")

        // Get posters up against the bar so the sampled band crosses poster
        // bodies, not the launch gap.
        grid.swipeUp(velocity: .slow)
        Thread.sleep(forTimeInterval: 0.6)

        // Points below the content top: clear of the ~15pt top inset gap
        // above the first poster row, yet shallow enough that the (former)
        // scrim is still strong here (it fades over 56pt, so deeper offsets
        // leak poster colour and blur the signal). At this depth the scrim
        // crushed variance to <100 vs thousands without it.
        let bandOffsets: [CGFloat] = [20, 24, 28, 32]

        var maxVariance: Double = 0
        // Several small nudges: a single offset can land a band in a card
        // gap (flat) even without the scrim. We take the max — without the
        // scrim at least one offset/nudge spans vivid posters; with it,
        // every one stays flattened toward the background.
        for _ in 0..<6 {
            let img = XCUIScreen.main.screenshot().image
            for o in bandOffsets {
                if let s = rowStats(img, atPointY: grid.frame.minY + o,
                                    screenWidth: app.frame.width) {
                    maxVariance = max(maxVariance, s.variance)
                }
            }
            grid.swipeUp(velocity: .slow)
            Thread.sleep(forTimeInterval: 0.5)
        }

        let attach = XCTAttachment(screenshot: XCUIScreen.main.screenshot())
        attach.name = "navbar-under-bar-band"
        attach.lifetime = .keepAlways
        add(attach)

        XCTAssertGreaterThan(
            maxVariance, 1500,
            """
            The grid's upper band stayed flattened (max luminance variance \
            \(Int(maxVariance))) across every scroll offset — posters there \
            are washed toward the app background. That is the fade scrim's \
            signature; the shading under the navbar was not removed.
            """
        )
    }

    // MARK: - pixel sampling

    /// Mean + variance of luminance across a horizontal line of the
    /// screenshot at the given point-space y, sampling the middle 80% of the
    /// width (skipping the safe-area edges). Returns nil if unreadable.
    private func rowStats(_ image: UIImage, atPointY pointY: CGFloat,
                          screenWidth: CGFloat) -> (mean: Double, variance: Double)? {
        guard let cg = image.cgImage,
              let data = cg.dataProvider?.data,
              let ptr = CFDataGetBytePtr(data) else { return nil }

        let scale = CGFloat(cg.width) / screenWidth   // pixels per point
        let py = Int((pointY * scale).rounded())
        guard py >= 0, py < cg.height else { return nil }

        let bpr = cg.bytesPerRow
        let bpp = cg.bitsPerPixel / 8
        let x0 = Int(0.10 * CGFloat(cg.width))
        let x1 = Int(0.90 * CGFloat(cg.width))
        let step = max(1, (x1 - x0) / 40)             // ~40 samples

        var lums: [Double] = []
        var px = x0
        while px < x1 {
            let off = py * bpr + px * bpp
            let r = Double(ptr[off]), g = Double(ptr[off + 1]), b = Double(ptr[off + 2])
            lums.append(0.299 * r + 0.587 * g + 0.114 * b)
            px += step
        }
        guard lums.count > 1 else { return nil }
        let mean = lums.reduce(0, +) / Double(lums.count)
        let variance = lums.reduce(0) { $0 + ($1 - mean) * ($1 - mean) } / Double(lums.count)
        return (mean, variance)
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
