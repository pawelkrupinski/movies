import Foundation

/// Read-only display-geometry readout for the dev tuning screen. It leads with
/// the *available* logical size that actually drives layout (points on iOS) —
/// more useful than the raw panel resolution for dialling in the two-per-row
/// showtime fit — then the screen scale and the physical pixel resolution it
/// works out to.
///
/// Pure + Foundation-only so it lives in `KinowoCore` and is unit-tested on
/// Linux; the SwiftUI screen feeds it `GeometryReader` points + `UIScreen`
/// scale.
enum DisplayInfo {
    /// e.g. `viewport 390×844 pt · @3× → 1170×2532 px`
    static func tuningReadout(pointWidth: Double, pointHeight: Double, scale: Double) -> String {
        let width = Int(pointWidth.rounded())
        let height = Int(pointHeight.rounded())
        let pixelWidth = Int((pointWidth * scale).rounded())
        let pixelHeight = Int((pointHeight * scale).rounded())
        return "viewport \(width)×\(height) pt · @\(scaleText(scale))× → \(pixelWidth)×\(pixelHeight) px"
    }

    /// `2.0` → `2`, `2.625` → `2.625` — drop a trailing `.0`, keep real fractions.
    private static func scaleText(_ scale: Double) -> String {
        String(format: "%g", scale)
    }
}
