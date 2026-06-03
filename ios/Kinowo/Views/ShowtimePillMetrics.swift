#if canImport(CoreText)
import CoreText
import CoreGraphics
import Foundation

/// Width math for a single showtime pill, shared by the on-screen
/// `ShowtimeBadge` (which renders at these font sizes) and
/// `ShowingsView`'s truncation estimator (which sums pill widths to
/// predict how many rows the `FlowLayout` will produce).
///
/// Lives in `KinowoCore` — CoreText, no UIKit — so the "two pills fit
/// one row" guarantee is unit-testable via `swift test` on macOS, where
/// the system font's glyph advances match the device. CoreText is the
/// same shaping engine UIKit measures with, so the estimate tracks what
/// SwiftUI's `Text` actually paints.
enum ShowtimePillMetrics {
    /// Font sizes for the time and the format tag inside a pill. The
    /// SwiftUI `ShowtimeBadge` renders at exactly these sizes, and the
    /// web pill (`_pillStyles`) lands in the same ballpark. Sized so two
    /// canonical pills ("12:55 2D DUB" + "22:55 3D NAP") share one row in
    /// the narrowest two-column portrait card — pinned by
    /// `ShowtimePillMetricsTests`. The same constants drive both
    /// orientations, so the showtime text is one consistent size whether
    /// the phone is held portrait or landscape.
    static let timeFontSize: CGFloat = 10
    static let formatFontSize: CGFloat = 7

    /// Per-side horizontal inset inside the pill (`.padding(.horizontal,)`).
    static let horizontalInset: CGFloat = 4
    /// Gap between the time and the format tag (`HStack(spacing:)`).
    static let internalGap: CGFloat = 3
    /// Gap between adjacent pills (`FlowLayout(spacing:)`).
    static let interPillGap: CGFloat = 4

    /// SwiftUI font weights, mirrored onto CoreText's normalised
    /// (-1...1) weight axis: `.semibold` ≈ 0.3, `.medium` ≈ 0.23.
    private static let timeWeight: CGFloat = 0.3
    private static let formatWeight: CGFloat = 0.23

    /// Rendered width of one pill: both insets, the time text, and —
    /// when a format tag survives token filtering — the internal gap
    /// plus the tag. Matches `ShowtimeBadge`'s layout exactly.
    static func pillWidth(time: String, format: String) -> CGFloat {
        let timeWidth = textWidth(time, size: timeFontSize, weight: timeWeight)
        let trimmedFormat = format.trimmingCharacters(in: .whitespacesAndNewlines)
        let formatWidth: CGFloat = trimmedFormat.isEmpty
            ? 0
            : internalGap + textWidth(trimmedFormat, size: formatFontSize, weight: formatWeight)
        return 2 * horizontalInset + timeWidth + formatWidth
    }

    /// Typographic width of `s` in the system font at `size`/`weight`.
    static func textWidth(_ s: String, size: CGFloat, weight: CGFloat) -> CGFloat {
        guard !s.isEmpty else { return 0 }
        let attributes: [CFString: Any] = [kCTFontAttributeName: systemFont(size: size, weight: weight)]
        let attributed = CFAttributedStringCreate(nil, s as CFString, attributes as CFDictionary)!
        let line = CTLineCreateWithAttributedString(attributed)
        return CGFloat(CTLineGetTypographicBounds(line, nil, nil, nil))
    }

    /// The San Francisco system font at the given size, re-created with
    /// an explicit weight trait so semibold/medium glyph advances match
    /// what SwiftUI renders.
    private static func systemFont(size: CGFloat, weight: CGFloat) -> CTFont {
        let base = CTFontCreateUIFontForLanguage(.system, size, nil)
            ?? CTFontCreateWithName("Helvetica" as CFString, size, nil)
        let traits: [CFString: Any] = [kCTFontWeightTrait: weight]
        let attributes: [CFString: Any] = [kCTFontTraitsAttribute: traits]
        let descriptor = CTFontDescriptorCreateCopyWithAttributes(
            CTFontCopyFontDescriptor(base), attributes as CFDictionary)
        return CTFontCreateWithFontDescriptor(descriptor, size, nil)
    }
}
#endif
