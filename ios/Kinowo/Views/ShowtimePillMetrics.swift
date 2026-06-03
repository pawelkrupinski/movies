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
    /// web pill (`_pillStyles`) lands in the same ballpark. Two canonical
    /// pills ("12:55 2D DUB" + "22:55 3D NAP") must share one row in the
    /// narrowest supported two-column portrait card — a hard, pinned constraint
    /// (see `ShowtimePillMetricsTests`). Dialled in on the tuning screen: time
    /// 10.5 / format 8.5, both `.medium`, with the 4 pt uniform inset and the
    /// 2.5 / 2 gaps below — that leaves ~4.8 pt of two-up margin on the
    /// `narrowestSupportedWidth` (390 pt) phone. Note these wrap on the 375 pt
    /// phones (SE / 8 / X / 12·13 mini), which we deliberately dropped from the
    /// two-up floor. The same constants drive both orientations, so the
    /// showtime text is one consistent size whether the phone is held portrait
    /// or landscape.
    static let timeFontSize: CGFloat = 10.5
    static let formatFontSize: CGFloat = 8.5

    /// Pill colours, mirroring the web `.badge-time` pill: a `#3a3a6e` fill
    /// (`#5a5a9e` while pressed/held, matching the web hover), `#aad4ff` time
    /// text, and the format tag at `formatAlpha` of that same blue. RGB in
    /// 0...1 so they live in this SwiftUI-free module and stay unit-testable;
    /// `ShowtimeBadge` builds its `Color`s from them. Keep in lockstep with the
    /// web palette and Android's `ShowtimeChipBackground` / `CinemaBlue`.
    static let backgroundRGB: (red: Double, green: Double, blue: Double) = (58 / 255, 58 / 255, 110 / 255)
    static let pressedBackgroundRGB: (red: Double, green: Double, blue: Double) = (90 / 255, 90 / 255, 158 / 255)
    static let textRGB: (red: Double, green: Double, blue: Double) = (170 / 255, 212 / 255, 255 / 255)
    static let formatAlpha: Double = 0.7

    /// Inset inside the pill, uniform on both axes so the time sits in an even
    /// box of padding (`.padding(.horizontal,)` / `.padding(.vertical,)`). 4 pt
    /// is the roomiest uniform inset that still lets two pills share a row at
    /// 11/7 on the narrowest card (~5 pt margin) — the horizontal half is the
    /// binding one. Keep the two equal; see `ShowtimePillMetricsTests`.
    static let horizontalInset: CGFloat = 4
    static let verticalInset: CGFloat = 4
    /// Gap between the time and the format tag (`HStack(spacing:)`).
    static let internalGap: CGFloat = 2.5
    /// Gap between adjacent pills (`FlowLayout(spacing:)`).
    static let interPillGap: CGFloat = 2

    /// SwiftUI font weights, mirrored onto CoreText's normalised
    /// (-1...1) weight axis: `.medium` ≈ 0.23. The time and format are both
    /// drawn `.medium` (see `ShowtimePillStyle` defaults / `ShowtimeBadge`),
    /// so the width math measures both at 0.23 to match — keep these in
    /// lockstep with the view's `weight:`.
    private static let timeWeight: CGFloat = 0.23
    private static let formatWeight: CGFloat = 0.23

    /// Narrowest portrait screen width the two-pills-per-row guarantee is held
    /// against. 390 pt — the iPhone 12/13/14 generation — is the floor going
    /// forward; the 375 pt phones (SE, 8, X/XS/11 Pro, 12/13 mini) are
    /// deliberately excluded (their pills wrap to one per row). Single source of
    /// truth for the floor: `ShowtimePillMetricsTests` and the tuning screen's
    /// fit readout both measure against this.
    static let narrowestSupportedWidth: CGFloat = 390

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

    /// Width available to one card's showings `FlowLayout` on a phone of
    /// the given logical screen width. Mirrors `FilmGridView`'s adaptive
    /// `GridItem`: the grid's 12 pt horizontal padding + 12 pt spacing
    /// between the two columns give a card of `(screen − 36) / 2`, clamped
    /// to the `[160, 220]` adaptive bounds; `FilmCardView`'s `.padding(12)`
    /// then takes 24 pt off the inside. Pure arithmetic so the fit test
    /// can sweep it across every iPhone width — keep it the single source
    /// of this derivation (don't re-spell `(screen − 36) / 2` elsewhere).
    static func cardShowingsWidth(screenWidth: CGFloat) -> CGFloat {
        let columnWidth = (screenWidth - 36) / 2
        let cardWidth = min(220, max(160, columnWidth))
        return cardWidth - 24
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
