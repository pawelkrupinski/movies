import SwiftUI
import UIKit

struct ShowingsView: View {
    let film: Film
    /// When the parent already announces the cinema (Kina tab's
    /// per-cinema section header), suppress the per-card cinema label
    /// to avoid duplication. Mirrors the web's `_filmShowings`
    /// `showCinemaHeaders` flag, which `_cinemaCards` flips to false.
    var showCinemaHeaders: Bool = true
    /// When true, showtimes beyond `maxVisibleLines` are truncated and
    /// a "… +N seansów" label appears at the bottom. Tapping the card
    /// navigates to the film detail page (the whole card is a
    /// NavigationLink), matching the web's "więcej" behavior.
    var truncatable: Bool = false
    var maxVisibleLines: Int = 13

    var body: some View {
        let allDays = film.showings
        let (visibleDays, hiddenShowtimes) = truncated(allDays)
        let shouldTruncate = truncatable && hiddenShowtimes > 0
        let days = shouldTruncate ? visibleDays : allDays

        VStack(alignment: .leading, spacing: 6) {
            ForEach(days, id: \.date) { day in
                Text(day.label.uppercased())
                    .font(.system(size: 10, weight: .semibold))
                    .foregroundColor(Color.white.opacity(0.85))
                    .tracking(0.5)
                    .padding(.top, 4)
                ForEach(day.cinemas, id: \.cinema) { cinema in
                    let commonTokens = FormatTokenFilter.commonTokens(cinema)
                    VStack(alignment: .leading, spacing: 4) {
                        if showCinemaHeaders {
                            cinemaLabel(cinema)
                        }
                        FlowLayout(spacing: ShowtimePillMetrics.interPillGap, lineSpacing: 4) {
                            ForEach(cinema.showtimes) { st in
                                ShowtimeBadge(
                                    showtime: st,
                                    displayFormat: FormatTokenFilter.filter(st.format, removing: commonTokens)
                                )
                            }
                        }
                    }
                }
            }
            if shouldTruncate {
                moreLabel(hiddenShowtimes: hiddenShowtimes)
            }
        }
    }

    private func truncated(_ allDays: [DayShowings]) -> (visible: [DayShowings], hidden: Int) {
        var lineCount = 0
        var visibleDays: [DayShowings] = []
        var hidden = 0
        let contentWidth = Self.cardShowingsWidth

        for day in allDays {
            var keptCinemas: [CinemaShowings] = []
            var dayLines = 1
            for cinema in day.cinemas {
                let commonTokens = FormatTokenFilter.commonTokens(cinema)
                let pillRows = Self.pillRowCount(cinema.showtimes, commonTokens: commonTokens, contentWidth: contentWidth)
                let cinemaLines = 1 + pillRows
                if lineCount + dayLines + cinemaLines <= maxVisibleLines {
                    keptCinemas.append(cinema)
                    dayLines += cinemaLines
                } else {
                    hidden += cinema.showtimes.count
                }
            }
            if keptCinemas.isEmpty { continue }
            visibleDays.append(DayShowings(date: day.date, label: day.label, cinemas: keptCinemas))
            lineCount += dayLines
        }

        if hidden <= minHiddenShowtimes {
            return (allDays, 0)
        }
        return (visibleDays, hidden)
    }

    private let minHiddenShowtimes: Int = 3

    // MARK: – pill-row sizing

    /// Simulates the FlowLayout's wrap: walks pills in order, tallying
    /// the running row width against `contentWidth` and starting a
    /// new row when the next pill won't fit. Returns the row count
    /// that the on-screen layout will actually produce — no
    /// per-device special casing, just measurement.
    private static func pillRowCount(_ showtimes: [Showtime], commonTokens: Set<String> = [], contentWidth: CGFloat) -> Int {
        guard !showtimes.isEmpty else { return 0 }
        var rows = 1
        var rowWidth: CGFloat = 0
        for st in showtimes {
            let w = pillWidth(for: st, commonTokens: commonTokens)
            if rowWidth == 0 {
                rowWidth = w
            } else if rowWidth + ShowtimePillMetrics.interPillGap + w <= contentWidth {
                rowWidth += ShowtimePillMetrics.interPillGap + w
            } else {
                rows += 1
                rowWidth = w
            }
        }
        return rows
    }

    /// Rendered width of one showtime pill — delegated to
    /// `ShowtimePillMetrics`, the same CoreText-backed measurement that
    /// `ShowtimeBadge` is sized from, so this row count tracks what the
    /// `FlowLayout` actually produces pill-by-pill (no average / no fudge).
    private static func pillWidth(for st: Showtime, commonTokens: Set<String> = []) -> CGFloat {
        ShowtimePillMetrics.pillWidth(
            time: st.time,
            format: FormatTokenFilter.filter(st.format, removing: commonTokens)
        )
    }

    /// Width available to the showings FlowLayout inside one grid card,
    /// derived from the device screen width by
    /// `ShowtimePillMetrics.cardShowingsWidth` (the same formula the
    /// fit test sweeps every iPhone width through). Computed once at
    /// view-construction time — the iOS app is phone-only and the screen
    /// width is constant per launch.
    private static let cardShowingsWidth: CGFloat =
        ShowtimePillMetrics.cardShowingsWidth(screenWidth: UIScreen.main.bounds.width)

    @ViewBuilder
    private func cinemaLabel(_ cinema: CinemaShowings) -> some View {
        let labelColor = Color(red: 0.40, green: 0.67, blue: 0.87)
        if let url = cinema.cinemaURL {
            Link(destination: url) {
                Text("\(cinema.cinema) ↗")
                    .font(.system(size: 10))
                    .foregroundColor(labelColor)
            }
            .buttonStyle(.plain)
        } else {
            Text(cinema.cinema)
                .font(.system(size: 10))
                .foregroundColor(labelColor)
        }
    }

    @ViewBuilder
    private func moreLabel(hiddenShowtimes: Int) -> some View {
        let labelColor = Color(red: 0.40, green: 0.67, blue: 0.87)
        HStack(spacing: 4) {
            Image(systemName: "ellipsis")
                .font(.system(size: 9, weight: .semibold))
            Text("+\(hiddenShowtimes) \(showtimeNoun(hiddenShowtimes))")
                .font(.system(size: 11, weight: .medium))
        }
        .foregroundColor(labelColor)
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
        .background(Color.white.opacity(0.06), in: Capsule())
        .padding(.top, 6)
    }

    /// Polish plural for "seans". 1 → seans, 2–4 (excluding 12–14) →
    /// seanse, else (0, 5+, 11–14, 25–30 …) → seansów. Standard
    /// last-two-digits rule.
    private func showtimeNoun(_ n: Int) -> String {
        if n == 1 { return "seans" }
        let mod10 = n % 10
        let mod100 = n % 100
        if mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14) { return "seanse" }
        return "seansów"
    }
}

private struct ShowtimeBadge: View {
    @Environment(\.openURL) private var openURL
    let showtime: Showtime
    var displayFormat: String = ""

    /// True while a long-press is held down — drives the room tooltip.
    /// Set when the long-press succeeds (≥0.3s) and cleared on release, so
    /// a quick tap never flashes the tooltip.
    @State private var holding = false

    // Web `.badge-time` palette, sourced from the SwiftUI-free `ShowtimePillMetrics`.
    private static let fill = Color(
        red: ShowtimePillMetrics.backgroundRGB.red,
        green: ShowtimePillMetrics.backgroundRGB.green,
        blue: ShowtimePillMetrics.backgroundRGB.blue)
    private static let pressedFill = Color(
        red: ShowtimePillMetrics.pressedBackgroundRGB.red,
        green: ShowtimePillMetrics.pressedBackgroundRGB.green,
        blue: ShowtimePillMetrics.pressedBackgroundRGB.blue)
    private static let timeColor = Color(
        red: ShowtimePillMetrics.textRGB.red,
        green: ShowtimePillMetrics.textRGB.green,
        blue: ShowtimePillMetrics.textRGB.blue)

    var body: some View {
        let trimmedFormat = displayFormat.trimmingCharacters(in: .whitespacesAndNewlines)
        let room = showtime.displayRoom

        let pill = HStack(spacing: ShowtimePillMetrics.internalGap) {
            Text(showtime.time)
                .font(.system(size: ShowtimePillMetrics.timeFontSize, weight: .semibold))
                .foregroundColor(Self.timeColor)
            if !trimmedFormat.isEmpty {
                Text(trimmedFormat)
                    .font(.system(size: ShowtimePillMetrics.formatFontSize, weight: .medium))
                    .foregroundColor(Self.timeColor.opacity(ShowtimePillMetrics.formatAlpha))
            }
        }
        .padding(.horizontal, ShowtimePillMetrics.horizontalInset)
        .padding(.vertical, 4)
        .background(holding ? Self.pressedFill : Self.fill, in: RoundedRectangle(cornerRadius: 5))
        .overlay(alignment: .top) {
            if holding, let room {
                roomTooltip(room)
            }
        }
        .animation(.easeOut(duration: 0.1), value: holding)

        // A quick tap opens the booking link (when present); pressing and
        // holding reveals the room name for as long as the finger stays
        // down, mirroring the web's long-press tooltip and Android's
        // `detectTapGestures(onTap/onLongPress)`. Tap and long-press are
        // mutually exclusive, so a deliberate hold never also fires the
        // booking link on release — and a quick tap never flashes the
        // tooltip, because `holding` only flips once the press is recognised.
        if let room {
            pill
                .onTapGesture { if let url = showtime.bookingURL { openURL(url) } }
                .onLongPressGesture(minimumDuration: 0.3) {
                    holding = true
                } onPressingChanged: { pressing in
                    if !pressing { holding = false }
                }
                .accessibilityHint("Przytrzymaj, aby zobaczyć salę: \(room)")
        } else if let url = showtime.bookingURL {
            Link(destination: url) { pill }
                .buttonStyle(.plain)
        } else {
            pill
        }
    }

    @ViewBuilder
    private func roomTooltip(_ room: String) -> some View {
        Text(room)
            .font(.system(size: 10, weight: .medium))
            .foregroundColor(Color(white: 0.85))
            .padding(.horizontal, 7)
            .padding(.vertical, 3)
            .background(Color(red: 0.055, green: 0.055, blue: 0.118), in: RoundedRectangle(cornerRadius: 4))
            .overlay(
                RoundedRectangle(cornerRadius: 4)
                    .stroke(Color(red: 0.23, green: 0.23, blue: 0.43), lineWidth: 1)
            )
            .fixedSize()
            .offset(y: -27)
            .transition(.opacity)
            .allowsHitTesting(false)
            .zIndex(1)
    }
}
