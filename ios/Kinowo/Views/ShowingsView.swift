import SwiftUI
import UIKit

struct ShowingsView: View {
    let film: Film
    /// When the parent already announces the cinema (Kina tab's
    /// per-cinema section header), suppress the per-card cinema label
    /// to avoid duplication. Mirrors the web's `_filmShowings`
    /// `showCinemaHeaders` flag, which `_cinemaCards` flips to false.
    var showCinemaHeaders: Bool = true
    /// When true, anything beyond `maxCollapsedLines` worth of
    /// vertical content hides behind an expand toggle. Listing cards
    /// (`FilmCardView`) pass true so a daily-for-two-weeks film
    /// doesn't take a screen-and-a-half on the grid. The /film detail
    /// screen leaves this false so it always shows the full schedule.
    var collapsible: Bool = false
    /// Soft cap on the visual height of the showings rail when
    /// collapsed, counted in "lines": each day header is 1, each
    /// cinema header is 1, and each row the pills wrap into is 1.
    /// Pill rows are computed from the actual rendered pill widths
    /// (variable by format tag — "IMAX 3D NAP" is far wider than
    /// "2D" or no format at all) against the actual card content
    /// width (derived from screen width, so a 13 mini packs fewer
    /// pills per row than a 17 Pro Max). 13 keeps a typical Dziś
    /// card under ~½ a screen height; lower further by passing in.
    var maxCollapsedLines: Int = 13
    @State private var isExpanded: Bool = false

    var body: some View {
        let allDays = film.showings
        let (visibleDays, hiddenShowtimes) = collapsed(allDays)
        let canCollapse = collapsible && hiddenShowtimes > 0
        let days = (canCollapse && !isExpanded) ? visibleDays : allDays

        VStack(alignment: .leading, spacing: 6) {
            ForEach(days, id: \.date) { day in
                Text(day.label.uppercased())
                    .font(.system(size: 10, weight: .semibold))
                    .foregroundColor(Color.white.opacity(0.85))
                    .tracking(0.5)
                    .padding(.top, 4)
                ForEach(day.cinemas, id: \.cinema) { cinema in
                    VStack(alignment: .leading, spacing: 4) {
                        if showCinemaHeaders {
                            cinemaLabel(cinema)
                        }
                        FlowLayout(spacing: 4, lineSpacing: 4) {
                            ForEach(cinema.showtimes) { st in
                                ShowtimeBadge(showtime: st)
                            }
                        }
                    }
                }
            }
            if canCollapse {
                expandToggleButton(hiddenShowtimes: hiddenShowtimes)
            }
        }
    }

    /// Walk the full day/cinema/showtime tree once, accumulating a
    /// line-count estimate. Stop adding new cinemas the moment the
    /// next one would push past `maxCollapsedLines`; everything beyond
    /// counts toward `hidden`. Always truncates at cinema boundaries
    /// (never mid-cinema) so the visible block never reads as half a
    /// cinema's slots.
    ///
    /// If the truncation would only hide a few screenings
    /// (≤ `minHiddenShowtimes`), skip it and return the full list
    /// — tucking a 1- or 2-pill remainder behind a tap is more
    /// friction than it's worth, even when those pills happen to
    /// span multiple cinema labels (line-count would tick over the
    /// threshold while the user still reads it as "just two more
    /// seanse").
    private func collapsed(_ allDays: [DayShowings]) -> (visible: [DayShowings], hidden: Int) {
        var lineCount = 0
        var visibleDays: [DayShowings] = []
        var hidden = 0
        let contentWidth = Self.cardShowingsWidth

        for day in allDays {
            var keptCinemas: [CinemaShowings] = []
            var dayLines = 1  // the date label itself
            for cinema in day.cinemas {
                let pillRows = Self.pillRowCount(cinema.showtimes, contentWidth: contentWidth)
                let cinemaLines = 1 + pillRows  // cinema label + pill rows
                if lineCount + dayLines + cinemaLines <= maxCollapsedLines {
                    keptCinemas.append(cinema)
                    dayLines += cinemaLines
                } else {
                    hidden += cinema.showtimes.count
                }
            }
            if keptCinemas.isEmpty {
                // The cap is already saturated; this whole day is
                // hidden (its showtimes were already counted into
                // `hidden` in the inner loop).
                continue
            }
            visibleDays.append(DayShowings(date: day.date, label: day.label, cinemas: keptCinemas))
            lineCount += dayLines
        }

        // "+1 seans" / "+2 seanse" / "+3 seanse" toggles aren't worth
        // a tap — render the whole list instead.
        if hidden <= minHiddenShowtimes {
            return (allDays, 0)
        }
        return (visibleDays, hidden)
    }

    /// Don't bother collapsing when the truncation would only tuck
    /// this many screenings or fewer behind the toggle.
    private let minHiddenShowtimes: Int = 3

    // MARK: – pill-row sizing

    /// Simulates the FlowLayout's wrap: walks pills in order, tallying
    /// the running row width against `contentWidth` and starting a
    /// new row when the next pill won't fit. Returns the row count
    /// that the on-screen layout will actually produce — no
    /// per-device special casing, just measurement.
    private static func pillRowCount(_ showtimes: [Showtime], contentWidth: CGFloat) -> Int {
        guard !showtimes.isEmpty else { return 0 }
        var rows = 1
        var rowWidth: CGFloat = 0
        for st in showtimes {
            let w = pillWidth(for: st)
            if rowWidth == 0 {
                rowWidth = w
            } else if rowWidth + pillGap + w <= contentWidth {
                rowWidth += pillGap + w
            } else {
                rows += 1
                rowWidth = w
            }
        }
        return rows
    }

    /// Actual rendered width of one showtime pill, broken down to
    /// match `ShowtimeBadge`'s layout: 7+7 horizontal padding, the
    /// time text at 12pt semibold, an optional 9pt-medium format tag
    /// preceded by 4pt spacing, then the 4pt + 9pt star. UIFont's
    /// `(NSString).size(withAttributes:)` returns the same width
    /// SwiftUI's Text uses, so the estimate tracks pill-by-pill
    /// reality (no average / no fudge).
    private static func pillWidth(for st: Showtime) -> CGFloat {
        let timeWidth = textWidth(st.time, font: timeFont)
        let trimmedFormat = st.format.trimmingCharacters(in: .whitespacesAndNewlines)
        let formatWidth: CGFloat = trimmedFormat.isEmpty
            ? 0
            : pillInternalGap + textWidth(trimmedFormat, font: formatFont)
        return pillHorizontalPadding + timeWidth + formatWidth + pillInternalGap + starWidth
    }

    private static func textWidth(_ s: String, font: UIFont) -> CGFloat {
        (s as NSString).size(withAttributes: [.font: font]).width
    }

    private static let timeFont   = UIFont.systemFont(ofSize: 12, weight: .semibold)
    private static let formatFont = UIFont.systemFont(ofSize: 9,  weight: .medium)
    private static let starWidth: CGFloat = 9            // SF Symbol "star" at 9pt
    private static let pillHorizontalPadding: CGFloat = 14  // 7 leading + 7 trailing
    private static let pillInternalGap: CGFloat = 4      // HStack(spacing: 4) inside the pill
    private static let pillGap: CGFloat = 4              // FlowLayout(spacing: 4) between pills

    /// Width available to the showings FlowLayout inside one grid
    /// card. Mirrors `FilmGridView`'s adaptive `GridItem`:
    /// `.padding(.horizontal, 12)` on the LazyVGrid + 12pt spacing
    /// between two columns gives card width `(screen − 36) / 2`,
    /// clamped to the [160, 220] adaptive bounds; FilmCardView's own
    /// `.padding(12)` then takes 24pt off the inside.
    /// Computed once at view-construction time — the iOS app is
    /// phone-only and the screen width is constant per launch.
    private static let cardShowingsWidth: CGFloat = {
        let screenWidth = UIScreen.main.bounds.width
        let columnWidth = (screenWidth - 36) / 2
        let cardWidth = min(220, max(160, columnWidth))
        return cardWidth - 24
    }()

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

    /// "… +N seansów" / "↑ zwiń" toggle sitting at the bottom of the
    /// showings rail when the card overflows the line cap. Same blue
    /// as the cinema label so it reads as part of the showings, not
    /// chrome from elsewhere.
    @ViewBuilder
    private func expandToggleButton(hiddenShowtimes: Int) -> some View {
        let labelColor = Color(red: 0.40, green: 0.67, blue: 0.87)
        Button {
            withAnimation(.easeInOut(duration: 0.15)) { isExpanded.toggle() }
        } label: {
            HStack(spacing: 4) {
                Image(systemName: isExpanded ? "chevron.up" : "ellipsis")
                    .font(.system(size: 9, weight: .semibold))
                Text(isExpanded ? "zwiń" : "+\(hiddenShowtimes) \(showtimeNoun(hiddenShowtimes))")
                    .font(.system(size: 11, weight: .medium))
            }
            .foregroundColor(labelColor)
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            // Subtle dark fill so the toggle reads as a button, not
            // floating text — same value as the showtime badge.
            .background(Color.white.opacity(0.06), in: Capsule())
        }
        .buttonStyle(.plain)
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
    let showtime: Showtime

    var body: some View {
        let trimmedFormat = showtime.format.trimmingCharacters(in: .whitespacesAndNewlines)

        let body = HStack(spacing: 4) {
            Text(showtime.time)
                .font(.system(size: 12, weight: .semibold))
                .foregroundColor(.white)
            if !trimmedFormat.isEmpty {
                Text(trimmedFormat)
                    .font(.system(size: 9, weight: .medium))
                    .foregroundColor(.secondary)
            }
        }
        .padding(.horizontal, 7)
        .padding(.vertical, 4)
        .background(Color.white.opacity(0.08), in: RoundedRectangle(cornerRadius: 5))

        if let url = showtime.bookingURL {
            Link(destination: url) { body }
                .buttonStyle(.plain)
        } else {
            body
        }
    }
}
