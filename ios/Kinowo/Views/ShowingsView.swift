import SwiftUI

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
    /// cinema header is 1, every `pillsPerLine` showtimes ≈ 1 row of
    /// pills. 20 keeps a typical card under one screen height; tune
    /// here without touching the truncation logic.
    var maxCollapsedLines: Int = 20
    /// Approximate showtime pills that fit on one row inside a grid
    /// card (~155pt content width ÷ ~55pt average pill). Used only by
    /// the line-count estimator; underestimate is safer (truncates
    /// sooner, so the cap holds) than overestimate.
    private let pillsPerLine: Int = 3
    @State private var isExpanded: Bool = false

    var body: some View {
        let allDays = film.showings
        let (visibleDays, hiddenShowtimes) = collapsed(allDays)
        let canCollapse = collapsible && hiddenShowtimes > 0
        let days = (canCollapse && !isExpanded) ? visibleDays : allDays

        VStack(alignment: .leading, spacing: 6) {
            ForEach(days, id: \.date) { day in
                Text(day.label)
                    .font(.system(size: 10))
                    .foregroundColor(.secondary)
                    .padding(.top, 4)
                ForEach(day.cinemas, id: \.cinema) { cinema in
                    VStack(alignment: .leading, spacing: 4) {
                        if showCinemaHeaders {
                            cinemaLabel(cinema)
                        }
                        FlowLayout(spacing: 4, lineSpacing: 4) {
                            ForEach(cinema.showtimes) { st in
                                ShowtimeBadge(film: film, cinema: cinema.cinema, day: day, showtime: st)
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
    private func collapsed(_ allDays: [DayShowings]) -> (visible: [DayShowings], hidden: Int) {
        var lineCount = 0
        var visibleDays: [DayShowings] = []
        var hidden = 0

        for day in allDays {
            var keptCinemas: [CinemaShowings] = []
            var dayLines = 1  // the date label itself
            for cinema in day.cinemas {
                let pillRows = max(1, (cinema.showtimes.count + pillsPerLine - 1) / pillsPerLine)
                let cinemaLines = 1 + pillRows  // cinema label + pill rows
                if lineCount + dayLines + cinemaLines <= maxCollapsedLines {
                    keptCinemas.append(cinema)
                    dayLines += cinemaLines
                } else {
                    hidden += cinema.showtimes.count
                }
            }
            if keptCinemas.isEmpty {
                // The cap is already saturated; everything from here
                // on goes into `hidden` (including this whole day).
                continue
            }
            visibleDays.append(DayShowings(date: day.date, label: day.label, cinemas: keptCinemas))
            lineCount += dayLines
        }
        return (visibleDays, hidden)
    }

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
    let film: Film
    let cinema: String
    let day: DayShowings
    let showtime: Showtime
    @EnvironmentObject var prefs: UserPreferences

    var body: some View {
        let id = UserPreferences.screeningId(
            title: film.title, cinema: cinema, date: day.date, time: showtime.time
        )
        let isFav = prefs.favouriteScreenings.contains(id)
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
            Button {
                prefs.toggleFavouriteScreening(id)
            } label: {
                Image(systemName: isFav ? "star.fill" : "star")
                    .font(.system(size: 9))
                    .foregroundColor(isFav ? .yellow : .gray.opacity(0.55))
            }
            .buttonStyle(.plain)
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
