import SwiftUI

struct ShowingsView: View {
    let film: Film
    /// When the parent already announces the cinema (Kina tab's
    /// per-cinema section header), suppress the per-card cinema label
    /// to avoid duplication. Mirrors the web's `_filmShowings`
    /// `showCinemaHeaders` flag, which `_cinemaCards` flips to false.
    var showCinemaHeaders: Bool = true
    /// When true, only the first `collapsedDayCount` days render and
    /// the rest sit behind an expand toggle. Listing cards
    /// (`FilmCardView`) pass true so a daily-for-two-weeks film
    /// doesn't take a screen-and-a-half on the grid. The /film detail
    /// screen leaves this false so it always shows the full schedule.
    var collapsible: Bool = false
    /// How many days to show when collapsed. Two is a sweet spot:
    /// "today + tomorrow" reads as the same shape the Dziś / Jutro
    /// filters surface, so the collapsed view doesn't look truncated
    /// — just naturally focused on the near term.
    var collapsedDayCount: Int = 2
    @State private var isExpanded: Bool = false

    var body: some View {
        let allDays = film.showings
        let canCollapse = collapsible && allDays.count > collapsedDayCount
        let visibleDays = (canCollapse && !isExpanded)
            ? Array(allDays.prefix(collapsedDayCount))
            : allDays

        VStack(alignment: .leading, spacing: 6) {
            ForEach(visibleDays, id: \.date) { day in
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
                expandToggleButton(hiddenDayCount: allDays.count - collapsedDayCount)
            }
        }
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

    /// "… +N dni" / "… zwiń" toggle that sits at the bottom of the
    /// showings list when the card has more days than fit. Uses the
    /// same blue as the cinema label so it reads as part of the
    /// showings rail rather than chrome from elsewhere.
    @ViewBuilder
    private func expandToggleButton(hiddenDayCount: Int) -> some View {
        let labelColor = Color(red: 0.40, green: 0.67, blue: 0.87)
        Button {
            withAnimation(.easeInOut(duration: 0.15)) { isExpanded.toggle() }
        } label: {
            HStack(spacing: 4) {
                Image(systemName: isExpanded ? "chevron.up" : "ellipsis")
                    .font(.system(size: 9, weight: .semibold))
                Text(isExpanded ? "zwiń" : "+\(hiddenDayCount) \(dayCountLabel(hiddenDayCount))")
                    .font(.system(size: 11, weight: .medium))
            }
            .foregroundColor(labelColor)
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            // Subtle background so the toggle reads as a button, not
            // floating text — same dark fill as the showtime badge.
            .background(Color.white.opacity(0.06), in: Capsule())
        }
        .buttonStyle(.plain)
        .padding(.top, 6)
    }

    /// Polish plural for "day". 1 → dzień, 2+ → dni (the secondary
    /// 5+ form is also "dni", so two-bucket logic is enough).
    private func dayCountLabel(_ n: Int) -> String {
        n == 1 ? "dzień" : "dni"
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
