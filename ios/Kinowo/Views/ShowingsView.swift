import SwiftUI

struct ShowingsView: View {
    let film: Film

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            ForEach(film.showings, id: \.date) { day in
                Text(day.label)
                    .font(.system(size: 10))
                    .foregroundColor(.secondary)
                    .padding(.top, 4)
                ForEach(day.cinemas, id: \.cinema) { cinema in
                    VStack(alignment: .leading, spacing: 4) {
                        cinemaLabel(cinema)
                        FlowLayout(spacing: 4, lineSpacing: 4) {
                            ForEach(cinema.showtimes) { st in
                                ShowtimeBadge(film: film, cinema: cinema.cinema, day: day, showtime: st)
                            }
                        }
                    }
                }
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
