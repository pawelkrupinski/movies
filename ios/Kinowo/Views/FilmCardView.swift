import SwiftUI

struct FilmCardView: View {
    let film: Film
    @EnvironmentObject var prefs: UserPreferences

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            PosterView(film: film)
            VStack(alignment: .leading, spacing: 8) {
                HStack(alignment: .firstTextBaseline, spacing: 6) {
                    Text(film.title)
                        .font(.system(size: 14, weight: .semibold))
                        .foregroundColor(.white)
                        .lineLimit(2)
                        .multilineTextAlignment(.leading)
                    Spacer(minLength: 0)
                    if let mins = film.runtimeMinutes, mins > 0 {
                        Text(formatRuntime(mins))
                            .font(.system(size: 11))
                            .foregroundColor(.secondary)
                    }
                }
                if !film.ratings.isEmpty {
                    RatingBadgesView(ratings: film.ratings)
                        // Padding lives BELOW the FlowLayout (rather
                        // than as VStack spacing or .padding(.top, …)
                        // on ShowingsView) because FlowLayout's last
                        // wrapped row was painting too close to the
                        // next sibling — adding clearance INSIDE the
                        // RatingBadgesView's frame guarantees the
                        // bottom of every pill is followed by ≥ 14 pt
                        // before any neighbour can paint, regardless
                        // of which row the last pill ended up on.
                        .padding(.bottom, 14)
                }
                ShowingsView(film: film)
            }
            .padding(12)
        }
        .background(Color(red: 0.12, green: 0.12, blue: 0.18))
        .clipShape(RoundedRectangle(cornerRadius: 12))
    }

    private func formatRuntime(_ mins: Int) -> String {
        let h = mins / 60, m = mins % 60
        if h == 0 { return "\(m)min" }
        if m == 0 { return "\(h)h" }
        return "\(h)h \(m)min"
    }
}

private struct PosterView: View {
    let film: Film
    @EnvironmentObject var prefs: UserPreferences

    var body: some View {
        ZStack(alignment: .topLeading) {
            poster
                .frame(maxWidth: .infinity)
                .aspectRatio(2.0/3.0, contentMode: .fit)
                .clipped()
            // Mirror the web layout: ★ on top-left (favourite,
            // `.fav-poster-btn`), X on top-right (hide, `.hide-btn`).
            HStack {
                favButton
                Spacer()
                hideButton
            }
            .padding(6)
        }
    }

    @ViewBuilder
    private var poster: some View {
        if let url = film.posterURL {
            AsyncImage(url: url) { phase in
                switch phase {
                case .success(let img):
                    img.resizable().aspectRatio(contentMode: .fill)
                case .empty:
                    Rectangle()
                        .fill(Color(red: 0.16, green: 0.16, blue: 0.24))
                        .overlay(ProgressView().tint(.gray))
                case .failure:
                    noPoster
                @unknown default:
                    noPoster
                }
            }
        } else {
            noPoster
        }
    }

    private var noPoster: some View {
        Rectangle()
            .fill(Color(red: 0.16, green: 0.16, blue: 0.24))
            .overlay(
                Text("Brak plakatu")
                    .font(.system(size: 12)).italic()
                    .foregroundColor(.secondary)
            )
    }

    private var hideButton: some View {
        Button {
            withAnimation { prefs.hide(film.title) }
        } label: {
            Image(systemName: "xmark")
                .font(.system(size: 10, weight: .bold))
                .foregroundColor(.white)
                .frame(width: 26, height: 26)
                .background(.black.opacity(0.55), in: Circle())
        }
        .buttonStyle(.plain)
    }

    private var favButton: some View {
        Button {
            prefs.toggleFavouriteMovie(film.title)
        } label: {
            Image(systemName: prefs.favouriteMovies.contains(film.title) ? "star.fill" : "star")
                .font(.system(size: 12, weight: .semibold))
                .foregroundColor(prefs.favouriteMovies.contains(film.title) ? .yellow : .white.opacity(0.85))
                .frame(width: 26, height: 26)
                .background(.black.opacity(0.55), in: Circle())
        }
        .buttonStyle(.plain)
    }
}
