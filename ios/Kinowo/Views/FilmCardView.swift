import SwiftUI

struct FilmCardView: View {
    let film: Film
    /// Passed through to `ShowingsView`. Kina-tab section headers
    /// already name the cinema, so cards in that layout drop the
    /// duplicate label — see `CinemaSectionedGridView`.
    var showCinemaHeaders: Bool = true
    @EnvironmentObject var prefs: UserPreferences

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            PosterView(film: film)
            VStack(alignment: .leading, spacing: 8) {
                HStack(alignment: .firstTextBaseline, spacing: 6) {
                    Text(film.title)
                        .font(.system(size: 14, weight: .semibold))
                        .foregroundColor(.white)
                        .fixedSize(horizontal: false, vertical: true)
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
                ShowingsView(film: film, showCinemaHeaders: showCinemaHeaders, collapsible: true)
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
            // Mirror the web's `<img data-fallbacks=... onerror=...>` from
            // _movieCard: try the primary URL first, then walk through
            // every non-primary cinema/TMDB/IMDb poster on .failure.
            // AsyncImage has no built-in fallback so we own a `@State`
            // index that advances after each error.
            PosterImage(primary: url, fallbacks: film.fallbackPosterURLs, noPoster: { noPoster })
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

/// Walking AsyncImage: tries `primary`, then every entry in `fallbacks`
/// in order on `.failure`. Mirrors `_movieCard`'s
/// `<img data-fallbacks=... onerror=...>` so cinema-side 4xxs walk
/// through other cinemas + TMDB + IMDb before "Brak plakatu" shows.
private struct PosterImage<NoPoster: View>: View {
    let primary: URL
    let fallbacks: [URL]
    @ViewBuilder var noPoster: () -> NoPoster
    @State private var index = 0

    var body: some View {
        // index 0 = primary; 1…N = fallbacks[i-1]. Out-of-bounds means
        // we've exhausted the chain and `.failure` should show noPoster.
        let url: URL? =
            index == 0 ? primary
            : (index - 1 < fallbacks.count ? fallbacks[index - 1] : nil)
        AsyncImage(url: url) { phase in
            switch phase {
            case .success(let img):
                img.resizable().aspectRatio(contentMode: .fill)
            case .empty:
                Rectangle()
                    .fill(Color(red: 0.16, green: 0.16, blue: 0.24))
                    .overlay(ProgressView().tint(.gray))
            case .failure:
                if index <= fallbacks.count - 1 {
                    // Advance to the next URL in the chain. SwiftUI
                    // re-keys AsyncImage on the new URL and kicks off
                    // the next fetch. `index - 1 < fallbacks.count` is
                    // still true after the bump for indices 0…N-1.
                    Color.clear.onAppear { index += 1 }
                } else {
                    noPoster()
                }
            @unknown default:
                noPoster()
            }
        }
    }
}
