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
            HStack {
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

}

/// Walking AsyncImage: tries `primary`, then every entry in `fallbacks`
/// in order on `.failure`. Mirrors `_movieCard`'s
/// `<img data-fallbacks=... onerror=...>` so cinema-side 4xxs walk
/// through other cinemas + TMDB + IMDb before "Brak plakatu" shows.
///
/// When the whole chain is exhausted (no URL loaded) we don't sit on
/// "Brak plakatu" forever — an exponential-backoff retry restarts
/// from the primary URL after 2s, 6s, 18s, 54s, 162s (then 162s
/// forever). Each retry bumps `generation`; that's both used as the
/// SwiftUI `.id(...)` to remount the AsyncImage subtree (forcing a
/// fresh load) AND stitched into the URL as `_kinowo_t=<n>` so
/// `URLCache` can't serve us back a stale failure. The cycle resets
/// to 2s on every `scenePhase == .active` transition — opening the
/// app or returning from background gives the cinema CDN one more
/// chance.
private struct PosterImage<NoPoster: View>: View {
    let primary: URL
    let fallbacks: [URL]
    @ViewBuilder var noPoster: () -> NoPoster

    @State private var index = 0
    @State private var generation = 0
    @State private var cycleAttempt = 0
    @State private var retryTask: Task<Void, Never>?
    @Environment(\.scenePhase) private var scenePhase

    var body: some View {
        // index 0 = primary; 1…N = fallbacks[i-1]. Out-of-bounds means
        // we've exhausted the chain and `.failure` should show
        // noPoster + schedule the next retry.
        let baseURL: URL? =
            index == 0 ? primary
            : (index - 1 < fallbacks.count ? fallbacks[index - 1] : nil)
        let url = baseURL.flatMap { withRetryToken($0, generation) }
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
                    Color.clear.onAppear { index += 1 }
                } else {
                    noPoster()
                        .onAppear { scheduleNextRetry() }
                }
            @unknown default:
                noPoster()
            }
        }
        .id(generation)
        .onChange(of: scenePhase) { phase in
            // Bringing the app to the foreground resets the backoff
            // clock so a flaky CDN gets a fresh attempt immediately,
            // not on the 64-second tail of an old cycle. Only fire the
            // restart when we're currently sitting on a failed chain —
            // a successfully loaded poster shouldn't be re-fetched.
            guard phase == .active, index > fallbacks.count else { return }
            retryTask?.cancel()
            retryTask = nil
            cycleAttempt = 0
            index = 0
            generation += 1
        }
        .onDisappear { retryTask?.cancel(); retryTask = nil }
    }

    private func scheduleNextRetry() {
        // `noPoster`'s onAppear can fire more than once (scroll
        // off+on, sibling state churn). Don't stack retry tasks.
        guard retryTask == nil else { return }
        let delaySeconds = RetryBackoff.seconds(forAttempt: cycleAttempt)
        retryTask = Task { @MainActor in
            try? await Task.sleep(nanoseconds: UInt64(delaySeconds) * 1_000_000_000)
            if Task.isCancelled { return }
            cycleAttempt += 1
            index = 0
            generation += 1
            retryTask = nil
        }
    }

    /// `URLCache` keys responses by URL identity, so a retry of the
    /// exact same URL can be served the cached failure. Stitching a
    /// monotonically-incrementing `_kinowo_t` query param onto every
    /// retry keeps the cinema CDN seeing the canonical URL (unknown
    /// params get ignored) while invalidating the local cache key.
    /// `generation == 0` is the first attempt — leave the URL alone
    /// so we don't pollute the CDN cache key on the happy path.
    private func withRetryToken(_ base: URL, _ gen: Int) -> URL? {
        guard gen > 0 else { return base }
        var c = URLComponents(url: base, resolvingAgainstBaseURL: false) ?? URLComponents()
        var items = c.queryItems ?? []
        items.append(URLQueryItem(name: "_kinowo_t", value: "\(gen)"))
        c.queryItems = items
        return c.url ?? base
    }

}
