import SwiftUI
import UIKit

struct FilmCardView: View {
    let film: Film
    /// Passed through to `ShowingsView`. Kina-tab section headers
    /// already name the cinema, so cards in that layout drop the
    /// duplicate label — see `CinemaSectionedGridView`.
    var showCinemaHeaders: Bool = true
    var truncatable: Bool = true
    @EnvironmentObject var prefs: UserPreferences
    /// Vertical gaps. Defaults to the shipping layout; the non-prod
    /// `ShowtimeTuningScreen` overrides it through the environment.
    @Environment(\.cardSpacingStyle) private var spacing

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // The poster owns the long-press share menu, NOT the whole card:
            // a card-wide `.contextMenu` swallows the showtime pills' own
            // long-press (their room tooltip), because both hold-gestures live
            // in the same subtree and the context menu wins. Scoping it to the
            // poster leaves the pills free. iOS has no address bar to copy from,
            // so this menu is where the canonical `/<city>/film?title=…` URL
            // surfaces — Udostępnij opens the system sheet, Skopiuj link drops it
            // on the pasteboard. The menu only appears once a city is selected
            // (always true behind the city gate), since the link is city-scoped.
            PosterView(film: film)
                .contextMenu {
                    if let citySlug = prefs.selectedCity {
                        ShareLink(item: FilmShareLink.url(forTitle: film.title, citySlug: citySlug), subject: Text(film.title)) {
                            Label("Udostępnij", systemImage: "square.and.arrow.up")
                        }
                        Button {
                            UIPasteboard.general.string = FilmShareLink.url(forTitle: film.title, citySlug: citySlug).absoluteString
                        } label: {
                            Label("Skopiuj link", systemImage: "link")
                        }
                    }
                }
            VStack(alignment: .leading, spacing: spacing.sectionSpacing) {
                Text(film.title)
                    .font(.system(size: 14, weight: .semibold))
                    .foregroundColor(.white)
                    .fixedSize(horizontal: false, vertical: true)
                    .multilineTextAlignment(.leading)
                    .frame(maxWidth: .infinity, alignment: .leading)
                // Runtime + year + the first three genres, mirroring the web
                // `_cardTitle` (`movie.genres.take(3)`); the detail screen
                // shows them all.
                MetaPillsView(
                    runtimeMinutes: film.runtimeMinutes,
                    releaseYear: film.releaseYear,
                    genres: Array(film.genres.prefix(3))
                )
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
                        .padding(.bottom, spacing.ratingsBottom)
                }
                ShowingsView(film: film, showCinemaHeaders: showCinemaHeaders, truncatable: truncatable)
            }
            .padding(12)
        }
        .background(Color(red: 0.12, green: 0.12, blue: 0.18))
        .clipShape(RoundedRectangle(cornerRadius: 12))
    }
}

/// Runtime / year / genre chips for a film's title block — the iOS
/// counterpart of `_movieCard` / `/film`'s `.pill.runtime`,
/// `.pill.year`, `.pill.genre` row. The year renders as plain text
/// (matching the web's `.year { background: transparent; border: none }`),
/// runtime and genres as capsule pills. Genres default to none; the
/// listing card passes the first three, the detail page passes them all.
/// Renders nothing when there's no runtime, year, or genre.
struct MetaPillsView: View {
    let runtimeMinutes: Int?
    let releaseYear: Int?
    var genres: [String] = []

    var body: some View {
        let runtime = (runtimeMinutes ?? 0) > 0 ? Self.formatRuntime(runtimeMinutes!) : nil
        let year = releaseYear.map(String.init)
        if runtime != nil || year != nil || !genres.isEmpty {
            // Bottom-align so the larger plain-text year shares a bottom
            // edge with the smaller pills.
            FlowLayout(spacing: 6, lineSpacing: 6, bottomAligned: true) {
                if let runtime { pill(runtime) }
                if let year { yearText(year) }
                ForEach(genres, id: \.self) { pill($0) }
            }
        }
    }

    private func pill(_ text: String) -> some View {
        Text(text)
            .font(.system(size: 11, weight: .medium))
            .foregroundColor(Color(white: 0.8))
            .padding(.horizontal, 8)
            .padding(.vertical, 3)
            .background(Color(red: 0.2, green: 0.2, blue: 0.28), in: Capsule())
    }

    /// Year as plain text, mirroring the web's `.year` (no pill
    /// background or border, dimmer `#888` ink). Rendered a touch larger
    /// than the pills; the flow row bottom-aligns items and the matching
    /// vertical padding keeps the year's text bottom flush with the
    /// pilled text bottom.
    private func yearText(_ text: String) -> some View {
        Text(text)
            .font(.system(size: 13, weight: .medium))
            .foregroundColor(Color(white: 0.53))
            .padding(.vertical, 3)
    }

    static func formatRuntime(_ totalMinutes: Int) -> String {
        let hours = totalMinutes / 60, minutes = totalMinutes % 60
        if hours == 0 { return "\(minutes)min" }
        if minutes == 0 { return "\(hours)h" }
        return "\(hours)h \(minutes)min"
    }
}

private struct PosterView: View {
    let film: Film
    @EnvironmentObject var prefs: UserPreferences

    var body: some View {
        ZStack(alignment: .topLeading) {
            // The 2:3 box is defined by a zero-intrinsic-size `Color.clear`,
            // NOT by the poster itself. A wide/landscape source image
            // (e.g. the "Kino bez barier" banner) under `.aspectRatio(_,
            // .fit)` reported a frame WIDER than its grid column and
            // painted over the neighbouring card — `.clipped()` never
            // reined the width in because the `.fill` image was the thing
            // sizing the frame. `Color.clear` can't be driven past the
            // proposed column width, so the box is always exactly one
            // column wide; the poster fills it via `.overlay` (which can't
            // affect the parent's size) and the overflow is clipped.
            Color.clear
                .frame(maxWidth: .infinity)
                .aspectRatio(2.0/3.0, contentMode: .fit)
                .overlay { poster }
                .clipped()
                // `.clipped()` only masks the *render*: a `.fill` image
                // keeps its oversized layout/hit/accessibility bounds, so
                // a wide poster's tap target (and the VoiceOver element)
                // still spilled past the column into the next card.
                // `contentShape` reins the hit region back to the clipped
                // box; the image carries no information the title text
                // doesn't already, so drop it from the a11y tree.
                .contentShape(Rectangle())
                .accessibilityHidden(true)
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
/// forever). Each retry bumps `generation`, used as the SwiftUI
/// `.id(...)` to remount the subtree and force a fresh load: the
/// remount re-runs `CachedAsyncImage`'s `.task`, and `PosterStore`
/// never caches a failure, so the URL is genuinely re-fetched. The
/// cycle resets to 2s on every `scenePhase == .active` transition —
/// opening the app or returning from background gives the cinema CDN
/// one more chance.
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
        // `CachedAsyncImage` (not `AsyncImage`) so a poster downloads once and
        // is served from `PosterStore`'s on-disk cache thereafter; it emits the
        // same `AsyncImagePhase` values, so the fallback-walk and backoff-retry
        // logic below is unchanged. A retry remounts this view via
        // `.id(generation)`, which re-runs the load — no cache-busting URL
        // token needed (`PosterStore` bypasses `URLCache` and never caches a
        // failure).
        CachedAsyncImage(url: baseURL) { phase in
            switch phase {
            case .success(let image):
                image.resizable().aspectRatio(contentMode: .fill)
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

}
