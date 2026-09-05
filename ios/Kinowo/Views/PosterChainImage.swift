import SwiftUI

/// The one poster loader in the app: walks a film's poster chain and
/// serves every hop from `PosterStore`'s on-disk cache.
///
/// Mirrors the web's `<img data-fallbacks=… onerror=…>` from `_movieCard`
/// — try `primary`, then each entry of `fallbacks` in order on `.failure`
/// — so cinema-side 4xxs walk through other cinemas + TMDB + IMDb before
/// "Brak plakatu" shows.
///
/// It exists because the listing card and the detail header used to carry
/// two copies of that walk, and only the card's copy went through
/// `PosterStore`: the detail header called `AsyncImage` directly, so it
/// re-downloaded a poster the app already had on disk. The user-visible
/// result was a film whose card rendered fine while its detail screen said
/// "Brak plakatu" — the card was serving cached bytes, the detail screen
/// was hitting an origin that happened to be refusing it
/// (`Minimaraton: Spider-Man`, a Multikino-primary film, 2026-09-05).
/// One loader for both screens means the detail header can only be blank
/// when the card is too.
///
/// When the whole chain is exhausted we don't sit on "Brak plakatu"
/// forever — an exponential-backoff retry restarts from the primary URL
/// after 2s, 6s, 18s, 54s, 162s (then 162s forever). Each retry bumps
/// `generation`, used as the SwiftUI `.id(…)` to remount the subtree and
/// force a fresh load: the remount re-runs `CachedAsyncImage`'s `.task`,
/// and `PosterStore` never caches a failure, so the URL is genuinely
/// re-fetched. The cycle resets on every `scenePhase == .active`
/// transition — opening the app or returning from background gives the
/// cinema CDN one more chance.
struct PosterChainImage<Loading: View, NoPoster: View>: View {
    let primary: URL
    let fallbacks: [URL]
    /// `.fill` for the listing card (which clips to a fixed frame),
    /// `.fit` for the detail header and the full-screen viewer.
    let contentMode: ContentMode
    @ViewBuilder var loading: () -> Loading
    @ViewBuilder var noPoster: () -> NoPoster

    /// 0 = primary; 1…N = `fallbacks[index - 1]`.
    @State private var index = 0
    /// True once the chain ran out with nothing loaded. Tracked
    /// explicitly rather than inferred from `index`, which stops on the
    /// LAST fallback whether that one loaded or failed.
    @State private var exhausted = false
    @State private var generation = 0
    @State private var cycleAttempt = 0
    @State private var retryTask: Task<Void, Never>?
    @Environment(\.scenePhase) private var scenePhase

    var body: some View {
        // Walking past the last fallback can only mean the chain was
        // re-pointed under us (see the `.onChange` below) — treat it as the
        // primary rather than handing `CachedAsyncImage` a nil URL, which
        // would park the view in `.empty` forever with nothing to retry it.
        let baseURL: URL =
            index == 0 || index - 1 >= fallbacks.count ? primary
            : fallbacks[index - 1]
        // `CachedAsyncImage` (not `AsyncImage`) so a poster downloads once
        // and is served from `PosterStore`'s on-disk cache thereafter; it
        // emits the same `AsyncImagePhase` values, so the fallback-walk and
        // backoff-retry logic below reads the same either way. A retry
        // remounts this view via `.id(generation)`, which re-runs the load —
        // no cache-busting URL token needed (`PosterStore` bypasses
        // `URLCache` and never caches a failure).
        CachedAsyncImage(url: baseURL) { phase in
            switch phase {
            case .success(let image):
                image.resizable()
                    .aspectRatio(contentMode: contentMode)
                    .accessibilityIdentifier(A11y.Poster.loaded)
            case .empty:
                loading()
            case .failure:
                if index < fallbacks.count {
                    Color.clear.onAppear { index += 1 }
                } else {
                    noPoster().onAppear {
                        exhausted = true
                        scheduleNextRetry()
                    }
                }
            @unknown default:
                noPoster()
            }
        }
        .id(generation)
        .onChange(of: scenePhase) { phase in
            // Bringing the app to the foreground resets the backoff clock so
            // a flaky CDN gets a fresh attempt immediately, not on the tail
            // of an old cycle. Only fire the restart when we're currently
            // sitting on a failed chain — a successfully loaded poster
            // shouldn't be re-fetched.
            guard phase == .active, exhausted else { return }
            restartFromPrimary()
        }
        // A card's `@State` is keyed to `Film.id` (the title), so a
        // repertoire refresh can hand the same view a different poster chain
        // — a re-enriched film swapping its primary artwork, or dropping a
        // fallback. Start the new chain from its own primary instead of
        // resuming at an index that belonged to the old one.
        .onChange(of: primary) { _ in restartFromPrimary() }
        .onChange(of: fallbacks) { _ in restartFromPrimary() }
        .onDisappear { retryTask?.cancel(); retryTask = nil }
    }

    private func scheduleNextRetry() {
        // `noPoster`'s onAppear can fire more than once (scroll off+on,
        // sibling state churn). Don't stack retry tasks.
        guard retryTask == nil else { return }
        let delaySeconds = RetryBackoff.seconds(forAttempt: cycleAttempt)
        retryTask = Task { @MainActor in
            try? await Task.sleep(nanoseconds: UInt64(delaySeconds) * 1_000_000_000)
            if Task.isCancelled { return }
            cycleAttempt += 1
            restartFromPrimary(cancelPendingRetry: false)
            retryTask = nil
        }
    }

    private func restartFromPrimary(cancelPendingRetry: Bool = true) {
        if cancelPendingRetry {
            retryTask?.cancel()
            retryTask = nil
            cycleAttempt = 0
        }
        index = 0
        exhausted = false
        generation += 1
    }
}
