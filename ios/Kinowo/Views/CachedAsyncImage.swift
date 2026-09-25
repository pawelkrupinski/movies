import SwiftUI

/// Drop-in stand-in for SwiftUI's `AsyncImage(url:content:)` that pulls
/// bytes from `PosterStore` (disk-first, downloaded at most once) instead
/// of re-fetching every appearance the way `AsyncImage`/`URLCache` does.
///
/// It hands the caller the same `AsyncImagePhase` values — `.empty` while
/// loading, `.success` with the decoded image, `.failure` when the store
/// returns no bytes — so `PosterChainImage`'s fallback/retry
/// `switch` reads identically whether it's backed by `AsyncImage` or this.
///
/// The store is the composition root's (`KinowoApp`), read from the
/// environment rather than a global, so the whole view tree shares the one
/// instance the daily purge also runs against.
struct CachedAsyncImage<Content: View>: View {
    let url: URL?
    @ViewBuilder let content: (AsyncImagePhase) -> Content

    @Environment(\.posterStore) private var posters
    @State private var phase: AsyncImagePhase = .empty

    var body: some View {
        content(phase)
            // Re-run whenever the URL changes: the fallback walk swaps
            // `url` to the next candidate, and a retry remounts us via
            // `.id(generation)` — both restart the load.
            .task(id: url) { await load() }
    }

    private func load() async {
        phase = .empty
        guard let url else { return }
        if let data = await posters?.data(for: url),
           let image = UIImage(data: data) {
            phase = .success(Image(uiImage: image))
        } else {
            // Mirror AsyncImage's failure phase so PosterChainImage advances to
            // the next fallback / schedules a retry exactly as before.
            phase = .failure(URLError(.cannotDecodeContentData))
        }
    }
}

/// The app's one `PosterStore`, installed at the root by `KinowoApp`. No
/// default instance: a tree it wasn't installed in (a preview) has no cache,
/// and its images take the failure phase — the "no poster" placeholder —
/// rather than silently reaching for a second, process-wide store.
private struct PosterStoreKey: EnvironmentKey {
    static let defaultValue: PosterStore? = nil
}

extension EnvironmentValues {
    var posterStore: PosterStore? {
        get { self[PosterStoreKey.self] }
        set { self[PosterStoreKey.self] = newValue }
    }
}
