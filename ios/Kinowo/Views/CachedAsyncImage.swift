import SwiftUI

/// Drop-in stand-in for SwiftUI's `AsyncImage(url:content:)` that pulls
/// bytes from `PosterStore` (disk-first, downloaded at most once) instead
/// of re-fetching every appearance the way `AsyncImage`/`URLCache` does.
///
/// It hands the caller the same `AsyncImagePhase` values — `.empty` while
/// loading, `.success` with the decoded image, `.failure` when the store
/// returns no bytes — so `FilmCardView.PosterImage`'s fallback/retry
/// `switch` reads identically whether it's backed by `AsyncImage` or this.
struct CachedAsyncImage<Content: View>: View {
    let url: URL?
    @ViewBuilder let content: (AsyncImagePhase) -> Content

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
        if let data = await PosterStore.shared.data(for: url),
           let image = UIImage(data: data) {
            phase = .success(Image(uiImage: image))
        } else {
            // Mirror AsyncImage's failure phase so PosterImage advances to
            // the next fallback / schedules a retry exactly as before.
            phase = .failure(URLError(.cannotDecodeContentData))
        }
    }
}
