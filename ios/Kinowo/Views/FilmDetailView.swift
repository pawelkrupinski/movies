import SwiftUI

/// Per-film detail screen — iOS counterpart of `/film?title=…`. Mirrors
/// the web layout: poster on the left of the header row, title +
/// ratings + cinema-link buttons on the right, then Opis / Reżyseria /
/// Obsada / Zwiastuny meta blocks, then a `Seanse` section with the
/// full day-by-day showings tree.
///
/// `film` is the listing row that fed the navigation — used to render
/// the poster + title + ratings + showings immediately while the
/// detail HTML is still loading (so the screen never blinks empty).
/// Once `FilmDetailStore` finishes the fetch, detail-only fields
/// (synopsis, director, cast, cinema-links, trailers, full-week
/// showings) replace the placeholder.
struct FilmDetailView: View {
    let film: Film
    @StateObject private var store = FilmDetailStore()
    @EnvironmentObject var prefs: UserPreferences
    @State private var playingTrailerIndex: Int? = nil

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                header
                metaBlocks
                trailersSection
                showingsSection
            }
            .padding(.horizontal, 16)
            .padding(.bottom, 24)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .ignoresSafeArea(edges: [.bottom, .horizontal])
        .background(Color(red: 0.067, green: 0.067, blue: 0.067).ignoresSafeArea())
        .navigationTitle(film.title)
        .navigationBarTitleDisplayMode(.inline)
        .task {
            // One fetch per appearance. Re-runs on navigate-back-and-in
            // because the @StateObject is recreated alongside the view.
            if store.detail == nil { await store.load(title: film.title) }
        }
    }

    private var detail: FilmDetail? { store.detail }

    // MARK: – header (poster + title + ratings + cinema-links)

    @ViewBuilder
    private var header: some View {
        // Size the poster to match a listing card's poster on the same
        // device — the user's mental model is "tapping a card opens
        // the same film bigger", so the poster shouldn't shrink on
        // arrival. 2:3 aspect carries the height; no maxHeight cap.
        let width = Self.listingCardPosterWidth
        HStack(alignment: .top, spacing: 16) {
            poster
                .frame(width: width, height: width * 1.5)
                .clipShape(RoundedRectangle(cornerRadius: 12))
            VStack(alignment: .leading, spacing: 8) {
                Text(film.title)
                    .font(.system(size: 22, weight: .bold))
                    .foregroundColor(.white)
                    .fixedSize(horizontal: false, vertical: true)
                if !ratingsForDisplay.isEmpty {
                    RatingBadgesView(ratings: ratingsForDisplay)
                }
                cinemaLinks
            }
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .padding(.top, 8)
    }

    /// Width a poster occupies in the listing's `FilmGridView`. The
    /// grid uses `GridItem(.adaptive(minimum: 160, maximum: 220),
    /// spacing: 12)` inside `.padding(.horizontal, 12)`, so on phones
    /// (always 2 columns) each card is `(screen − 24 − 12) / 2`
    /// clamped to [160, 220]. Computed once at view-construction
    /// time; the detail screen doesn't need to react to orientation
    /// changes finer than that.
    private static var listingCardPosterWidth: CGFloat {
        let screenWidth = UIScreen.main.bounds.width
        let columnWidth = (screenWidth - 24 - 12) / 2
        return min(220, max(160, columnWidth))
    }

    private var ratingsForDisplay: Film.Ratings {
        // Prefer the detail-page ratings once the fetch lands — it can
        // include URLs the listing didn't emit (the listing only
        // attaches an href when the score is non-null on its side).
        // Fall back to the listing's so the badges paint immediately.
        if let d = detail, !d.ratings.isEmpty { return d.ratings }
        return film.ratings
    }

    @ViewBuilder
    private var poster: some View {
        if let primary = detail?.posterURL ?? film.posterURL {
            DetailPosterImage(
                primary: primary,
                fallbacks: (detail?.fallbackPosterURLs.isEmpty == false
                                ? detail!.fallbackPosterURLs
                                : film.fallbackPosterURLs),
                noPoster: { noPosterPlaceholder }
            )
        } else {
            noPosterPlaceholder
        }
    }

    private var noPosterPlaceholder: some View {
        Rectangle()
            .fill(Color(red: 0.165, green: 0.165, blue: 0.243))
            .aspectRatio(2.0/3.0, contentMode: .fit)
            .overlay(
                Text("Brak plakatu")
                    .font(.system(size: 12)).italic()
                    .foregroundColor(.secondary)
            )
    }

    @ViewBuilder
    private var cinemaLinks: some View {
        if let links = detail?.cinemaLinks, !links.isEmpty {
            FlowLayout(spacing: 6, lineSpacing: 6) {
                ForEach(links, id: \.url) { link in
                    Link(destination: link.url) {
                        Text("\(link.cinema) ↗")
                            .font(.system(size: 12, weight: .medium))
                            .foregroundColor(Color(red: 0.667, green: 0.831, blue: 1.0))
                            .padding(.horizontal, 10)
                            .padding(.vertical, 5)
                            .background(
                                Color(red: 0.227, green: 0.227, blue: 0.431),
                                in: RoundedRectangle(cornerRadius: 8)
                            )
                    }
                    .buttonStyle(.plain)
                }
            }
            .padding(.top, 2)
        }
    }

    // MARK: – Opis / Reżyseria / Obsada

    @ViewBuilder
    private var metaBlocks: some View {
        if let d = detail {
            VStack(alignment: .leading, spacing: 12) {
                metaBlock(label: "Opis",      value: d.synopsis)
                metaBlock(label: "Reżyseria", value: d.director)
                metaBlock(label: "Obsada",    value: d.cast)
            }
        } else if store.isLoading {
            HStack(spacing: 8) {
                ProgressView()
                Text("Ładowanie szczegółów…").foregroundStyle(.secondary).font(.callout)
            }
            .padding(.vertical, 8)
        } else if let err = store.error {
            VStack(alignment: .leading, spacing: 4) {
                Text("Nie udało się pobrać szczegółów.")
                    .foregroundStyle(.orange)
                    .font(.callout)
                Text(err.localizedDescription)
                    .foregroundStyle(.secondary)
                    .font(.caption)
            }
        }
    }

    @ViewBuilder
    private func metaBlock(label: String, value: String?) -> some View {
        if let value, !value.isEmpty {
            VStack(alignment: .leading, spacing: 4) {
                Text(label.uppercased())
                    .font(.system(size: 11, weight: .semibold))
                    .foregroundColor(.secondary)
                    .tracking(0.6)
                Text(value)
                    .font(.system(size: 14))
                    .foregroundColor(Color(white: 0.87))
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
    }

    // MARK: – Zwiastuny

    @ViewBuilder
    private var trailersSection: some View {
        if let trailers = detail?.trailerURLs, !trailers.isEmpty {
            VStack(alignment: .leading, spacing: 8) {
                Text("ZWIASTUNY")
                    .font(.system(size: 11, weight: .semibold))
                    .foregroundColor(.secondary)
                    .tracking(0.6)
                FlowLayout(spacing: 8, lineSpacing: 8) {
                    ForEach(Array(trailers.enumerated()), id: \.offset) { (idx, url) in
                        trailerButton(idx: idx, url: url)
                    }
                }
                if let active = playingTrailerIndex, trailers.indices.contains(active) {
                    TrailerEmbedView(url: trailers[active])
                        .aspectRatio(16.0/9.0, contentMode: .fit)
                        .clipShape(RoundedRectangle(cornerRadius: 12))
                }
            }
        }
    }

    @ViewBuilder
    private func trailerButton(idx: Int, url: URL) -> some View {
        let active = playingTrailerIndex == idx
        Button {
            // Re-tapping the active trailer toggles it off (matches the
            // web's playTrailer behaviour: clear src + hide frame).
            playingTrailerIndex = active ? nil : idx
        } label: {
            Text("Zwiastun \(idx + 1)")
                .font(.system(size: 13, weight: .medium))
                .foregroundColor(active ? Color(red: 0.10, green: 0.10, blue: 0.18) : Color(red: 0.667, green: 0.831, blue: 1.0))
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(
                    active ? Color(red: 0.667, green: 0.831, blue: 1.0)
                           : Color(red: 0.227, green: 0.227, blue: 0.431),
                    in: RoundedRectangle(cornerRadius: 8)
                )
        }
        .buttonStyle(.plain)
    }

    // MARK: – Seanse

    @ViewBuilder
    private var showingsSection: some View {
        let days = detail?.showings.isEmpty == false ? detail!.showings : film.showings
        if !days.isEmpty {
            VStack(alignment: .leading, spacing: 12) {
                Text("Seanse")
                    .font(.system(size: 18, weight: .semibold))
                    .foregroundColor(.white)
                ShowingsView(film: filmForShowings(days: days))
            }
            .padding(.top, 4)
        }
    }

    /// `ShowingsView` consumes a `Film`. Build a thin synthetic Film
    /// from whichever set of `days` we ended up rendering — the listing
    /// row's showings before the fetch resolves, the full /film tree
    /// after.
    private func filmForShowings(days: [DayShowings]) -> Film {
        Film(
            title: film.title,
            posterURL: film.posterURL,
            fallbackPosterURLs: film.fallbackPosterURLs,
            runtimeMinutes: film.runtimeMinutes,
            ratings: film.ratings,
            countries: film.countries,
            directors: film.directors,
            cast: film.cast,
            showings: days
        )
    }
}

/// Same walking-AsyncImage as `FilmCardView.PosterImage` but sized for
/// the detail-page header (large frame, contentMode .fit). Kept as a
/// separate type so the listing card's variant can keep its
/// aspect-ratio-fit clipping behaviour without leaking sizing
/// assumptions across pages.
private struct DetailPosterImage<NoPoster: View>: View {
    let primary: URL
    let fallbacks: [URL]
    @ViewBuilder var noPoster: () -> NoPoster
    @State private var index = 0

    var body: some View {
        let url: URL? =
            index == 0 ? primary
            : (index - 1 < fallbacks.count ? fallbacks[index - 1] : nil)
        AsyncImage(url: url) { phase in
            switch phase {
            case .success(let img):
                img.resizable().aspectRatio(contentMode: .fit)
            case .empty:
                Rectangle()
                    .fill(Color(red: 0.165, green: 0.165, blue: 0.243))
                    .aspectRatio(2.0/3.0, contentMode: .fit)
                    .overlay(ProgressView().tint(.gray))
            case .failure:
                if index <= fallbacks.count - 1 {
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

#if canImport(WebKit)
import WebKit

/// Lightweight WKWebView wrapper for an embedded YouTube trailer.
/// SwiftUI's `VideoPlayer` doesn't speak the `/embed/<id>` URL the
/// web template hands us, and the embed page is the simplest
/// cross-platform way to play arbitrary `youtube.com` / `youtu.be`
/// IDs without an API key.
private struct TrailerEmbedView: UIViewRepresentable {
    let url: URL

    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.allowsInlineMediaPlayback = true
        config.mediaTypesRequiringUserActionForPlayback = []
        let view = WKWebView(frame: .zero, configuration: config)
        view.backgroundColor = .black
        view.isOpaque = false
        view.scrollView.isScrollEnabled = false
        return view
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        // Append `autoplay=1` (matches the web's playTrailer) so tapping
        // the trailer pill starts playback without a second tap inside
        // the player chrome. YouTube's mobile embed honours the param.
        var components = URLComponents(url: url, resolvingAgainstBaseURL: false)
        var items = components?.queryItems ?? []
        if !items.contains(where: { $0.name == "autoplay" }) {
            items.append(URLQueryItem(name: "autoplay", value: "1"))
            items.append(URLQueryItem(name: "playsinline", value: "1"))
            components?.queryItems = items
        }
        let final = components?.url ?? url
        if webView.url != final {
            webView.load(URLRequest(url: final))
        }
    }
}
#else
private struct TrailerEmbedView: View {
    let url: URL
    var body: some View { Link("Otwórz zwiastun", destination: url) }
}
#endif
