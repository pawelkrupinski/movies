import SwiftUI

/// Per-film detail screen — iOS counterpart of `/film?title=…`. Mirrors
/// the web layout: poster on the left of the header row, title +
/// ratings + cinema-link buttons on the right, then Opis / Reżyseria /
/// Obsada / Zwiastuny meta blocks, then a `Seanse` section with the
/// full day-by-day showings tree.
///
/// Everything is built from data already in hand: the listing `Film`
/// (poster, ratings, runtime, showings, directors, cast — and cinema
/// links derived from the showings' `cinemaURL`s) plus the synopsis +
/// trailer URLs the launch-time `/api/details` fetch deposited in
/// `DetailsStore`. There's no per-screen fetch any more; if details
/// haven't landed for this title yet, the Film-derived parts render
/// immediately and synopsis/trailers simply stay hidden until the
/// details map populates.
struct FilmDetailView: View {
    let film: Film
    @EnvironmentObject var details: DetailsStore
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
    }

    private var filmDetails: FilmDetails? { details.details(for: film.title) }
    private var cinemaLinkList: [CinemaLink] { film.showings.cinemaLinks() }

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
                if !film.ratings.isEmpty {
                    RatingBadgesView(ratings: film.ratings)
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

    @ViewBuilder
    private var poster: some View {
        if let primary = film.posterURL {
            DetailPosterImage(
                primary: primary,
                fallbacks: film.fallbackPosterURLs,
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
        let links = cinemaLinkList
        if !links.isEmpty {
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
        // Director / cast come straight off the listing `Film`, so they
        // render immediately. Opis (synopsis) comes from the details
        // map — it's simply absent until `/api/details` lands, or for
        // films the backend has no synopsis for.
        VStack(alignment: .leading, spacing: 12) {
            metaBlock(label: "Opis",      value: filmDetails?.synopsis)
            metaBlock(label: "Reżyseria", value: joined(film.directors))
            metaBlock(label: "Obsada",    value: joined(film.cast))
        }
    }

    /// `nil` for an empty list so `metaBlock` omits the whole section
    /// (matches the web, which doesn't render an empty Reżyseria/Obsada).
    private func joined(_ values: [String]) -> String? {
        values.isEmpty ? nil : values.joined(separator: ", ")
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
        if let trailers = filmDetails?.trailerURLs, !trailers.isEmpty {
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
        if !film.showings.isEmpty {
            VStack(alignment: .leading, spacing: 12) {
                Text("Seanse")
                    .font(.system(size: 18, weight: .semibold))
                    .foregroundColor(.white)
                // The listing already carries the full showings tree, so
                // `ShowingsView` renders the same `Film` we were handed.
                ShowingsView(film: film)
            }
            .padding(.top, 4)
        }
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

private struct TrailerEmbedView: UIViewRepresentable {
    let url: URL

    final class Coordinator { var loadedURL: URL? }
    func makeCoordinator() -> Coordinator { Coordinator() }

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
        let embedURL = TrailerEmbedHTML.withAutoplay(url)
        guard context.coordinator.loadedURL != embedURL else { return }
        context.coordinator.loadedURL = embedURL
        webView.loadHTMLString(
            TrailerEmbedHTML.embedPage(videoURL: embedURL),
            baseURL: kinowoBaseURL
        )
    }
}
#else
private struct TrailerEmbedView: View {
    let url: URL
    var body: some View { Link("Otwórz zwiastun", destination: url) }
}
#endif
