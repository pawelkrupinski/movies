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
    // The share link is city-scoped (`/<city>/film?title=…`), so the detail
    // screen needs the selected-city slug the same way the listing card does.
    @EnvironmentObject var prefs: UserPreferences
    @Environment(\.filmDetailStyle) private var style
    @State private var playingTrailerIndex: Int? = nil
    @State private var showFullScreenPoster = false

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: style.sectionSpacing) {
                header
                metaBlocks
                trailersSection
                showingsSection
            }
            .padding(.horizontal, 16)
            .padding(.bottom, 24)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        // Ignore only the BOTTOM safe area (content scrolls under the home
        // indicator) — NOT horizontal. In landscape the horizontal inset is the
        // Dynamic Island / rounded-corner region; ignoring it ran the poster,
        // synopsis and showtimes under the island. Respecting it keeps the
        // detail content clear of the island while the background below still
        // bleeds edge-to-edge, so the leftover margin stays dark. No-op in
        // portrait (horizontal inset is 0). Guarded by
        // RotationColumnsUITests.testFilmDetailStaysClearOfDynamicIslandInLandscape.
        .ignoresSafeArea(edges: .bottom)
        .background(Color(red: 0.067, green: 0.067, blue: 0.067).ignoresSafeArea())
        .fullScreenCover(isPresented: $showFullScreenPoster) {
            if let primary = film.posterURL {
                FullScreenPosterView(
                    primary: primary,
                    fallbacks: film.fallbackPosterURLs,
                    onClose: { showFullScreenPoster = false }
                )
            }
        }
        .navigationTitle(film.title)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            if let citySlug = prefs.selectedCity {
                ToolbarItem(placement: .navigationBarTrailing) {
                    // Shares the canonical `/<city>/film?title=…` link — same URL
                    // a user would copy from the website's address bar.
                    ShareLink(item: FilmShareLink.url(forTitle: film.title, citySlug: citySlug), subject: Text(film.title)) {
                        Image(systemName: "square.and.arrow.up")
                    }
                }
            }
        }
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
            // A Button (not a bare tap gesture) so the poster is always a
            // queryable accessibility element — even while the AsyncImage is
            // still loading its placeholder — and VoiceOver announces it.
            // The long-press is the platform-conventional "peek"; the tap the
            // obvious one. Both open the same full-screen cover.
            Button { openFullScreenPoster() } label: {
                poster
                    .frame(width: width, height: width * 1.5)
                    .clipShape(RoundedRectangle(cornerRadius: 12))
            }
            .buttonStyle(.plain)
            .accessibilityIdentifier(A11y.FilmDetail.poster)
            .simultaneousGesture(LongPressGesture().onEnded { _ in openFullScreenPoster() })
            VStack(alignment: .leading, spacing: style.headerColumnSpacing) {
                Text(film.title)
                    .font(.system(size: style.titleFontSize, weight: style.titleWeight))
                    .foregroundColor(.white)
                    .fixedSize(horizontal: false, vertical: true)
                    .accessibilityIdentifier(A11y.Tuning.detailTitle)
                // Original (production-language) title, when the backend
                // reports one distinct from the Polish cinema title.
                if let original = filmDetails?.originalTitle, !original.isEmpty {
                    Text(original)
                        .font(.system(size: style.originalTitleFontSize))
                        .italic()
                        .foregroundColor(.secondary)
                        .fixedSize(horizontal: false, vertical: true)
                }
                // Runtime / year / genre pills — the `/film` title block
                // shows every genre (no cap, unlike the listing card).
                MetaPillsView(
                    runtimeMinutes: film.runtimeMinutes,
                    releaseYear: film.releaseYear,
                    genres: film.genres
                )
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

    /// Open the full-screen viewer — no-op when the film has no poster, so the
    /// button is a dead tap rather than presenting an empty cover.
    private func openFullScreenPoster() {
        if film.posterURL != nil { showFullScreenPoster = true }
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
        VStack(alignment: .leading, spacing: style.metaBlockSpacing) {
            metaBlock(label: "Opis",      value: filmDetails?.synopsis, markdown: true)
            metaBlock(label: "Reżyseria", value: joined(film.directors))
            metaBlock(label: "Obsada",    value: joined(film.cast))
            metaBlock(label: "Kraj(e) produkcji", value: joined(film.countries))
        }
    }

    /// `nil` for an empty list so `metaBlock` omits the whole section
    /// (matches the web, which doesn't render an empty Reżyseria/Obsada).
    private func joined(_ values: [String]) -> String? {
        values.isEmpty ? nil : values.joined(separator: ", ")
    }

    /// `markdown: true` only for the synopsis — it carries `**bold**`/`*italic*`
    /// + newlines (see `SynopsisMarkdown`). Director/cast/countries are plain
    /// comma-joined strings and must render verbatim.
    @ViewBuilder
    private func metaBlock(label: String, value: String?, markdown: Bool = false) -> some View {
        if let value, !value.isEmpty {
            VStack(alignment: .leading, spacing: style.metaLabelToValue) {
                Text(label.uppercased())
                    .font(.system(size: style.metaLabelFontSize, weight: .semibold))
                    .foregroundColor(.secondary)
                    .tracking(0.6)
                (markdown ? Text(SynopsisMarkdown.attributed(value)) : Text(value))
                    .font(.system(size: style.metaValueFontSize))
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
                    ForEach(Array(trailers.enumerated()), id: \.offset) { (index, url) in
                        trailerButton(index: index, url: url)
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
    private func trailerButton(index: Int, url: URL) -> some View {
        let active = playingTrailerIndex == index
        Button {
            // Re-tapping the active trailer toggles it off (matches the
            // web's playTrailer behaviour: clear src + hide frame).
            playingTrailerIndex = active ? nil : index
        } label: {
            Text("Zwiastun \(index + 1)")
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
                    .font(.system(size: style.showingsHeaderFontSize, weight: .semibold))
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
            case .success(let image):
                image.resizable().aspectRatio(contentMode: .fit)
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

/// Full-screen poster viewer presented from `FilmDetailView` on a tap or
/// long-press of the header poster. Black backdrop, the whole poster shown
/// (fit), pinch-to-zoom and drag-to-pan up to 4×, dismissed by a tap on the
/// backdrop or the close button. Reuses `DetailPosterImage` so the
/// fallback-walking logic lives in one place. Mirrors the Android
/// `FullScreenPoster`.
private struct FullScreenPosterView: View {
    let primary: URL
    let fallbacks: [URL]
    let onClose: () -> Void

    @State private var scale: CGFloat = 1
    @State private var lastScale: CGFloat = 1
    @State private var offset: CGSize = .zero
    @State private var lastOffset: CGSize = .zero
    // Vertical translation of an in-progress swipe-to-dismiss (only while not
    // zoomed). The poster follows the finger; on release we either commit the
    // dismissal or spring back to centre.
    @State private var dismissOffset: CGSize = .zero

    /// A swipe whose vertical travel exceeds this commits the dismissal.
    /// Mirrors the Android viewer's 80dp threshold (`FullScreenPoster.kt`).
    private static let dismissThreshold: CGFloat = 100

    /// How long the poster takes to finish sliding off-screen once the swipe is
    /// committed, before the cover is removed.
    private static let flyOffDuration: Double = 0.25

    var body: some View {
        ZStack {
            // Fade the backdrop out as the poster is flicked away, so the
            // gesture reads as a dismissal rather than a pan.
            Color.black.opacity(backdropOpacity).ignoresSafeArea()
                .accessibilityIdentifier(A11y.FilmDetail.fullScreen)

            DetailPosterImage(primary: primary, fallbacks: fallbacks, noPoster: { EmptyView() })
                .scaleEffect(scale)
                .offset(x: offset.width + dismissOffset.width,
                        y: offset.height + dismissOffset.height)
                .gesture(
                    MagnificationGesture()
                        .onChanged { scale = min(4, max(1, lastScale * $0)) }
                        .onEnded { _ in
                            lastScale = scale
                            // Snap back to centre once the user pinches all the
                            // way out — a panned-then-shrunk poster shouldn't
                            // get stranded off-screen.
                            if scale <= 1 { withAnimation { offset = .zero; lastOffset = .zero } }
                        }
                )
                .simultaneousGesture(
                    DragGesture()
                        .onChanged { value in
                            if scale > 1 {
                                // Zoomed in: a drag pans the magnified poster.
                                offset = CGSize(width: lastOffset.width + value.translation.width,
                                                height: lastOffset.height + value.translation.height)
                            } else {
                                // At rest (1×): a vertical drag flicks the poster
                                // away to dismiss. Follow the finger vertically only
                                // so the gesture stays a clean up/down swipe.
                                dismissOffset = CGSize(width: 0, height: value.translation.height)
                            }
                        }
                        .onEnded { value in
                            if scale > 1 {
                                lastOffset = offset
                            } else if abs(value.translation.height) > Self.dismissThreshold {
                                flickAway(up: value.translation.height < 0)
                            } else {
                                withAnimation(.spring()) { dismissOffset = .zero }
                            }
                        }
                )

            VStack {
                HStack {
                    Spacer()
                    Button(action: onClose) {
                        Image(systemName: "xmark")
                            .font(.system(size: 16, weight: .semibold))
                            .foregroundColor(.white)
                            .padding(10)
                            .background(Color.black.opacity(0.5), in: Circle())
                    }
                    .accessibilityIdentifier(A11y.FilmDetail.closeButton)
                    .padding(.top, 8)
                    .padding(.trailing, 16)
                }
                Spacer()
            }
        }
        // A tap anywhere on the backdrop dismisses; the close Button and the
        // zoom/pan gestures take their touches first.
        .contentShape(Rectangle())
        .onTapGesture { onClose() }
    }

    /// Commit the swipe: slide the poster the rest of the way off-screen in the
    /// direction it was thrown, fading the backdrop with it, then remove the
    /// cover WITHOUT its default slide-down transition — that transition pulls
    /// the content back down regardless of swipe direction, which reads as the
    /// poster bouncing the wrong way on an up-swipe. We let the poster finish
    /// flying off, then drop the (now off-screen, transparent) cover instantly.
    private func flickAway(up: Bool) {
        let travel = UIScreen.main.bounds.height + 200
        withAnimation(.easeOut(duration: Self.flyOffDuration)) {
            dismissOffset = CGSize(width: 0, height: up ? -travel : travel)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.flyOffDuration) {
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) { onClose() }
        }
    }

    /// Backdrop dims toward transparent in step with the swipe — fully clear by
    /// the time the poster has travelled a screen-height off, so the fly-off
    /// reads as a dismissal rather than a pan.
    private var backdropOpacity: Double {
        let progress = min(abs(dismissOffset.height) / UIScreen.main.bounds.height, 1)
        return 1 - progress
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
