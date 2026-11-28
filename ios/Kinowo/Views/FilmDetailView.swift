import SwiftUI

/// Per-film detail screen — iOS counterpart of `/film?title=…`. Mirrors
/// the web layout: poster on the left of the header row, title +
/// ratings + cinema-link buttons on the right, then synopsis / director /
/// cast / trailer meta blocks, then a showtimes section with the
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
    @Environment(DetailsStore.self) var details
    // The share link is city-scoped (`/<city>/film?title=…`), so the detail
    // screen needs the selected-city slug the same way the listing card does.
    @Environment(UserPreferences.self) var prefs
    @Environment(\.filmDetailStyle) private var style
    @Environment(\.locale) private var locale
    @Environment(\.listingViewportWidth) private var viewportWidth
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
        .fullScreenCover(isPresented: $showFullScreenPoster) { fullScreenPoster }
        .navigationTitle(film.title)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar { shareItem }
    }

    @ViewBuilder
    private var fullScreenPoster: some View {
        if let primary = film.posterURL {
            FullScreenPosterView(
                primary: primary,
                fallbacks: film.fallbackPosterURLs,
                onClose: { showFullScreenPoster = false }
            )
        }
    }

    @ToolbarContentBuilder
    private var shareItem: some ToolbarContent {
        if let citySlug = prefs.selectedCity {
            ToolbarItem(placement: .navigationBarTrailing) {
                // Shares the canonical `/<city>/film?title=…` link — same URL
                // a user would copy from the website's address bar.
                ShareLink(item: FilmShareLink.url(origin: prefs.selectedCountry.baseURL.absoluteString, for: film, citySlug: citySlug), subject: Text(film.title)) {
                    Image(systemName: "square.and.arrow.up")
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
        let width = ShowtimePillMetrics.cardWidth(viewportWidth: viewportWidth)
        HStack(alignment: .top, spacing: 16) {
            // A Button (not a bare tap gesture) so the poster is always a
            // queryable accessibility element — even while the AsyncImage is
            // still loading its placeholder — and VoiceOver announces it.
            // The long-press is the platform-conventional "peek"; the tap the
            // obvious one. Both open the same full-screen cover.
            Button { openFullScreenPoster() } label: {
                poster
                    .frame(width: width, height: width * 1.5)
                    .clipShape(RoundedRectangle(cornerRadius: PosterMetrics.cornerRadius))
            }
            .buttonStyle(.plain)
            .accessibilityIdentifier(A11y.FilmDetail.poster)
            .simultaneousGesture(LongPressGesture().onEnded { _ in openFullScreenPoster() })
            titleColumn
        }
        .padding(.top, 8)
    }

    private var titleColumn: some View {
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
                genres: film.genres,
                ageRating: film.ageRating
            )
            if !film.ratings.isEmpty {
                RatingBadgesView(ratings: film.ratings)
            }
            cinemaLinks
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }

    /// Open the full-screen viewer — no-op when the film has no poster, so the
    /// button is a dead tap rather than presenting an empty cover.
    private func openFullScreenPoster() {
        if film.posterURL != nil { showFullScreenPoster = true }
    }

    @ViewBuilder
    private var poster: some View {
        if let primary = film.posterURL {
            detailPoster(primary: primary, fallbacks: film.fallbackPosterURLs) { noPosterPlaceholder }
        } else {
            noPosterPlaceholder
        }
    }

    private var noPosterPlaceholder: some View {
        Rectangle()
            .fill(Color.kinowoPanel)
            .aspectRatio(PosterMetrics.aspectRatio, contentMode: .fit)
            .overlay(
                Text("poster.missing")
                    .font(.system(size: 12)).italic()
                    .foregroundColor(.secondary)
            )
            .accessibilityIdentifier(A11y.Poster.missing)
    }

    @ViewBuilder
    private var cinemaLinks: some View {
        let links = cinemaLinkList
        if !links.isEmpty {
            FlowLayout(spacing: 6, lineSpacing: 6) {
                // Identified by the whole link, not by `\.url` alone: a URL
                // shared by several cinemas gave every row the same identity,
                // and SwiftUI drew the first row once per duplicate (all 72
                // London pills read "Act One Cinema Acton"). `cinemaLinks()`
                // now collapses shared URLs, so this is belt-and-braces.
                ForEach(links, id: \.self) { link in
                    Link(destination: link.url) {
                        Text("\(link.label) ↗")
                            .font(.system(size: 12, weight: .medium))
                            .foregroundColor(Color.kinowoLinkAccent)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 5)
                            .background(
                                Color.kinowoButtonFill,
                                in: RoundedRectangle(cornerRadius: 8)
                            )
                    }
                    .buttonStyle(.plain)
                    .accessibilityIdentifier(A11y.FilmDetail.cinemaLink)
                }
            }
            .padding(.top, 2)
        }
    }

    // MARK: – synopsis / director / cast / countries

    @ViewBuilder
    private var metaBlocks: some View {
        // Director / cast come straight off the listing `Film`, so they
        // render immediately. The synopsis comes from the details map —
        // it's simply absent until `/api/details` lands, or for films the
        // backend has no synopsis for.
        //
        // `String(localized:)` rather than a `LocalizedStringKey` because
        // `metaBlock` uppercases the caption before rendering it, and the
        // key type carries no text to uppercase. Wording is deliberately
        // identical to Android's `meta_*` strings.
        VStack(alignment: .leading, spacing: style.metaBlockSpacing) {
            metaBlock(label: localizedString("filmdetail.meta_synopsis", locale: locale),
                      value: filmDetails?.synopsis, markdown: true)
            metaBlock(label: localizedString("filmdetail.meta_director", locale: locale),
                      value: joined(film.directors))
            metaBlock(label: localizedString("filmdetail.meta_cast", locale: locale),
                      value: joined(film.cast))
            metaBlock(label: localizedString("filmdetail.meta_countries", locale: locale),
                      value: joined(film.countries))
        }
    }

    /// `nil` for an empty list so `metaBlock` omits the whole section
    /// (matches the web, which doesn't render an empty director/cast block).
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
                Text("filmdetail.trailers")
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
                        .clipShape(RoundedRectangle(cornerRadius: PosterMetrics.cornerRadius))
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
            Text(String(format: localizedString("filmdetail.trailer_number", locale: locale), index + 1))
                .font(.system(size: 13, weight: .medium))
                .foregroundColor(active ? Color(red: 0.10, green: 0.10, blue: 0.18) : Color.kinowoLinkAccent)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(
                    active ? Color.kinowoLinkAccent
                           : Color.kinowoButtonFill,
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
                Text("filmdetail.showings")
                    .font(.system(size: style.showingsHeaderFontSize, weight: .semibold))
                    .foregroundColor(.white)
                // We were handed the full, all-days film (ContentView's
                // `navigationDestination` re-resolves it from the unfiltered
                // `store.films`, undoing the grid's per-day prune), so
                // `ShowingsView` renders the complete schedule across every day.
                // The full schedule: no day of it is "brought in" by a search.
                ShowingsView(film: film)
                    .environment(\.outsideDays, nil)
            }
            .padding(.top, 4)
        }
    }
}
