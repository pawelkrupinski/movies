import SwiftUI

private let gridColumns = [
    GridItem(.adaptive(minimum: 160, maximum: 220), spacing: 12, alignment: .top)
]

/// A paged `TabView` (UIPageViewController) lays its scroll view a fixed 17pt
/// ABOVE the top of the container it's given — verified identical on iPhone
/// 17e and 17 Pro Max, so it's a constant offset, not a device-scaled one.
/// The grids live in a VStack slot below the top bar, so they add this back to
/// their top padding; otherwise the first poster row tucks 17pt up under the
/// bar. (See ContentView's VStack and InitialGapUITests.)
private let pagedTabViewTopOverflow: CGFloat = 17

/// Anchor id pinned to the first row of every grid. Changing a grid's
/// `scrollResetToken` (e.g. picking a different day) scrolls back to this
/// anchor so the user lands at row one of the new listing instead of being
/// stranded mid-scroll on the previous day's rows.
private let gridTopAnchorID = "kinowo.grid.top"

/// Carries the current pane's vertical scroll offset (points scrolled down,
/// ≥ 0) up to `DayCarousel`, which mirrors it onto the neighbour panes so the
/// revealed day appears scrolled in lockstep. Reduce keeps the first reported
/// value per frame.
private struct ScrollOffsetKey: PreferenceKey {
    static var defaultValue: CGFloat = 0
    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {}
}

private extension View {
    /// Quickly animate the enclosing `ScrollViewReader` back to `gridTopAnchorID`
    /// whenever `token` changes — a brief scroll up rather than an abrupt snap.
    /// Shared by both grids so the reset rule lives in one place.
    func resetsScrollToTop<T: Equatable>(on token: T, using proxy: ScrollViewProxy) -> some View {
        onChange(of: token) { _ in
            withAnimation(.easeOut(duration: 0.25)) {
                proxy.scrollTo(gridTopAnchorID, anchor: .top)
            }
        }
    }
}

private struct EmptyRepertoireView: View {
    var body: some View {
        VStack(spacing: 8) {
            Image(systemName: "film").font(.largeTitle).foregroundStyle(.secondary)
            Text("Brak repertuaru.").foregroundStyle(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

struct FilmGridView: View {
    let films: [Film]
    /// Forwarded to each `FilmCardView` → `ShowingsView`: when false, the
    /// per-card cinema label is dropped (e.g. when only one cinema is in
    /// view, so naming it on every card is redundant).
    var showCinemaHeaders: Bool = true
    /// Picking a different day (or any change to this token) snaps the grid
    /// back to the top. Defaults to a constant so previews / callers that
    /// don't drive a reset opt out cleanly.
    var scrollResetToken: AnyHashable = AnyHashable(0)
    /// A paged `TabView` lays its scroll view a fixed 17pt above its
    /// container, so a grid hosted in one adds that back to its top inset.
    /// The single-page films screen hosts the grid directly in a VStack and
    /// passes false, so it doesn't over-pad. Defaults true for callers that
    /// still live inside a paged TabView (the tuning screen).
    var pagedInTabView: Bool = true
    /// Set by `DayCarousel` on the current (interactive) pane: called with the
    /// live vertical scroll offset so the carousel can mirror it onto the
    /// neighbour panes. `nil` for ordinary callers.
    var scrollOffsetReporter: ((CGFloat) -> Void)? = nil
    /// Set by `DayCarousel` on a neighbour (mirror) pane: renders the grid as a
    /// non-scrolling, read-only column translated up by this many points, so it
    /// tracks the current pane's scroll during a drag. `nil` for the normal
    /// interactive grid.
    var mirroredOffset: CGFloat? = nil

    private var topInset: CGFloat {
        // 10pt breathing gap below the bar; plus the paged overflow when the
        // grid sits inside a paged TabView.
        (pagedInTabView ? pagedTabViewTopOverflow : 0) + 10
    }

    var body: some View {
        if films.isEmpty {
            EmptyRepertoireView()
        } else if let mirroredOffset {
            mirrorGrid(offsetY: mirroredOffset)
        } else {
            interactiveGrid()
        }
    }

    /// The film cards laid out in the production grid. Shared by the interactive
    /// ScrollView and the carousel's read-only mirror panes so both render an
    /// identical column.
    private var gridContent: some View {
        LazyVGrid(columns: gridColumns, alignment: .leading, spacing: 12) {
            ForEach(films) { film in
                // Whole-card tap navigates to /film. Inner Buttons (⭐/✕ on the
                // poster, ★ on showtime pills) and Links (cinema-label, booking
                // URL) keep their own hit areas — SwiftUI routes the tap to the
                // innermost gesture handler, so the NavigationLink only fires on
                // the gaps between them.
                NavigationLink(value: film) {
                    FilmCardView(film: film, showCinemaHeaders: showCinemaHeaders)
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier(A11y.FilmGrid.cell)
            }
        }
        .padding(.horizontal, 12)
        // First poster row rests just below the bar instead of tucking under it.
        .padding(.top, topInset)
        .padding(.bottom, 70)
    }

    private func interactiveGrid() -> some View {
        ScrollViewReader { proxy in
            ScrollView {
                gridContent
                    .id(gridTopAnchorID)
                    // Report the live scroll offset (distance scrolled down,
                    // ≥ 0) so the carousel can mirror it onto neighbour panes.
                    .background(
                        GeometryReader { geo in
                            Color.clear.preference(
                                key: ScrollOffsetKey.self,
                                value: -geo.frame(in: .named(scrollSpace)).minY
                            )
                        }
                    )
            }
            .coordinateSpace(name: scrollSpace)
            .onPreferenceChange(ScrollOffsetKey.self) { y in
                scrollOffsetReporter?(max(0, y))
            }
            .scrollDismissesKeyboard(.immediately)
            .modifier(ScrollClipDisabledIfAvailable())
            .resetsScrollToTop(on: scrollResetToken, using: proxy)
        }
    }

    /// Read-only neighbour: the same column with no ScrollView, translated up by
    /// the shared scroll offset so it mirrors the current day's vertical
    /// position. Pinned to the top so the offset reads against row one.
    private func mirrorGrid(offsetY: CGFloat) -> some View {
        gridContent
            .offset(y: -offsetY)
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .allowsHitTesting(false)
    }

    private let scrollSpace = "kinowo.grid.scroll"
}

/// Kina-tab layout: one section per cinema, each section holding the
/// films that play there. Mirrors the web's `_cinemaCards` shape
/// (cinema → movie → screening) — the section header announces the
/// cinema, so the inner cards run with `showCinemaHeaders=false` to
/// drop the now-redundant per-card cinema label.
///
/// `showSectionHeaders` is false when a single cinema is pinned: the pill
/// row already names it, so a per-section header would just repeat it.
struct CinemaSectionedGridView<Header: View>: View {
    let sections: [CinemaSection]
    let showSectionHeaders: Bool
    /// See `FilmGridView.scrollResetToken` — changing it (e.g. picking a
    /// different day) snaps the grid back to the top. Constant by default so
    /// the tuning screen / previews opt out.
    let scrollResetToken: AnyHashable
    let header: () -> Header
    @Environment(\.cinemaHeaderStyle) private var headerStyle

    init(sections: [CinemaSection], showSectionHeaders: Bool = true,
         scrollResetToken: AnyHashable = AnyHashable(0),
         @ViewBuilder header: @escaping () -> Header = { EmptyView() }) {
        self.sections = sections
        self.showSectionHeaders = showSectionHeaders
        self.scrollResetToken = scrollResetToken
        self.header = header
    }

    var body: some View {
        ScrollViewReader { proxy in
            ScrollView {
                VStack(spacing: 0) {
                    header()
                    if sections.isEmpty {
                        EmptyRepertoireView()
                            .frame(minHeight: 300)
                    } else {
                        LazyVStack(alignment: .leading, spacing: headerStyle.sectionSpacing) {
                            ForEach(sections) { section in
                                VStack(alignment: .leading, spacing: headerStyle.headerToGrid) {
                                    if showSectionHeaders {
                                        sectionHeader(CinemaSection.pillName(for: section.cinema))
                                    }
                                    LazyVGrid(columns: gridColumns, alignment: .leading, spacing: 12) {
                                        ForEach(section.films) { film in
                                            NavigationLink(value: film) {
                                                FilmCardView(film: film, showCinemaHeaders: false)
                                            }
                                            .buttonStyle(.plain)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                .padding(.horizontal, 12)
                // Clear the paged TabView's fixed top overflow so the pill-row
                // header rests just below the bar; the header's own vertical
                // padding then supplies the gap to the first poster row.
                .padding(.top, pagedTabViewTopOverflow)
                .padding(.bottom, 70)
                .id(gridTopAnchorID)
            }
            .scrollDismissesKeyboard(.immediately)
            .modifier(ScrollClipDisabledIfAvailable())
            .resetsScrollToTop(on: scrollResetToken, using: proxy)
        }
    }

    @ViewBuilder
    private func sectionHeader(_ name: String) -> some View {
        // Matches the web's `.cinema-section-title`: #aad4ff text on a
        // thin #3a3a6e underline.
        Text(name)
            .font(.system(size: headerStyle.fontSize, weight: headerStyle.fontWeight))
            .foregroundColor(Color(red: 0.667, green: 0.831, blue: 1.0))
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.bottom, headerStyle.titleBottomPadding)
            .overlay(alignment: .bottom) {
                Rectangle()
                    .fill(Color(red: 0.227, green: 0.227, blue: 0.431))
                    .frame(height: headerStyle.underlineThickness)
            }
            .accessibilityIdentifier(A11y.CinemaPage.sectionHeader)
    }
}

private struct ScrollClipDisabledIfAvailable: ViewModifier {
    func body(content: Content) -> some View {
        if #available(iOS 17.0, *) {
            content.scrollClipDisabled()
        } else {
            content
        }
    }
}
