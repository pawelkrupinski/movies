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

    var body: some View {
        if films.isEmpty {
            EmptyRepertoireView()
        } else {
            ScrollView {
                LazyVGrid(columns: gridColumns, alignment: .leading, spacing: 12) {
                    ForEach(films) { film in
                        // Whole-card tap navigates to /film. Inner
                        // Buttons (⭐/✕ on the poster, ★ on showtime
                        // pills) and Links (cinema-label, booking
                        // URL) keep their own hit areas — SwiftUI
                        // routes the tap to the innermost gesture
                        // handler, so the NavigationLink only fires
                        // on the gaps between them.
                        NavigationLink(value: film) {
                            FilmCardView(film: film)
                        }
                        .buttonStyle(.plain)
                        .accessibilityIdentifier(A11y.FilmGrid.cell)
                    }
                }
                .padding(.horizontal, 12)
                // Clear the paged TabView's fixed top overflow, then a 10pt
                // breathing gap so the first poster row rests just below the
                // bar instead of tucking under it.
                .padding(.top, pagedTabViewTopOverflow + 10)
                .padding(.bottom, 70)
            }
            .scrollDismissesKeyboard(.immediately)
            .modifier(ScrollClipDisabledIfAvailable())
        }
    }
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
    let header: () -> Header

    init(sections: [CinemaSection], showSectionHeaders: Bool = true,
         @ViewBuilder header: @escaping () -> Header = { EmptyView() }) {
        self.sections = sections
        self.showSectionHeaders = showSectionHeaders
        self.header = header
    }

    var body: some View {
        ScrollView {
            VStack(spacing: 0) {
                header()
                if sections.isEmpty {
                    EmptyRepertoireView()
                        .frame(minHeight: 300)
                } else {
                    LazyVStack(alignment: .leading, spacing: 20) {
                        ForEach(sections) { section in
                            VStack(alignment: .leading, spacing: 12) {
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
        }
        .scrollDismissesKeyboard(.immediately)
        .modifier(ScrollClipDisabledIfAvailable())
    }

    @ViewBuilder
    private func sectionHeader(_ name: String) -> some View {
        // Matches the web's `.cinema-section-title`: #aad4ff text on a
        // thin #3a3a6e underline.
        Text(name)
            .font(.system(size: 15, weight: .semibold))
            .foregroundColor(Color(red: 0.667, green: 0.831, blue: 1.0))
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.bottom, 6)
            .overlay(alignment: .bottom) {
                Rectangle()
                    .fill(Color(red: 0.227, green: 0.227, blue: 0.431))
                    .frame(height: 1)
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
