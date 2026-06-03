import SwiftUI
import UIKit

private let gridColumns = [
    GridItem(.adaptive(minimum: 160, maximum: 220), spacing: 12, alignment: .top)
]

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
    /// Screen-space height of the floating top bar, supplied by `ContentView`.
    /// The grid renders edge-to-edge under the bar; this padding keeps the
    /// first poster row resting just below it (a stable, explicit inset — not
    /// the scroll view's `.automatic` safe-area inset, which could collapse).
    var topInset: CGFloat = 0

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
                // `topInset` clears the floating top bar (the grid scrolls
                // under it); +10 is the breathing gap so the first poster row
                // doesn't sit flush against the bar's bottom edge.
                .padding(.top, topInset + 10)
                .padding(.bottom, 70)
            }
            .scrollDismissesKeyboard(.immediately)
            .modifier(SoftTopScrollEdgeIfAvailable())
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
    /// Screen-space height of the floating top bar (see `FilmGridView.topInset`):
    /// the cinema grid scrolls under the bar, this keeps the pill-row header
    /// resting just below it.
    let topInset: CGFloat
    let header: () -> Header

    init(sections: [CinemaSection], showSectionHeaders: Bool = true,
         topInset: CGFloat = 0,
         @ViewBuilder header: @escaping () -> Header = { EmptyView() }) {
        self.sections = sections
        self.showSectionHeaders = showSectionHeaders
        self.topInset = topInset
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
            // `topInset` clears the floating top bar (the grid scrolls under
            // it); the pill row's own vertical padding then supplies the gap
            // between the row and the first poster.
            .padding(.top, topInset)
            .padding(.bottom, 70)
        }
        .scrollDismissesKeyboard(.immediately)
        .modifier(SoftTopScrollEdgeIfAvailable())
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

/// The iOS-26 "scroll edge effect" in its `.soft` style: the system applies a
/// variable blur + gradient dim to scroll content near the top edge, so posters
/// scrolling under the bar stay visible but progressively blur and dim — the way
/// iOS Settings / Music keep content legible-but-blurred beneath the navigation
/// bar. No-op before iOS 26.
///
/// Two requirements, both met by `ContentView`'s layout: the content must render
/// under the bar (the grids are edge-to-edge, `ignoresSafeArea(.top)`), and the
/// scroll view must clip its content (so we deliberately do NOT disable scroll
/// clipping here — that was what stopped the effect from ever showing).
private struct SoftTopScrollEdgeIfAvailable: ViewModifier {
    func body(content: Content) -> some View {
        if #available(iOS 26.0, *) {
            content.scrollEdgeEffectStyle(.soft, for: .top)
        } else {
            content
        }
    }
}
