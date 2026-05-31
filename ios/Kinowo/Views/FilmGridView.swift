import SwiftUI
import UIKit

private let gridColumns = [
    GridItem(.adaptive(minimum: 160, maximum: 220), spacing: 12, alignment: .top)
]

/// Pins the enclosing `UIScrollView`'s content inset so it never
/// auto-adjusts. SwiftUI leaves `contentInsetAdjustmentBehavior` at
/// `.automatic`, and UIKit recomputes the adjusted inset the moment the
/// scroll view receives its first touch — which yanked the grid up by
/// ~17pt on the first drag and parked the top row under the floating top
/// bar (the "skip" + misalignment). Because the top bar already reserves
/// its height via `ContentView`'s `.safeAreaInset(edge: .top)`, the grid
/// wants a fixed, stable inset, not UIKit's automatic one.
///
/// Scoped to the grids on purpose: the Filtry `Form` and the detail
/// `ScrollView` still want the automatic adjustment to clear their nav
/// bars, so this must not be a global `UIScrollView.appearance()` change.
private struct PinScrollContentInset: UIViewRepresentable {
    func makeUIView(context: Context) -> UIView { UIView() }

    func updateUIView(_ uiView: UIView, context: Context) {
        // The backing UIScrollView may not be in the responder chain yet
        // on the first pass, so retry on a few short delays until found.
        pin(from: uiView, attemptsLeft: 6)
    }

    private func pin(from view: UIView, attemptsLeft: Int) {
        // Pin EVERY enclosing UIScrollView in this view's ancestry — the
        // grid's own vertical scroll and the paged TabView's horizontal
        // pager both auto-adjust their inset, and either one moving on the
        // first touch shifts the grid up under the bar. Scoped to this
        // hierarchy, so the Filtry sheet / detail scroll are untouched.
        var found = false
        var ancestor: UIView? = view.superview
        while let v = ancestor {
            if let scroll = v as? UIScrollView {
                scroll.contentInsetAdjustmentBehavior = .never
                found = true
            }
            ancestor = v.superview
        }
        guard !found, attemptsLeft > 0 else { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) {
            pin(from: view, attemptsLeft: attemptsLeft - 1)
        }
    }
}

private extension View {
    /// Place inside a ScrollView's content so it can reach the backing
    /// `UIScrollView` and pin its content inset. See `PinScrollContentInset`.
    func pinScrollContentInset() -> some View {
        background(PinScrollContentInset())
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
                // The shared TopBar's height is already reserved by
                // ContentView's `.safeAreaInset(edge: .top)`; this small
                // top pad is just a breathing gap so the first poster row
                // doesn't sit flush against the bar.
                .padding(.top, 16)
                .padding(.bottom, 70)
                .pinScrollContentInset()
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
struct CinemaSectionedGridView<Header: View>: View {
    let sections: [CinemaSection]
    let header: () -> Header

    init(sections: [CinemaSection], @ViewBuilder header: @escaping () -> Header = { EmptyView() }) {
        self.sections = sections
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
                                sectionHeader(CinemaSection.pillName(for: section.cinema))
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
            // See FilmGridView: the bar's height is reserved by the
            // ContentView `.safeAreaInset(edge: .top)`; this is just the
            // breathing gap below it.
            .padding(.top, 16)
            .padding(.bottom, 70)
            .pinScrollContentInset()
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
