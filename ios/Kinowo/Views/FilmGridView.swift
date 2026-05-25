import SwiftUI

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
                .padding(.top, 56)
                .padding(.bottom, 70)
            }
            .scrollDismissesKeyboard(.immediately)
            .ignoresSafeArea(edges: .bottom)
        }
    }
}

/// Kina-tab layout: one section per cinema, each section holding the
/// films that play there. Mirrors the web's `_cinemaCards` shape
/// (cinema → movie → screening) — the section header announces the
/// cinema, so the inner cards run with `showCinemaHeaders=false` to
/// drop the now-redundant per-card cinema label.
struct CinemaSectionedGridView: View {
    let sections: [CinemaSection]

    var body: some View {
        if sections.isEmpty {
            EmptyRepertoireView()
        } else {
            ScrollView {
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
                .padding(.horizontal, 12)
                .padding(.bottom, 70)
            }
            .scrollDismissesKeyboard(.immediately)
            .ignoresSafeArea(edges: .bottom)
        }
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
