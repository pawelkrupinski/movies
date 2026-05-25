import SwiftUI
import UIKit

// Top safe-area inset: 🎬 brand mark + four date-filter pills sharing
// the available width + Filtry button on the right. Built as a plain
// HStack instead of SwiftUI `ToolbarItem`s because the native nav bar
// clips its contents to a few dozen points of width.
//
// Every numeric size below is multiplied by `scale = viewportWidth / 393`
// (iPhone 17 reference width). Smaller phones get a proportionally
// smaller row, larger phones get a taller / fontier one — the whole
// bar tracks the device's viewport rather than rendering one fixed
// size that has to fit the smallest screen.
struct TopBar: View {
    @Binding var dateFilter: DateFilter
    let filtersActive: Bool
    let onTapFilters: () -> Void

    var body: some View {
        let s = TopBar.viewportScale
        // `spacing: 6 * s` matches the inter-pill gap inside
        // `DatePillsRow`, so the gap from 🎬 → first pill and from
        // last pill → Filtry icon reads as the same width as the
        // gaps between the four pills.
        HStack(spacing: 6 * s) {
            Text("🎬")
                .font(.system(size: 24 * s))
            DatePillsRow(dateFilter: $dateFilter, scale: s)
                .frame(maxWidth: .infinity)
            Button(action: onTapFilters) {
                // `line.3.horizontal.decrease.circle` is the funnel-in-
                // circle SF Symbol the iOS app shipped with — the circle
                // is baked into the glyph itself, no painted background.
                // `.fill` variant swaps in when any filter axis is
                // active so the icon tells you at a glance.
                //
                // Font size 30pt at scale 1.0 visually matches a pill's
                // total height (14pt text + 2×7pt v-padding ≈ 28pt) — the
                // SF Symbol's glyph height is a hair smaller than its font
                // size, so 30pt lands the circle on the same baseline as
                // the pill capsules either side.
                Image(systemName: filtersActive
                      ? "line.3.horizontal.decrease.circle.fill"
                      : "line.3.horizontal.decrease.circle")
                    .font(.system(size: 30 * s))
                    .foregroundStyle(filtersActive ? Color.accentColor : .primary)
            }
            .buttonStyle(BounceButtonStyle())
            // The Image has no text label so XCUITest can't find the
            // button by its accessibility label — give it both a label
            // and an identifier. The label is also useful for
            // VoiceOver users in production.
            .accessibilityLabel("Filtry")
            .accessibilityIdentifier(A11y.TopBar.filtryButton)
        }
        .padding(.horizontal, 10 * s)
        // Tiny top padding so the pills hug the status bar; the
        // safeAreaInset already reserves the strip above. Larger bottom
        // padding keeps a small breathing buffer between the bar and the
        // grid scrolling beneath it.
        .padding(.top, 2 * s)
        .padding(.bottom, 8 * s)
        // Plain translucent material strip — no glassEffect refraction
        // edge, no content-wide opacity. Just the background shape gets
        // dialed down so the grid scrolls visibly behind the bar.
        // `.ignoresSafeArea(edges: .top)` pushes the rectangle up past
        // the safeAreaInset slot, all the way to the device's rounded
        // top corners, so the status bar reads as part of the same
        // translucent strip instead of a separate opaque band above it.
        .background {
            Rectangle()
                .fill(.ultraThinMaterial)
                .opacity(0.55)
                .ignoresSafeArea(edges: .top)
        }
    }

    /// Linear scale relative to the iPhone 17 viewport (393pt wide).
    /// iPhone 13 mini → 0.95, iPhone 17 → 1.00, iPhone 17 Pro Max →
    /// 1.09. Clamped so iPad split-view / odd window widths can't
    /// produce a comically scaled bar. Read once at view init via
    /// `UIScreen.main` — the value is constant per launch on iPhone,
    /// which is where this app runs.
    private static let viewportScale: CGFloat = {
        let w = UIScreen.main.bounds.width
        return max(0.85, min(1.2, w / 393))
    }()
}

// Three short pills (Dziś / Jutro / 7 dni) share one width via
// `.frame(maxWidth: .infinity)` so the leftover row-width is split
// equally between them. Wszystkie skips the maxWidth so it stays at
// its intrinsic 9-character width — naturally the widest pill, but
// the short pills now sit close to it rather than collapsed to their
// own 4–5-character intrinsics. The result: three uniform shorter
// pills and one slightly wider Wszystkie, no font shrinking.
//
// Inter-pill spacing is fixed at `6 * scale` to match the
// brand → pills and pills → Filtry gaps in `TopBar` — every gap on
// the bar reads as the same width.
struct DatePillsRow: View {
    @Binding var dateFilter: DateFilter
    let scale: CGFloat
    @Environment(\.horizontalSizeClass) private var hSize

    var body: some View {
        let landscape = hSize == .regular || UIScreen.main.bounds.width > UIScreen.main.bounds.height
        HStack(spacing: 6 * scale) {
            ForEach(DateFilter.presets, id: \.self) { f in
                Button {
                    dateFilter = f
                } label: {
                    Text(f.label)
                        .font(.system(size: 14 * scale, weight: .medium))
                        .lineLimit(1)
                        .frame(maxWidth: (f == .anytime && !landscape) ? nil : .infinity)
                        .padding(.horizontal, 12 * scale)
                        .padding(.vertical, 7 * scale)
                        .background(
                            dateFilter == f
                                ? Color.accentColor.opacity(0.85)
                                : Color(.systemGray5),
                            in: Capsule()
                        )
                        .foregroundColor(dateFilter == f ? .white : .primary)
                }
                .buttonStyle(.plain)
                .fixedSize(horizontal: f == .anytime && !landscape, vertical: false)
            }
        }
    }
}

// Bottom safe-area inset: a single floating search pill, styled like
// the native iOS search field (Settings / Contacts / Mail). No outer
// chrome container — the pill sits over the grid content with a
// translucent `GlassyPillBackground`, so the grid scrolls visibly
// behind it.
struct SearchBar: View {
    @Binding var search: String
    @FocusState.Binding var focused: Bool

    var body: some View {
        HStack(spacing: 10) {
            Image(systemName: "magnifyingglass")
                .font(.system(size: 17))
                .foregroundStyle(.secondary)
            TextField("Szukaj filmu", text: $search)
                .textInputAutocapitalization(.never)
                .autocorrectionDisabled()
                .focused($focused)
                .foregroundColor(.primary)
            if !search.isEmpty {
                Button {
                    search = ""
                } label: {
                    Image(systemName: "xmark.circle.fill")
                        .font(.system(size: 17))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
            }
        }
        .padding(.horizontal, 18)
        .padding(.vertical, 14)
        .modifier(GlassyPillBackground())
        .padding(.horizontal, 24)
        // Pull the pill 14pt past the safeAreaInset edge into the
        // home-indicator zone, so it sits thumb-anchored at the
        // very bottom of the screen. `.offset` only paints lower —
        // the safeAreaInset still reserves the original layout slot,
        // so the grid stays out from under the pill above the
        // home indicator.
        .offset(y: 14)
    }
}

struct CinemaPillsRow: View {
    let allCinemas: [String]
    @Binding var pinnedCinema: String?

    static let pillNames: [String: String] = [
        "Cinema City Kinepolis": "Kinepolis",
        "Cinema City Poznań Plaza": "Poznań Plaza",
        "Helios Posnania": "Helios",
        "Kino Apollo": "Apollo",
        "Kino Bułgarska 19": "Bułgarska 19",
        "Kino Malta Charlie Monroe": "Malta Charlie Monroe",
        "Kino Muza": "Muza",
        "Kino Pałacowe": "Pałacowe",
        "Kino Rialto": "Rialto",
        "Multikino Stary Browar": "Multikino",
    ]

    private var selection: Binding<String> {
        Binding(
            get: { pinnedCinema ?? "" },
            set: { pinnedCinema = $0.isEmpty ? nil : $0 }
        )
    }

    var body: some View {
        Picker("Kino", selection: selection) {
            Text("Wszystkie").tag("")
            ForEach(allCinemas, id: \.self) { cinema in
                Text(Self.pillNames[cinema] ?? cinema).tag(cinema)
            }
        }
        .pickerStyle(.wheel)
        .frame(height: 100)
        .padding(.vertical, -16)
    }
}

// Floating label that names the current tab (Filmy / Kina). Shown on
// app launch and on every swipe between tabs; fades out after ~0.7 s.
// Positioned by the caller via `.overlay(alignment: .bottom)` plus
// padding so it sits clear of the search bar.
struct TabLabelOverlay: View {
    let text: String

    var body: some View {
        Text(text)
            .font(.system(size: 28, weight: .semibold))
            .foregroundStyle(.primary)
            .padding(.horizontal, 28)
            .padding(.vertical, 12)
            .background(.ultraThinMaterial, in: Capsule())
            .overlay(
                Capsule().strokeBorder(Color.white.opacity(0.12))
            )
            .accessibilityIdentifier({
                switch text {
                case "Filmy":    return A11y.TabOverlay.filmy
                case "Kina":     return A11y.TabOverlay.kina
                default:         return A11y.TabOverlay.filmy
                }
            }())
    }
}

// Tap feedback: spring-scale the label down on press and back on
// release. Used by the Filtry icon (and any future TopBar button) so
// taps feel like they registered without needing to wait for the
// sheet to slide in. Works on iOS 16+; `.symbolEffect(.bounce)` would
// be the more idiomatic choice but it's iOS 17+ only.
struct BounceButtonStyle: ButtonStyle {
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .scaleEffect(configuration.isPressed ? 0.85 : 1.0)
            .animation(
                .spring(response: 0.25, dampingFraction: 0.55),
                value: configuration.isPressed
            )
    }
}

// Translucent capsule background for the search pill. On iOS 26+ uses
// the Liquid-Glass `.glassEffect` modifier, which refracts the film
// grid scrolling underneath — that's the "distorting like a fish eye"
// feel the user asked for. On iOS 16-25 we fall back to a Capsule
// filled with `.ultraThinMaterial`. Both paths get dialed-down opacity
// so the grid shows through more strongly than the default.
//
// Not used for the top bar — the glassEffect refraction edge + the
// content-wide opacity made the bar feel like a floating box rather
// than a flush translucent strip, so the bar uses a plain
// `Rectangle().fill(.ultraThinMaterial).opacity(0.55)` background
// inline instead.
private struct GlassyPillBackground: ViewModifier {
    @ViewBuilder
    func body(content: Content) -> some View {
        if #available(iOS 26.0, macOS 26.0, *) {
            content
                .glassEffect(in: Capsule())
                .opacity(0.8)
        } else {
            content.background {
                Capsule()
                    .fill(.ultraThinMaterial)
                    .opacity(0.55)
            }
        }
    }
}

// Filtry sheet — mirrors the web's Filtry dropdown: cinema multi-select,
// Wymiar / Wersja radios, Tylko IMAX toggle, and Od godziny lower-bound.
// Cinema state is persisted (UserPreferences.disabledCinemas, same key
// as the web's localStorage); format/from-hour are ephemeral session
// state owned by ContentView.
struct FiltersSheet: View {
    @Binding var formatFilter: FormatFilter
    @Binding var excludedCountries: Set<String>
    @Binding var excludedDirectors: Set<String>
    @Binding var excludedCast: Set<String>
    @ObservedObject var prefs: UserPreferences
    let allCinemas: [String]
    let allCountries: [(name: String, count: Int)]
    let allDirectors: [(name: String, count: Int)]
    let allCast: [(name: String, count: Int)]
    var showCinemaSection: Bool = true
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        NavigationStack {
            Form {
                // Hidden films get a single row at the top of Filtry that
                // pushes a child screen — the inline list would otherwise
                // crowd out every other filter when the set grows. Row
                // hidden entirely when the set is empty so the sheet
                // stays uncluttered.
                if !prefs.hiddenFilms.isEmpty {
                    Section {
                        NavigationLink {
                            HiddenFilmsList(prefs: prefs)
                        } label: {
                            HStack {
                                Text("Ukryte filmy")
                                Spacer()
                                Text("\(prefs.hiddenFilms.count)")
                                    .foregroundStyle(.secondary)
                            }
                        }
                    }
                }

                if showCinemaSection && !allCinemas.isEmpty {
                    Section("Kina") {
                        Toggle("Wszystkie kina", isOn: Binding(
                            get: { prefs.disabledCinemas.isEmpty },
                            set: { on in
                                prefs.setDisabledCinemas(on ? [] : Set(allCinemas))
                            }
                        ))
                        ForEach(allCinemas, id: \.self) { cinema in
                            Toggle(CinemaPillsRow.pillNames[cinema] ?? cinema, isOn: Binding(
                                get: { !prefs.disabledCinemas.contains(cinema) },
                                set: { on in prefs.toggleCinema(cinema, disabled: !on) }
                            ))
                        }
                    }
                }

                if !allCountries.isEmpty {
                    Section {
                        NavigationLink {
                            NameFilterList(
                                title: "Kraj produkcji",
                                allEntries: allCountries,
                                excluded: $excludedCountries
                            )
                        } label: {
                            HStack {
                                Text("Kraj produkcji")
                                Spacer()
                                if !excludedCountries.isEmpty {
                                    Text("\(allCountries.count - excludedCountries.count)/\(allCountries.count)")
                                        .foregroundStyle(.secondary)
                                }
                            }
                        }
                    }
                }

                if !allDirectors.isEmpty {
                    Section {
                        NavigationLink {
                            NameFilterList(
                                title: "Reżyseria",
                                allEntries: allDirectors,
                                excluded: $excludedDirectors
                            )
                        } label: {
                            HStack {
                                Text("Reżyseria")
                                Spacer()
                                if !excludedDirectors.isEmpty {
                                    Text("\(allDirectors.count - excludedDirectors.count)/\(allDirectors.count)")
                                        .foregroundStyle(.secondary)
                                }
                            }
                        }
                    }
                }

                if !allCast.isEmpty {
                    Section {
                        NavigationLink {
                            NameFilterList(
                                title: "Obsada",
                                allEntries: allCast,
                                excluded: $excludedCast
                            )
                        } label: {
                            HStack {
                                Text("Obsada")
                                Spacer()
                                if !excludedCast.isEmpty {
                                    Text("\(allCast.count - excludedCast.count)/\(allCast.count)")
                                        .foregroundStyle(.secondary)
                                }
                            }
                        }
                    }
                }

                Section("Wymiar") {
                    Picker("Wymiar", selection: $formatFilter.dimension) {
                        Text("Wszystkie").tag("")
                        Text("2D").tag("2D")
                        Text("3D").tag("3D")
                    }
                    .pickerStyle(.segmented)
                }

                Section("Wersja") {
                    Picker("Wersja", selection: $formatFilter.language) {
                        Text("Wszystkie").tag("")
                        Text("Napisy").tag("NAP")
                        Text("Dubbing").tag("DUB")
                    }
                    .pickerStyle(.segmented)
                }

                Section {
                    Toggle("Tylko IMAX", isOn: $formatFilter.imax)
                }

                Section("Od godziny") {
                    Picker("Godzina", selection: $formatFilter.fromHour) {
                        Text("Dowolna").tag(-1)
                        ForEach(0..<24, id: \.self) { h in
                            Text(String(format: "%02d", h)).tag(h)
                        }
                    }
                    if formatFilter.fromHour >= 0 {
                        Picker("Minuta", selection: $formatFilter.fromMinute) {
                            ForEach([0, 15, 30, 45], id: \.self) { m in
                                Text(String(format: "%02d", m)).tag(m)
                            }
                        }
                    }
                }

                Section {
                    Button(role: .destructive) {
                        formatFilter = .empty
                        excludedCountries = []
                        excludedDirectors = []
                        excludedCast = []
                        prefs.setDisabledCinemas([])
                    } label: {
                        Text("Wyczyść")
                            .frame(maxWidth: .infinity)
                    }
                }
            }
            .navigationTitle("Filtry")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .confirmationAction) {
                    Button("Gotowe") { dismiss() }
                        .accessibilityIdentifier(A11y.FiltersSheet.doneButton)
                }
            }
            .accessibilityIdentifier(A11y.FiltersSheet.root)
        }
    }
}

// Pushed from Filtry's "Ukryte filmy" row. Lists every hidden title with
// a per-row Pokaż button and a Pokaż wszystkie destructive bulk action.
// When the set drains to empty, pop back to Filtry automatically — there
// is nothing left to manage and the row itself would also have disappeared
// from the parent screen.
struct HiddenFilmsList: View {
    @ObservedObject var prefs: UserPreferences
    @Environment(\.dismiss) private var dismiss

    private var sortedTitles: [String] {
        Array(prefs.hiddenFilms).sorted {
            $0.localizedCaseInsensitiveCompare($1) == .orderedAscending
        }
    }

    var body: some View {
        Form {
            Section {
                ForEach(sortedTitles, id: \.self) { title in
                    HiddenFilmRow(title: title, prefs: prefs)
                }
            }

            Section {
                Button(role: .destructive) {
                    withAnimation { prefs.unhideAll() }
                } label: {
                    Text("Pokaż wszystkie")
                        .frame(maxWidth: .infinity)
                }
            }
        }
        .navigationTitle("Ukryte filmy")
        .navigationBarTitleDisplayMode(.inline)
        .onChange(of: prefs.hiddenFilms) { new in
            if new.isEmpty { dismiss() }
        }
    }
}

struct NameFilterList: View {
    let title: String
    let allEntries: [(name: String, count: Int)]
    @Binding var excluded: Set<String>

    private var allNames: Set<String> { Set(allEntries.map(\.name)) }

    var body: some View {
        Form {
            Section {
                Toggle("Wszystkie", isOn: Binding(
                    get: { excluded.isEmpty },
                    set: { on in
                        excluded = on ? [] : allNames
                    }
                ))
            }
            Section {
                ForEach(allEntries, id: \.name) { entry in
                    Toggle(isOn: Binding(
                        get: { !excluded.contains(entry.name) },
                        set: { on in
                            if on { excluded.remove(entry.name) }
                            else  { excluded.insert(entry.name) }
                        }
                    )) {
                        HStack {
                            Text(entry.name)
                            Spacer()
                            Text("(\(entry.count))")
                                .foregroundStyle(.secondary)
                        }
                    }
                }
            }
        }
        .navigationTitle(title)
        .navigationBarTitleDisplayMode(.inline)
    }
}

private struct HiddenFilmRow: View {
    let title: String
    @ObservedObject var prefs: UserPreferences

    var body: some View {
        HStack {
            Text(title).lineLimit(1)
            Spacer()
            Button("Pokaż") {
                withAnimation { prefs.unhide(title) }
            }
            .buttonStyle(.bordered)
            .controlSize(.small)
        }
    }
}
