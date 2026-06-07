import SwiftUI
import UIKit

// Top safe-area inset: 🎬 brand mark + four date-filter pills + (on wide
// screens) the search field + Filtry button on the right. Built as a plain
// HStack instead of SwiftUI `ToolbarItem`s because the native nav bar
// clips its contents to a few dozen points of width.
//
// `searchInline` (decided by `TopBarLayout` from the viewport width) moves
// search onto this row, between the pills and Filtry; on narrow screens it
// stays a floating bottom pill (`SearchBar`) and this row holds only the
// pills. The search field is capped at a sensible max width so it doesn't
// sprawl across a wide screen; the date pills always spread to fill the
// leftover row width.
//
// Every numeric size below is multiplied by `scale = viewportWidth / 393`
// (iPhone 17 reference width). Smaller phones get a proportionally
// smaller row, larger phones get a taller / fontier one — the whole
// bar tracks the device's viewport rather than rendering one fixed
// size that has to fit the smallest screen.
struct TopBar: View {
    @Binding var dateFilter: DateFilter
    /// The day to HIGHLIGHT — `previewFilter ?? dateFilter` from ContentView, so
    /// the pill can show the day a carousel drag would commit to mid-drag while
    /// `dateFilter` (the real selection) stays put until release. Taps still set
    /// `dateFilter`; this only drives which pill paints as selected.
    let highlightedFilter: DateFilter
    @Binding var search: String
    @FocusState.Binding var searchFocused: Bool
    let searchInline: Bool
    let filtersActive: Bool
    let onTapFilters: () -> Void

    var body: some View {
        let s = TopBar.viewportScale
        // `spacing: 6 * s` matches the inter-pill gap inside
        // `DatePillsRow`, so every gap on the bar — 🎬 → pills,
        // pills → search/Filtry — reads as the same width as the
        // gaps between the four pills.
        HStack(spacing: 6 * s) {
            Text("🎬")
                .font(.system(size: 24 * s))
            DatePillsRow(dateFilter: $dateFilter, highlightedFilter: highlightedFilter, scale: s)
                .frame(maxWidth: .infinity)
            if searchInline {
                // Cap the field so it stays a comfortable type-into width
                // rather than eating the whole row — the pills get the rest.
                InlineSearchField(search: $search, focused: $searchFocused, scale: s)
                    .frame(maxWidth: 240 * s)
            }
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
        // No background here on purpose — no frosted material. The bar's
        // backing is a gradient *fade* (a scrim, not a frost) drawn in
        // `ContentView`: opaque app-background behind the pills, fading to
        // clear just below the bar, so the grid scrolling up gently fades out
        // toward the bar instead of meeting a frosted strip.
        // Zero-visual automation anchor at the bar's true bottom edge. The
        // pills / Filtry button sit on the row above, inside the 8pt bottom
        // padding, so they can't stand in for the bar bottom in UI tests.
        .overlay(alignment: .bottom) {
            Color.clear
                .frame(maxWidth: .infinity, maxHeight: 1)
                .accessibilityElement()
                .accessibilityIdentifier(A11y.TopBar.bottomEdge)
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

// The four date pills (Dziś / Jutro / 7 dni / Wszystkie). When the row is wide
// enough to hold four copies of the widest pill (Wszystkie) plus the gaps, all
// four render at one uniform width — `.frame(maxWidth: .infinity)` splits the
// row equally. When it isn't, they drop to their intrinsic widths via
// `.fixedSize` so every label still fits — fitting all the text beats a uniform
// row. The choice is `TopBarLayout.datePillsEqualWidth`, fed the offered row
// width (measured below) and the pills' intrinsic widths.
//
// Inter-pill spacing is fixed at `6 * scale` to match the brand → pills and
// pills → Filtry gaps in `TopBar` — every gap on the bar reads as the same
// width.
struct DatePillsRow: View {
    @Binding var dateFilter: DateFilter
    /// Which pill paints as selected — `previewFilter ?? dateFilter`. Taps still
    /// write `dateFilter`; this only drives the highlight so a carousel drag can
    /// preview the day it would commit to without moving the real selection.
    let highlightedFilter: DateFilter
    let scale: CGFloat

    /// Width offered to the row, measured from the full-width frame below so it
    /// reflects the space available regardless of how the pills size within it.
    @State private var availableWidth: CGFloat = 0

    private var spacing: CGFloat { 6 * scale }
    private var fontSize: CGFloat { 14 * scale }
    private var horizontalPadding: CGFloat { 12 * scale }

    /// Each pill's natural width: its label's rendered text width plus the
    /// horizontal padding on both sides.
    private var intrinsicWidths: [CGFloat] {
        let font = UIFont.systemFont(ofSize: fontSize, weight: .medium)
        return DateFilter.presets.map { preset in
            let textWidth = (preset.label as NSString)
                .size(withAttributes: [.font: font]).width
            return textWidth.rounded(.up) + 2 * horizontalPadding
        }
    }

    private var equalWidth: Bool {
        TopBarLayout.datePillsEqualWidth(
            available: availableWidth,
            intrinsicWidths: intrinsicWidths,
            spacing: spacing
        )
    }

    var body: some View {
        // Equal-width mode shares the row between four uniform pills through a
        // fixed gap. The fallback keeps each pill at its text-fitting width but
        // justifies them across the full row (space-between): `spacing: 0` plus
        // an expanding `Spacer` between pills distributes the slack as the
        // inter-pill gaps, so the row always fills its width — never centred or
        // leading-aligned with empty margins.
        HStack(spacing: equalWidth ? spacing : 0) {
            ForEach(Array(DateFilter.presets.enumerated()), id: \.element) { index, f in
                pill(f)
                if !equalWidth, index < DateFilter.presets.count - 1 {
                    Spacer(minLength: spacing)
                }
            }
        }
        // Fill the offered width so the measurement reflects the space the row
        // is given, not the width the pills happen to occupy.
        .frame(maxWidth: .infinity)
        .background(
            GeometryReader { geo in
                Color.clear
                    .onAppear { availableWidth = geo.size.width }
                    .onChange(of: geo.size.width) { availableWidth = $0 }
            }
        )
    }

    private func pill(_ f: DateFilter) -> some View {
        let isHighlighted = highlightedFilter == f
        return Button {
            dateFilter = f
        } label: {
            Text(f.label)
                .font(.system(size: fontSize, weight: .medium))
                .lineLimit(1)
                .frame(maxWidth: equalWidth ? .infinity : nil)
                .padding(.horizontal, horizontalPadding)
                .padding(.vertical, 7 * scale)
                .background(
                    isHighlighted
                        ? Color.accentColor.opacity(0.85)
                        : Color.clear,
                    in: Capsule()
                )
                .foregroundColor(isHighlighted ? .white : .primary)
        }
        .buttonStyle(.plain)
        .fixedSize(horizontal: !equalWidth, vertical: false)
        .accessibilityIdentifier(Self.accessibilityId(for: f))
        .accessibilityAddTraits(isHighlighted ? .isSelected : [])
    }

    private static func accessibilityId(for f: DateFilter) -> String {
        switch f {
        case .today:    return A11y.TopBar.datePillToday
        case .tomorrow: return A11y.TopBar.datePillTomorrow
        case .week:     return A11y.TopBar.datePillWeek
        case .anytime:  return A11y.TopBar.datePillAnytime
        case .specific: return A11y.TopBar.datePillAnytime
        }
    }
}

// The magnifier + text field + clear-button row shared by both search
// placements: the floating bottom `SearchBar` and the inline
// `InlineSearchField` on the top bar. Glyph size and font scale with the
// placement so the inline field matches the date-pill height.
struct SearchFieldContent: View {
    @Binding var search: String
    @FocusState.Binding var focused: Bool
    var iconSize: CGFloat = 17
    var font: Font = .body

    var body: some View {
        HStack(spacing: 10) {
            Image(systemName: "magnifyingglass")
                .font(.system(size: iconSize))
                .foregroundStyle(.secondary)
            TextField("Szukaj filmu", text: $search)
                .font(font)
                .textInputAutocapitalization(.never)
                .autocorrectionDisabled()
                .focused($focused)
                .foregroundColor(.primary)
                .accessibilityIdentifier(A11y.Search.field)
            if !search.isEmpty {
                Button {
                    search = ""
                } label: {
                    Image(systemName: "xmark.circle.fill")
                        .font(.system(size: iconSize))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
            }
        }
    }
}

// Inline search on the top bar (wide screens): the shared field in a
// translucent capsule sized to the date pills, sitting between the pills
// and the Filtry button. Sizes track the same `scale` as the rest of the
// bar so it matches the pill height.
struct InlineSearchField: View {
    @Binding var search: String
    @FocusState.Binding var focused: Bool
    let scale: CGFloat

    var body: some View {
        SearchFieldContent(
            search: $search,
            focused: $focused,
            iconSize: 15 * scale,
            font: .system(size: 14 * scale)
        )
        .padding(.horizontal, 12 * scale)
        .padding(.vertical, 6 * scale)
        .modifier(GlassyPillBackground())
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
        SearchFieldContent(search: $search, focused: $focused)
        .padding(.horizontal, 18)
        .padding(.vertical, 14)
        .modifier(GlassyPillBackground())
        .padding(.horizontal, 24)
        .padding(.top, 14)
        // The pill is placed at the bottom safe-area edge by ContentView's
        // `.overlay(alignment: .bottom)`, so its gap to the physical screen
        // bottom is the home-indicator inset. Push down by a third of that
        // inset to shave a third off the gap — sits a little lower.
        .padding(.bottom, -Self.bottomSafeInset / 3)
    }

    /// Bottom safe-area inset of the key window (the home-indicator strip),
    /// read once per launch — constant per device on iPhone.
    private static let bottomSafeInset: CGFloat = {
        UIApplication.shared.connectedScenes
            .compactMap { $0 as? UIWindowScene }
            .flatMap(\.windows)
            .first { $0.isKeyWindow }?
            .safeAreaInsets.bottom ?? 0
    }()
}

// Floating label that names the day the user just swiped to (Dziś / Jutro
// / 7 dni / Wszystkie). Shown on every day-swipe; fades out after ~0.7 s.
// Positioned by the caller via `.overlay` so it sits centre-screen.
struct DayLabelOverlay: View {
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
            .accessibilityIdentifier(A11y.DayOverlay.label)
    }
}

// Once-a-day onboarding hint — a swipe-hand glyph over one line of copy,
// telling first-time users they can swipe to change the selected day. Shown
// centre-screen right after the first repertoire load and retired for good
// on the user's first-ever day-swipe. The Android counterpart is the
// `SwipeHintOverlay` composable in ListScreen.kt.
struct SwipeHintOverlay: View {
    var body: some View {
        VStack(spacing: 8) {
            Image(systemName: "hand.draw")
                .font(.system(size: 32, weight: .regular))
            Text("Przesuń, aby zmienić dzień")
                .font(.system(size: 15, weight: .medium))
        }
        .foregroundStyle(.primary)
        .padding(.horizontal, 24)
        .padding(.vertical, 16)
        .background(.ultraThinMaterial, in: RoundedRectangle(cornerRadius: 20))
        .overlay(
            RoundedRectangle(cornerRadius: 20).strokeBorder(Color.white.opacity(0.12))
        )
        .accessibilityIdentifier(A11y.SwipeHint.overlay)
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
// the Liquid-Glass `.glassEffect` modifier; on iOS 16-25 falls back
// to `.ultraThinMaterial`.
private struct GlassyPillBackground: ViewModifier {
    @ViewBuilder
    func body(content: Content) -> some View {
        if #available(iOS 26.0, macOS 26.0, *) {
            content.glassEffect(in: Capsule())
        } else {
            content.background(.ultraThinMaterial, in: Capsule())
        }
    }
}

// Filtry sheet — mirrors the web's Filtry dropdown: cinema multi-select,
// Wymiar / Wersja radios, Tylko IMAX toggle, and Od godziny lower-bound.
// Cinema state is persisted (UserPreferences.disabledCinemas, same key
// as the web's localStorage); format/from-hour are ephemeral session
// state owned by ContentView.
struct FiltersSheet: View {
    @Binding var sortOption: SortOption
    @Binding var formatFilter: FormatFilter
    @Binding var excludedCountries: Set<String>
    @Binding var excludedGenres: Set<String>
    @Binding var excludedDirectors: Set<String>
    @Binding var excludedCast: Set<String>
    @ObservedObject var prefs: UserPreferences
    let allCinemas: [String]
    let allCountries: [(name: String, count: Int)]
    let allGenres: [(name: String, count: Int)]
    let allDirectors: [(name: String, count: Int)]
    let allCast: [(name: String, count: Int)]
    @EnvironmentObject var authService: AuthService
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var details: DetailsStore
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

                // Mirrors the web's `#sort-by` dropdown: earliest showtime
                // (default) or weighted rating.
                Section("Sortowanie") {
                    Picker("Sortuj", selection: $sortOption) {
                        ForEach(SortOption.allCases, id: \.self) { option in
                            Text(option.label).tag(option)
                        }
                    }
                }

                if !allCinemas.isEmpty {
                    Section("Kina") {
                        Toggle("Wszystkie kina", isOn: Binding(
                            get: { prefs.allCinemasSelected(in: allCinemas) },
                            set: { on in
                                prefs.setAllCinemas(in: allCinemas, selected: on)
                            }
                        ))
                        ForEach(allCinemas, id: \.self) { cinema in
                            Toggle(CinemaSection.pillName(for: cinema), isOn: Binding(
                                get: { !prefs.disabledCinemas.contains(cinema) },
                                set: { on in prefs.toggleCinema(cinema, disabled: !on) }
                            ))
                        }
                    }
                }

                NameFilterSection(title: "Kraj produkcji", allEntries: allCountries, excluded: $excludedCountries)
                NameFilterSection(title: "Gatunek",        allEntries: allGenres,    excluded: $excludedGenres)
                NameFilterSection(title: "Reżyseria",      allEntries: allDirectors, excluded: $excludedDirectors)
                NameFilterSection(title: "Obsada",         allEntries: allCast,      excluded: $excludedCast)

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
                        sortOption = .earliest
                        formatFilter = .empty
                        excludedCountries = []
                        excludedGenres = []
                        excludedDirectors = []
                        excludedCast = []
                        prefs.setDisabledCinemas([])
                    } label: {
                        Text("Wyczyść")
                            .frame(maxWidth: .infinity)
                    }
                }

                // ── Miasto ───────────────────────────────────────
                // Always available (signed in or out): picks which city's
                // repertoire the app shows. Changing it re-points both
                // stores at the new `/{slug}/api/…` path and persists the
                // choice. The picker lists every `City.all` entry.
                Section("Miasto") {
                    Picker("Miasto", selection: Binding(
                        get: { prefs.selectedCity ?? City.default.slug },
                        set: { slug in
                            prefs.setCity(slug)
                            store.use(citySlug: slug)
                            details.use(citySlug: slug)
                        }
                    )) {
                        ForEach(City.allSorted, id: \.slug) { city in
                            Text(city.name).tag(city.slug)
                        }
                    }
                }

                // ── Account ──────────────────────────────────────
                if let user = authService.user {
                    Section("Konto") {
                        HStack {
                            Image(systemName: providerIcon(user.provider))
                            Text(user.displayName ?? user.email ?? user.provider)
                        }
                        Button("Wyloguj") {
                            Task { await authService.signOut() }
                        }
                        Button("Usuń konto", role: .destructive) {
                            Task {
                                await authService.deleteAccount()
                                prefs.unhideAll()
                                prefs.setDisabledCinemas([])
                            }
                        }
                    }
                } else {
                    Section("Zaloguj się") {
                        Button { Task { await authService.signInWithGoogle() } } label: {
                            Label("Zaloguj przez Google", systemImage: "g.circle.fill")
                        }
                        Button { Task { await authService.signInWithFacebook() } } label: {
                            Label("Zaloguj przez Facebook", systemImage: "f.circle.fill")
                        }
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

    private func providerIcon(_ provider: String) -> String {
        switch provider {
        case "apple":    return "apple.logo"
        case "google":   return "g.circle.fill"
        case "facebook": return "f.circle.fill"
        default:         return "person.circle"
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

/// One Filtry row (country / genre / director / cast) that pushes a
/// `NameFilterList` and shows an "included/total" count badge once the
/// user has excluded anything. Renders nothing when there are no entries
/// to filter, so the four call sites stay declarative one-liners.
struct NameFilterSection: View {
    let title: String
    let allEntries: [(name: String, count: Int)]
    @Binding var excluded: Set<String>

    var body: some View {
        if !allEntries.isEmpty {
            Section {
                NavigationLink {
                    NameFilterList(title: title, allEntries: allEntries, excluded: $excluded)
                } label: {
                    HStack {
                        Text(title)
                        Spacer()
                        if !excluded.isEmpty {
                            Text("\(allEntries.count - excluded.count)/\(allEntries.count)")
                                .foregroundStyle(.secondary)
                        }
                    }
                }
            }
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
