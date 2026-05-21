import SwiftUI

// Top safe-area inset: 🎬 brand mark + horizontally-scrolling date
// pills hugging the left edge, with the Filtry button on the right.
// Built as a plain HStack instead of SwiftUI `ToolbarItem`s because
// the native nav bar clips a ScrollView inside `.navigationBarLeading`
// down to a few dozen points of width, which made the pill row
// effectively invisible.
struct TopBar: View {
    @Binding var dateFilter: DateFilter
    let filtersActive: Bool
    let onTapFilters: () -> Void

    var body: some View {
        HStack(spacing: 12) {
            Text("🎬")
                .font(.system(size: 28))
            DatePillsRow(dateFilter: $dateFilter)
                .frame(maxWidth: .infinity, alignment: .leading)
            Button(action: onTapFilters) {
                // Match the translucent circle buttons iOS uses for
                // close / info / mode-toggle chrome (Sheet dismiss, Apple
                // News article close, Camera mode buttons). Three-line
                // icon stripped from its built-in circle so we can paint
                // our own `.ultraThinMaterial` disc behind it. Foreground
                // accent-tints when any filter axis is active.
                Image(systemName: "line.3.horizontal.decrease")
                    .font(.system(size: 15, weight: .semibold))
                    .foregroundStyle(filtersActive ? Color.accentColor : .primary)
                    .frame(width: 32, height: 32)
                    .background(.ultraThinMaterial, in: Circle())
            }
            .buttonStyle(BounceButtonStyle())
        }
        .padding(.horizontal, 16)
        // Tiny top padding so the pills hug the status bar; the
        // safeAreaInset already reserves the strip above. Larger bottom
        // padding keeps a small breathing buffer between the bar and the
        // grid scrolling beneath it.
        .padding(.top, 2)
        .padding(.bottom, 8)
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
}

// Horizontally-scrolling date-filter pills. Sits in the middle of
// the `TopBar` HStack between the 🎬 brand mark and the Filtry icon,
// inside the top safeAreaInset. The ScrollView covers the case where
// four pills overflow on iPhone-mini-class widths.
struct DatePillsRow: View {
    @Binding var dateFilter: DateFilter

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 6) {
                ForEach(DateFilter.presets, id: \.self) { f in
                    Button {
                        dateFilter = f
                    } label: {
                        Text(f.label)
                            .font(.system(size: 16, weight: .medium))
                            .padding(.horizontal, 14)
                            .padding(.vertical, 8)
                            .background(
                                dateFilter == f
                                    ? Color.accentColor.opacity(0.85)
                                    : Color(.systemGray5),
                                in: Capsule()
                            )
                            .foregroundColor(dateFilter == f ? .white : .primary)
                    }
                    .buttonStyle(.plain)
                }
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
    @ObservedObject var prefs: UserPreferences
    /// Sorted, de-duplicated cinema names derived from the films
    /// currently in `store.films`. A cinema that has zero showings on
    /// any day doesn't appear — there's nothing to filter.
    let allCinemas: [String]
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

                if !allCinemas.isEmpty {
                    Section("Kina") {
                        Toggle("Wszystkie kina", isOn: Binding(
                            get: { prefs.disabledCinemas.isEmpty },
                            set: { on in
                                prefs.setDisabledCinemas(on ? [] : Set(allCinemas))
                            }
                        ))
                        ForEach(allCinemas, id: \.self) { cinema in
                            Toggle(cinema, isOn: Binding(
                                get: { !prefs.disabledCinemas.contains(cinema) },
                                set: { on in prefs.toggleCinema(cinema, disabled: !on) }
                            ))
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
                }
            }
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
