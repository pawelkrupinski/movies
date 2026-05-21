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
                Image(systemName: filtersActive
                      ? "line.3.horizontal.decrease.circle.fill"
                      : "line.3.horizontal.decrease.circle")
                    .font(.system(size: 28))
                    .foregroundColor(.accentColor)
            }
            .buttonStyle(.plain)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
        .background(.ultraThinMaterial)
    }
}

// Horizontally-scrolling date-filter pills. Lives in the navigation
// bar's `.principal` toolbar slot (alongside the camera-icon "logo")
// so the whole top chrome — Filtry button, brand mark, date filter —
// collapses into a single row instead of stacking a separate
// safe-area-inset bar below the nav bar.
//
// Compact dimensions are deliberate: nav bar slot is ~32pt tall, so
// pills use a 12pt font + 3pt vertical padding to clear the chrome
// without clipping descenders. The horizontal ScrollView covers the
// case where four pills overflow on iPhone-mini-class widths.
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
// translucent `regularMaterial` background, so the grid scrolls
// visibly behind it.
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

// Translucent capsule background. On iOS 26+ uses the Liquid-Glass
// `.glassEffect` modifier, which refracts the film grid scrolling
// underneath — that's the "distorting like a fish eye" feel the
// user asked for. On iOS 16-25 we fall back to a Capsule filled
// with `.ultraThinMaterial`. Both paths get dialed-down opacity so
// the grid shows through more strongly than the default material /
// glass.
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
                // Hidden films open at the top of Filtry — mirrors the web's
                // "Ukryte filmy" row inside the Filtry dropdown. Section
                // omitted entirely when the set is empty so the sheet
                // stays uncluttered.
                if !prefs.hiddenFilms.isEmpty {
                    Section("Ukryte filmy") {
                        ForEach(
                            Array(prefs.hiddenFilms).sorted {
                                $0.localizedCaseInsensitiveCompare($1) == .orderedAscending
                            },
                            id: \.self
                        ) { title in
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
                        Button(role: .destructive) {
                            withAnimation { prefs.unhideAll() }
                        } label: {
                            Text("Pokaż wszystkie")
                                .frame(maxWidth: .infinity)
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
