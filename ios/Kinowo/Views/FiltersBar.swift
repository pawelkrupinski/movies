import SwiftUI

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
            HStack(spacing: 4) {
                ForEach(DateFilter.presets, id: \.self) { f in
                    Button {
                        dateFilter = f
                    } label: {
                        Text(f.label)
                            .font(.system(size: 12, weight: .medium))
                            .padding(.horizontal, 9)
                            .padding(.vertical, 3)
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
        HStack(spacing: 6) {
            Image(systemName: "magnifyingglass")
                .font(.system(size: 14))
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
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
            }
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 8)
        .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 12, style: .continuous))
        .padding(.horizontal, 16)
        .padding(.bottom, 8)
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
