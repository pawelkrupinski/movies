import SwiftUI

// Filtry sheet — mirrors the web's Filtry dropdown: dimension / version radios,
// an IMAX-only toggle, and a from-hour lower bound (plus sort, hidden films,
// cinemas, name filters, city, account). Format/from-hour are ephemeral session
// state owned by ContentView; the cinema picks persist in `UserPreferences`.
struct FiltersSheet: View {
    @Binding var sortOption: SortOption
    @Binding var formatFilter: FormatFilter
    @Binding var excludedCountries: Set<String>
    @Binding var excludedGenres: Set<String>
    @Binding var excludedDirectors: Set<String>
    @Binding var excludedCast: Set<String>
    var prefs: UserPreferences
    let allCountries: [(name: String, count: Int)]
    let allGenres: [(name: String, count: Int)]
    let allDirectors: [(name: String, count: Int)]
    let allCast: [(name: String, count: Int)]
    @Environment(AuthService.self) var authService
    @Environment(CatalogStore.self) var catalog
    @Environment(RepertoireStore.self) var store
    @Environment(\.dismiss) private var dismiss
    @Environment(\.locale) private var locale

    // ScrollViewReader anchor for the account section (Konto / Zaloguj się) —
    // shared across both branches so a sign-in can scroll it into view.
    private let accountAnchor = "account"

    // Gate the actual sign-out behind a yes/no confirmation.
    @State private var showSignOutConfirm = false

    /// "‹city›, ‹country›" for the current selection, FROZEN at the value it
    /// held when the sheet appeared — captured once (`.onAppear`) rather than
    /// read live from `prefs.selectedCity`. "Choose another city" clears that
    /// preference as part of closing the sheet (see its button below), and a
    /// live computed property would react to that clear before the dismiss
    /// animation finishes, flashing the country's DEFAULT city in for a frame
    /// where the actually-selected one belongs. Nothing else in this sheet can
    /// change the selection while it's open, so one snapshot is enough.
    @State private var currentCityLabel: String?

    private func computeCurrentCityLabel() -> String {
        let slug = prefs.selectedCity ?? catalog.defaultCity(inCountry: prefs.selectedCountry.code)?.slug ?? City.default.slug
        let cityName = catalog.cities.first { $0.slug == slug }?.name ?? slug
        // The city half stays the city's own name (never translated — see
        // `CityGate`'s country pill); the country half shares
        // `CountryDisplayName.localized` with the picker, rather than its own
        // `String(localized: "country.\(code)")` — see that helper's doc
        // comment for why the dynamic-interpolation form silently fails.
        return "\(cityName), \(CountryDisplayName.localized(prefs.selectedCountry.code, locale: locale))"
    }

    var body: some View {
        NavigationStack {
            ScrollViewReader { proxy in
                Form {
                // Sortowanie leads the sheet (above Ukryte filmy, matching
                // Android). Mirrors the web's `#sort-by` dropdown: earliest
                // showtime (default) or weighted rating.
                Section("filtersheet.sort_section") {
                    Picker("filtersheet.sort_picker", selection: $sortOption) {
                        ForEach(SortOption.allCases, id: \.self) { option in
                            Text(option.label(locale: locale)).tag(option)
                        }
                    }
                    .accessibilityIdentifier(A11y.FiltersSheet.sortPicker)
                }

                // Hidden films get a single row that pushes a child screen —
                // the inline list would otherwise crowd out every other filter
                // when the set grows. Row hidden entirely when the set is empty
                // so the sheet stays uncluttered.
                if !prefs.hiddenFilms.isEmpty {
                    Section {
                        NavigationLink {
                            HiddenFilmsList(prefs: prefs)
                        } label: {
                            HStack {
                                Text("filtersheet.hidden_films")
                                Spacer()
                                Text("\(prefs.hiddenFilms.count)")
                                    .foregroundStyle(.secondary)
                            }
                        }
                    }
                }

                // Kina — the one cinema filter, over `prefs.disabledCinemas`.
                // Sits above the name filters, mirroring the web's Filtry order.
                // (Web's "Sale" room picker has no iOS equivalent.)
                CinemaFilterSectionView(catalog: store.catalog, prefs: prefs)

                NameFilterSection(title: "filtersheet.country_of_production", allEntries: allCountries, excluded: $excludedCountries)
                NameFilterSection(title: "filtersheet.genre",                allEntries: allGenres,    excluded: $excludedGenres)
                NameFilterSection(title: "filtersheet.director",             allEntries: allDirectors, excluded: $excludedDirectors)
                NameFilterSection(title: "filtersheet.cast",                 allEntries: allCast,      excluded: $excludedCast)

                Section(header: Text("filtersheet.dimension")
                    .accessibilityIdentifier(A11y.FiltersSheet.dimensionSection)) {
                    Picker("filtersheet.dimension", selection: $formatFilter.dimension) {
                        Text("filtersheet.all").tag("")
                        Text(verbatim: "2D").tag("2D")
                        Text(verbatim: "3D").tag("3D")
                    }
                    .pickerStyle(.segmented)
                }

                // The two choices filter on a LITERAL format token, so they carry
                // the selected country's own pair from the catalog (`OmU`/`DF` in
                // Germany, `VOSE`/`DOB` in Spain) — Poland's, hardcoded here
                // before, matched nothing anywhere else.
                let versionTokens = catalog.country(code: prefs.selectedCountry.code).versionTokens
                Section(header: Text("filtersheet.version")
                    .accessibilityIdentifier(A11y.FiltersSheet.versionSection)) {
                    Picker("filtersheet.version", selection: $formatFilter.language) {
                        Text("filtersheet.all").tag("")
                        Text("filtersheet.subtitles").tag(versionTokens.subtitled)
                        Text("filtersheet.dubbing").tag(versionTokens.dubbed)
                    }
                    .pickerStyle(.segmented)
                }

                // Only where some loaded showtime is IMAX — elsewhere the
                // toggle could only blank the list (the web hides it too).
                if store.films.hasImaxShowtime {
                    Section {
                        Toggle("filtersheet.imax_only", isOn: $formatFilter.imax)
                            .accessibilityIdentifier(A11y.FiltersSheet.imaxToggle)
                    }
                }

                Section(header: Text("filtersheet.from_hour")
                    .accessibilityIdentifier(A11y.FiltersSheet.fromHourSection)) {
                    Picker("filtersheet.hour", selection: $formatFilter.fromHour) {
                        Text("filtersheet.any_hour").tag(-1)
                        ForEach(0..<24, id: \.self) { h in
                            Text(String(format: "%02d", h)).tag(h)
                        }
                    }
                    if formatFilter.fromHour >= 0 {
                        Picker("filtersheet.minute", selection: $formatFilter.fromMinute) {
                            ForEach([0, 15, 30, 45], id: \.self) { m in
                                Text(String(format: "%02d", m)).tag(m)
                            }
                        }
                    }
                }

                Section {
                    Button(role: .destructive) {
                        // Reset only the actual filters — cinema selection and
                        // hidden films persist (they're not "filtering"; see
                        // ActiveFilters) — then close the sheet.
                        sortOption = .earliest
                        formatFilter = .empty
                        excludedCountries = []
                        excludedGenres = []
                        excludedDirectors = []
                        excludedCast = []
                        dismiss()
                    } label: {
                        Text("filtersheet.clear")
                            .frame(maxWidth: .infinity)
                    }
                    .accessibilityIdentifier(A11y.FiltersSheet.clearButton)
                }

                // ── City ─────────────────────────────────────────
                // A single entry point back to the first-launch picker
                // (`CityChoiceView`), replacing the inline Kraj/Miasto pickers:
                // tapping re-arms the explicit-pick flow (`awaitExplicitCityPick`)
                // and clears the persisted city (`clearCity`) — the same pair the
                // country switch used to trigger here — so `CityGate` re-mounts on
                // the country/city chooser once the sheet closes. The current city
                // may not exist under a newly chosen country's host, so we never
                // leave the app pointed at a stale city on a new base URL.
                Section("filtersheet.city") {
                    HStack {
                        Text(currentCityLabel ?? computeCurrentCityLabel())
                        Spacer()
                    }
                    .foregroundStyle(.secondary)
                    Button {
                        prefs.awaitExplicitCityPick()
                        prefs.clearCity()
                        dismiss()
                    } label: {
                        Text("citygate.choose_other")
                            .frame(maxWidth: .infinity)
                    }
                    .accessibilityIdentifier(A11y.FiltersSheet.pickAnotherCityButton)
                }

                // ── Language ─────────────────────────────────────
                // The UI language, fully independent of country (see
                // `LanguageSelection`) — switching country never changes it.
                Section("filtersheet.language") {
                    Picker("filtersheet.language", selection: Binding(
                        get: { prefs.selectedLanguage },
                        set: { prefs.setLanguage($0) }
                    )) {
                        ForEach(LanguageSelection.supported, id: \.self) { code in
                            Text(LanguageDisplayName.native(code)).tag(code)
                        }
                    }
                    .accessibilityIdentifier(A11y.FiltersSheet.languagePicker)
                }

                // ── Account ──────────────────────────────────────
                if let user = authService.user {
                    Section("filtersheet.account") {
                        HStack {
                            Image(systemName: providerIcon(user.provider))
                            Text(user.displayName ?? user.email ?? user.provider)
                        }
                        Button("filtersheet.sign_out") {
                            showSignOutConfirm = true
                        }
                        .confirmationDialog(
                            "filtersheet.sign_out_confirm",
                            isPresented: $showSignOutConfirm,
                            titleVisibility: .visible
                        ) {
                            Button("filtersheet.sign_out", role: .destructive) {
                                Task { await authService.signOut() }
                            }
                            Button("filtersheet.cancel", role: .cancel) {}
                        }
                        Button("filtersheet.delete_account", role: .destructive) {
                            Task {
                                await authService.deleteAccount()
                                prefs.clearAllHiddenFilms()
                                prefs.setDisabledCinemas([])
                            }
                        }
                    }
                    .id(accountAnchor)
                } else {
                    Section("filtersheet.sign_in") {
                        Button { Task { await authService.signInWithGoogle() } } label: {
                            Label("filtersheet.sign_in_google", systemImage: "g.circle.fill")
                        }
                        Button { Task { await authService.signInWithFacebook() } } label: {
                            Label("filtersheet.sign_in_facebook", systemImage: "f.circle.fill")
                        }
                    }
                    .id(accountAnchor)
                }
            }
            // After a successful sign-in (user goes nil → set), scroll the sheet
            // down to the freshly-revealed Konto section so the result of logging
            // in lands in view rather than off the bottom.
            .onChange(of: authService.user != nil) { _, loggedIn in
                if loggedIn {
                    withAnimation { proxy.scrollTo(accountAnchor, anchor: .bottom) }
                }
            }
            .navigationTitle("filtersheet.title")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .confirmationAction) {
                    Button("filtersheet.done") { dismiss() }
                        .accessibilityIdentifier(A11y.FiltersSheet.doneButton)
                }
            }
            .accessibilityIdentifier(A11y.FiltersSheet.root)
            .onAppear {
                if currentCityLabel == nil { currentCityLabel = computeCurrentCityLabel() }
            }
            }
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
