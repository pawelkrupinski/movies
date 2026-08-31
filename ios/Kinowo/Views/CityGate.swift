import SwiftUI

/// First-launch city gate. Until `prefs.selectedCity` is set, the user can't
/// see a repertoire — there's no sensible default beyond "nearest". Once a
/// city is chosen (by location or manual pick), the stores are pointed at its
/// slug and `ContentView` takes over.
///
/// SwiftUI / CoreLocation — excluded from `KinowoCore`. The decision logic it
/// leans on (`City.nearestWithin100km`) is pure and tested there.
struct CityGate: View {
    @EnvironmentObject var prefs: UserPreferences
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var details: DetailsStore

    var body: some View {
        if let slug = prefs.selectedCity {
            ContentView()
                // Re-point the stores at the persisted city before the grid's
                // own `.task` fires its first fetch, so nothing ever hits the
                // fallback-city path on a cold launch with a saved choice.
                .task(id: slug) {
                    store.use(citySlug: slug)
                    details.use(citySlug: slug)
                }
        } else {
            CityResolverView()
        }
    }
}

/// Shown while we attempt to resolve the user's city from their location.
/// On a usable fix we ASK the user to confirm the detected city (rather than
/// silently adopting it); confirming persists it and the gate flips to
/// `ContentView`, "choose another" drops to the manual list. No fix → the
/// manual `CityChoiceView` directly.
struct CityResolverView: View {
    @EnvironmentObject var prefs: UserPreferences
    @EnvironmentObject var catalog: CatalogStore
    @StateObject private var resolver = LocationCityResolver()
    @State private var detected: City?
    @State private var showChoice = false

    var body: some View {
        Group {
            if showChoice {
                // Carry the detected nearest (if any) into the manual picker so a
                // deliberate pick of a *different* city can pre-suppress the
                // "you're nearer …" prompt that would otherwise fire the instant
                // the gate flips to the repertoire.
                CityChoiceView(nearest: detected)
            } else if let city = detected {
                CityConfirmView(
                    city: city,
                    onConfirm: { prefs.setCity(city.slug) },
                    onChooseOther: { showChoice = true }
                )
            } else {
                VStack(spacing: 16) {
                    ProgressView()
                    Text("citygate.resolving")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
        .task {
            #if DEBUG
            // UI tests reach the confirm screen deterministically — no
            // CoreLocation permission dialog, no 8s resolve timeout — by
            // injecting the "detected" city directly.
            if let slug = ProcessInfo.processInfo.environment["KINOWO_FORCE_DETECTED_CITY"],
               let city = City.all.first(where: { $0.slug == slug }) {
                detected = city
                return
            }
            #endif
            // The user reached this gate by naming a country, so the answer
            // they are owed is that country's cities — not a location fix, and
            // not the permission dialog that taking one would raise.
            if prefs.awaitingExplicitCityPick {
                showChoice = true
                return
            }
            switch await resolver.resolve(in: prefs.selectedCountry.code, cities: catalog.cities) {
            case .city(let city):
                detected = city
            case .unavailable:
                showChoice = true
            }
        }
    }
}

/// Confirmation shown when location detected a nearby city on first launch:
/// adopt it, or fall through to the manual picker. We confirm rather than
/// auto-adopt so a user near a city border (or who simply wants another
/// city's repertoire) isn't silently committed to the detected one.
struct CityConfirmView: View {
    let city: City
    let onConfirm: () -> Void
    let onChooseOther: () -> Void

    var body: some View {
        VStack(spacing: 20) {
            Spacer()
            Image(systemName: "location.fill")
                .font(.largeTitle)
                .foregroundStyle(.tint)
            Text("citygate.near_label")
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
            Text(city.name)
                .font(.title).bold()
            Spacer()
            Button(action: onConfirm) {
                Text(String(format: String(localized: "citygate.show_repertoire"), city.name))
                    .frame(maxWidth: .infinity)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .accessibilityIdentifier(A11y.CityGate.confirmButton)
            Button(action: onChooseOther) {
                Text("citygate.choose_other")
                    .frame(maxWidth: .infinity)
            }
            .buttonStyle(.bordered)
            .controlSize(.large)
            .accessibilityIdentifier(A11y.CityGate.chooseOtherButton)
        }
        .padding(24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

/// Manual city picker — the fallback when location is unavailable or the user
/// is outside every served city, and the whole of the pick when the user chose
/// this country themselves. A native grouped `List` driven by the live catalog,
/// so it grows automatically as cities are added.
///
/// Two steps where the country groups its cities (the US, by state), one
/// everywhere else — see `pickingRegion`.
struct CityChoiceView: View {
    @EnvironmentObject var prefs: UserPreferences
    @EnvironmentObject var catalog: CatalogStore
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var details: DetailsStore
    /// The location-detected nearest city, when one was found — used only to
    /// pre-suppress the switch prompt for a deliberate pick of another city.
    /// `nil` when location was unavailable (then there's nothing to suppress).
    var nearest: City?

    /// Live search text; narrows the list to the cities whose folded name
    /// contains it (diacritic-insensitive, so "lodz" finds "Łódź"), or — on the
    /// first step of a grouped country — to the matching regions.
    @State private var query = ""
    /// The region being browsed, on a country whose cities are grouped. `nil` is
    /// the first step (pick a state); non-nil the second (pick a city in it).
    @State private var region: String?

    private var countryCode: String { prefs.selectedCountry.code }

    /// A country that groups its cities (the US, by state) is picked in two
    /// steps: 457 metros in one A-to-Z is not a list anybody reads. Everywhere
    /// else this is empty and the view collapses to the single flat list.
    private var regions: [String] { catalog.regions(inCountry: countryCode) }
    private var pickingRegion: Bool { !regions.isEmpty && region == nil }

    private var visibleRegions: [String] { catalog.regionsMatching(query, inCountry: countryCode) }
    private var visibleCities: [City] { catalog.matching(query, inCountry: countryCode, region: region) }

    var body: some View {
        NavigationStack {
            List {
                // In-app country switch: picking a country swaps the API base URL
                // and forces the UI language. Sits above the city list so the
                // user chooses country → city top-to-bottom on first launch.
                Section {
                    countryPicker
                } header: {
                    Text("country.label")
                }

                if pickingRegion {
                    Section {
                        ForEach(visibleRegions, id: \.self) { name in
                            Button { region = name; query = "" } label: {
                                row(name)
                            }
                            .foregroundStyle(.primary)
                        }
                    } header: {
                        Text("citygate.choose_region_title")
                    }

                    if visibleRegions.isEmpty {
                        Text(String(format: String(localized: "citygate.no_region_match"), query))
                            .foregroundStyle(.secondary)
                    }
                } else {
                    Section {
                        ForEach(visibleCities, id: \.slug) { city in
                            Button {
                                choose(city)
                            } label: {
                                row(city.name)
                            }
                            .foregroundStyle(.primary)
                        }
                    } header: {
                        // Inside a region, the header is the region itself: it is
                        // the only thing on this screen that says which state's
                        // cities these are.
                        if let region {
                            Text(region)
                        } else {
                            Text("citygate.choose_title")
                        }
                    } footer: {
                        if region != nil {
                            Button {
                                region = nil
                                query = ""
                            } label: {
                                Label("citygate.back_to_regions", systemImage: "chevron.left")
                            }
                            .accessibilityIdentifier(A11y.CityGate.backToRegionsButton)
                        }
                    }

                    if visibleCities.isEmpty {
                        // Keeps the search field anchored (an empty List would let
                        // it collapse) and tells the user nothing matched.
                        Text(String(format: String(localized: "citygate.no_match"), query))
                            .foregroundStyle(.secondary)
                    }
                }
            }
            .navigationTitle("citygate.nav_title")
            .navigationBarTitleDisplayMode(.inline)
            .searchable(text: $query,
                        placement: .navigationBarDrawer(displayMode: .always),
                        prompt: Text(pickingRegion ? "citygate.search_region_hint" : "citygate.search_hint"))
            .autocorrectionDisabled()
            .textInputAutocapitalization(.never)
            // Switching country changes what both steps mean, so neither a
            // half-typed query nor a state from the country just left survives it.
            .onChange(of: countryCode) { _ in
                query = ""
                region = nil
            }
        }
    }

    /// One selectable segment per `Country.all`. Selecting persists the choice
    /// (forcing its language) and re-points the stores at the new deployment so
    /// the city list below immediately reflects the chosen country's server.
    private var countryPicker: some View {
        Picker("country.label", selection: Binding(
            get: { prefs.selectedCountry },
            set: { country in
                prefs.setCountry(country)
                store.use(country: country)
                details.use(country: country)
            }
        )) {
            ForEach(catalog.countries, id: \.code) { country in
                Text(country.displayName).tag(country)
            }
        }
        .pickerStyle(.segmented)
    }

    /// One tappable row: a label and the disclosure chevron both steps use.
    private func row(_ label: String) -> some View {
        HStack {
            Text(label)
            Spacer()
            Image(systemName: "chevron.right")
                .font(.footnote)
                .foregroundStyle(.tertiary)
        }
    }

    /// Adopt the picked city. When it differs from the location-detected
    /// nearest, record that pair so the "you're nearer …" prompt doesn't fire
    /// the moment the repertoire appears — the user just chose this city on
    /// purpose. Order matters: seed the key before `setCity` flips the gate.
    private func choose(_ city: City) {
        if let key = City.initialChoiceSuppressKey(chosenSlug: city.slug, nearestSlug: nearest?.slug) {
            prefs.setCitySwitchPromptKey(key)
        }
        prefs.setCity(city.slug)
    }
}
