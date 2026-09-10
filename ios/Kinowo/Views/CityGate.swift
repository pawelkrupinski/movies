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
    // For the chosen city's own zone — a country has one to offer and the US
    // needs six. Injected app-wide in `KinowoApp`, same as the others.
    @EnvironmentObject var catalog: CatalogStore

    var body: some View {
        if let slug = prefs.selectedCity {
            ContentView()
                // Re-point the stores at the persisted city before the grid's
                // own `.task` fires its first fetch, so nothing ever hits the
                // fallback-city path on a cold launch with a saved choice.
                .task(id: slug) {
                    store.use(citySlug: slug, timeZone: catalog.zone(ofSlug: slug, inCountry: prefs.selectedCountry))
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
    // For `choose(_:)`'s fallback suppression — see there.
    @EnvironmentObject var authService: AuthService
    /// The location-detected nearest city, when one was found — used only to
    /// pre-suppress the switch prompt for a deliberate pick of another city.
    /// `nil` when location was unavailable (then there's nothing to suppress).
    var nearest: City?

    /// The "use my location" toolbar button's own resolver — separate from the
    /// gate's `CityResolverView`, since this button re-runs the same check on
    /// demand rather than once on first launch.
    @StateObject private var locateResolver = LocationCityResolver()
    /// A fix is in flight for the toolbar button — disables it and swaps its
    /// icon for a spinner so a second tap can't stack a second request.
    @State private var locating = false
    /// A hit from the toolbar button, awaiting confirmation — presented via
    /// `CityConfirmView`, same as the first-launch flow.
    @State private var located: City?
    /// The toolbar button's last attempt found nothing within 100 km (or was
    /// denied) — shown inline, cleared on the next attempt or a country/step
    /// change.
    @State private var noNearbyLocate = false

    /// Live search text; narrows the list to the cities whose folded name
    /// contains it (diacritic-insensitive, so "lodz" finds "Łódź"), or — on a
    /// grouped step — to the matching region/subregion names.
    @State private var query = ""
    /// The region being browsed, on a country whose cities are grouped. `nil` is
    /// the first step (pick a nation/state); non-nil the second.
    @State private var region: String?
    /// The subregion being browsed, within `region` — the third step, reached
    /// only where `region` itself splits into sub-groups holding more than one
    /// city (the UK's West Midlands / Glamorgan / Antrim). `nil` everywhere else.
    @State private var subregion: String?

    private var countryCode: String { prefs.selectedCountry.code }

    /// A country that groups its cities — the US by state, Germany by
    /// Bundesland, the UK by nation — is picked in two steps: 468 US places in
    /// one A-to-Z is not a list anybody reads. In Poland and Spain this is empty
    /// and the view collapses to the single flat list.
    private var regions: [String] { catalog.regions(inCountry: countryCode) }
    private var pickingRegion: Bool { !regions.isEmpty && region == nil }

    private var visibleRegions: [String] { catalog.regionsMatching(query, inCountry: countryCode) }
    /// Cities whose TOP group collapsed onto them alone (Berlin, Hamburg —
    /// Germany's single-region city-states; Delaware, Vermont — US states too
    /// small to split), shown as direct rows right on the region step since
    /// there is no group left to name. Empty for a fully flat country (Poland,
    /// Spain), whose whole list already renders through the `else` branch below.
    private var visibleTopDirectCities: [City] {
        guard pickingRegion else { return [] }
        return catalog.matching(query, inCountry: countryCode).filter { $0.region == nil }
    }

    /// The subregions within `region` that hold more than one city — the
    /// second step's own group rows. Empty for every region without one
    /// (which is most of them, and the whole of Germany and the US).
    private var visibleSubregions: [String] {
        guard let region else { return [] }
        return catalog.subregionsMatching(query, inCountry: countryCode, region: region)
    }
    /// The second step's DIRECT rows: cities in `region` with no subregion of
    /// their own. Falls back to the whole (unscoped) match for a flat country,
    /// where `region` is always `nil`.
    private var visibleDirectCities: [City] {
        if let region {
            return catalog.matchingDirect(query, inCountry: countryCode, region: region)
        }
        return catalog.matching(query, inCountry: countryCode)
    }

    /// The third step's rows: cities within one `subregion` of `region`.
    private var visibleSubregionCities: [City] {
        guard let region, let subregion else { return [] }
        return catalog.matching(query, inCountry: countryCode, region: region, subregion: subregion)
    }

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

                if noNearbyLocate {
                    Text("citygate.no_nearby_locate")
                        .foregroundStyle(.secondary)
                        .accessibilityIdentifier(A11y.CityGate.noNearbyLocateLabel)
                }

                if pickingRegion {
                    Section {
                        ForEach(visibleRegions, id: \.self) { name in
                            Button { region = name; query = "" } label: {
                                row(name)
                            }
                            .foregroundStyle(.primary)
                        }
                        ForEach(visibleTopDirectCities, id: \.slug) { city in
                            Button {
                                choose(city)
                            } label: {
                                row(city.name)
                            }
                            .foregroundStyle(.primary)
                        }
                    } header: {
                        Text("citygate.choose_region_title")
                    }

                    if visibleRegions.isEmpty && visibleTopDirectCities.isEmpty {
                        Text(String(format: String(localized: "citygate.no_region_match"), query))
                            .foregroundStyle(.secondary)
                    }
                } else if subregion == nil {
                    // At the TOP of the section — above every row, not buried in
                    // a footer below them — so it stays visible without
                    // scrolling on a long list, matching Android's
                    // `CityChoiceScreen` (its back button sits above the search
                    // field, not after the results).
                    if region != nil {
                        Button {
                            region = nil
                            query = ""
                        } label: {
                            Label("citygate.back_to_regions", systemImage: "chevron.left")
                        }
                        .accessibilityIdentifier(A11y.CityGate.backToRegionsButton)
                    }

                    Section {
                        ForEach(visibleSubregions, id: \.self) { name in
                            Button { subregion = name; query = "" } label: {
                                row(name)
                            }
                            .foregroundStyle(.primary)
                        }
                        ForEach(visibleDirectCities, id: \.slug) { city in
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
                    }

                    if visibleSubregions.isEmpty && visibleDirectCities.isEmpty {
                        // Keeps the search field anchored (an empty List would let
                        // it collapse) and tells the user nothing matched.
                        Text(String(format: String(localized: "citygate.no_match"), query))
                            .foregroundStyle(.secondary)
                    }
                } else {
                    // Third step: browsing one subregion's cities. Same
                    // above-the-list back-button placement as the second step.
                    Button {
                        subregion = nil
                        query = ""
                    } label: {
                        Label("citygate.back_to_region", systemImage: "chevron.left")
                    }
                    .accessibilityIdentifier(A11y.CityGate.backToRegionButton)

                    Section {
                        ForEach(visibleSubregionCities, id: \.slug) { city in
                            Button {
                                choose(city)
                            } label: {
                                row(city.name)
                            }
                            .foregroundStyle(.primary)
                        }
                    } header: {
                        Text(subregion ?? "")
                    }

                    if visibleSubregionCities.isEmpty {
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
            // Switching country changes what every step means, so neither a
            // half-typed query nor a state from the country just left survives it.
            .onChange(of: countryCode) { _ in
                query = ""
                region = nil
                subregion = nil
                noNearbyLocate = false
            }
            .accessibilityIdentifier(A11y.CityGate.picker)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: locate) {
                        if locating {
                            ProgressView()
                        } else {
                            Image(systemName: "location.fill")
                        }
                    }
                    .disabled(locating)
                    .accessibilityLabel("citygate.locate_me")
                    .accessibilityIdentifier(A11y.CityGate.locateButton)
                }
            }
            // A hit re-uses the exact confirm UI the first-launch flow shows —
            // "you're near X", adopt or choose another — rather than a second,
            // divergent presentation for what is the same decision.
            .sheet(isPresented: Binding(
                get: { located != nil },
                set: { if !$0 { located = nil } }
            )) {
                if let city = located {
                    CityConfirmView(
                        city: city,
                        onConfirm: { choose(city); located = nil },
                        onChooseOther: { located = nil }
                    )
                }
            }
        }
    }

    /// Re-runs the location check on demand, for a visitor who already has a
    /// city (or is re-picking one) and wants to check what's nearby without
    /// typing it. Unlike the first-launch flow this searches EVERY country
    /// (`resolveAnyCountry`, not `resolve(in:cities:)`), so it finds the right
    /// city even when the country tab open at the moment isn't the one the
    /// device is actually in — `choose(_:)` is what switches the country to
    /// match, if the hit calls for it. Otherwise reuses `LocationCityResolver`
    /// as-is: permission request, cached-fix reuse, timeouts and the 100 km
    /// cutoff are all already there; this view only routes the outcome.
    private func locate() {
        guard !locating else { return }
        locating = true
        noNearbyLocate = false
        #if DEBUG
        // The same UI-test seam `CityResolverView` uses for the first-launch
        // flow (`KINOWO_FORCE_DETECTED_CITY`), so a test can drive this
        // button deterministically too — no CoreLocation dialog, no fix
        // timeout. `KINOWO_FORCE_LOCATE_UNAVAILABLE` covers the miss case, and
        // is checked FIRST so a test can reach the picker via the first-launch
        // hit (`KINOWO_FORCE_DETECTED_CITY`, unaffected by this flag — only
        // `CityResolverView` reads it) and still drive the button's own miss.
        let env = ProcessInfo.processInfo.environment
        if env["KINOWO_FORCE_LOCATE_UNAVAILABLE"] == "1" {
            locating = false
            noNearbyLocate = true
            return
        }
        if let slug = env["KINOWO_FORCE_DETECTED_CITY"], let city = catalog.cities.first(where: { $0.slug == slug }) {
            locating = false
            located = city
            return
        }
        #endif
        Task {
            let outcome = await locateResolver.resolveAnyCountry(cities: catalog.cities)
            locating = false
            switch outcome {
            case .city(let city): located = city
            case .unavailable: noNearbyLocate = true
            }
        }
    }

    /// One selectable pill per `Country.all`, in a horizontally scrolling row.
    /// A segmented `Picker` squeezed every segment into an equal share of the
    /// row width, so "United Kingdom" / "United States" clipped to an ellipsis
    /// the moment a fourth or fifth country was deployed. Pills instead render
    /// at each label's own intrinsic width — never truncated — and the row
    /// scrolls to fit however many countries the catalog carries, mirroring
    /// the Android `CountryPicker`.
    private var countryPicker: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 8) {
                ForEach(catalog.countries, id: \.code) { country in
                    let selected = country.code == prefs.selectedCountry.code
                    Button {
                        guard country != prefs.selectedCountry else { return }
                        prefs.setCountry(country)
                        store.use(country: country)
                        details.use(country: country)
                    } label: {
                        Text(country.displayName)
                            .lineLimit(1)
                            .fixedSize()
                            .font(.subheadline.weight(selected ? .semibold : .regular))
                            .padding(.horizontal, 14)
                            .padding(.vertical, 8)
                            .background(
                                selected ? Color.accentColor.opacity(0.85) : Color(.secondarySystemFill),
                                in: Capsule()
                            )
                            .foregroundStyle(selected ? Color.white : Color.primary)
                    }
                    .buttonStyle(.plain)
                }
            }
            .padding(.vertical, 2)
        }
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

    /// Adopt the picked city, and settle the "nearer city" prompt for it —
    /// see `City.switchPromptSuppression`. Order matters: apply the
    /// suppression before `setCity` flips the gate to `ContentView`, whose
    /// `onAppear` fires the very next check.
    ///
    /// [nearest] (and so [located], via the toolbar button) may now sit in a
    /// country OTHER than the one currently open — `locate()` searches every
    /// country — so switch the country first when it does, exactly like the
    /// country pill itself does. Safe to do here without losing anything:
    /// this view only ever shows while `prefs.selectedCity` is still nil.
    private func choose(_ city: City) {
        if city.country != prefs.selectedCountry.code {
            let country = catalog.country(code: city.country)
            prefs.setCountry(country)
            store.use(country: country)
            details.use(country: country)
        }
        switch City.switchPromptSuppression(chosenSlug: city.slug, nearestSlug: nearest?.slug) {
        case .seedKey(let key):
            prefs.setCitySwitchPromptKey(key)
        case .suppressNextCheck:
            authService.citySwitchSuppressor.suppressNextCheck()
        case .none:
            break
        }
        prefs.setCity(city.slug)
    }
}
