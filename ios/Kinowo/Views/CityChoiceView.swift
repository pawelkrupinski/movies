import SwiftUI

/// Manual city picker — the fallback when location is unavailable or the user
/// is outside every served city, and the whole of the pick when the user chose
/// this country themselves. A native grouped `List` driven by the live catalog,
/// so it grows automatically as cities are added.
///
/// Two steps where the country groups its cities (the US, by state), one
/// everywhere else — see `pickingRegion`.
struct CityChoiceView: View {
    @Environment(UserPreferences.self) var prefs
    @Environment(CatalogStore.self) var catalog
    @Environment(RepertoireStore.self) var store
    @Environment(DetailsStore.self) var details
    // For `choose(_:)`'s fallback suppression — see there.
    @Environment(AuthService.self) var authService
    @Environment(\.locale) private var locale
    /// The location-detected nearest city, when one was found — used only to
    /// pre-suppress the switch prompt for a deliberate pick of another city.
    /// `nil` when location was unavailable (then there's nothing to suppress).
    var nearest: City?

    /// The app's one resolver (`KinowoApp`), which the "use my location"
    /// toolbar button re-runs on demand.
    @EnvironmentObject private var locateResolver: LocationCityResolver
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
    /// city (a UK county: Glamorgan, West Midlands…). `nil` everywhere else.
    @State private var subregion: String?

    private var countryCode: String { prefs.selectedCountry.code }

    /// A country that groups its cities — the US by state, Germany by
    /// Bundesland, the UK by nation — is picked in two steps: 468 US places in
    /// one A-to-Z is not a list anybody reads. In Poland and Spain this is empty
    /// and the view collapses to the single flat list.
    private var regions: [String] { catalog.regions(inCountry: countryCode) }
    private var pickingRegion: Bool { !regions.isEmpty && region == nil }

    /// The first step's rows — one per region, interleaved with any city whose
    /// top group collapsed onto it alone (Berlin, Hamburg; Delaware, Vermont),
    /// in the catalog's own order (see `City.mergedRows`), narrowed to `query`.
    private var topLevelRows: [City.PickerRow] {
        guard pickingRegion else { return [] }
        return catalog.topLevelRows(matching: query, inCountry: countryCode)
    }

    /// The second step's rows — one per subregion (reached only for the UK's
    /// counties today: Glamorgan, West Midlands…) interleaved with `region`'s
    /// own directly-listed cities, in the catalog's own order. Falls back to
    /// the whole flat list (as plain city rows) for a country with no
    /// grouping, where `region` is always `nil`.
    private var secondLevelRows: [City.PickerRow] {
        if let region {
            return catalog.secondLevelRows(matching: query, inCountry: countryCode, region: region)
        }
        return catalog.matching(query, inCountry: countryCode).map { .city($0) }
    }

    /// The third step's rows: cities within one `subregion` of `region`.
    private var visibleSubregionCities: [City] {
        guard let region, let subregion else { return [] }
        return catalog.matching(query, inCountry: countryCode, region: region, subregion: subregion)
    }

    /// A query flattens the WHOLE country instead of narrowing just the level
    /// currently drilled into — see `Array<City>.searchRows(matching:inCountry:)`.
    /// Only meaningful for a grouped country: a country with no regions (Spain)
    /// has nothing to flatten, so its own per-level search above already reaches
    /// every city.
    private var searchingAcrossLevels: Bool {
        !regions.isEmpty && !query.trimmingCharacters(in: .whitespaces).isEmpty
    }

    private var searchRows: [City.SearchRow] {
        catalog.searchRows(matching: query, inCountry: countryCode)
    }

    var body: some View {
        NavigationStack {
            List {
                // In-app country switch: picking a country swaps the API base
                // URL. The UI language is unaffected (see `LanguageSelection`)
                // — sits above the city list so the user chooses country →
                // city top-to-bottom on first launch.
                Section {
                    CountryPickerRow(countries: catalog.countries, selected: prefs.selectedCountry) { country in
                        switchCountry(to: country, prefs: prefs, store: store, details: details)
                    }
                } header: {
                    Text("country.label")
                }

                // A manual row, not the native `.searchable` bar — SwiftUI
                // always pins that directly under the nav title, ABOVE every
                // list row including the Country section above, regardless of
                // where in this builder it's attached. A plain row renders
                // exactly where it's placed, so country-then-search reads
                // top-to-bottom here the same way it already does on Android's
                // `CityChoiceScreen`.
                searchField

                if noNearbyLocate {
                    Text("citygate.no_nearby_locate")
                        .foregroundStyle(.secondary)
                        .accessibilityIdentifier(A11y.CityGate.noNearbyLocateLabel)
                }

                step
            }
            .navigationTitle("citygate.nav_title")
            .navigationBarTitleDisplayMode(.inline)
            // Switching country changes what every step means, so neither a
            // half-typed query nor a state from the country just left survives it.
            .onChange(of: countryCode) {
                query = ""
                region = nil
                subregion = nil
                noNearbyLocate = false
            }
            .accessibilityIdentifier(A11y.CityGate.picker)
            .toolbar { locateToolbarItem }
            .sheet(isPresented: locatedSheetShown) { locatedSheet }
        }
    }

    private var locatedSheetShown: Binding<Bool> {
        Binding(get: { located != nil }, set: { if !$0 { located = nil } })
    }

    /// A hit re-uses the exact confirm UI the first-launch flow shows — "you're near X", adopt
    /// or choose another — rather than a second, divergent presentation for what is the same
    /// decision.
    @ViewBuilder
    private var locatedSheet: some View {
        if let city = located {
            CityConfirmView(
                city: city,
                onConfirm: { choose(city); located = nil },
                onChooseOther: { located = nil }
            )
        }
    }

    @ViewBuilder
    private var step: some View {
        if searchingAcrossLevels {
            searchStep
        } else if pickingRegion {
            regionStep
        } else if subregion == nil {
            cityStep
        } else {
            subregionStep
        }
    }

    /// Replaces whichever step the visitor was on with ONE flat, alphabetically merged list
    /// spanning every region, subregion and city in the country — the whole point of
    /// flattening the search is that it no longer matters which level you were browsing.
    @ViewBuilder
    private var searchStep: some View {
        Section {
            ForEach(searchRows, id: \.self) { row in
                switch row {
                case .region(let name):
                    rowButton(name) { region = name; subregion = nil; query = "" }
                case .subregion(let name, let parentRegion):
                    rowButton(name) { region = parentRegion; subregion = name; query = "" }
                case .city(let city):
                    rowButton(city.name) { choose(city) }
                }
            }
        } header: {
            Text("citygate.choose_title")
        }

        if searchRows.isEmpty {
            noMatchLabel("citygate.no_match")
        }
    }

    @ViewBuilder
    private var regionStep: some View {
        Section {
            ForEach(topLevelRows, id: \.self) { row in
                switch row {
                case .heading(let name):
                    rowButton(name) { region = name; query = "" }
                case .city(let city):
                    rowButton(city.name) { choose(city) }
                }
            }
        } header: {
            Text("citygate.choose_region_title")
        }

        if topLevelRows.isEmpty {
            noMatchLabel("citygate.no_region_match")
        }
    }

    /// The flat list, or a region's own cities and subregions. The back button sits at the
    /// TOP — above every row, not in a footer — so it stays visible without scrolling,
    /// matching Android's `CityChoiceScreen`.
    @ViewBuilder
    private var cityStep: some View {
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
            ForEach(secondLevelRows, id: \.self) { row in
                switch row {
                case .heading(let name):
                    rowButton(name) { subregion = name; query = "" }
                case .city(let city):
                    rowButton(city.name) { choose(city) }
                }
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

        if secondLevelRows.isEmpty {
            noMatchLabel("citygate.no_match")
        }
    }

    /// Third step: browsing one subregion's cities, with the same above-the-list back button.
    @ViewBuilder
    private var subregionStep: some View {
        Button {
            subregion = nil
            query = ""
        } label: {
            Label("citygate.back_to_region", systemImage: "chevron.left")
        }
        .accessibilityIdentifier(A11y.CityGate.backToRegionButton)

        Section {
            ForEach(visibleSubregionCities, id: \.slug) { city in
                rowButton(city.name) { choose(city) }
            }
        } header: {
            Text(subregion ?? "")
        }

        if visibleSubregionCities.isEmpty {
            noMatchLabel("citygate.no_match")
        }
    }

    private var locateToolbarItem: some ToolbarContent {
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

    /// The localization key(s) for the group level(s) a query from HERE can
    /// still reach — see `City.searchLevelKeys`, computed from this view's
    /// own `region`/`subregion` state exactly as `pickerSearchLevels()` in
    /// landing.scala.html reads `pickerRegion`/`pickerSubregion`.
    private var searchLevelKeys: [LocalizedStringKey] {
        City.searchLevelKeys(country: countryCode, region: region, subregion: subregion).map { LocalizedStringKey($0) }
    }

    /// The search field's prompt, naming what a query can ACTUALLY reach from
    /// THIS level — "state" in the US, "nation"/"county" in the UK, never a
    /// one-size-fits-all "region" that would misname either. Mirrors
    /// `updatePickerSearchCopy()` in landing.scala.html so the same visitor
    /// reads the same wording on web and here. Composed from nested `Text`
    /// (not `String(localized:)`), so it re-localizes in-session along with
    /// everything else `.environment(\.locale)` drives — see
    /// reference_ios_locale_environment_vs_string_localized.
    private var searchPrompt: Text {
        let levels = searchLevelKeys
        switch levels.count {
        case 2: return Text("Search for a \(Text(levels[0])), \(Text(levels[1])) or city")
        case 1: return Text("Search for a \(Text(levels[0])) or city")
        default: return Text("citygate.search_hint")
        }
    }

    /// The picker's own search row — a manual field rather than the native
    /// `.searchable` bar, so it can sit BELOW the country picker (see the
    /// comment where it's placed in `body`). Its prompt swaps contextually —
    /// see `searchPrompt`.
    private var searchField: some View {
        HStack(spacing: 8) {
            Image(systemName: "magnifyingglass")
                .foregroundStyle(.secondary)
            TextField("citygate.search_hint", text: $query, prompt: searchPrompt)
                .autocorrectionDisabled()
                .textInputAutocapitalization(.never)
                .accessibilityIdentifier(A11y.CityGate.searchField)
            if !query.isEmpty {
                Button {
                    query = ""
                } label: {
                    Image(systemName: "xmark.circle.fill")
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
                .accessibilityLabel("citygate.clear_search")
            }
        }
    }

    /// The "nothing matches <query>" line under an empty step — also keeps the
    /// search field anchored, since an empty `List` would let it collapse.
    private func noMatchLabel(_ key: String) -> some View {
        Text(String(format: localizedString(key, locale: locale), query))
            .foregroundStyle(.secondary)
    }

    /// One tappable row — a label and the disclosure chevron — as every step
    /// (and the flattened search) renders its regions, subregions and cities.
    private func rowButton(_ label: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            HStack {
                Text(label)
                Spacer()
                Image(systemName: "chevron.right")
                    .font(.footnote)
                    .foregroundStyle(.tertiary)
            }
        }
        .foregroundStyle(.primary)
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
            switchCountry(to: catalog.country(code: city.country), prefs: prefs, store: store, details: details)
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
