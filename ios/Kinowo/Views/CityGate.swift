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
/// On a usable fix it persists the nearest city and the gate flips to
/// `ContentView`; otherwise it falls through to the manual `CityChoiceView`.
struct CityResolverView: View {
    @EnvironmentObject var prefs: UserPreferences
    @StateObject private var resolver = LocationCityResolver()
    @State private var showChoice = false

    var body: some View {
        Group {
            if showChoice {
                CityChoiceView()
            } else {
                VStack(spacing: 16) {
                    ProgressView()
                    Text("Ustalamy najbliższe miasto…")
                        .font(.callout)
                        .foregroundStyle(.secondary)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
        .task {
            switch await resolver.resolve() {
            case .city(let city):
                prefs.setCity(city.slug)
            case .unavailable:
                showChoice = true
            }
        }
    }
}

/// Manual city picker — the fallback when location is unavailable or the user
/// is outside every served city. One row per `City.all`; with a single city
/// it's a one-tap screen, but the list scales as cities are added.
struct CityChoiceView: View {
    @EnvironmentObject var prefs: UserPreferences

    var body: some View {
        NavigationStack {
            List {
                Section {
                    ForEach(City.all, id: \.slug) { city in
                        Button {
                            prefs.setCity(city.slug)
                        } label: {
                            HStack {
                                Text(city.name)
                                Spacer()
                                Image(systemName: "chevron.right")
                                    .font(.footnote)
                                    .foregroundStyle(.tertiary)
                            }
                        }
                        .foregroundStyle(.primary)
                    }
                } header: {
                    Text("Wybierz miasto")
                }
            }
            .navigationTitle("Miasto")
            .navigationBarTitleDisplayMode(.inline)
        }
    }
}
