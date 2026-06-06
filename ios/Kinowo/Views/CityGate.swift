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
                    Text("Ustalamy najbliższe miasto…")
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
            switch await resolver.resolve() {
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
            Text("Wygląda na to, że jesteś w pobliżu miasta")
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
            Text(city.name)
                .font(.title).bold()
            Spacer()
            Button(action: onConfirm) {
                Text("Pokaż repertuar — \(city.name)")
                    .frame(maxWidth: .infinity)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .accessibilityIdentifier(A11y.CityGate.confirmButton)
            Button(action: onChooseOther) {
                Text("Wybierz inne miasto")
                    .frame(maxWidth: .infinity)
            }
            .buttonStyle(.bordered)
            .controlSize(.large)
        }
        .padding(24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

/// Manual city picker — the fallback when location is unavailable or the user
/// is outside every served city. A native grouped `List`, one row per
/// `City.all`, so it grows automatically as cities are added.
struct CityChoiceView: View {
    @EnvironmentObject var prefs: UserPreferences
    /// The location-detected nearest city, when one was found — used only to
    /// pre-suppress the switch prompt for a deliberate pick of another city.
    /// `nil` when location was unavailable (then there's nothing to suppress).
    var nearest: City?

    var body: some View {
        NavigationStack {
            List {
                Section {
                    ForEach(City.all, id: \.slug) { city in
                        Button {
                            choose(city)
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
