import SwiftUI

/// Height floor for the first-launch city-gate buttons — mirrors Android's
/// `ControlMinHeight` (56dp): a comfortable native touch target, well above the
/// ~44pt system-button default, so the gate's actions read as primary choices
/// rather than compact system controls.
private let cityControlMinHeight: CGFloat = 56

/// Full-width, tall (≥`cityControlMinHeight`) filled button — the primary city
/// action. Mirrors Android's `TallFilledButton` (17sp, SemiBold).
private struct TallFilledButton: View {
    let title: String
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.system(size: 17, weight: .semibold))
                .frame(maxWidth: .infinity, minHeight: cityControlMinHeight)
        }
        .buttonStyle(.borderedProminent)
    }
}

/// Full-width, tall (≥`cityControlMinHeight`) outlined button — a real
/// secondary control rather than a thin text link. Mirrors Android's
/// `TallOutlinedButton` (17sp).
private struct TallOutlinedButton: View {
    let title: String
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.system(size: 17))
                .frame(maxWidth: .infinity, minHeight: cityControlMinHeight)
        }
        .buttonStyle(.bordered)
    }
}

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
            // UI tests skip CoreLocation so the manual picker shows
            // deterministically (no permission dialog, no 8s timeout wait).
            if ProcessInfo.processInfo.environment["KINOWO_SKIP_LOCATION"] != nil {
                showChoice = true
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
            TallFilledButton(title: "Pokaż repertuar — \(city.name)", action: onConfirm)
            TallOutlinedButton(title: "Wybierz inne miasto", action: onChooseOther)
        }
        .padding(24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

/// Manual city picker — the fallback when location is unavailable or the user
/// is outside every served city. One tall button per `City.all`; with a single
/// city it's a one-tap screen, but it scales as cities are added. Mirrors
/// Android's `CityChoiceScreen` (centred title + full-width tall buttons).
struct CityChoiceView: View {
    @EnvironmentObject var prefs: UserPreferences
    /// The location-detected nearest city, when one was found — used only to
    /// pre-suppress the switch prompt for a deliberate pick of another city.
    /// `nil` when location was unavailable (then there's nothing to suppress).
    var nearest: City?

    var body: some View {
        VStack(spacing: 12) {
            Spacer()
            Text("Wybierz miasto")
                .font(.system(size: 22, weight: .bold))
            Text("Repertuar pokazujemy dla wybranego miasta.")
                .font(.callout)
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .padding(.bottom, 8)
            ForEach(City.all, id: \.slug) { city in
                TallFilledButton(title: city.name) { choose(city) }
                    .accessibilityIdentifier(A11y.CityGate.choiceButton)
            }
            Spacer()
        }
        .padding(24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
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
