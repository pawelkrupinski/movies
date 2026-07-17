import SwiftUI

@main
struct KinowoApp: App {
    @StateObject private var store = RepertoireStore()
    @StateObject private var details = DetailsStore()
    @StateObject private var catalog = CatalogStore()
    @StateObject private var prefs: UserPreferences
    @StateObject private var authService: AuthService
    @StateObject private var sync: StateSyncService
    @StateObject private var deepLink = DeepLinkCoordinator()

    init() {
        // Force the selected country's language at process start so the bundle's
        // preferred localization (what `Text`/`String(localized:)` resolve
        // against) matches the choice from the first frame — Poland's Polish by
        // default. iOS reads `AppleLanguages` only at launch, so an in-session
        // country switch persists the new tag here and fully lands on relaunch;
        // the root `.environment(\.locale)` below flips SwiftUI `Text` in-session.
        UserDefaults.standard.set([CountrySelection.current().languageCode], forKey: "AppleLanguages")
        let preferences = UserPreferences()
        let authService = AuthService()
        _prefs = StateObject(wrappedValue: preferences)
        _authService = StateObject(wrappedValue: authService)
        _sync = StateObject(wrappedValue: StateSyncService(
            prefs: preferences,
            userPublisher: authService.$user.eraseToAnyPublisher(),
            client: HttpUserStateClient()
        ))
    }

    var body: some Scene {
        WindowGroup {
            root
                .environmentObject(store)
                .environmentObject(details)
                .environmentObject(catalog)
                .environmentObject(prefs)
                .environmentObject(authService)
                .environmentObject(sync)
                .environmentObject(deepLink)
                // Follow the selected country's language for in-session SwiftUI
                // `Text(LocalizedStringKey)` resolution (keyed to the choice so a
                // switch re-localizes the view tree).
                .environment(\.locale, Locale(identifier: prefs.selectedCountry.languageCode))
                .id(prefs.selectedCountry.code)
                .preferredColorScheme(.dark)
                .tint(Color(red: 0.42, green: 0.67, blue: 0.87))
                .task { await authService.checkSession() }
                // A kinowo.fly.dev Universal Link (or kinowo:// link) opened the
                // app. Switch the city eagerly so the CityGate flips straight to
                // it on a cold launch; ContentView applies the filters + film.
                .onOpenURL { handleDeepLink($0) }
                #if DEBUG
                // UI tests can't deliver a real Universal Link, so they inject
                // one through the same path via a launch-env var (mirrors the
                // KINOWO_UITEST_FIXTURE / KINOWO_FORCE_DETECTED_CITY hooks).
                .task {
                    if let raw = ProcessInfo.processInfo.environment["KINOWO_UITEST_DEEPLINK"],
                       let url = URL(string: raw) {
                        handleDeepLink(url)
                    }
                }
                #endif
        }
    }

    /// Route an inbound deep link: switch the city eagerly (so a cold launch
    /// from a link lands on the linked city's CityGate result) and park the
    /// parsed link for `ContentView` to apply its filters + film push.
    private func handleDeepLink(_ url: URL) {
        guard let link = DeepLink.parse(url, knownCitySlugs: catalog.allSlugs) else { return }
        if link.citySlug != prefs.selectedCity {
            prefs.setCity(link.citySlug)
            store.use(citySlug: link.citySlug)
            details.use(citySlug: link.citySlug)
        }
        deepLink.pending = link
    }

    /// Normally `ContentView`. In DEBUG, setting the `KINOWO_TUNING` launch
    /// env var swaps in the non-prod `ShowtimeTuningScreen` instead — a quick
    /// way to dial in the showtime-pill look on a real device without adding
    /// any UI to the shipping app.
    @ViewBuilder
    private var root: some View {
        #if DEBUG
        if ProcessInfo.processInfo.environment["KINOWO_TUNING"] != nil {
            ShowtimeTuningScreen()
        } else {
            CityGate()
        }
        #else
        CityGate()
        #endif
    }
}
