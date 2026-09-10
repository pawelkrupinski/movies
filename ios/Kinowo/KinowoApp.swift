import SwiftUI
import UIKit
import StoreKit

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
        // Force the resolved language at process start so the bundle's
        // preferred localization (what `Text`/`String(localized:)` resolve
        // against) matches the choice from the first frame. StoreKit's
        // storefront lookup is async and can't block this synchronous
        // initializer, so the first resolution only sees an explicit pick or
        // the device-preferred language (falling back to English) —
        // `resolveStorefrontLanguage()` below re-resolves with the storefront
        // once it's available and primes `AppleLanguages` for the *next*
        // launch. iOS reads `AppleLanguages` only at launch, so an in-session
        // language switch persists the new tag here too and fully lands on
        // relaunch; the root `.environment(\.locale)` below flips SwiftUI
        // `Text` in-session.
        UserDefaults.standard.set([LanguageSelection.resolve(storefrontCountry: nil)], forKey: "AppleLanguages")
        let preferences = UserPreferences()
        let authService = AuthService()
        _prefs = StateObject(wrappedValue: preferences)
        _authService = StateObject(wrappedValue: authService)
        _sync = StateObject(wrappedValue: StateSyncService(
            prefs: preferences,
            userPublisher: authService.$user.eraseToAnyPublisher(),
            client: HttpUserStateClient()
        ))
        #if DEBUG
        Self.seedUITestPoster()
        #endif
    }

    /// Re-resolve the language once the real App Store storefront is known,
    /// and prime `AppleLanguages` for the *next* launch — this can't change
    /// the current session's bundle (iOS only reads `AppleLanguages` at
    /// process start), only make the following launch storefront-aware.
    /// Skipped if the user has since made an explicit pick, which always wins.
    private func resolveStorefrontLanguage() async {
        let storefrontCountry = await Storefront.current?.countryCode
        guard LanguageSelection.explicit() == nil else { return }
        let resolved = LanguageSelection.resolve(storefrontCountry: storefrontCountry)
        UserDefaults.standard.set([resolved], forKey: "AppleLanguages")
    }

    #if DEBUG
    /// `KINOWO_UITEST_SEED_POSTER=1`: put one poster in `PosterStore` before
    /// the first frame, under a URL nothing can download
    /// (`RepertoireStore.uiTestSeededPosterURL`). Every screen that reads the
    /// cache renders it; any screen that re-downloads instead shows "Brak
    /// plakatu" — which is how `DetailPosterCacheUITests` tells the two apart
    /// without a network.
    private static func seedUITestPoster() {
        guard RepertoireStore.uiTestPosterSeedEnabled else { return }
        let size = CGSize(width: 60, height: 90)
        let image = UIGraphicsImageRenderer(size: size).image { context in
            UIColor.systemTeal.setFill()
            context.fill(CGRect(origin: .zero, size: size))
        }
        guard let data = image.pngData() else { return }
        PosterStore.shared.seed(data, for: RepertoireStore.uiTestSeededPosterURL)
    }
    #endif

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
                // Follow the selected (country-independent) language for
                // in-session SwiftUI `Text(LocalizedStringKey)` resolution
                // (keyed to the choice so a switch re-localizes the view tree).
                //
                // Language ONLY, deliberately NOT country too: `/api/catalog`
                // (`CatalogController.catalog()`, web) is served identically by
                // every deployment — no per-country branching at all — so a
                // country switch has nothing catalog-side that needs a rebuild
                // to pick up. Repertoire data is re-pointed directly instead,
                // by whoever actually changes country: `handleDeepLink` calls
                // `store.use(citySlug:)`/`details.use(citySlug:)` itself, and
                // `CityGate`'s own `ContentView().task(id: slug)` re-fires off
                // the CITY slug the moment one is chosen for the new country —
                // neither needs this tree torn down. Keying on country too
                // (tried, reverted) instead tore down `CityGate`'s own in-progress
                // country→city picker flow (`CityChoiceView`) the instant a
                // country was tapped, since the remount reset the whole subtree's
                // local `@State` before the user could pick a city for it.
                .environment(\.locale, Locale(identifier: prefs.selectedLanguage))
                .id(prefs.selectedLanguage)
                .preferredColorScheme(.dark)
                .tint(Color(red: 0.42, green: 0.67, blue: 0.87))
                .task { await authService.checkSession() }
                .task { await resolveStorefrontLanguage() }
                // A kinowo.net Universal Link (or kinowo:// link) opened the
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
        guard let link = DeepLink.parse(url, knownCitySlugs: catalog.allSlugs,
                                        languageTokens: { catalog.versionTokens(ofSlug: $0).accepted }) else { return }
        // A link on another country's deployment (showtimes-uk / showtimes-de)
        // must switch the country too, or the city would resolve against the
        // wrong deployment's catalog. Setting it re-points `kinowoBaseURL`; the
        // UI language is unaffected — it's a fully independent preference (see
        // `LanguageSelection`), so a cross-country link never changes it. No-ops
        // when already in that country (e.g. a same-country kinowo.net link).
        if let countryCode = catalog.cities.country(ofSlug: link.citySlug) {
            prefs.setCountry(catalog.country(code: countryCode))
        }
        if link.citySlug != prefs.selectedCity {
            prefs.setCity(link.citySlug)
            store.use(citySlug: link.citySlug, timeZone: catalog.zone(ofSlug: link.citySlug, inCountry: prefs.selectedCountry))
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
