import Foundation
// See `RepertoireClient.swift`: a no-op in the Xcode app's flat module, true
// only in the `KinowoNetworking` SPM target that lets `swift test` drive this.
#if canImport(KinowoCore)
@testable import KinowoCore
#endif
#if canImport(KinowoAuth)
@testable import KinowoAuth
#endif

/// Switch the whole app to `country`: persist the pick AND re-point both
/// stores at that country's deployment. The stores hold their own `base`,
/// captured at init from the launch-time country, so persisting the pick
/// alone leaves every later `use(citySlug:)` fetching the new country's city
/// from the OLD deployment. The one entry point every country change goes
/// through — the picker's country pills, a located/chosen city in another
/// country, and a deep link into another deployment.
@MainActor
func switchCountry(to country: Country, prefs: UserPreferences, store: RepertoireStore, details: DetailsStore) {
    prefs.setCountry(country)
    store.use(country: country)
    details.use(country: country)
}
