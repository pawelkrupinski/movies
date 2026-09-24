import Foundation
import Combine

/// One local hiddenFilms edit, as `UserPreferences.hiddenFilmsChanges`
/// reports it — specific enough that `StateSyncService` can call the
/// matching `HiddenFilmsClient` method directly instead of diffing a
/// before/after `Set`.
enum HiddenFilmsChange: Equatable {
    case hidden(String)
    case unhidden(String)
    case clearedAll
}

/// UserDefaults-backed per-device preferences: hidden films and the
/// cross-platform `disabledCinemas` exclusion set.
/// Mirrors what the web app stores in `localStorage` for anonymous users.
final class UserPreferences: ObservableObject {
    /// The hidden titles of the country being browsed. Stored PER COUNTRY
    /// (one `hiddenFilms_<code>` key each), like the server's
    /// `/api/me/{country}/hidden-films` buckets and Android's store, so a
    /// country switch shows that country's own set instead of carrying the
    /// previous one across. Re-read from the store by `setCountry`.
    @Published private(set) var hiddenFilms: Set<String> = []
    /// The excluded-cinemas set, shared in NAME only with the web's
    /// `disabledCinemas` localStorage key — both are device-local, neither
    /// round-trips through `StateSyncService` any more (only hiddenFilms
    /// does). It is THE cinema filter: the Filtry sheet's "Kina" section
    /// adds/removes names here — one checkbox per cinema on a flat city, area
    /// groups on a split one — and `filteredFor` drops them. Global across
    /// cities like the web, so a stale name from another city never matches
    /// (which is why a city switch needs no reset).
    @Published private(set) var disabledCinemas: Set<String> = []
    /// True once the user has swiped between Filmy / Kina at least once.
    @Published private(set) var hasSwipedScreens: Bool = false
    /// `yyyy-MM-dd` of the last day the swipe hint was shown, or "" if never.
    @Published private(set) var swipeHintShownDate: String = ""
    /// Slug of the city the user is browsing, or `nil` until the
    /// first-launch gate resolves one (by location or explicit pick).
    @Published private(set) var selectedCity: String?
    /// The most-recent `chosen→nearest` pair the "switch city?" prompt was
    /// shown for, or `nil` if never. Only the single latest pair is kept, so
    /// returning to a previously-declined city re-arms the prompt.
    @Published private(set) var citySwitchPromptKey: String?
    /// Countries `StateSyncService` has done the one-time local→server
    /// hiddenFilms migration for (see `HiddenFilmsClient`). Per-country, not
    /// one global flag — hiddenFilms is per-country now, and a device might
    /// migrate PL today and not touch the US bucket for months. After a
    /// country is in this set the server is authoritative for it on every
    /// reconcile (so a removal sticks); cleared on logout so the next
    /// sign-in migrates every country afresh.
    private var hiddenFilmsMigratedCountries: Set<String> = []
    /// Slugs of split cities whose first-visit area picker the user has already
    /// completed, so it shows once per city (never on a flat city). Device-local.
    @Published private(set) var areaPickerSeenCities: Set<String> = []
    /// True while the city gate must present the country's list instead of
    /// offering a located city. Set when the user picks a country themselves:
    /// they have just said which country they want, and answering that with
    /// "you're near Poznań" offers the very thing they navigated away from.
    /// Cleared by `setCity(_:)`, however the city was reached.
    @Published private(set) var awaitingExplicitCityPick: Bool = false
    /// The selected country (see `Country`) — which deployment the app talks
    /// to. Defaults to Poland until the user picks otherwise. Persisted via
    /// `CountrySelection` (same `store`) so it's readable at launch by
    /// `kinowoBaseURL`.
    @Published private(set) var selectedCountry: Country
    /// The selected UI language — fully independent of `selectedCountry` (see
    /// `LanguageSelection`). Resolved at init from the persisted pick, else
    /// device-preferred, else English — the storefront-informed fallback is
    /// primed asynchronously by `KinowoApp` for the *next* launch, since
    /// StoreKit's lookup can't run inside this synchronous initializer.
    @Published private(set) var selectedLanguage: String

    private let store: UserDefaults
    /// Builds before per-country sets kept ONE device-wide set here; `init`
    /// folds it into the country it was made in (see `settleLegacyHiddenFilms`).
    private let kHiddenLegacy  = "hiddenFilms"
    private let kHiddenPrefix  = "hiddenFilms_"
    private let kDisabled      = "disabledCinemas"
    private let kSwiped        = "swipedScreens"
    private let kHintDate      = "swipeHintShownDate"
    private let kCity          = "selectedCity"
    private let kSwitchPrompt  = "citySwitchPromptKey"
    private let kHiddenFilmsMigrated = "hiddenFilmsMigratedCountries"
    private let kHiddenFilmsETags        = "hiddenFilmsETags"
    private let kHiddenFilmsLastModified = "hiddenFilmsLastModified"
    /// Written by builds whose device-wide set needed to remember which
    /// country it mirrored; only ever removed now.
    private let kHiddenFilmsMirroredLegacy = "hiddenFilmsMirroredCountry"
    private let kAreaSeen       = "areaPickerSeenCities"
    private let kPendingLanguage = "pendingLanguagePush"
    private let kExplicitPick   = "awaitingExplicitCityPick"

    init(store: UserDefaults = .standard) {
        self.store = store
        disabledCinemas     = Set(store.stringArray(forKey: kDisabled)      ?? [])
        hasSwipedScreens    = store.bool(forKey: kSwiped)
        swipeHintShownDate  = store.string(forKey: kHintDate)              ?? ""
        selectedCity        = store.string(forKey: kCity)
        citySwitchPromptKey = store.string(forKey: kSwitchPrompt)
        hiddenFilmsMigratedCountries = Set(store.stringArray(forKey: kHiddenFilmsMigrated) ?? [])
        awaitingExplicitCityPick = store.bool(forKey: kExplicitPick)
        areaPickerSeenCities = Set(store.stringArray(forKey: kAreaSeen) ?? [])
        selectedCountry     = CountrySelection.current(store)
        selectedLanguage    = LanguageSelection.resolve(storefrontCountry: nil, defaults: store)
        settleLegacyHiddenFilms()
        hiddenFilms         = hiddenFilms(country: selectedCountry.code)

        #if DEBUG
        // UI tests force the first-launch city gate by ignoring any persisted
        // city, so the gate's buttons can be measured on a deterministic screen.
        if ProcessInfo.processInfo.environment["KINOWO_CLEAR_CITY"] != nil {
            selectedCity = nil
        }
        #endif
    }

    /// Fires once per hide/unhide/clear-all — the SPECIFIC operation, not
    /// just "the set changed" — so `StateSyncService` can call the matching
    /// granular endpoint (`hide`/`unhide`/`clear`) instead of having to
    /// infer one from a before/after diff. A `PassthroughSubject`, not
    /// `$hiddenFilms`: it never replays a stored value at subscribe time, so
    /// unlike the old `$hiddenFilms.dropFirst()` sync observation, there's
    /// no startup value to skip.
    let hiddenFilmsChanges = PassthroughSubject<HiddenFilmsChange, Never>()

    /// Hide/unhide/clear-all act on the country being browsed.
    func hide(_ title: String) {
        setHiddenFilms(hiddenFilms.union([title]), country: selectedCountry.code)
        hiddenFilmsChanges.send(.hidden(title))
    }

    func unhide(_ title: String) {
        setHiddenFilms(hiddenFilms.subtracting([title]), country: selectedCountry.code)
        hiddenFilmsChanges.send(.unhidden(title))
    }

    func unhideAll() {
        setHiddenFilms([], country: selectedCountry.code)
        hiddenFilmsChanges.send(.clearedAll)
    }

    /// Replace the whole excluded-cinemas set — the single writer. The Filtry
    /// sheet's "Kina" section works the new set out via `CinemaFilterSection`
    /// (which keeps other cities' entries intact); `StateSyncService`, the
    /// `?cinema=` deep link and the first-visit area picker write here too.
    func setDisabledCinemas(_ s: Set<String>) {
        disabledCinemas = s
        store.set(Array(disabledCinemas), forKey: kDisabled)
    }

    /// Mark a split city's first-visit area picker as completed (shows once).
    func markAreaPickerSeen(_ slug: String) {
        guard !areaPickerSeenCities.contains(slug) else { return }
        areaPickerSeenCities.insert(slug)
        store.set(Array(areaPickerSeenCities), forKey: kAreaSeen)
    }

    /// Forget every country's hidden titles — account deletion, where
    /// `unhideAll` (the browsed country only) would leave the other countries'
    /// sets for the next sign-in's first-login union to upload. Mirrors
    /// Android's `clearAllHiddenFilms`.
    func clearAllHiddenFilms() {
        store.dictionaryRepresentation().keys
            .filter { $0.hasPrefix(kHiddenPrefix) }
            .forEach(store.removeObject(forKey:))
        if !hiddenFilms.isEmpty { hiddenFilms = [] }
    }

    /// `country`'s hidden titles (server code space — `pl`, `uk`, …),
    /// whether or not it is the country being browsed.
    func hiddenFilms(country: String) -> Set<String> {
        Set(store.stringArray(forKey: kHiddenPrefix + country) ?? [])
    }

    /// Replace `country`'s whole hidden-films set — used by `StateSyncService`
    /// when the server is authoritative (mirror the remote set, dropping
    /// local-only entries the user removed elsewhere). Updates the published
    /// `hiddenFilms` only when `country` is the one being browsed.
    func setHiddenFilms(_ s: Set<String>, country: String) {
        store.set(Array(s), forKey: kHiddenPrefix + country)
        if country == selectedCountry.code, hiddenFilms != s { hiddenFilms = s }
    }

    /// Fold the pre-per-country device-wide set into the country it was made
    /// in — the one selected when this build first launches — unless that
    /// country already has its own set. Idempotent; the legacy key is gone
    /// after the first run.
    ///
    /// Also forgets every stored validator: the device-wide set was reconciled
    /// against whichever country was selected at the time, so no country's
    /// ETag describes what its new bucket holds, and replaying one would draw
    /// a 304 that strands the wrong set. The migrated flags stay, so each
    /// country's next reconcile takes a fresh 200 and REPLACES its bucket.
    private func settleLegacyHiddenFilms() {
        store.removeObject(forKey: kHiddenFilmsMirroredLegacy)
        guard let legacy = store.stringArray(forKey: kHiddenLegacy) else { return }
        let key = kHiddenPrefix + selectedCountry.code
        if store.object(forKey: key) == nil { store.set(legacy, forKey: key) }
        store.removeObject(forKey: kHiddenLegacy)
        store.removeObject(forKey: kHiddenFilmsETags)
        store.removeObject(forKey: kHiddenFilmsLastModified)
    }

    /// Whether `country` has completed its one-time local→server hiddenFilms
    /// migration (see `hiddenFilmsMigratedCountries`'s doc comment).
    func isHiddenFilmsMigrated(country: String) -> Bool {
        hiddenFilmsMigratedCountries.contains(country)
    }

    /// Mark `country`'s migration done. Never unmarks a single country —
    /// logout clears ALL of them at once, see `clearHiddenFilmsMigration()`.
    func setHiddenFilmsMigrated(country: String) {
        guard !hiddenFilmsMigratedCountries.contains(country) else { return }
        hiddenFilmsMigratedCountries.insert(country)
        store.set(Array(hiddenFilmsMigratedCountries), forKey: kHiddenFilmsMigrated)
    }

    /// Undo every country's migration flag AND forget every stored
    /// validator — a genuine logout, so the next sign-in (possibly a
    /// different account) migrates every country afresh rather than reusing
    /// this device's previous account's ETags. See `StateSyncService`.
    func clearHiddenFilmsMigration() {
        hiddenFilmsMigratedCountries.removeAll()
        store.removeObject(forKey: kHiddenFilmsMigrated)
        store.removeObject(forKey: kHiddenFilmsETags)
        store.removeObject(forKey: kHiddenFilmsLastModified)
    }

    /// The stored `(ETag, Last-Modified)` pair for `country`'s last known
    /// hiddenFilms fetch/write, or `(nil, nil)` if this device has never
    /// synced that country. Fed back as `If-None-Match`/`If-Modified-Since`
    /// on the next conditional fetch.
    func hiddenFilmsValidators(country: String) -> (etag: String?, lastModified: String?) {
        let etags         = store.dictionary(forKey: kHiddenFilmsETags)        as? [String: String] ?? [:]
        let lastModifieds = store.dictionary(forKey: kHiddenFilmsLastModified) as? [String: String] ?? [:]
        return (etags[country], lastModifieds[country])
    }

    /// Store the validators a fetch or write just returned for `country`, so
    /// the NEXT fetch can take the conditional-GET fast path.
    func setHiddenFilmsValidators(country: String, etag: String, lastModified: String) {
        var etags = store.dictionary(forKey: kHiddenFilmsETags) as? [String: String] ?? [:]
        etags[country] = etag
        store.set(etags, forKey: kHiddenFilmsETags)
        var lastModifieds = store.dictionary(forKey: kHiddenFilmsLastModified) as? [String: String] ?? [:]
        lastModifieds[country] = lastModified
        store.set(lastModifieds, forKey: kHiddenFilmsLastModified)
    }

    func markSwiped() {
        guard !hasSwipedScreens else { return }
        hasSwipedScreens = true
        store.set(true, forKey: kSwiped)
    }

    func markSwipeHintShown(_ day: String) {
        swipeHintShownDate = day
        store.set(day, forKey: kHintDate)
    }

    func setCity(_ slug: String) {
        // The gate is satisfied however the city was reached, so an
        // explicit-pick request never outlives the gate that asked for it.
        // Cleared before the guard: a re-pick of the same slug still ends it.
        clearExplicitCityPick()
        guard selectedCity != slug else { return }
        selectedCity = slug
        store.set(slug, forKey: kCity)
        // No cinema reset needed: `disabledCinemas` is global and scoped to the
        // current city at read time, so the new city simply starts with none of
        // its own cinemas excluded.
    }

    /// Clear the selected city, re-gating the app to the city chooser. Used by
    /// the in-app country switch: the old city may not exist under the new
    /// country's deployment, so drop it and let the gate re-ask — the same state
    /// the app starts in before a city is chosen. No-op when no city is set.
    func clearCity() {
        guard selectedCity != nil else { return }
        selectedCity = nil
        store.removeObject(forKey: kCity)
    }

    func setCitySwitchPromptKey(_ key: String) {
        guard citySwitchPromptKey != key else { return }
        citySwitchPromptKey = key
        store.set(key, forKey: kSwitchPrompt)
    }

    /// Persist the chosen country. The caller re-points the repertoire/details
    /// stores (`use(country:)`) so new fetches hit the new deployment. The UI
    /// language is unaffected — it's a fully independent preference (see
    /// `setLanguage`), so switching country never changes what the user reads.
    func setCountry(_ country: Country) {
        guard selectedCountry != country else { return }
        selectedCountry = country
        CountrySelection.select(country, in: store)
        hiddenFilms = hiddenFilms(country: country.code)
        // The user just said which country they want, so let them say which
        // city too: the re-gated app offers that country's list rather than
        // whatever city the device happens to sit near.
        awaitExplicitCityPick()
    }

    /// Persist an explicit language pick. iOS reads `AppleLanguages` at
    /// process start, so the bundle switch fully lands on the next launch;
    /// the in-session locale is injected at the root via
    /// `.environment(\.locale)`, keyed off `selectedLanguage`.
    func setLanguage(_ code: String) {
        guard selectedLanguage != code else { return }
        selectedLanguage = code
        LanguageSelection.select(code, in: store)
    }

    /// The persisted EXPLICIT language pick, or nil if the user has never
    /// made one. Unlike `selectedLanguage` — always resolved to a real value
    /// via the device/storefront fallback, never nil — this distinguishes
    /// "chose one" from "still on the resolved default", which
    /// `StateSyncService` needs: pushing a resolved default up would stamp
    /// it on the account the first time a signed-in visitor merely opens the
    /// app, before they have ever touched the language picker.
    var explicitLanguage: String? { LanguageSelection.explicit(store) }

    /// A language pick the account hasn't confirmed yet (see
    /// `StateSyncService.pendingLanguage`). Persisted so a push that failed —
    /// or never ran because the app was killed inside the debounce — is still
    /// retried after a relaunch instead of losing to the account's older value.
    var pendingLanguagePush: String? { store.string(forKey: kPendingLanguage) }

    func setPendingLanguagePush(_ code: String?) {
        if let code { store.set(code, forKey: kPendingLanguage) } else { store.removeObject(forKey: kPendingLanguage) }
    }

    /// Ask the city gate for an explicit pick rather than a located offer.
    func awaitExplicitCityPick() {
        guard !awaitingExplicitCityPick else { return }
        awaitingExplicitCityPick = true
        store.set(true, forKey: kExplicitPick)
    }

    private func clearExplicitCityPick() {
        guard awaitingExplicitCityPick else { return }
        awaitingExplicitCityPick = false
        store.removeObject(forKey: kExplicitPick)
    }
}
