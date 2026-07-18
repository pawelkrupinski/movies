import Foundation
import Combine

/// UserDefaults-backed per-device preferences: hidden films and the
/// cross-platform `disabledCinemas` exclusion set.
/// Mirrors what the web app stores in `localStorage` for anonymous users.
final class UserPreferences: ObservableObject {
    @Published private(set) var hiddenFilms: Set<String> = []
    /// The excluded-cinemas set, shared with the web's `disabledCinemas`
    /// localStorage (round-trips via `StateSyncService` + the `?cinema=` deep
    /// link). It is THE cinema filter: the Filtry sheet's "Kina" section
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
    /// True once `StateSyncService` has done its one-time migration of this
    /// device's local picks up to the account. After that the server is the
    /// source of truth on every launch (so removals stick); cleared on logout
    /// so the next sign-in migrates afresh.
    @Published private(set) var serverStateSynced: Bool = false
    /// Slugs of split cities whose first-visit area picker the user has already
    /// completed, so it shows once per city (never on a flat city). Device-local.
    @Published private(set) var areaPickerSeenCities: Set<String> = []
    /// The selected country (see `Country`) — which deployment the app talks to
    /// and which language it forces. Defaults to Poland until the user picks
    /// otherwise. Persisted via `CountrySelection` (same `store`) so it's
    /// readable at launch by `kinowoBaseURL`.
    @Published private(set) var selectedCountry: Country

    private let store: UserDefaults
    private let kHidden        = "hiddenFilms"
    private let kDisabled      = "disabledCinemas"
    private let kSwiped        = "swipedScreens"
    private let kHintDate      = "swipeHintShownDate"
    private let kCity          = "selectedCity"
    private let kSwitchPrompt  = "citySwitchPromptKey"
    private let kServerSynced  = "serverStateSynced"
    private let kAreaSeen       = "areaPickerSeenCities"

    init(store: UserDefaults = .standard) {
        self.store = store
        hiddenFilms         = Set(store.stringArray(forKey: kHidden)        ?? [])
        disabledCinemas     = Set(store.stringArray(forKey: kDisabled)      ?? [])
        hasSwipedScreens    = store.bool(forKey: kSwiped)
        swipeHintShownDate  = store.string(forKey: kHintDate)              ?? ""
        selectedCity        = store.string(forKey: kCity)
        citySwitchPromptKey = store.string(forKey: kSwitchPrompt)
        serverStateSynced   = store.bool(forKey: kServerSynced)
        areaPickerSeenCities = Set(store.stringArray(forKey: kAreaSeen) ?? [])
        selectedCountry     = CountrySelection.current(store)

        #if DEBUG
        // UI tests force the first-launch city gate by ignoring any persisted
        // city, so the gate's buttons can be measured on a deterministic screen.
        if ProcessInfo.processInfo.environment["KINOWO_CLEAR_CITY"] != nil {
            selectedCity = nil
        }
        #endif
    }

    func hide(_ title: String) {
        hiddenFilms.insert(title)
        store.set(Array(hiddenFilms), forKey: kHidden)
    }

    func unhide(_ title: String) {
        hiddenFilms.remove(title)
        store.set(Array(hiddenFilms), forKey: kHidden)
    }

    func unhideAll() {
        hiddenFilms.removeAll()
        store.set(Array(hiddenFilms), forKey: kHidden)
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

    /// Replace the whole hidden-films set — used by `StateSyncService` when the
    /// server is authoritative (mirror the remote set, dropping local-only
    /// entries the user removed elsewhere).
    func setHiddenFilms(_ s: Set<String>) {
        hiddenFilms = s
        store.set(Array(hiddenFilms), forKey: kHidden)
    }

    /// Mark the one-time local→server migration done / undone. Set after the
    /// first successful sync, cleared on logout so the next sign-in migrates.
    func setServerStateSynced(_ v: Bool) {
        serverStateSynced = v
        store.set(v, forKey: kServerSynced)
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

    /// Persist the chosen country and force its language. The caller re-points
    /// the repertoire/details stores (`use(country:)`) so new fetches hit the
    /// new deployment; the language flip fully lands on the next launch (iOS
    /// reads `AppleLanguages` at process start), with the in-session locale
    /// injected at the root via `CountrySelection.localeForCurrentSelection`.
    func setCountry(_ country: Country) {
        guard selectedCountry != country else { return }
        selectedCountry = country
        CountrySelection.select(country, in: store)
    }
}
