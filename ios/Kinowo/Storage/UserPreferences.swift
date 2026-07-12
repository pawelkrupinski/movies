import Foundation
import Combine

/// UserDefaults-backed per-device preferences: hidden films, the selected
/// cinema pill, and a web-sync mirror of the browser's `disabledCinemas`.
/// Mirrors what the web app stores in `localStorage` for anonymous users.
final class UserPreferences: ObservableObject {
    @Published private(set) var hiddenFilms: Set<String> = []
    /// Single-select cinema pill: `nil` = Wszystkie (no cinema constraint),
    /// otherwise the full name of the one cinema whose screenings are shown.
    /// Per-device (not server-synced); cleared when the user switches city so
    /// a name from the old city can't linger and blank the new city's screen.
    @Published private(set) var selectedCinema: String?
    /// The excluded-cinemas set, shared with the web's `disabledCinemas`
    /// localStorage (round-trips via `StateSyncService` + the `?cinema=` deep
    /// link). It is the active cinema filter for SPLIT cities (London): the
    /// multi-select area picker adds/removes names here and `filteredFor` drops
    /// them. FLAT cities still use the single-select `selectedCinema` pill and
    /// don't consult this set. Global across cities like the web — a stale name
    /// from another city simply never matches.
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
    private let kSelectedCinema = "selectedCinema"
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
        selectedCinema      = store.string(forKey: kSelectedCinema)
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

    /// Pick the single active cinema (nil = Wszystkie). Persisted per-device.
    func setSelectedCinema(_ cinema: String?) {
        selectedCinema = cinema
        if let cinema { store.set(cinema, forKey: kSelectedCinema) }
        else          { store.removeObject(forKey: kSelectedCinema) }
    }

    /// Replace the whole excluded-cinemas set. Written by `StateSyncService`,
    /// the `?cinema=` deep link, and the split-city area picker's mutators below.
    func setDisabledCinemas(_ s: Set<String>) {
        disabledCinemas = s
        store.set(Array(disabledCinemas), forKey: kDisabled)
    }

    /// Enable/disable one cinema (split-city area picker's per-cinema checkbox).
    func setCinemaEnabled(_ cinema: String, _ enabled: Bool) {
        var s = disabledCinemas
        if enabled { s.remove(cinema) } else { s.insert(cinema) }
        setDisabledCinemas(s)
    }

    /// Enable/disable every cinema in an area at once (the area checkbox). Other
    /// areas' and other cities' entries are left untouched.
    func setAreaEnabled(_ cinemas: [String], _ enabled: Bool) {
        var s = disabledCinemas
        if enabled { cinemas.forEach { s.remove($0) } } else { cinemas.forEach { s.insert($0) } }
        setDisabledCinemas(s)
    }

    /// Enable/disable every cinema in the current (split) city — the "all
    /// cinemas" master. `cityCinemas` is the city's full universe; entries
    /// naming other cities' cinemas are preserved.
    func setAllCinemasEnabled(_ cityCinemas: [String], _ enabled: Bool) {
        var s = disabledCinemas
        if enabled { cityCinemas.forEach { s.remove($0) } } else { cityCinemas.forEach { s.insert($0) } }
        setDisabledCinemas(s)
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
        // A cinema pill from the old city names nothing in the new one; clear it
        // so the new city opens on Wszystkie rather than an empty (guarded) list.
        setSelectedCinema(nil)
    }

    /// Clear the selected city, re-gating the app to the city chooser. Used by
    /// the in-app country switch: the old city may not exist under the new
    /// country's deployment, so drop it (and its now-stale cinema pill) and let
    /// the gate re-ask — the same state the app starts in before a city is
    /// chosen. No-op when no city is set.
    func clearCity() {
        guard selectedCity != nil else { return }
        selectedCity = nil
        store.removeObject(forKey: kCity)
        // A cinema pill belongs to the old city's list; drop it so a re-picked
        // city can't open on a stale (guarded-away, empty) selection.
        setSelectedCinema(nil)
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
