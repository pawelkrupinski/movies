import Foundation
import Combine

/// UserDefaults-backed per-device hidden-films + disabled-cinemas state.
/// Mirrors what the web app stores in `localStorage` for anonymous users.
final class UserPreferences: ObservableObject {
    @Published private(set) var hiddenFilms: Set<String> = []
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

    private let store: UserDefaults
    private let kHidden        = "hiddenFilms"
    private let kDisabled      = "disabledCinemas"
    private let kSwiped        = "swipedScreens"
    private let kHintDate      = "swipeHintShownDate"
    private let kCity          = "selectedCity"
    private let kSwitchPrompt  = "citySwitchPromptKey"
    private let kServerSynced  = "serverStateSynced"

    init(store: UserDefaults = .standard) {
        self.store = store
        hiddenFilms         = Set(store.stringArray(forKey: kHidden)        ?? [])
        disabledCinemas     = Set(store.stringArray(forKey: kDisabled)      ?? [])
        hasSwipedScreens    = store.bool(forKey: kSwiped)
        swipeHintShownDate  = store.string(forKey: kHintDate)              ?? ""
        selectedCity        = store.string(forKey: kCity)
        citySwitchPromptKey = store.string(forKey: kSwitchPrompt)
        serverStateSynced   = store.bool(forKey: kServerSynced)

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

    func setDisabledCinemas(_ s: Set<String>) {
        disabledCinemas = s
        store.set(Array(disabledCinemas), forKey: kDisabled)
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

    func toggleCinema(_ cinema: String, disabled: Bool) {
        if disabled { disabledCinemas.insert(cinema) }
        else        { disabledCinemas.remove(cinema) }
        store.set(Array(disabledCinemas), forKey: kDisabled)
    }

    // `disabledCinemas` is ONE global set shared across every city (matching
    // the web's single `disabledCinemas` localStorage list) — switching city
    // doesn't touch it, so a cinema deselected in another city lingers even
    // though it isn't one of this city's cinemas. Deriving a count or the
    // "Wszystkie kina" toggle from the raw set therefore reads stale entries:
    // the count came out one short and the master toggle showed "not all
    // selected" the moment you arrived. Every aggregate must be scoped to the
    // cinemas that actually belong to the current city. (A per-card membership
    // test — `filteredFor` — doesn't need this: a stale name never matches a
    // card of this city.)

    /// The disabled cinemas that belong to `cityCinemas` — the only ones whose
    /// state should drive this city's count / select-all.
    func disabledCinemas(in cityCinemas: [String]) -> Set<String> {
        disabledCinemas.intersection(cityCinemas)
    }

    /// True when every cinema of `cityCinemas` is selected (none disabled here).
    func allCinemasSelected(in cityCinemas: [String]) -> Bool {
        disabledCinemas.isDisjoint(with: cityCinemas)
    }

    /// Select-all (`selected = true`) / deselect-all scoped to one city.
    /// Deselections made in OTHER cities are preserved untouched.
    func setAllCinemas(in cityCinemas: [String], selected: Bool) {
        let others = disabledCinemas.subtracting(cityCinemas)
        setDisabledCinemas(selected ? others : others.union(cityCinemas))
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
    }

    func setCitySwitchPromptKey(_ key: String) {
        guard citySwitchPromptKey != key else { return }
        citySwitchPromptKey = key
        store.set(key, forKey: kSwitchPrompt)
    }
}
