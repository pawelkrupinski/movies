import Foundation

/// UserDefaults-backed per-device hidden-films + disabled-cinemas state.
/// Mirrors what the web app stores in `localStorage` for anonymous users.
final class UserPreferences: ObservableObject {
    @Published private(set) var hiddenFilms: Set<String> = []
    @Published private(set) var disabledCinemas: Set<String> = []

    private let store: UserDefaults
    private let kHidden        = "hiddenFilms"
    private let kDisabled      = "disabledCinemas"

    init(store: UserDefaults = .standard) {
        self.store = store
        hiddenFilms         = Set(store.stringArray(forKey: kHidden)        ?? [])
        disabledCinemas     = Set(store.stringArray(forKey: kDisabled)      ?? [])
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

    func toggleCinema(_ cinema: String, disabled: Bool) {
        if disabled { disabledCinemas.insert(cinema) }
        else        { disabledCinemas.remove(cinema) }
        store.set(Array(disabledCinemas), forKey: kDisabled)
    }
}
