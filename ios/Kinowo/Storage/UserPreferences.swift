import Foundation

/// UserDefaults-backed per-device favourites + hidden-films state.
/// Mirrors what the web app stores in `localStorage` for anonymous users.
final class UserPreferences: ObservableObject {
    @Published private(set) var favouriteMovies: Set<String> = []
    @Published private(set) var favouriteScreenings: Set<String> = []
    @Published private(set) var hiddenFilms: Set<String> = []

    private let store: UserDefaults
    private let kFavMovies     = "favouriteMovies"
    private let kFavScreenings = "favouriteScreenings"
    private let kHidden        = "hiddenFilms"

    init(store: UserDefaults = .standard) {
        self.store = store
        favouriteMovies     = Set(store.stringArray(forKey: kFavMovies)     ?? [])
        favouriteScreenings = Set(store.stringArray(forKey: kFavScreenings) ?? [])
        hiddenFilms         = Set(store.stringArray(forKey: kHidden)        ?? [])
    }

    func toggleFavouriteMovie(_ title: String) {
        if favouriteMovies.contains(title) { favouriteMovies.remove(title) }
        else { favouriteMovies.insert(title) }
        store.set(Array(favouriteMovies), forKey: kFavMovies)
    }

    func toggleFavouriteScreening(_ id: String) {
        if favouriteScreenings.contains(id) { favouriteScreenings.remove(id) }
        else { favouriteScreenings.insert(id) }
        store.set(Array(favouriteScreenings), forKey: kFavScreenings)
    }

    func hide(_ title: String) {
        hiddenFilms.insert(title)
        store.set(Array(hiddenFilms), forKey: kHidden)
    }

    func unhide(_ title: String) {
        hiddenFilms.remove(title)
        store.set(Array(hiddenFilms), forKey: kHidden)
    }

    /// Same id shape the web app uses (`title|cinema|YYYY-MM-DDTHH:MM`) so
    /// a future server-sync feature can reuse it without translation.
    static func screeningId(title: String, cinema: String, date: String, time: String) -> String {
        "\(title)|\(cinema)|\(date)T\(time)"
    }
}
