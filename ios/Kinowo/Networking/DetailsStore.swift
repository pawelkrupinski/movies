import Foundation
// See `RepertoireClient.swift`: a no-op in the Xcode app's flat module, true
// only in the `KinowoNetworking` SPM target that lets `swift test` drive this.
#if canImport(KinowoCore)
@testable import KinowoCore
#endif
#if canImport(KinowoAuth)
@testable import KinowoAuth
#endif

/// Fetches `/{city}/api/details` and exposes a title → details lookup the
/// detail screen reads synopsis + trailers from. Shares the conditional GET,
/// disk cache and switch handling with `RepertoireStore` through
/// `ConditionalListEndpoint`, but keys the payload into a map so
/// `FilmDetailView` can resolve a single film in O(1).
///
/// The business logic — JSON decode and `keyedByTitle` — lives on
/// `[FilmDetails]` in `KinowoCore`, so this stays a thin URLSession
/// shim and the map-building rule is unit-tested without the network.
@MainActor
final class DetailsStore: ObservableObject {
    /// title → details. Sparse: the backend only ships films that have
    /// a synopsis or a trailer, so a missing key is the common case.
    @Published private(set) var byTitle: [String: FilmDetails] = [:]

    private let endpoint: ConditionalListEndpoint<FilmDetails>

    /// `base` is the bare host; the fetch URL is `…/{citySlug}/api/details`.
    /// Same city-qualification contract as `RepertoireStore`.
    init(base: URL = kinowoBaseURL, citySlug: String = City.default.slug, session: URLSession = .shared) {
        endpoint = ConditionalListEndpoint(
            base: base, citySlug: citySlug, endpoint: "details", cache: .details, session: session)
    }

    /// Re-point at a different country's deployment and reload (see
    /// `RepertoireStore.use(country:)`).
    func use(country: Country) {
        guard endpoint.repoint(base: country.baseURL) else { return }
        resetForCitySwitch()
        Task { await reload() }
    }

    /// Re-point at a different city and reload (see `RepertoireStore.use`).
    func use(citySlug: String) {
        guard endpoint.repoint(citySlug: citySlug) else { return }
        resetForCitySwitch()
        Task { await reload() }
    }

    /// Drop the OUTGOING city's details, as `RepertoireStore` drops its films:
    /// a same-titled film in the new city must not show the old city's
    /// synopsis while the new fetch is in flight. A warm disk cache for the
    /// new city refills it at once.
    private func resetForCitySwitch() {
        byTitle = [:]
        loadCachedData()
    }

    /// Synopsis + trailers for a listing title, or `nil` when the
    /// backend had neither for this film (or details haven't loaded yet).
    func details(for title: String) -> FilmDetails? { byTitle[title] }

    func loadCachedData() {
        if byTitle.isEmpty, let cached = endpoint.cachedBody() {
            byTitle = cached.keyedByTitle()
        }
    }

    func reload(now: Date = Date()) async {
        do {
            switch try await endpoint.fetch(now: now, callerIsEmpty: { byTitle.isEmpty }) {
            case .fresh(let decoded):                 byTitle = decoded.keyedByTitle()
            case .notModified(cached: let cached?):   byTitle = cached.keyedByTitle()
            case .notModified(cached: nil), .superseded: break
            }
        } catch {
            // Details are non-essential — the listing still renders the
            // film without synopsis/trailers. Swallow the error rather
            // than surfacing it; a stale or empty map is acceptable.
        }
    }

    func reloadIfStale(now: Date = Date()) async {
        guard endpoint.isStale(now: now) else { return }
        await reload(now: now)
    }

    /// Build a store pre-seeded with details, bypassing the network — used by
    /// the non-prod tuning pager's Film page so the synopsis / trailer
    /// typography has real text to render. Never reached in a shipping run
    /// (only `ShowtimeTuningScreen` calls it), but not `#if DEBUG`-gated
    /// because that screen itself compiles in every configuration.
    static func seeded(_ details: [FilmDetails]) -> DetailsStore {
        let store = DetailsStore()
        store.byTitle = details.keyedByTitle()
        return store
    }
}
