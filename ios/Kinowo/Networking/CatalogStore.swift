import Foundation
import Combine

/// The live country + city catalog the app runs off. It is:
///  - **seeded** from the bundled `catalog-seed.json` (shipped in the distro), so
///    a fresh install renders offline and its very first fetch already carries the
///    build's ETag — a `304` when the app is current with the server;
///  - **refreshed** on each app open with a conditional GET (`If-None-Match`), so
///    a country/city added server-side appears without an app update; and
///  - **persisted**, so a relaunch starts from the last fetch, not the seed.
///
/// It publishes `countries`/`cities` for the UI; the per-country query logic lives
/// on `[City]`/`[Country]` (KinowoCore, unit-tested) and is called through here.
@MainActor
final class CatalogStore: ObservableObject {
    @Published private(set) var countries: [Country]
    @Published private(set) var cities: [City]

    private let session: URLSession
    private let defaults: UserDefaults
    private var etag: String?
    private var lastFetched: Date?
    /// Revalidate at most hourly on foreground; a cold open always revalidates.
    private let staleAfter: TimeInterval = 60 * 60

    private let kBody = "catalogBody"
    private let kEtag = "catalogEtag"

    init(session: URLSession = .shared, defaults: UserDefaults = .standard) {
        self.session = session
        self.defaults = defaults
        // Prefer the last persisted fetch; else the bundled seed; else the
        // compile-time fallback registry (should never be needed).
        if let body = defaults.string(forKey: kBody), let parsed = CatalogBody.decode(body) {
            countries = parsed.countries
            cities = parsed.cities
            etag = defaults.string(forKey: kEtag)
        } else if let seed = CatalogStore.loadBundledSeed() {
            countries = seed.body.countries
            cities = seed.body.cities
            etag = seed.etag
        } else {
            countries = Country.all
            cities = City.all
            etag = nil
        }
    }

    /// Cold-open refresh: always revalidates against `baseURL`.
    func reload(baseURL: URL, now: Date = Date()) async { await fetch(baseURL: baseURL, now: now) }

    /// Foreground refresh: revalidates only if the last fetch is older than
    /// [staleAfter], so returning to the app repeatedly doesn't spam the server.
    func reloadIfStale(baseURL: URL, now: Date = Date()) async {
        if let last = lastFetched, now.timeIntervalSince(last) < staleAfter { return }
        await fetch(baseURL: baseURL, now: now)
    }

    private func fetch(baseURL: URL, now: Date) async {
        var request = URLRequest(url: baseURL.appendingPathComponent("api").appendingPathComponent("catalog"))
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        request.cachePolicy = .reloadIgnoringLocalCacheData
        if let etag { request.setValue(etag, forHTTPHeaderField: "If-None-Match") }
        do {
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse else { return }
            if http.statusCode == 304 { lastFetched = now; return }        // unchanged — no body
            guard (200..<300).contains(http.statusCode) else { return }
            guard let body = String(data: data, encoding: .utf8), let parsed = CatalogBody.decode(body) else { return }
            countries = parsed.countries
            cities = parsed.cities
            etag = http.value(forHTTPHeaderField: "Etag")
            lastFetched = now
            defaults.set(body, forKey: kBody)
            defaults.set(etag, forKey: kEtag)
        } catch {
            // Offline / transient: keep whatever's loaded (persisted, seed, or fallback).
        }
    }

    private static func loadBundledSeed() -> (etag: String, body: (countries: [Country], cities: [City]))? {
        guard let url = Bundle.main.url(forResource: "catalog-seed", withExtension: "json"),
              let data = try? Data(contentsOf: url),
              let envelope = try? JSONDecoder().decode(CatalogEnvelope.self, from: data)
        else { return nil }
        return (envelope.etag, envelope.catalog.resolved())
    }

    // MARK: - Per-country query passthroughs (delegating to the pure helpers)

    func sorted(inCountry code: String) -> [City] { cities.sortedForPicker(inCountry: code) }
    func matching(_ query: String, inCountry code: String) -> [City] { cities.matching(query, inCountry: code) }
    func defaultCity(inCountry code: String) -> City? { cities.defaultCity(inCountry: code) }
    func nearestWithin100km(lat: Double, lon: Double, inCountry code: String) -> City? {
        cities.nearestWithin100km(lat: lat, lon: lon, inCountry: code)
    }
    func switchSuggestion(chosenSlug: String, lat: Double, lon: Double, lastPromptKey: String?, inCountry code: String) -> City.CitySwitchSuggestion? {
        cities.switchSuggestion(chosenSlug: chosenSlug, lat: lat, lon: lon, lastPromptKey: lastPromptKey, inCountry: code)
    }

    /// The country for a (possibly legacy) persisted code, from the live list,
    /// falling back to the compile-time registry.
    func country(code: String?) -> Country {
        countries.withCode(Country.normalizeCode(code)) ?? Country.byCode(code)
    }
    var isSwitchable: Bool { countries.isSwitchable }
    var allSlugs: Set<String> { Set(cities.map(\.slug)) }
}

// MARK: - Wire / seed decoding

/// The `/api/catalog` response body (and the `catalog` field of the bundled
/// seed): `{"countries":[…],"cities":[…]}`. Countries arrive in the wire shape
/// ([CountryDTO]) and are resolved to [Country]; cities decode directly.
struct CatalogBody: Decodable {
    let countries: [CountryDTO]
    let cities: [City]

    func resolved() -> (countries: [Country], cities: [City]) {
        (countries.compactMap { $0.toCountry() }, cities)
    }

    static func decode(_ json: String) -> (countries: [Country], cities: [City])? {
        guard let data = json.data(using: .utf8),
              let body = try? JSONDecoder().decode(CatalogBody.self, from: data) else { return nil }
        return body.resolved()
    }
}

/// The bundled seed file shape: `{"etag":<server ETag>,"catalog":<body>}`.
struct CatalogEnvelope: Decodable {
    let etag: String
    let catalog: CatalogBody
}
