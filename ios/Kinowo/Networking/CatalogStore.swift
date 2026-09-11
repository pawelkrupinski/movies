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
    private let cache: CatalogCache
    private var etag: String?
    private var lastFetched: Date?
    /// Revalidate at most hourly on foreground; a cold open always revalidates.
    private let staleAfter: TimeInterval = 60 * 60

    init(session: URLSession = .shared, cache: CatalogCache = CatalogCache()) {
        self.session = session
        self.cache = cache
        #if DEBUG
        // UI tests that assert on the BUNDLED seed's shape need it deterministically
        // — a prior interactive/test run's live fetch otherwise persists indefinitely
        // and silently wins over a freshly-updated seed, since nothing else ever
        // clears `CatalogCache`.
        if ProcessInfo.processInfo.environment["KINOWO_CLEAR_CATALOG_CACHE"] != nil {
            cache.clear()
        }
        #endif
        // Prefer the last persisted fetch; else the bundled seed; else the
        // compile-time fallback registry (should never be needed).
        if let persisted = cache.load(), let parsed = CatalogBody.decode(persisted.body) {
            countries = parsed.countries
            cities = parsed.cities
            etag = persisted.etag
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
            cache.save(body: body, etag: etag)
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
    func regions(inCountry code: String) -> [String] { cities.regions(inCountry: code) }
    func topLevelRows(matching query: String, inCountry code: String) -> [City.PickerRow] {
        cities.topLevelRows(matching: query, inCountry: code)
    }
    func matching(_ query: String, inCountry code: String, region: String?) -> [City] {
        cities.matching(query, inCountry: code, region: region)
    }
    func secondLevelRows(matching query: String, inCountry code: String, region: String) -> [City.PickerRow] {
        cities.secondLevelRows(matching: query, inCountry: code, region: region)
    }
    func matching(_ query: String, inCountry code: String, region: String, subregion: String) -> [City] {
        cities.matching(query, inCountry: code, region: region, subregion: subregion)
    }
    func defaultCity(inCountry code: String) -> City? { cities.defaultCity(inCountry: code) }
    /// The zone to reason about `slug`'s showtimes in — its own where the catalog
    /// gave it one, else the country's. The one place the app resolves a city to a
    /// clock, so the pruning and the day buckets cannot disagree.
    func zone(ofSlug slug: String?, inCountry country: Country) -> TimeZone {
        cities.zone(ofSlug: slug, fallback: country.timeZone)
    }
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
    /// The version-filter pair of the country `slug` belongs to — what a deep
    /// link's `?lang=` is validated against, since the linked city names the
    /// country and the pair is the country's own. Poland's for a slug the
    /// catalog does not know (the link is rejected on the slug anyway).
    func versionTokens(ofSlug slug: String) -> VersionTokens {
        country(code: cities.country(ofSlug: slug)).versionTokens
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
