import XCTest
@testable import KinowoCore
#if canImport(FoundationNetworking)
// Linux Swift splits URLSession / URLRequest out into a separate
// module; on Darwin everything lives under Foundation.
import FoundationNetworking
#endif

final class ProductionHomeSmokeTests: XCTestCase {

    private static let homeURL = URL(string: "https://kinowo.fly.dev/")!

    private func requireSmoke() throws {
        guard ProcessInfo.processInfo.environment["RUN_SMOKE_TESTS"] == "1" else {
            throw XCTSkip("set RUN_SMOKE_TESTS=1 to run smoke tests")
        }
    }

    func testProductionHomeParses() async throws {
        try requireSmoke()
        let html = try await fetchHTML(Self.homeURL)
        let films = HTMLParser.parse(html: html)
        XCTAssertGreaterThanOrEqual(films.count, 20, "got \(films.count) films")
        for film in films {
            XCTAssertFalse(film.title.isEmpty, "film with empty title")
        }
    }

    func testProductionHomeHasShowtimes() async throws {
        try requireSmoke()
        let html = try await fetchHTML(Self.homeURL)
        let films = HTMLParser.parse(html: html)
        let totalShowtimes = films.flatMap { $0.showings }
            .flatMap { $0.cinemas }
            .reduce(0) { $0 + $1.showtimes.count }
        XCTAssertGreaterThanOrEqual(totalShowtimes, 100, "got \(totalShowtimes) showtimes")
    }

    func testProductionHomeHasRatings() async throws {
        try requireSmoke()
        let html = try await fetchHTML(Self.homeURL)
        let films = HTMLParser.parse(html: html)
        let withImdb = films.filter { $0.ratings.imdb != nil }.count
        XCTAssertGreaterThan(withImdb, 0, "no films carried an IMDb rating")
    }

    func testProductionHomePosterURLsAreClean() async throws {
        try requireSmoke()
        let html = try await fetchHTML(Self.homeURL)
        let films = HTMLParser.parse(html: html)
        let dirtyPrimaries = films.compactMap { $0.posterURL?.absoluteString }
            .filter { $0.contains("&amp;") || $0.contains("amp;") }
        let dirtyFallbacks = films.flatMap { $0.fallbackPosterURLs.map(\.absoluteString) }
            .filter { $0.contains("&amp;") || $0.contains("amp;") }
        XCTAssertTrue(
            dirtyPrimaries.isEmpty && dirtyFallbacks.isEmpty,
            "found HTML-escaped & in \(dirtyPrimaries.count) primary + \(dirtyFallbacks.count) fallback URLs; first: \((dirtyPrimaries + dirtyFallbacks).first ?? "?")"
        )
    }

    func testProductionHomeHasFallbackPosterChains() async throws {
        try requireSmoke()
        let html = try await fetchHTML(Self.homeURL)
        let films = HTMLParser.parse(html: html)
        XCTAssertFalse(films.isEmpty)
        let withFallback = films.filter { !$0.fallbackPosterURLs.isEmpty }.count
        let ratio = Double(withFallback) / Double(films.count)
        // >=50% — every cinema-CDN poster ships a fallback chain; only
        // TMDB-only films lack one. A near-zero ratio means the
        // data-fallbacks regex has drifted against a template change
        // and the iOS app can no longer recover from cinema 4xxs.
        XCTAssertGreaterThanOrEqual(ratio, 0.5, "only \(withFallback)/\(films.count) films had a fallback chain")
    }
}

private func fetchHTML(_ url: URL) async throws -> String {
    var req = URLRequest(url: url)
    req.setValue("KinowoTests/1.0", forHTTPHeaderField: "User-Agent")
    req.cachePolicy = .reloadIgnoringLocalCacheData
    let (data, response) = try await URLSession.shared.data(for: req)
    if let http = response as? HTTPURLResponse, !(200..<300).contains(http.statusCode) {
        throw URLError(.badServerResponse)
    }
    return String(decoding: data, as: UTF8.self)
}
