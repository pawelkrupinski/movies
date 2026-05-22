import XCTest
@testable import KinowoCore

final class ProductionFilmDetailSmokeTests: XCTestCase {

    private static let homeURL = URL(string: "https://kinowo.fly.dev/")!

    private func requireSmoke() throws {
        guard ProcessInfo.processInfo.environment["RUN_SMOKE_TESTS"] == "1" else {
            throw XCTSkip("set RUN_SMOKE_TESTS=1 to run smoke tests")
        }
    }

    private func fetchFirstFilmDetail() async throws -> FilmDetail {
        let homeHTML = try await fetchHTML(Self.homeURL)
        let films = HTMLParser.parse(html: homeHTML)
        guard let first = films.first else {
            XCTFail("home page yielded zero films")
            throw URLError(.cannotParseResponse)
        }
        var components = URLComponents(string: "https://kinowo.fly.dev/film")!
        components.queryItems = [URLQueryItem(name: "title", value: first.title)]
        guard let url = components.url else {
            XCTFail("failed to build detail URL for title=\(first.title)")
            throw URLError(.badURL)
        }
        let detailHTML = try await fetchHTML(url)
        return FilmDetailParser.parse(html: detailHTML, fallbackTitle: first.title)
    }

    func testProductionFilmDetailParses() async throws {
        try requireSmoke()
        let detail = try await fetchFirstFilmDetail()
        XCTAssertFalse(detail.title.isEmpty, "detail title was empty")
        if let poster = detail.posterURL {
            XCTAssertFalse(poster.absoluteString.isEmpty)
        }
    }

    func testProductionFilmDetailHasCinemaLinks() async throws {
        try requireSmoke()
        let detail = try await fetchFirstFilmDetail()
        XCTAssertFalse(detail.cinemaLinks.isEmpty, "no cinema-link buttons parsed")
    }

    func testProductionFilmDetailHasShowings() async throws {
        try requireSmoke()
        let detail = try await fetchFirstFilmDetail()
        XCTAssertFalse(detail.showings.isEmpty, "showings tree was empty")
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
