import UIKit
import XCTest

/// A dense Warsaw grid with real posters, served by the test runner's own `FixtureServer` -- for
/// the suites that need many cards with poster images on screen (PosterOverlapUITests,
/// NavbarFadeUITests), which read production's grid until 2026-11 and so rode on its listing and
/// the runner's network.
///
/// The captured `repertoire-warszawa.json` (six films, a week of showings, `DAY+n` dates) is
/// repeated `copies` times, each copy its own film (a title and slug of its own, so the grid keys
/// them apart), and every film gets a poster the same server serves at `/posters/<n>.png`. The
/// posters are vivid vertical stripes -- a band across them is never flat, which is what the fade
/// test's variance reads -- in three shapes, the 2:3 portrait the grid boxes for, a wide landscape
/// and a square: a landscape source is what once sized its card past its column
/// (PosterOverlapUITests), so the overlap test needs one on screen.
enum PosterGridFixture {
    static let listingPath = "/warszawa/api/repertoire"

    /// A server serving the grid, dated relative to `today` (the caller's clock).
    static func serve(today: Date, copies: Int = 4) throws -> FixtureServer {
        let bundle = Bundle(for: FixtureServer.self)
        guard let url = bundle.url(forResource: "repertoire-warszawa", withExtension: "json") else {
            throw FixtureError.missingListing
        }
        let dated = RelativeListing.dated(try Data(contentsOf: url), today: today)
        guard let films = try JSONSerialization.jsonObject(with: dated) as? [[String: Any]] else {
            throw FixtureError.notAListing
        }
        let server = try FixtureServer()
        var routes: [String: Data] = [:]
        var grid: [[String: Any]] = []
        for copy in 0..<copies {
            for film in films {
                let n = grid.count
                let path = "/posters/\(n).png"
                var copied = film
                if copy > 0 {
                    copied["title"] = "\(film["title"] as? String ?? "Film") \(copy + 1)"
                    copied["slug"] = "\(film["slug"] as? String ?? "film")-\(copy + 1)"
                }
                copied["posterURL"] = server.baseURL.appendingPathComponent(path).absoluteString
                routes[path] = poster(n)
                grid.append(copied)
            }
        }
        routes[listingPath] = try JSONSerialization.data(withJSONObject: grid)
        server.addRoutes(routes)
        return server
    }

    /// Fails unless the grid on screen is this server's: the app fetched its listing AND some of
    /// its posters (waiting up to `timeout` for the posters, which load after the cards). The films
    /// are real ones, so production may list them too; only the requests tell the two apart.
    static func assertServed(by server: FixtureServer, timeout: TimeInterval = 15,
                             file: StaticString = #filePath, line: UInt = #line) {
        XCTAssertTrue(server.requestedPaths.contains(listingPath),
                      "The grid came from somewhere else: the app asked the fixture server for \(server.requestedPaths).",
                      file: file, line: line)
        let deadline = Date().addingTimeInterval(timeout)
        while postersRequested(from: server) == 0, Date() < deadline {
            Thread.sleep(forTimeInterval: 0.25)
        }
        XCTAssertGreaterThan(postersRequested(from: server), 0,
                             "The app fetched no poster from the fixture server: \(server.requestedPaths).",
                             file: file, line: line)
    }

    private static func postersRequested(from server: FixtureServer) -> Int {
        Set(server.requestedPaths.filter { $0.hasPrefix("/posters/") }).count
    }

    enum FixtureError: Error { case missingListing, notAListing }

    private static let shapes = [CGSize(width: 400, height: 600), CGSize(width: 900, height: 400),
                                 CGSize(width: 500, height: 500)]
    private static let colours: [UIColor] = [.systemRed, .systemYellow, .systemBlue, .systemGreen,
                                             .black, .systemOrange, .white, .systemPurple]

    /// Poster `n`: eight vertical stripes of contrasting colours, rotated by `n`, in shape `n % 3`.
    private static func poster(_ n: Int) -> Data {
        let size = shapes[n % shapes.count]
        let format = UIGraphicsImageRendererFormat()
        format.scale = 1
        let image = UIGraphicsImageRenderer(size: size, format: format).image { context in
            let stripe = size.width / CGFloat(colours.count)
            for i in colours.indices {
                colours[(i + n) % colours.count].setFill()
                context.fill(CGRect(x: CGFloat(i) * stripe, y: 0, width: stripe + 1, height: size.height))
            }
        }
        return image.pngData() ?? Data()
    }
}
