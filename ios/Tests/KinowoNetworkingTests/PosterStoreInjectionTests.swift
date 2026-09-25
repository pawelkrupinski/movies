import XCTest
import KinowoTestSupport
@testable import KinowoCore
@testable import KinowoNetworking

/// The poster cache is the composition root's instance, handed down — not a
/// process-wide `PosterStore.shared` every caller reaches for. So the daily
/// purge runs against the store `RepertoireStore` was GIVEN, and a store's
/// downloads go through the session IT was built with.
@MainActor
final class PosterStoreInjectionTests: XCTestCase {

    private var directory: URL!

    override func setUp() {
        super.setUp()
        directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("PosterStoreInjectionTests-\(UUID().uuidString)", isDirectory: true)
        UserDefaults.standard.removeObject(forKey: "posterPurgeLastDay")
    }

    override func tearDown() {
        try? FileManager.default.removeItem(at: directory)
        UserDefaults.standard.removeObject(forKey: "posterPurgeLastDay")
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    func testTheDailyPurgeRunsAgainstTheInjectedStore() async throws {
        let showing = URL(string: "https://img.invalid/showing.jpg")!
        let finished = URL(string: "https://img.invalid/finished.jpg")!
        let posters = PosterStore(directory: directory, fetch: { _ in nil })
        posters.seed(Data([1]), for: showing)
        posters.seed(Data([2]), for: finished)

        let store = RepertoireStore(session: URLProtocolStub.session(), posters: posters)
        store.films = [film(poster: showing)]
        await store.reconcilePostersIfNeeded()

        let kept = await posters.data(for: showing)
        let purged = await posters.data(for: finished)
        XCTAssertEqual(kept, Data([1]))
        XCTAssertNil(purged,
                     "the finished film's poster should have been purged from the store RepertoireStore was given")
    }

    func testTheDefaultFetchDownloadsThroughTheSessionItWasGiven() async {
        URLProtocolStub.handler = { _ in .init(statusCode: 200, headers: [:], body: Data("art".utf8)) }
        let fetch = PosterStore.networkFetch(through: URLProtocolStub.session())

        let data = await fetch(URL(string: "https://img.invalid/stubbed.jpg")!)

        XCTAssertEqual(data, Data("art".utf8), "`.invalid` never resolves, so only the given stub session can answer")
    }

    private func film(poster: URL) -> Film {
        Film(title: "Showing", posterURL: poster, fallbackPosterURLs: [], runtimeMinutes: nil,
             releaseYear: nil, genres: [], ageRating: nil, ratings: .empty, countries: [],
             directors: [], cast: [], showings: [])
    }
}
