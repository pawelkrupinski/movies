import XCTest
@testable import KinowoCore

final class DetailsCacheTests: XCTestCase {

    private let poland = URL(string: "https://kinowo.net")!
    private let germany = URL(string: "https://showtimes.cc/de")!

    override func tearDown() {
        DetailsCache.save([], deployment: poland, city: "", lastModified: nil)
        super.tearDown()
    }

    func testSaveAndLoadDetailsRoundTrips() {
        let details = [
            FilmDetails(title: "A", synopsis: "opis", trailerURLs: [URL(string: "https://x/embed/1")!]),
            FilmDetails(title: "B", synopsis: nil, trailerURLs: []),
        ]
        DetailsCache.save(details, deployment: poland, city: "poznan", lastModified: nil)
        XCTAssertEqual(DetailsCache.load(deployment: poland, city: "poznan"), details)
    }

    func testSaveAndLoadLastModifiedForSameDeploymentAndCity() {
        let value = "Sun, 31 May 2026 10:00:00 GMT"
        DetailsCache.save([], deployment: poland, city: "poznan", lastModified: value)
        XCTAssertEqual(DetailsCache.lastModified(deployment: poland, city: "poznan"), value)
    }

    /// See `RepertoireCacheLastModifiedTests`: the global server timestamp must
    /// not be replayed across a city switch.
    func testLastModifiedIsNilForADifferentCity() {
        DetailsCache.save([], deployment: poland, city: "poznan",
                          lastModified: "Sun, 31 May 2026 10:00:00 GMT")
        XCTAssertNil(DetailsCache.lastModified(deployment: poland, city: "warszawa"))
    }

    /// …nor across a deployment switch, which is the same trap: this endpoint
    /// also answers `200 []` for a city the deployment doesn't serve.
    func testLastModifiedIsNilForADifferentDeploymentOfTheSameCity() {
        DetailsCache.save([], deployment: poland, city: "berlin",
                          lastModified: "Sun, 31 May 2026 10:00:00 GMT")
        XCTAssertNil(DetailsCache.lastModified(deployment: germany, city: "berlin"))
    }

    func testCachedBodyIsNilForADifferentDeploymentOfTheSameCity() {
        let details = [FilmDetails(title: "A", synopsis: "opis", trailerURLs: [])]
        DetailsCache.save(details, deployment: poland, city: "berlin", lastModified: nil)
        XCTAssertNil(DetailsCache.load(deployment: germany, city: "berlin"))
    }

    /// See `RepertoireCacheLastModifiedTests`: a 304 vouches for the cached
    /// entry, so an empty caller adopts it rather than staying empty.
    func testNotModifiedHandsBackTheCachedBodyWhenTheCallerHasNothing() {
        let details = [FilmDetails(title: "A", synopsis: "opis", trailerURLs: [])]
        DetailsCache.save(details, deployment: germany, city: "berlin", lastModified: "x")
        XCTAssertEqual(DetailsCache.bodyForNotModified(
            callerIsEmpty: true, deployment: germany, city: "berlin"), details)
    }

    func testNotModifiedLeavesANonEmptyCallerAlone() {
        DetailsCache.save([], deployment: germany, city: "berlin", lastModified: "x")
        XCTAssertNil(DetailsCache.bodyForNotModified(
            callerIsEmpty: false, deployment: germany, city: "berlin"))
    }

    func testLastModifiedReturnsNilWhenNotSaved() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("details-meta.txt")
        try? FileManager.default.removeItem(at: url)
        XCTAssertNil(DetailsCache.lastModified(deployment: poland, city: "poznan"))
    }
}
