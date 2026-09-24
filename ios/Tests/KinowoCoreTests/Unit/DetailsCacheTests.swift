import XCTest
@testable import KinowoCore

final class DetailsCacheTests: XCTestCase {

    private let poland = URL(string: "https://kinowo.net")!
    private let germany = URL(string: "https://showtimes.cc/de")!

    override func tearDown() {
        ConditionalPayloadCache.details.save([], deployment: poland, city: "", lastModified: nil)
        super.tearDown()
    }

    func testSaveAndLoadDetailsRoundTrips() {
        let details = [
            FilmDetails(title: "A", synopsis: "opis", trailerURLs: [URL(string: "https://x/embed/1")!]),
            FilmDetails(title: "B", synopsis: nil, trailerURLs: []),
        ]
        ConditionalPayloadCache.details.save(details, deployment: poland, city: "poznan", lastModified: nil)
        XCTAssertEqual(ConditionalPayloadCache.details.load(deployment: poland, city: "poznan"), details)
    }

    func testSaveAndLoadLastModifiedForSameDeploymentAndCity() {
        let value = "Sun, 31 May 2026 10:00:00 GMT"
        ConditionalPayloadCache.details.save([], deployment: poland, city: "poznan", lastModified: value)
        XCTAssertEqual(ConditionalPayloadCache.details.lastModified(deployment: poland, city: "poznan"), value)
    }

    /// See `RepertoireCacheLastModifiedTests`: the global server timestamp must
    /// not be replayed across a city switch.
    func testLastModifiedIsNilForADifferentCity() {
        ConditionalPayloadCache.details.save([], deployment: poland, city: "poznan",
                          lastModified: "Sun, 31 May 2026 10:00:00 GMT")
        XCTAssertNil(ConditionalPayloadCache.details.lastModified(deployment: poland, city: "warszawa"))
    }

    /// …nor across a deployment switch, which is the same trap: this endpoint
    /// also answers `200 []` for a city the deployment doesn't serve.
    func testLastModifiedIsNilForADifferentDeploymentOfTheSameCity() {
        ConditionalPayloadCache.details.save([], deployment: poland, city: "berlin",
                          lastModified: "Sun, 31 May 2026 10:00:00 GMT")
        XCTAssertNil(ConditionalPayloadCache.details.lastModified(deployment: germany, city: "berlin"))
    }

    func testCachedBodyIsNilForADifferentDeploymentOfTheSameCity() {
        let details = [FilmDetails(title: "A", synopsis: "opis", trailerURLs: [])]
        ConditionalPayloadCache.details.save(details, deployment: poland, city: "berlin", lastModified: nil)
        XCTAssertNil(ConditionalPayloadCache.details.load(deployment: germany, city: "berlin"))
    }

    /// See `RepertoireCacheLastModifiedTests`: a 304 vouches for the cached
    /// entry, so an empty caller adopts it rather than staying empty.
    func testNotModifiedHandsBackTheCachedBodyWhenTheCallerHasNothing() {
        let details = [FilmDetails(title: "A", synopsis: "opis", trailerURLs: [])]
        ConditionalPayloadCache.details.save(details, deployment: germany, city: "berlin", lastModified: "x")
        XCTAssertEqual(ConditionalPayloadCache.details.bodyForNotModified(
            callerIsEmpty: true, deployment: germany, city: "berlin"), details)
    }

    func testNotModifiedLeavesANonEmptyCallerAlone() {
        ConditionalPayloadCache.details.save([], deployment: germany, city: "berlin", lastModified: "x")
        XCTAssertNil(ConditionalPayloadCache.details.bodyForNotModified(
            callerIsEmpty: false, deployment: germany, city: "berlin"))
    }

    func testLastModifiedReturnsNilWhenNotSaved() {
        ConditionalPayloadCache.details.remove()
        XCTAssertNil(ConditionalPayloadCache.details.lastModified(deployment: poland, city: "poznan"))
    }

    /// Background saves land in the order they were issued: a city switch's
    /// save can't be overtaken by the previous city's, which used to run in an
    /// unordered detached task and could leave the OLD city cached (or pair
    /// one city's metadata with the other's body) after the grid moved on.
    func testBackgroundSavesLandInTheOrderTheyWereIssued() {
        for round in 0..<50 {
            let old = [FilmDetails(title: "Old \(round)", synopsis: nil, trailerURLs: [])]
            let new = [FilmDetails(title: "New \(round)", synopsis: nil, trailerURLs: [])]
            ConditionalPayloadCache.details.saveInBackground(body: encoded(old), deployment: poland, city: "poznan", lastModified: "lm-old")
            ConditionalPayloadCache.details.saveInBackground(body: encoded(new), deployment: poland, city: "warszawa", lastModified: "lm-new")

            XCTAssertEqual(ConditionalPayloadCache.details.load(deployment: poland, city: "warszawa"), new)
            XCTAssertEqual(ConditionalPayloadCache.details.lastModified(deployment: poland, city: "warszawa"), "lm-new")
            XCTAssertNil(ConditionalPayloadCache.details.load(deployment: poland, city: "poznan"))
        }
    }

    /// A read sees every save issued before it, background or not — the
    /// store's own next reload, and every test that asserts on the disk right
    /// after a reload, used to race the detached write.
    func testAReadSeesABackgroundSaveIssuedBeforeIt() {
        for round in 0..<50 {
            let saved = [FilmDetails(title: "Saved \(round)", synopsis: nil, trailerURLs: [])]
            ConditionalPayloadCache.details.saveInBackground(body: encoded(saved), deployment: poland, city: "gdansk", lastModified: "lm-\(round)")

            XCTAssertEqual(ConditionalPayloadCache.details.load(deployment: poland, city: "gdansk"), saved)
            XCTAssertEqual(ConditionalPayloadCache.details.lastModified(deployment: poland, city: "gdansk"), "lm-\(round)")
        }
    }

    /// A direct save lands after every background save issued before it, so
    /// a test's tearDown reset can't be overwritten by the reload it follows.
    func testADirectSaveIsNotOvertakenByAnEarlierBackgroundSave() {
        for round in 0..<50 {
            let background = [FilmDetails(title: "Background \(round)", synopsis: nil, trailerURLs: [])]
            ConditionalPayloadCache.details.saveInBackground(body: encoded(background), deployment: poland, city: "gdansk", lastModified: nil)
            ConditionalPayloadCache.details.save([], deployment: poland, city: "sopot", lastModified: nil)

            XCTAssertNil(ConditionalPayloadCache.details.load(deployment: poland, city: "gdansk"))
            XCTAssertEqual(ConditionalPayloadCache.details.load(deployment: poland, city: "sopot"), [])
        }
    }

    /// A payload as the server sends it — the form `saveInBackground` takes.
    private func encoded(_ payload: [FilmDetails]) -> Data {
        (try? JSONEncoder().encode(payload)) ?? Data()
    }
}
