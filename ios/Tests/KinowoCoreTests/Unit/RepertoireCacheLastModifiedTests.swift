import XCTest
@testable import KinowoCore

final class RepertoireCacheLastModifiedTests: XCTestCase {

    private let poland = URL(string: "https://kinowo.net")!
    private let germany = URL(string: "https://de.showtimes.cc")!

    override func tearDown() {
        // Reset the bound meta so cases don't leak into one another.
        RepertoireCache.save([], deployment: poland, city: "", lastModified: nil)
        super.tearDown()
    }

    func testSaveAndLoadLastModifiedForSameDeploymentAndCity() {
        let value = "Sun, 25 May 2026 10:00:00 GMT"
        RepertoireCache.save([], deployment: poland, city: "poznan", lastModified: value)
        XCTAssertEqual(RepertoireCache.lastModified(deployment: poland, city: "poznan"), value)
    }

    /// The server's `Last-Modified` is a single global value, so replaying
    /// poznań's timestamp while fetching warszawa would draw a 304 and strand
    /// the grid on the old city. A different city must therefore get no
    /// conditional header (nil).
    func testLastModifiedIsNilForADifferentCity() {
        RepertoireCache.save([], deployment: poland, city: "poznan",
                             lastModified: "Sun, 25 May 2026 10:00:00 GMT")
        XCTAssertNil(RepertoireCache.lastModified(deployment: poland, city: "warszawa"))
    }

    /// The same slug can live on two deployments, and asking the wrong one is
    /// not an error — `kinowo.net/berlin` answers `200 []` because Berlin
    /// isn't a Polish city. Replaying THAT timestamp against the German
    /// deployment drew a 304, so a deep link into Berlin came up empty even
    /// though Germany had a full listing. A deployment switch must send no
    /// conditional header.
    func testLastModifiedIsNilForADifferentDeploymentOfTheSameCity() {
        RepertoireCache.save([], deployment: poland, city: "berlin",
                             lastModified: "Sun, 26 Jul 2026 17:29:46 GMT")
        XCTAssertNil(RepertoireCache.lastModified(deployment: germany, city: "berlin"))
    }

    /// The body is bound the same way: an empty listing cached off the wrong
    /// deployment must not be painted under the right one.
    func testCachedBodyIsNilForADifferentDeploymentOfTheSameCity() {
        RepertoireCache.save([], deployment: poland, city: "berlin", lastModified: nil)
        XCTAssertNil(RepertoireCache.load(deployment: germany, city: "berlin"))
    }

    func testCachedBodyRoundTripsForTheDeploymentThatProducedIt() {
        RepertoireCache.save([], deployment: germany, city: "berlin", lastModified: nil)
        XCTAssertEqual(RepertoireCache.load(deployment: germany, city: "berlin")?.count, 0)
    }

    // MARK: - what a 304 means
    //
    // The conditional header vouches for the CACHED entry, not for whatever the
    // caller happens to be holding. A cold launch reads the disk cache before a
    // deep link re-points the store, so the read can be skipped for the wrong
    // city and leave the caller empty — and a 304 taken at face value then
    // strands an empty listing on a city that has a full one. That is the
    // second half of the "no screenings" bug.

    func testNotModifiedHandsBackTheCachedBodyWhenTheCallerHasNothing() {
        RepertoireCache.save([], deployment: germany, city: "berlin", lastModified: "x")
        XCTAssertNotNil(RepertoireCache.bodyForNotModified(
            callerIsEmpty: true, deployment: germany, city: "berlin"))
    }

    /// A caller that already holds the listing keeps it — no needless re-read,
    /// and no chance of clobbering a fresher in-memory copy.
    func testNotModifiedLeavesANonEmptyCallerAlone() {
        RepertoireCache.save([], deployment: germany, city: "berlin", lastModified: "x")
        XCTAssertNil(RepertoireCache.bodyForNotModified(
            callerIsEmpty: false, deployment: germany, city: "berlin"))
    }

    /// A 304 from another deployment can't be honoured with this entry either.
    func testNotModifiedHandsBackNothingForADifferentDeployment() {
        RepertoireCache.save([], deployment: poland, city: "berlin", lastModified: "x")
        XCTAssertNil(RepertoireCache.bodyForNotModified(
            callerIsEmpty: true, deployment: germany, city: "berlin"))
    }

    func testLastModifiedReturnsNilWhenNotSaved() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("repertoire-meta.txt")
        try? FileManager.default.removeItem(at: url)
        XCTAssertNil(RepertoireCache.lastModified(deployment: poland, city: "poznan"))
    }

    /// A meta file written by an older build carries only city + timestamp. It
    /// matches no deployment, so it reads as "nothing cached" and the next
    /// fetch is unconditional — a stale entry costs one full response, never a
    /// wrong one.
    func testAPreDeploymentMetaFileIsIgnored() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("repertoire-meta.txt")
        try? "berlin\nSun, 26 Jul 2026 17:29:46 GMT".write(to: url, atomically: true, encoding: .utf8)
        XCTAssertNil(RepertoireCache.lastModified(deployment: poland, city: "berlin"))
        XCTAssertNil(RepertoireCache.load(deployment: poland, city: "berlin"))
    }
}
