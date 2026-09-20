import XCTest
@testable import KinowoAuth

/// The URL-building half of `HttpHiddenFilmsClient`, tested in isolation
/// from any network call — this is exactly the kind of thing that "looks
/// right" and silently double-encodes, or mis-splits an embedded `/` into
/// an extra path segment, only on the one title that actually has one.
final class HiddenFilmsClientURLTests: XCTestCase {

    private func encode(_ title: String) -> String {
        title.addingPercentEncoding(withAllowedCharacters: HttpHiddenFilmsClient.titleAllowed) ?? title
    }

    func testFetchAndClearURLHaveNoTitleSegment() {
        let url = HttpHiddenFilmsClient.url(country: "pl")
        XCTAssertEqual(url.path, "/api/me/pl/hidden-films")
    }

    func testPlainAsciiTitleRoundTrips() {
        let url = HttpHiddenFilmsClient.url(country: "pl", encodedTitleSegment: encode("Sing"))
        XCTAssertEqual(url.path, "/api/me/pl/hidden-films/Sing")
    }

    /// A space must become a SINGLE path segment (percent-encoded), never
    /// two segments and never a literal space breaking the URL.
    func testTitleWithASpaceStaysOneSegment() {
        let url = HttpHiddenFilmsClient.url(country: "pl", encodedTitleSegment: encode("Top Gun"))
        // "/", api, me, pl, hidden-films, "Top Gun" — six components, the
        // space kept inside the last one rather than splitting it in two.
        XCTAssertEqual(url.pathComponents.count, 6)
        XCTAssertEqual(url.pathComponents.last, "Top Gun") // decoded back by URL's own pathComponents
    }

    /// THE case this whole test file exists for: a title containing a
    /// LITERAL `/` must percent-encode to `%2F` and stay ONE path segment
    /// — not get split into two, which `appendingPathComponent` would do.
    func testTitleWithASlashStaysOneSegmentNotTwo() {
        let title = "S/He"
        let encoded = encode(title)
        XCTAssertFalse(encoded.contains("/"), "the slash must be percent-encoded, not literal")
        XCTAssertTrue(encoded.contains("%2F"))

        let url = HttpHiddenFilmsClient.url(country: "pl", encodedTitleSegment: encoded)
        // Exactly 5 path components: "/", api, me, pl, hidden-films, <title> = 6.
        // (pathComponents always includes a leading "/".)
        XCTAssertEqual(url.pathComponents.count, 6, "a literal / in the title must not add an extra path component")
        XCTAssertEqual(url.pathComponents.last, title, "URL's own decoding must recover the original title")
    }

    func testTitleWithDiacriticsAndPunctuationRoundTrips() {
        let title = "Diabeł ubiera się u Prady 2: Powrót"
        let encoded = encode(title)
        let url = HttpHiddenFilmsClient.url(country: "pl", encodedTitleSegment: encoded)
        XCTAssertEqual(url.pathComponents.last, title)
    }
}
