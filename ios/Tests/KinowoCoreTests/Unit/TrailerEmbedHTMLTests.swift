import XCTest
@testable import KinowoCore

final class TrailerEmbedHTMLTests: XCTestCase {

    func testWithAutoplayAppendsParams() {
        let url = URL(string: "https://www.youtube.com/embed/abc123")!
        let result = TrailerEmbedHTML.withAutoplay(url)
        XCTAssertTrue(result.absoluteString.contains("autoplay=1"))
        XCTAssertTrue(result.absoluteString.contains("playsinline=1"))
    }

    func testWithAutoplayPreservesExistingParam() {
        let url = URL(string: "https://www.youtube.com/embed/abc123?autoplay=0")!
        let result = TrailerEmbedHTML.withAutoplay(url)
        XCTAssertEqual(result.absoluteString, "https://www.youtube.com/embed/abc123?autoplay=0",
                       "should not add a second autoplay param")
    }

    func testEmbedPageContainsIframeWithSrc() {
        let url = URL(string: "https://www.youtube.com/embed/abc123?autoplay=1&playsinline=1")!
        let html = TrailerEmbedHTML.embedPage(videoURL: url)
        XCTAssertTrue(html.contains("<iframe"), "missing iframe tag")
        XCTAssertTrue(html.contains("src=\"https://www.youtube.com/embed/abc123?autoplay=1&amp;playsinline=1\""),
                       "iframe src should be HTML-escaped: \(html)")
    }

    func testEmbedPageAllowsAutoplayAndEncryptedMedia() {
        let url = URL(string: "https://www.youtube.com/embed/xyz")!
        let html = TrailerEmbedHTML.embedPage(videoURL: url)
        XCTAssertTrue(html.contains("allow=\"autoplay; encrypted-media; picture-in-picture\""))
        XCTAssertTrue(html.contains("allowfullscreen"))
    }

    func testEmbedPageHasBlackBackground() {
        let url = URL(string: "https://player.vimeo.com/video/12345")!
        let html = TrailerEmbedHTML.embedPage(videoURL: url)
        XCTAssertTrue(html.contains("background:#000"))
    }
}
