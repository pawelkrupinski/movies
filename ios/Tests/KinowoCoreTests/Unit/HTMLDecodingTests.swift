import XCTest
@testable import KinowoCore

final class HTMLDecodingTests: XCTestCase {

    func testDecodesNamedEntities() {
        XCTAssertEqual("Tom &amp; Jerry".htmlDecoded(), "Tom & Jerry")
        XCTAssertEqual("a &lt; b &gt; c".htmlDecoded(), "a < b > c")
        XCTAssertEqual("she said &quot;hi&quot;".htmlDecoded(), "she said \"hi\"")
        XCTAssertEqual("it&#39;s fine".htmlDecoded(), "it's fine")
        XCTAssertEqual("hard&nbsp;space".htmlDecoded(), "hard space")
    }

    func testDecodesNumericDecimalEntity() {
        XCTAssertEqual("&#65;BC".htmlDecoded(), "ABC")
    }

    func testDecodesHexEntity() {
        XCTAssertEqual("caf&#xE9;".htmlDecoded(), "café")
    }

    func testUnknownEntityFallsThroughLiterally() {
        XCTAssertEqual("&unknownentity; tail".htmlDecoded(), "&unknownentity; tail")
    }

    func testNoAmpersandShortCircuits() {
        let plain = "no entities here"
        XCTAssertEqual(plain.htmlDecoded(), plain)
    }

    func testDecodesQueryStringWithMultipleAmpEntities() {
        let raw = "https://x/?a=1&amp;b=2&amp;c=3"
        XCTAssertEqual(raw.htmlDecoded(), "https://x/?a=1&b=2&c=3")
    }

    func testLoneAmpersandKept() {
        XCTAssertEqual("AT&T".htmlDecoded(), "AT&T")
    }
}
