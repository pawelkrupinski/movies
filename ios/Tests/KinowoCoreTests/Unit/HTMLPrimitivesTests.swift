import XCTest
@testable import KinowoCore

final class HTMLPrimitivesTests: XCTestCase {

    func testRangesFindsEveryOccurrenceInSourceOrder() {
        let src = "AxAyAzA"
        let indexes = HTMLPrimitives.ranges(of: "A", in: src)
        XCTAssertEqual(indexes.count, 4)
        let offsets = indexes.map { src.distance(from: src.startIndex, to: $0) }
        XCTAssertEqual(offsets, [0, 2, 4, 6])
    }

    func testRangesReturnsEmptyWhenNeedleAbsent() {
        XCTAssertTrue(HTMLPrimitives.ranges(of: "Q", in: "abc").isEmpty)
    }

    func testRangesOnMultiCharNeedleAdvancesPastMatch() {
        let src = "ababab"
        let indexes = HTMLPrimitives.ranges(of: "aba", in: src)
        // After matching "aba" at 0, the search resumes from offset 3, finding nothing.
        XCTAssertEqual(indexes.count, 1)
    }

    func testCaptureReturnsFirstGroupOrNil() {
        XCTAssertEqual(HTMLPrimitives.capture("hello world", #"hello (\w+)"#), "world")
        XCTAssertNil(HTMLPrimitives.capture("hello world", #"goodbye (\w+)"#))
    }

    func testCaptureCrossesNewlinesWithDotMatchesLineSeparators() {
        let chunk = "<div>line1\nline2</div>"
        XCTAssertEqual(HTMLPrimitives.capture(chunk, #"<div>(.+)</div>"#), "line1\nline2")
    }

    func testAttributeReadsValueRegardlessOfOrder() {
        let attrs = #"href="/foo" class="badge-time" data-time="14:30" data-room="3""#
        XCTAssertEqual(HTMLPrimitives.attribute(attrs, "data-time"), "14:30")
        XCTAssertEqual(HTMLPrimitives.attribute(attrs, "data-room"), "3")
        XCTAssertEqual(HTMLPrimitives.attribute(attrs, "href"), "/foo")
        XCTAssertEqual(HTMLPrimitives.attribute(attrs, "class"), "badge-time")
    }

    func testAttributeReturnsNilWhenAbsent() {
        let attrs = #"class="badge-time" data-time="14:30""#
        XCTAssertNil(HTMLPrimitives.attribute(attrs, "data-format"))
    }
}
