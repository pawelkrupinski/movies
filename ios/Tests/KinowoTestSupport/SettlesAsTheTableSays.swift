import Foundation
import XCTest

/// Every `http:NNN` row of `source` in the retry-classification table, answered
/// (through `URLProtocolStub`) to each of `calls`: `isPermanent` agrees with the
/// row's verdict, no call reads the status as success, and a call fails with the
/// permanent-refusal error — `refusedStatus` non-nil, naming the status — exactly
/// when the row says permanent. The shape every client test holding a classifier
/// to the table shares; each call gets a session the stub answers.
public func assertEveryCallSettlesAsTheTableSays(
    source: String,
    isPermanent: (Int) -> Bool,
    refusedStatus: (Error) -> Int?,
    calls: [(name: String, call: (URLSession) async throws -> Void)],
    file: StaticString = #filePath, line: UInt = #line
) async throws {
    defer { URLProtocolStub.handler = nil }
    let table = try RetryClassificationTable.load()
    XCTAssertTrue(table.sources(consumedBy: "ios").contains(source), "\(source) must list ios as a consumer",
                  file: file, line: line)
    let rows = try table.rows(for: source)
    XCTAssertFalse(rows.isEmpty, "\(source) has no rows", file: file, line: line)
    for row in rows {
        let status = try XCTUnwrap(row.status, "\(row)", file: file, line: line)
        XCTAssertEqual(isPermanent(status), row.isPermanent, "\(row)", file: file, line: line)
        URLProtocolStub.handler = { _ in .init(statusCode: status, headers: [:], body: Data()) }
        for (name, call) in calls {
            do {
                try await call(URLProtocolStub.session())
                XCTFail("a \(status) must not read as a successful \(name)", file: file, line: line)
            } catch {
                let refused = refusedStatus(error)
                XCTAssertEqual(refused != nil, row.isPermanent, "\(name): \(row)", file: file, line: line)
                if row.isPermanent { XCTAssertEqual(refused, status, "\(name): \(row)", file: file, line: line) }
            }
        }
    }
}
