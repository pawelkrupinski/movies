import Foundation
import XCTest

enum Fixtures {
    static func load(_ name: String, ext: String = "html", file: StaticString = #filePath, line: UInt = #line) throws -> String {
        guard let url = Bundle.module.url(forResource: name, withExtension: ext, subdirectory: "Fixtures") else {
            XCTFail("missing fixture: \(name).\(ext)", file: file, line: line)
            throw NSError(domain: "Fixture", code: 1)
        }
        return try String(contentsOf: url, encoding: .utf8)
    }
}
