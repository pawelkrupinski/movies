import Foundation

/// The repo's checked-in retry/error classification table
/// (`test/resources/retry-classification.json`), which the Scala, Swift and
/// Kotlin specs each hold their production classifiers to, row by row — so a
/// verdict changed on one platform and not the others fails a test instead of
/// shipping (the 403 that dropped language picks on web, iOS and Android
/// alike, 78a6c72a2).
public struct RetryClassificationTable: Decodable {

    public struct Row: Decodable, CustomStringConvertible {
        public let source: String
        public let error: String
        public let verdict: String

        public var isPermanent: Bool { verdict == "permanent" }

        /// The status an `http:NNN` error names.
        public var status: Int? {
            let parts = error.split(separator: ":")
            return parts.count == 2 ? Int(parts[1]) : nil
        }

        public var description: String { "\(source)/\(error) → \(verdict)" }
    }

    public struct Source: Decodable {
        public let consumers: [String]
    }

    public let sources: [String: Source]
    public let rows: [Row]

    /// The rows of one source, which must be one the table declares.
    public func rows(for source: String) throws -> [Row] {
        guard sources[source] != nil else {
            throw NSError(domain: "RetryClassificationTable", code: 1,
                          userInfo: [NSLocalizedDescriptionKey: "no source '\(source)' in the table"])
        }
        return rows.filter { $0.source == source }
    }

    /// Every source this platform's code enforces.
    public func sources(consumedBy platform: String) -> Set<String> {
        Set(sources.filter { $0.value.consumers.contains(platform) }.map(\.key))
    }

    /// `test/resources/retry-classification.json` — found from THIS file,
    /// which sits at `ios/Tests/KinowoTestSupport/`, four levels below the
    /// repo root. (`#filePath` in the body, not as a default argument: there
    /// it would take each caller's path, and work only while every caller
    /// happens to sit at the same depth.)
    public static func load() throws -> RetryClassificationTable {
        let root = URL(fileURLWithPath: #filePath)
            .deletingLastPathComponent()   // KinowoTestSupport
            .deletingLastPathComponent()   // Tests
            .deletingLastPathComponent()   // ios
            .deletingLastPathComponent()   // repo root
        let data = try Data(contentsOf: root.appendingPathComponent("test/resources/retry-classification.json"))
        return try JSONDecoder().decode(RetryClassificationTable.self, from: data)
    }
}
