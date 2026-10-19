import Foundation

/// Dotted numeric version comparison — the Swift mirror of Scala
/// `models.ClientVersion` (web/src/main/scala/models/ClientSupport.scala),
/// which is the reference this and the Android gate are written against.
enum ClientVersion {

    /// `"1.2.3"` → `[1, 2, 3]`; nil for anything non-numeric. Trailing parts are
    /// optional, so `"1.0"` and `"1.0.0"` both parse and compare equal.
    static func parse(_ version: String) -> [Int]? {
        let parts = version.trimmingCharacters(in: .whitespaces)
            .split(separator: ".", omittingEmptySubsequences: false)
        guard !parts.isEmpty,
              parts.allSatisfy({ !$0.isEmpty && $0.allSatisfy { $0.isASCII && $0.isNumber } })
        else { return nil }
        let numbers = parts.compactMap { Int($0) }   // drops only an Int-overflowing part
        return numbers.count == parts.count ? numbers : nil
    }

    /// True when `version` is at least `minimum`, compared component-wise with
    /// missing components read as 0 (so 1.10 > 1.9 and 1.0 == 1.0.0); nil when
    /// either side doesn't parse.
    static func isAtLeast(_ version: String, minimum: String) -> Bool? {
        guard let v = parse(version), let m = parse(minimum) else { return nil }
        let width = max(v.count, m.count)
        let pad = { (s: [Int]) in s + Array(repeating: 0, count: width - s.count) }
        return zip(pad(v), pad(m)).first { $0 != $1 }.map { $0 > $1 } ?? true
    }
}

/// The `GET /api/client-support` body: `{"android":{…},"ios":{…}}`. Only the
/// `ios` half is read here; `android` is ignored so its shape can't break us.
struct ClientSupport: Decodable, Equatable {
    let ios: Platform

    /// One platform's rule — GATED only when both halves are present (a minimum
    /// with nowhere to send people would strand them in a dead app).
    struct Platform: Decodable, Equatable {
        let minimumVersion: String?
        let storeUrl: String?

        var isGated: Bool { minimumVersion != nil && storeUrl != nil }

        /// Unsupported ONLY when gated AND `version` parses AND it is below the
        /// minimum; an unparseable version (either side) counts as supported —
        /// locking someone out of a working app is the worse failure.
        func isSupported(version: String) -> Bool {
            guard isGated, let minimum = minimumVersion else { return true }
            return ClientVersion.isAtLeast(version, minimum: minimum) ?? true
        }

        /// Where to send a client on `version`: the store URL when it must
        /// update, nil when it may keep using the app.
        func updateURL(forVersion version: String) -> URL? {
            guard !isSupported(version: version), let storeUrl else { return nil }
            return URL(string: storeUrl)
        }
    }

    static func decode(_ data: Data) -> ClientSupport? {
        try? JSONDecoder().decode(ClientSupport.self, from: data)
    }
}
