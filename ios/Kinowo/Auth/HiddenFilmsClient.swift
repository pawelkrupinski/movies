import Foundation

/// One country's hidden-films state as the server hands it back — from a
/// fetch OR a write, since every write endpoint echoes the resulting state
/// plus fresh validators (so a client that just wrote never needs a
/// follow-up GET to learn its new `etag`/`lastModified`).
struct HiddenFilmsResult: Equatable {
    var hiddenFilms: Set<String>
    var etag: String
    var lastModified: String
}

/// `fetch`'s two outcomes: the server proved nothing changed since the
/// validators this call sent (`notModified` — no body was even transferred),
/// or here is the current state (`current`).
enum HiddenFilmsFetchResult: Equatable {
    case notModified
    case current(HiddenFilmsResult)
}

/// The per-country granular hidden-films API — `GET`/`PUT`/`DELETE
/// /api/me/:country/hidden-films(/:title)`. Replaces the old bulk
/// `UserStateClient` (`GET`/`PUT /api/me/state`), which is now UNUSED by
/// this app build: the server keeps serving it for older builds that
/// haven't updated (it feeds a retirement-usage gauge there), but nothing
/// here calls it any more. `disabledCinemas` never appears here at all —
/// it stopped being a server-synced field before this client existed (see
/// `UserPreferences.setDisabledCinemas`).
protocol HiddenFilmsClient: AnyObject {
    func fetch(country: String, etag: String?, lastModified: String?) async throws -> HiddenFilmsFetchResult
    func hide(country: String, title: String) async throws -> HiddenFilmsResult
    func unhide(country: String, title: String) async throws -> HiddenFilmsResult
    func clear(country: String) async throws -> HiddenFilmsResult
}

/// The server refused a hidden-films write for good — only
/// `UserStateController`'s own refusals: a 400 (a title over its length bound,
/// or a country it does not know) or a 413 (the country's bucket is full). No
/// amount of resending changes either, so the caller stops owing the write.
/// Every other failure is retried: a 403 in particular is as likely a
/// Cloudflare challenge in front of the app as anything the app said. Same
/// rule as Android's `HiddenFilmsWriteRefused` and the web's
/// `_hiddenFilmsWriteRefused`.
struct HiddenFilmsWriteRefused: Error, Equatable {
    let statusCode: Int

    static func isPermanent(_ statusCode: Int) -> Bool {
        statusCode == 400 || statusCode == 413
    }
}

final class HttpHiddenFilmsClient: HiddenFilmsClient {
    private let session: URLSession

    init(session: URLSession = .shared) {
        self.session = session
    }

    func fetch(country: String, etag: String?, lastModified: String?) async throws -> HiddenFilmsFetchResult {
        var request = URLRequest(url: Self.url(country: country))
        Self.applyCommonHeaders(&request)
        if let etag { request.setValue(etag, forHTTPHeaderField: "If-None-Match") }
        // Sent alongside If-None-Match too, not just when etag is nil: the
        // server's own precedence (RFC 7232 §3.3 — If-None-Match wins when
        // both are present) makes this always safe, and it means a client
        // that somehow lost its etag but kept lastModified still gets the
        // cheap path.
        if let lastModified { request.setValue(lastModified, forHTTPHeaderField: "If-Modified-Since") }

        let (data, response) = try await session.data(for: request)
        guard let http = response as? HTTPURLResponse else { throw URLError(.badServerResponse) }
        if http.statusCode == 304 { return .notModified }
        guard http.statusCode == 200 else { throw URLError(.userAuthenticationRequired) }
        return .current(try Self.decode(data: data, response: http))
    }

    func hide(country: String, title: String) async throws -> HiddenFilmsResult {
        try await write(method: "PUT", country: country, title: title)
    }

    func unhide(country: String, title: String) async throws -> HiddenFilmsResult {
        try await write(method: "DELETE", country: country, title: title)
    }

    func clear(country: String) async throws -> HiddenFilmsResult {
        var request = URLRequest(url: Self.url(country: country))
        request.httpMethod = "DELETE"
        return try await send(request)
    }

    private func write(method: String, country: String, title: String) async throws -> HiddenFilmsResult {
        let encodedTitle = title.addingPercentEncoding(withAllowedCharacters: Self.titleAllowed) ?? title
        var request = URLRequest(url: Self.url(country: country, encodedTitleSegment: encodedTitle))
        request.httpMethod = method
        return try await send(request)
    }

    /// One write: the resulting state, `HiddenFilmsWriteRefused` when the
    /// server will never accept it, any other error when a resend may land.
    private func send(_ request: URLRequest) async throws -> HiddenFilmsResult {
        var request = request
        Self.applyCommonHeaders(&request)
        let (data, response) = try await session.data(for: request)
        guard let http = response as? HTTPURLResponse else { throw URLError(.badServerResponse) }
        guard http.statusCode == 200 else {
            if HiddenFilmsWriteRefused.isPermanent(http.statusCode) { throw HiddenFilmsWriteRefused(statusCode: http.statusCode) }
            throw URLError(.userAuthenticationRequired)
        }
        return try Self.decode(data: data, response: http)
    }

    private static func applyCommonHeaders(_ request: inout URLRequest) {
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
    }

    private static func decode(data: Data, response: HTTPURLResponse) throws -> HiddenFilmsResult {
        // Case-insensitive lookup (HTTP header names aren't case-sensitive,
        // and `value(forHTTPHeaderField:)` already normalises that) — a
        // response with either validator missing is malformed, not merely
        // "no cache benefit", since callers persist both together.
        guard let etag = response.value(forHTTPHeaderField: "ETag"),
              let lastModified = response.value(forHTTPHeaderField: "Last-Modified") else {
            throw URLError(.badServerResponse)
        }
        let wire = try JSONDecoder().decode(WireHiddenFilms.self, from: data)
        return HiddenFilmsResult(hiddenFilms: Set(wire.hiddenFilms), etag: etag, lastModified: lastModified)
    }

    private struct WireHiddenFilms: Decodable {
        let hiddenFilms: [String]
    }

    /// Allowed characters for the `:title` PATH SEGMENT — everything else
    /// (spaces, diacritics, punctuation, a literal `/`) percent-encodes.
    ///
    /// Deliberately a SEPARATE definition from `FilmShareLink.titleAllowed`
    /// (`Models/Film.swift`), not a shared import, even though the character
    /// set is identical in spirit: `KinowoAuth` (this file's SPM target) has
    /// no dependency on `KinowoCore` (where `FilmShareLink` lives) in
    /// `Package.swift` — see that manifest's target list. Adding one would
    /// ripple into the Xcode app target's flat-module build in a way this
    /// change can't verify without a full Xcode toolchain. Keep this in sync
    /// with `FilmShareLink.titleAllowed` if either changes.
    /// Internal, not private: `HiddenFilmsClientURLTests` exercises this and
    /// `url(country:encodedTitleSegment:)` directly — the one place a
    /// double-encoding or a mis-split `/` would silently "look right" and
    /// only fail in production, on the one title that has a slash in it.
    static let titleAllowed: CharacterSet = {
        var set = CharacterSet()
        set.insert(charactersIn: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*-._")
        return set
    }()

    static func url(country: String, encodedTitleSegment: String? = nil) -> URL {
        let base = kinowoBaseURL
            .appendingPathComponent("api")
            .appendingPathComponent("me")
            .appendingPathComponent(country)
            .appendingPathComponent("hidden-films")
        guard let encodedTitleSegment else { return base }
        // NOT `appendingPathComponent(title)`: that method applies its OWN
        // percent-encoding (double-encoding an already-escaped `%`) and, worse,
        // treats an embedded `/` as a path separator rather than data to encode
        // — splitting one title into two segments instead of one. The segment
        // handed in here is ALREADY fully percent-encoded (see `titleAllowed`),
        // so it's appended as a literal string instead.
        return URL(string: base.absoluteString + "/" + encodedTitleSegment)!
    }
}
