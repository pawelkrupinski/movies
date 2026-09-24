import Foundation

/// The account's language pick — the one thing still riding the legacy
/// `/api/me/state` document now that hiddenFilms moved to `HiddenFilmsClient`
/// and disabledCinemas is device-local (see `UserPreferences.setDisabledCinemas`):
/// there's no granular endpoint for a single scalar pick, only for the two
/// sets that used to share this document with it. Mirrors Android's
/// equivalent split.
protocol LanguageClient: AnyObject {
    /// The account's current pick, or nil if it hasn't made one.
    func fetch() async throws -> String?

    /// Push this device's explicit pick as the account's. Throws when the
    /// server did not accept it: `LanguagePushRefused` when it never will (the
    /// caller then stops owing the pick), anything else when a retry may land
    /// (the caller keeps it pending).
    func push(_ language: String) async throws
}

/// The server refused a pushed pick for good — a 4xx other than 401 (signed
/// out), 408 (timed out) or 429 (throttled): a language it does not know, which
/// no amount of resending changes. Same rule as the web's `_retryable`.
struct LanguagePushRefused: Error, Equatable {
    let statusCode: Int

    static func isPermanent(_ statusCode: Int) -> Bool {
        (400..<500).contains(statusCode) && ![401, 408, 429].contains(statusCode)
    }
}

final class HttpLanguageClient: LanguageClient {
    private let session: URLSession

    init(session: URLSession = .shared) {
        self.session = session
    }

    func fetch() async throws -> String? {
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        let (data, response) = try await session.data(for: request)
        guard let http = response as? HTTPURLResponse, http.statusCode == 200 else {
            throw URLError(.userAuthenticationRequired)
        }
        return try JSONDecoder().decode(WireLanguage.self, from: data).language
    }

    // PUT /api/me/state is a partial update — a field this body omits keeps
    // its stored value, so sending `language` alone can never wipe the
    // hiddenFilms/disabledCinemas an older client (or this app, before this
    // split) left there. `language: String?`'s synthesized `Codable`
    // conformance already does the right thing with no extra code: encoding
    // OMITS the key when nil rather than sending an explicit JSON `null` —
    // which matters, because the server treats an absent key as "leave the
    // stored pick alone" and an explicit `null` as "clear it". This method
    // is never actually called with nothing to push, but the wire shape
    // stays correct either way.
    func push(_ language: String) async throws {
        let body = WireLanguage(language: language)
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.httpMethod = "PUT"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(body)
        let (_, response) = try await session.data(for: request)
        guard let http = response as? HTTPURLResponse else { throw URLError(.badServerResponse) }
        guard (200..<300).contains(http.statusCode) else {
            if LanguagePushRefused.isPermanent(http.statusCode) { throw LanguagePushRefused(statusCode: http.statusCode) }
            throw URLError(.badServerResponse)
        }
    }

    private struct WireLanguage: Codable {
        let language: String?
    }
}
