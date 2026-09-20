import Foundation

struct UserSyncState: Equatable {
    var hiddenFilms: Set<String>
    var disabledCinemas: Set<String>
    /// The account's explicit language pick, or nil if it has none yet.
    /// Unlike the two sets above, never itself the "resolved" value a
    /// visitor never chose — see `StateSyncService`'s reconcile for why
    /// that distinction matters (a resolved default sent here would clear
    /// or overwrite a genuine account pick).
    var language: String? = nil
}

protocol UserStateClient: AnyObject {
    func fetchState() async throws -> UserSyncState
    func putState(_ state: UserSyncState) async throws
}

final class HttpUserStateClient: UserStateClient {

    func fetchState() async throws -> UserSyncState {
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        let (data, response) = try await URLSession.shared.data(for: request)
        guard let http = response as? HTTPURLResponse, http.statusCode == 200 else {
            throw URLError(.userAuthenticationRequired)
        }
        let decoded = try JSONDecoder().decode(WireState.self, from: data)
        return UserSyncState(
            hiddenFilms: decoded.hiddenFilms, disabledCinemas: decoded.disabledCinemas, language: decoded.language)
    }

    func putState(_ state: UserSyncState) async throws {
        let body = WireState(
            hiddenFilms: state.hiddenFilms, disabledCinemas: state.disabledCinemas, language: state.language)
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.httpMethod = "PUT"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(body)
        _ = try await URLSession.shared.data(for: request)
    }

    // `language: String?`'s synthesized Codable conformance already does the
    // right thing on both sides with no extra code: encoding OMITS the key
    // when nil (Swift's derived `encode(to:)` uses `encodeIfPresent` for an
    // Optional property) rather than sending an explicit JSON `null` — which
    // matters, because the server treats an absent key as "leave the stored
    // pick alone" and an explicit `null` as "clear it". Decoding treats a
    // missing key and a JSON `null` alike (`decodeIfPresent`), both landing
    // as `nil` — exactly the server's "no pick yet" case.
    private struct WireState: Codable {
        let hiddenFilms: Set<String>
        let disabledCinemas: Set<String>
        let language: String?
    }
}
