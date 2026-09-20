import Foundation

/// hiddenFilms ONLY — disabledCinemas stopped being a server-synced field (it's
/// device-local now, see `UserPreferences.setDisabledCinemas`). `/api/me/state`
/// still accepts/returns disabledCinemas server-side, for whatever older app
/// build still sends it; this client just no longer models that half of the
/// payload.
struct UserSyncState: Equatable {
    var hiddenFilms: Set<String>
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
        // `WireState` has no `disabledCinemas` field, so `Decodable`'s default
        // "ignore keys the struct doesn't model" silently drops the server's
        // (still-present, for old clients) disabledCinemas from the response.
        let decoded = try JSONDecoder().decode(WireState.self, from: data)
        return UserSyncState(hiddenFilms: decoded.hiddenFilms)
    }

    func putState(_ state: UserSyncState) async throws {
        let body = WireState(hiddenFilms: state.hiddenFilms)
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.httpMethod = "PUT"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        // Encoding `WireState` never emits a `disabledCinemas` key. The legacy
        // PUT is a partial update server-side — an ABSENT key means "leave
        // whatever's already stored alone", not "clear it" — so this can never
        // wipe another (older) client's disabledCinemas writes.
        request.httpBody = try JSONEncoder().encode(body)
        _ = try await URLSession.shared.data(for: request)
    }

    private struct WireState: Codable {
        let hiddenFilms: Set<String>
    }
}
