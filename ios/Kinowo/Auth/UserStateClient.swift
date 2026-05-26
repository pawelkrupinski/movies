import Foundation

struct UserSyncState: Equatable {
    var hiddenFilms: Set<String>
    var disabledCinemas: Set<String>
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
        return UserSyncState(hiddenFilms: decoded.hiddenFilms, disabledCinemas: decoded.disabledCinemas)
    }

    func putState(_ state: UserSyncState) async throws {
        let body = WireState(hiddenFilms: state.hiddenFilms, disabledCinemas: state.disabledCinemas)
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.httpMethod = "PUT"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(body)
        _ = try await URLSession.shared.data(for: request)
    }

    private struct WireState: Codable {
        let hiddenFilms: Set<String>
        let disabledCinemas: Set<String>
    }
}
