import Foundation
import Combine

@MainActor
final class StateSyncService: ObservableObject {
    private let prefs: UserPreferences
    private let auth: AuthService
    private var cancellables = Set<AnyCancellable>()
    private var syncTask: Task<Void, Never>?
    private var debounceWorkItem: DispatchWorkItem?

    init(prefs: UserPreferences, auth: AuthService) {
        self.prefs = prefs
        self.auth = auth
        observeAuthState()
    }

    private func observeAuthState() {
        auth.$user
            .receive(on: DispatchQueue.main)
            .sink { [weak self] user in
                guard let self else { return }
                if user != nil {
                    self.onLogin()
                } else {
                    self.cancelSync()
                }
            }
            .store(in: &cancellables)
    }

    private func onLogin() {
        syncTask = Task { [weak self] in
            guard let self else { return }
            await self.mergeWithServer()
            self.startObservingPrefs()
        }
    }

    private func cancelSync() {
        syncTask?.cancel()
        syncTask = nil
        cancellables = cancellables.filter { _ in true }
    }

    private func mergeWithServer() async {
        do {
            let remote = try await fetchState()
            let mergedHidden = prefs.hiddenFilms.union(remote.hiddenFilms)
            let mergedDisabled = prefs.disabledCinemas.union(remote.disabledCinemas)
            for title in mergedHidden where !prefs.hiddenFilms.contains(title) {
                prefs.hide(title)
            }
            if mergedDisabled != prefs.disabledCinemas {
                prefs.setDisabledCinemas(mergedDisabled)
            }
            try await putState(hidden: mergedHidden, disabled: mergedDisabled)
        } catch {
            // Network error — local state is authoritative
        }
    }

    private func startObservingPrefs() {
        prefs.$hiddenFilms
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] _ in self?.schedulePush() }
            .store(in: &cancellables)

        prefs.$disabledCinemas
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] _ in self?.schedulePush() }
            .store(in: &cancellables)
    }

    private func schedulePush() {
        guard auth.user != nil else { return }
        debounceWorkItem?.cancel()
        let item = DispatchWorkItem { [weak self] in
            Task { @MainActor [weak self] in
                guard let self, self.auth.user != nil else { return }
                try? await self.putState(hidden: self.prefs.hiddenFilms, disabled: self.prefs.disabledCinemas)
            }
        }
        debounceWorkItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4, execute: item)
    }

    // MARK: - Network

    private struct ServerState: Codable {
        let hiddenFilms: Set<String>
        let disabledCinemas: Set<String>
    }

    private func fetchState() async throws -> ServerState {
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        let (data, response) = try await URLSession.shared.data(for: request)
        guard let http = response as? HTTPURLResponse, http.statusCode == 200 else {
            throw URLError(.userAuthenticationRequired)
        }
        return try JSONDecoder().decode(ServerState.self, from: data)
    }

    private func putState(hidden: Set<String>, disabled: Set<String>) async throws {
        let body = ServerState(hiddenFilms: hidden, disabledCinemas: disabled)
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me/state"))
        request.httpMethod = "PUT"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(body)
        _ = try await URLSession.shared.data(for: request)
    }
}
