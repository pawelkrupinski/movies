import Foundation
import Combine

@MainActor
final class StateSyncService: ObservableObject {
    private let prefs: UserPreferences
    private let client: UserStateClient
    private var isLoggedIn = false
    private var authCancellable: AnyCancellable?
    private var prefsCancellables = Set<AnyCancellable>()
    private var syncTask: Task<Void, Never>?
    private var debounceWorkItem: DispatchWorkItem?

    init(prefs: UserPreferences, userPublisher: AnyPublisher<UserProfile?, Never>, client: UserStateClient) {
        self.prefs = prefs
        self.client = client
        observeUser(userPublisher)
    }

    private func observeUser(_ publisher: AnyPublisher<UserProfile?, Never>) {
        authCancellable = publisher
            .receive(on: DispatchQueue.main)
            .sink { [weak self] user in
                guard let self else { return }
                if user != nil {
                    self.isLoggedIn = true
                    self.onLogin()
                } else {
                    self.isLoggedIn = false
                    self.cancelSync()
                }
            }
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
        prefsCancellables.removeAll()
    }

    func mergeWithServer() async {
        do {
            let remote = try await client.fetchState()
            let mergedHidden = prefs.hiddenFilms.union(remote.hiddenFilms)
            let mergedDisabled = prefs.disabledCinemas.union(remote.disabledCinemas)
            for title in mergedHidden where !prefs.hiddenFilms.contains(title) {
                prefs.hide(title)
            }
            if mergedDisabled != prefs.disabledCinemas {
                prefs.setDisabledCinemas(mergedDisabled)
            }
            try await client.putState(UserSyncState(hiddenFilms: mergedHidden, disabledCinemas: mergedDisabled))
        } catch {
            // Network error — local state is authoritative
        }
    }

    private func startObservingPrefs() {
        prefs.$hiddenFilms
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] _ in self?.schedulePush() }
            .store(in: &prefsCancellables)

        prefs.$disabledCinemas
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] _ in self?.schedulePush() }
            .store(in: &prefsCancellables)
    }

    private func schedulePush() {
        guard isLoggedIn else { return }
        debounceWorkItem?.cancel()
        let item = DispatchWorkItem { [weak self] in
            Task { @MainActor [weak self] in
                guard let self, self.isLoggedIn else { return }
                try? await self.client.putState(UserSyncState(
                    hiddenFilms: self.prefs.hiddenFilms,
                    disabledCinemas: self.prefs.disabledCinemas
                ))
            }
        }
        debounceWorkItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4, execute: item)
    }
}
