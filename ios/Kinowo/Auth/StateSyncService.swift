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
                    // Clear the migration flag only on a GENUINE logout, not the
                    // initial `nil` the publisher emits before a session restores
                    // — otherwise every cold start would re-run the first-login
                    // union instead of treating the server as authoritative.
                    if self.isLoggedIn { self.prefs.setServerStateSynced(false) }
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

    /// hiddenFilms only — disabledCinemas is device-local now (see
    /// `UserSyncState`'s doc comment), so this never reads or writes
    /// `prefs.disabledCinemas` in either direction.
    func mergeWithServer() async {
        do {
            let remote = try await client.fetchState()
            if prefs.serverStateSynced {
                // Already migrated — the server is the source of truth. Mirror it
                // so a hide removed on another device (or this one, last session)
                // stays removed instead of being resurrected by a union.
                if prefs.hiddenFilms != remote.hiddenFilms {
                    prefs.setHiddenFilms(remote.hiddenFilms)
                }
            } else {
                // First sync after login — union this device's local picks up so
                // nothing set while signed-out is lost, push it, then flip the flag.
                let mergedHidden = prefs.hiddenFilms.union(remote.hiddenFilms)
                if mergedHidden != prefs.hiddenFilms { prefs.setHiddenFilms(mergedHidden) }
                try await client.putState(UserSyncState(hiddenFilms: mergedHidden))
                prefs.setServerStateSynced(true)
            }
        } catch {
            // Network error — local state is authoritative; leave prefs + flag alone.
        }
    }

    private func startObservingPrefs() {
        prefs.$hiddenFilms
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
                try? await self.client.putState(UserSyncState(hiddenFilms: self.prefs.hiddenFilms))
            }
        }
        debounceWorkItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4, execute: item)
    }
}
