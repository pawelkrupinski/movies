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

    func mergeWithServer() async {
        do {
            let remote = try await client.fetchState()
            if prefs.serverStateSynced {
                // Already migrated — the server is the source of truth. Mirror it
                // so a hide/cinema removed on another device (or this one, last
                // session) stays removed instead of being resurrected by a union.
                if prefs.hiddenFilms != remote.hiddenFilms {
                    prefs.setHiddenFilms(remote.hiddenFilms)
                }
                if prefs.disabledCinemas != remote.disabledCinemas {
                    prefs.setDisabledCinemas(remote.disabledCinemas)
                }
            } else {
                // First sync after login — union this device's local picks up so
                // nothing set while signed-out is lost, push it, then flip the flag.
                let mergedHidden = prefs.hiddenFilms.union(remote.hiddenFilms)
                let mergedDisabled = prefs.disabledCinemas.union(remote.disabledCinemas)
                if mergedHidden != prefs.hiddenFilms { prefs.setHiddenFilms(mergedHidden) }
                if mergedDisabled != prefs.disabledCinemas { prefs.setDisabledCinemas(mergedDisabled) }
                try await client.putState(UserSyncState(hiddenFilms: mergedHidden, disabledCinemas: mergedDisabled))
                prefs.setServerStateSynced(true)
            }

            // Language is a scalar, not a set, so it skips the migration-flag
            // dance above entirely — there's no "removed on another device"
            // case a blind union could wrongly resurrect. Same rule on every
            // merge, first or not: the account's pick wins whenever it has
            // one (restored on login, per spec); otherwise this device's own
            // explicit pick, if any, becomes the account's.
            if let remoteLanguage = remote.language {
                if remoteLanguage != prefs.selectedLanguage { prefs.setLanguage(remoteLanguage) }
            } else if let explicit = prefs.explicitLanguage {
                try? await client.putState(UserSyncState(
                    hiddenFilms: prefs.hiddenFilms, disabledCinemas: prefs.disabledCinemas, language: explicit))
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

        prefs.$disabledCinemas
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] _ in self?.schedulePush() }
            .store(in: &prefsCancellables)

        // `.dropFirst()` skips the INIT value `UserPreferences` resolves at
        // launch (device/storefront fallback, not a pick) — every mutation
        // after that goes through `setLanguage`, which only ever runs for an
        // explicit pick, so every event this sink sees IS one.
        prefs.$selectedLanguage
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
                    disabledCinemas: self.prefs.disabledCinemas,
                    language: self.prefs.explicitLanguage
                ))
            }
        }
        debounceWorkItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4, execute: item)
    }
}
