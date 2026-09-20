import Foundation
import Combine

/// Keeps hiddenFilms in step with the server, per country, while signed in.
///
/// `disabledCinemas` never appears here — it's device-local (see
/// `UserPreferences.setDisabledCinemas`) and never has been synced by this
/// class since the cinema-hiding retirement.
///
/// Reconcile happens on THREE triggers, each closing a gap the previous
/// design had: login (always had this), app FOREGROUND RESUME (new —
/// repertoire already re-syncs on `scenePhase == .active`
/// (`ContentView.swift`), hiddenFilms didn't), and a COUNTRY SWITCH (new —
/// a country this device has never synced needs its own migration, not the
/// previously-selected country's). See `reconcile(country:)`.
///
/// Local hide/unhide/clear-all now push IMMEDIATELY (via
/// `UserPreferences.hiddenFilmsChanges`) instead of on a debounce: the old
/// 400ms debounce existed to batch several toggles into ONE bulk PUT body,
/// which `HiddenFilmsClient`'s per-title endpoints have no equivalent
/// need for — each hide/unhide is already its own idempotent request. That
/// also incidentally closes the "toggle right before backgrounding loses
/// the write" gap the debounce used to risk: there's no window to lose.
@MainActor
final class StateSyncService: ObservableObject {
    private let prefs: UserPreferences
    private let client: HiddenFilmsClient
    private var isLoggedIn = false
    private var authCancellable: AnyCancellable?
    private var prefsCancellables = Set<AnyCancellable>()
    private var syncTask: Task<Void, Never>?

    init(prefs: UserPreferences, userPublisher: AnyPublisher<UserProfile?, Never>, client: HiddenFilmsClient) {
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
                    // Clear every country's migration flag only on a GENUINE
                    // logout, not the initial `nil` the publisher emits before a
                    // session restores — otherwise every cold start would re-run
                    // the first-login union instead of treating the server as
                    // authoritative.
                    if self.isLoggedIn { self.prefs.clearHiddenFilmsMigration() }
                    self.isLoggedIn = false
                    self.cancelSync()
                }
            }
    }

    private func onLogin() {
        syncTask = Task { [weak self] in
            guard let self else { return }
            await self.reconcile(country: self.prefs.selectedCountry.code)
            self.observeLocalChanges()
            self.observeCountryChanges()
        }
    }

    private func cancelSync() {
        syncTask?.cancel()
        syncTask = nil
        prefsCancellables.removeAll()
    }

    /// Public entry point for the foreground-resume trigger
    /// (`ContentView.swift`'s `scenePhase` handler) — reconciles whatever
    /// country is currently selected. A no-op while signed out.
    func reconcileCurrentCountry() async {
        guard isLoggedIn else { return }
        await reconcile(country: prefs.selectedCountry.code)
    }

    /// The reconcile for ONE country. First call for that country (its
    /// migrated flag unset): fetch unconditionally, union local+server,
    /// push every LOCAL-ONLY title as its own `hide` call (there's no bulk
    /// push any more), store the resulting validators, mark migrated. Every
    /// call after that: fetch CONDITIONALLY using the stored validators — a
    /// "not modified" leaves local untouched; a fresh result REPLACES local
    /// (server authoritative, so a removal made elsewhere — another device,
    /// or this one last session — stays removed instead of being
    /// resurrected).
    private func reconcile(country: String) async {
        do {
            if prefs.isHiddenFilmsMigrated(country: country) {
                let (etag, lastModified) = prefs.hiddenFilmsValidators(country: country)
                switch try await client.fetch(country: country, etag: etag, lastModified: lastModified) {
                case .notModified:
                    break
                case .current(let remote):
                    if prefs.hiddenFilms != remote.hiddenFilms {
                        prefs.setHiddenFilms(remote.hiddenFilms)
                    }
                    prefs.setHiddenFilmsValidators(country: country, etag: remote.etag, lastModified: remote.lastModified)
                }
            } else {
                // No stored validators yet, so this is always a fresh 200.
                guard case .current(let remote) = try await client.fetch(country: country, etag: nil, lastModified: nil) else { return }
                let merged   = prefs.hiddenFilms.union(remote.hiddenFilms)
                let localOnly = prefs.hiddenFilms.subtracting(remote.hiddenFilms)
                if merged != prefs.hiddenFilms { prefs.setHiddenFilms(merged) }

                var latest = remote
                for title in localOnly {
                    latest = try await client.hide(country: country, title: title)
                }
                prefs.setHiddenFilmsValidators(country: country, etag: latest.etag, lastModified: latest.lastModified)
                prefs.setHiddenFilmsMigrated(country: country)
            }
        } catch {
            // Network error — local state is authoritative; leave prefs + flags
            // alone, a later reconcile (resume, country switch, next login)
            // retries.
        }
    }

    /// Push every local hide/unhide/clear-all immediately — see the class
    /// doc for why there's no debounce here any more.
    private func observeLocalChanges() {
        prefs.hiddenFilmsChanges
            .receive(on: DispatchQueue.main)
            .sink { [weak self] change in self?.push(change) }
            .store(in: &prefsCancellables)
    }

    private func push(_ change: HiddenFilmsChange) {
        guard isLoggedIn else { return }
        let country = prefs.selectedCountry.code
        Task { @MainActor [weak self] in
            guard let self, self.isLoggedIn else { return }
            do {
                let result: HiddenFilmsResult
                switch change {
                case .hidden(let title):   result = try await self.client.hide(country: country, title: title)
                case .unhidden(let title): result = try await self.client.unhide(country: country, title: title)
                case .clearedAll:          result = try await self.client.clear(country: country)
                }
                self.prefs.setHiddenFilmsValidators(country: country, etag: result.etag, lastModified: result.lastModified)
            } catch {
                // Best-effort — a later reconcile (resume, country switch, next
                // login) self-heals a write that silently failed.
            }
        }
    }

    /// A country switch reconciles that country the same way login does —
    /// it may be one this device has never synced. Skips a country already
    /// migrated on THIS reconcile pass isn't needed: `reconcile` itself is
    /// cheap (a conditional GET) once migrated, so there's no harm re-running
    /// it on every switch back to an already-migrated country either.
    private func observeCountryChanges() {
        prefs.$selectedCountry
            .dropFirst()
            .receive(on: DispatchQueue.main)
            .sink { [weak self] country in
                guard let self, self.isLoggedIn else { return }
                Task { await self.reconcile(country: country.code) }
            }
            .store(in: &prefsCancellables)
    }
}
