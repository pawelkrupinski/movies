import Foundation
import Combine

/// Keeps hiddenFilms — and the account's language pick — in step with the
/// server while signed in.
///
/// `disabledCinemas` never appears here — it's device-local (see
/// `UserPreferences.setDisabledCinemas`) and never has been synced by this
/// class since the cinema-hiding retirement.
///
/// hiddenFilms reconcile happens on THREE triggers, each closing a gap the
/// previous design had: login (always had this), app FOREGROUND RESUME (new —
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
///
/// The language pick rides the LEGACY `/api/me/state` document instead (via
/// `LanguageClient` — there's no granular endpoint for a single scalar), and
/// is NOT gated by the hiddenFilms per-country migration flags — see
/// `reconcileLanguage` for why a scalar pick doesn't need the two-phase
/// dance the sets do. It reconciles at the SAME triggers as hiddenFilms
/// (login, resume, country switch) but as its own independent step (a
/// separate fetch — `HiddenFilmsClient`'s response never carries `language`
/// at all), and pushes a local change on its OWN debounce (`schedulePush`),
/// never echoing back a value it just adopted from the server —
/// unlike the hiddenFilms writes, there was never really a batching case to
/// close for a picker choice, but this mirrors the shape the mechanism
/// always had here.
@MainActor
final class StateSyncService: ObservableObject {
    private let prefs: UserPreferences
    private let client: HiddenFilmsClient
    private let languageClient: LanguageClient
    private var isLoggedIn = false
    private var authCancellable: AnyCancellable?
    private var prefsCancellables = Set<AnyCancellable>()
    private var syncTask: Task<Void, Never>?
    private var debounceWorkItem: DispatchWorkItem?
    /// The language the account is known to hold — last fetched or
    /// successfully pushed. A `selectedLanguage` change to this value merely
    /// adopted the server's pick, so it is never pushed back.
    private var accountLanguage: String?
    /// A local pick the server hasn't confirmed yet: its debounced push is
    /// still waiting, or it failed. While set, a reconcile pushes it instead
    /// of fetching — the account's value is by definition OLDER than it.
    /// Persisted in `UserPreferences`, so it outlives a relaunch.
    private var pendingLanguage: String? {
        get { prefs.pendingLanguagePush }
        set { prefs.setPendingLanguagePush(newValue) }
    }

    init(
        prefs: UserPreferences,
        userPublisher: AnyPublisher<UserProfile?, Never>,
        client: HiddenFilmsClient,
        languageClient: LanguageClient
    ) {
        self.prefs = prefs
        self.client = client
        self.languageClient = languageClient
        observeUser(userPublisher)
    }

    private func observeUser(_ publisher: AnyPublisher<UserProfile?, Never>) {
        authCancellable = publisher
            .receive(on: DispatchQueue.main)
            .sink { [weak self] user in
                guard let self else { return }
                if user != nil {
                    // `AuthService.user` re-publishes the profile every time
                    // `checkSession()` re-runs (the root `.task` restarts when
                    // the language `.id` flips) — only the FIRST non-nil value
                    // is a login; re-running `onLogin` would stack a second set
                    // of change observers and PUT every hide twice.
                    guard !self.isLoggedIn else { return }
                    self.isLoggedIn = true
                    self.onLogin()
                } else {
                    // Clear every country's migration flag only on a GENUINE
                    // logout, not the initial `nil` the publisher emits before a
                    // session restores — otherwise every cold start would re-run
                    // the first-login union instead of treating the server as
                    // authoritative.
                    // The same goes for an unsent language pick: it is
                    // persisted precisely so the session restore after a
                    // relaunch can still push it.
                    if self.isLoggedIn {
                        self.prefs.clearHiddenFilmsMigration()
                        self.pendingLanguage = nil
                    }
                    self.isLoggedIn = false
                    self.cancelSync()
                }
            }
    }

    private func onLogin() {
        syncTask = Task { [weak self] in
            guard let self else { return }
            await self.reconcileLanguage()
            await self.reconcile(country: self.prefs.selectedCountry.code)
            self.observeLocalChanges()
            self.observeCountryChanges()
        }
    }

    private func cancelSync() {
        syncTask?.cancel()
        syncTask = nil
        debounceWorkItem?.cancel()
        debounceWorkItem = nil
        accountLanguage = nil
        prefsCancellables.removeAll()
    }

    /// Public entry point for the foreground-resume trigger
    /// (`ContentView.swift`'s `scenePhase` handler) — reconciles whatever
    /// country is currently selected, PLUS language (which needs no country
    /// at all — see `reconcileLanguage`). A no-op while signed out.
    func reconcileCurrentCountry() async {
        guard isLoggedIn else { return }
        await reconcileLanguage()
        await reconcile(country: prefs.selectedCountry.code)
    }

    /// The reconcile for ONE country. The local store keeps one set per
    /// country, exactly like the server, so each country reconciles against
    /// its OWN local bucket — a country switch can neither leak the previous
    /// country's titles into this one nor leave them on screen after a 304.
    ///
    /// - Already migrated: fetch CONDITIONALLY with the stored validators —
    ///   a "not modified" leaves local untouched; a fresh result REPLACES
    ///   local (server authoritative, so a removal made elsewhere stays
    ///   removed instead of being resurrected).
    /// - First reconcile of this country this sign-in: fetch
    ///   unconditionally, union local+server, push every LOCAL-ONLY title as
    ///   its own `hide` call (there's no bulk push).
    ///
    /// A result that lands after the user switched away writes to its own
    /// country's bucket, never over the newly selected one.
    private func reconcile(country: String) async {
        do {
            if prefs.isHiddenFilmsMigrated(country: country) {
                let (etag, lastModified) = prefs.hiddenFilmsValidators(country: country)
                if case .current(let remote) = try await client.fetch(country: country, etag: etag, lastModified: lastModified) {
                    prefs.setHiddenFilms(remote.hiddenFilms, country: country)
                    prefs.setHiddenFilmsValidators(country: country, etag: remote.etag, lastModified: remote.lastModified)
                }
            } else {
                // No usable validators, so this is always a fresh 200.
                guard case .current(let remote) = try await client.fetch(country: country, etag: nil, lastModified: nil) else { return }
                let local     = prefs.hiddenFilms(country: country)
                let localOnly = local.subtracting(remote.hiddenFilms)
                prefs.setHiddenFilms(local.union(remote.hiddenFilms), country: country)

                var latest = remote
                for title in localOnly {
                    latest = try await client.hide(country: country, title: title)
                }
                prefs.setHiddenFilmsValidators(country: country, etag: latest.etag, lastModified: latest.lastModified)
            }
            prefs.setHiddenFilmsMigrated(country: country)
        } catch {
            // Network error — local state is authoritative; leave prefs + flags
            // alone, a later reconcile (resume, country switch, next login)
            // retries.
        }
    }

    /// Language is a scalar, not a set, so it skips the per-country
    /// migration-flag dance `reconcile` needs entirely — there's no "removed
    /// on another device" case a blind overwrite could wrongly resurrect, so
    /// every reconcile (first or not) uses the same rule: the ACCOUNT's
    /// explicit pick wins whenever it has one (restored on login, per spec);
    /// otherwise this device's own explicit pick, if any, becomes the
    /// account's. A separate fetch from `reconcile`'s — `HiddenFilmsClient`'s
    /// response never carries `language` at all, only `LanguageClient`'s does.
    ///
    /// The one exception is a PENDING local pick (see `pendingLanguage`): it
    /// is newer than anything the account holds, so it is pushed rather than
    /// overwritten — whether it was made just before this reconcile (inside
    /// the push debounce), while the fetch was in flight, or its push failed.
    private func reconcileLanguage() async {
        guard isLoggedIn else { return }
        if let pending = pendingLanguage { return await pushLanguage(pending) }
        do {
            let remoteLanguage = try await languageClient.fetch()
            if let pending = pendingLanguage { return await pushLanguage(pending) }
            if let remoteLanguage {
                accountLanguage = remoteLanguage
                if remoteLanguage != prefs.selectedLanguage { prefs.setLanguage(remoteLanguage) }
            } else if let explicit = prefs.explicitLanguage {
                pendingLanguage = explicit
                await pushLanguage(explicit)
            }
        } catch {
            // Network error — local state is authoritative; leave prefs alone.
        }
    }

    /// Push `language` now, superseding any debounced push. On success the
    /// account holds it; on failure it stays pending, and the next reconcile
    /// (resume, country switch, next login) retries it — the same self-heal
    /// the hiddenFilms writes rely on.
    private func pushLanguage(_ language: String) async {
        debounceWorkItem?.cancel()
        debounceWorkItem = nil
        do {
            try await languageClient.push(language)
            accountLanguage = language
            if pendingLanguage == language { pendingLanguage = nil }
        } catch {
            // Left pending — see above.
        }
    }

    /// Push every local hide/unhide/clear-all immediately — see the class
    /// doc for why there's no debounce here any more. Language pushes on its
    /// own debounce instead — see `schedulePush`.
    private func observeLocalChanges() {
        prefs.hiddenFilmsChanges
            .receive(on: DispatchQueue.main)
            .sink { [weak self] change in self?.push(change) }
            .store(in: &prefsCancellables)

        // `.dropFirst()` skips the INIT value `UserPreferences` resolves at
        // launch (device/storefront fallback, not a pick) — every mutation
        // after that goes through `setLanguage`: either an explicit pick, or
        // `reconcileLanguage` adopting `accountLanguage`, which is skipped.
        // Delivered synchronously (no `receive(on:)` hop — `setLanguage` runs
        // on the main actor), so a pick is pending before any reconcile that
        // follows it can read the account's older value.
        prefs.$selectedLanguage
            .dropFirst()
            .sink { [weak self] language in self?.languageChanged(to: language) }
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

    private func languageChanged(to language: String) {
        guard language != accountLanguage else {
            // Back on (or adopted) what the account already holds — nothing to push.
            pendingLanguage = nil
            debounceWorkItem?.cancel()
            debounceWorkItem = nil
            return
        }
        pendingLanguage = language
        schedulePush()
    }

    /// Debounced push of `pendingLanguage` — 400ms, long enough that a rapid
    /// run of picker taps folds into one PUT.
    private func schedulePush() {
        debounceWorkItem?.cancel()
        let item = DispatchWorkItem { [weak self] in
            Task { @MainActor [weak self] in
                guard let self, self.isLoggedIn, let pending = self.pendingLanguage else { return }
                await self.pushLanguage(pending)
            }
        }
        debounceWorkItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4, execute: item)
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
