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
/// Each edit is queued (persisted) before it is sent and dequeued once the
/// server accepts it, so a write that failed — offline, or the app killed
/// mid-request — is re-sent by the next reconcile before it fetches.
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
    /// Bumped by every genuine logout, which forgets the hiddenFilms queue: a
    /// response from before it must not touch the next session's queue.
    private var session = 0
    private var authCancellable: AnyCancellable?
    private var prefsCancellables = Set<AnyCancellable>()
    private var syncTask: Task<Void, Never>?
    private let debounceScheduler: DebounceScheduler
    private var pendingDebounce: AnyCancellable?
    /// The language push on the wire, if any — see `sendPendingLanguage`.
    private var languageSendTask: Task<Void, Never>?
    /// Set while `adopt` writes the account's pick — see there.
    private var adopting = false
    /// Counts the language pushes the server confirmed, so a reconcile can
    /// tell that one landed while its fetch was on the wire.
    private var languagePushesConfirmed = 0
    /// Counts local picks, so a send can tell a pick made while it was on the
    /// wire from the one it sent — even when both are the same value.
    private var languagePicks = 0
    /// The hiddenFilms queue flush in progress, if any — see `sendPendingChanges`.
    private var flushTask: Task<Bool, Never>?
    /// The language the account is known to hold — last fetched or
    /// successfully pushed; nil when unknown (a push failed). A `selectedLanguage` change to this value merely
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
        languageClient: LanguageClient,
        debounceScheduler: DebounceScheduler = MainQueueDebounceScheduler()
    ) {
        self.prefs = prefs
        self.client = client
        self.languageClient = languageClient
        self.debounceScheduler = debounceScheduler
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
                        self.session += 1
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
            // Observe local edits BEFORE the login reconcile: a pick or hide
            // made while its fetch is in flight must already be pending, or
            // the account's older state would overwrite it.
            self.observeLocalChanges()
            await self.reconcileLanguage()
            await self.reconcile(country: self.prefs.selectedCountry.code)
            self.observeCountryChanges()
        }
    }

    private func cancelSync() {
        syncTask?.cancel()
        syncTask = nil
        pendingDebounce = nil
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
        // Unsent local edits first: until the server has them, its set is
        // older than local and must not replace it.
        guard await sendPendingChanges(country: country) else { return }
        let localBeforeFetch = prefs.hiddenFilms(country: country)
        do {
            if prefs.isHiddenFilmsMigrated(country: country) {
                let (etag, lastModified) = prefs.hiddenFilmsValidators(country: country)
                if case .current(let remote) = try await client.fetch(country: country, etag: etag, lastModified: lastModified) {
                    guard !editedDuringFetch(country: country, localBeforeFetch: localBeforeFetch) else { return }
                    prefs.setHiddenFilms(remote.hiddenFilms, country: country)
                    prefs.setHiddenFilmsValidators(country: country, etag: remote.etag, lastModified: remote.lastModified)
                }
            } else {
                // No usable validators, so this is always a fresh 200.
                guard case .current(let remote) = try await client.fetch(country: country, etag: nil, lastModified: nil),
                      !editedDuringFetch(country: country, localBeforeFetch: localBeforeFetch) else { return }
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

    /// Whether the user edited `country`'s set while its fetch was on the wire
    /// (the local set moved, or an edit is still queued). The response then
    /// predates that edit, so applying it would drop the edit locally; it is
    /// ignored instead — the edit's own write brings the server level, and
    /// the next reconcile (not yet migrated: the union) runs against both.
    private func editedDuringFetch(country: String, localBeforeFetch: Set<String>) -> Bool {
        prefs.hiddenFilms(country: country) != localBeforeFetch || !prefs.pendingHiddenFilmsChanges(country: country).isEmpty
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
    /// the push debounce), while the fetch was in flight, or its push failed —
    /// and a pick SENT while the fetch was in flight makes its answer stale,
    /// so it is dropped.
    private func reconcileLanguage() async {
        guard isLoggedIn else { return }
        if pendingLanguage != nil { return await sendPendingLanguage() }
        let confirmedBeforeFetch = languagePushesConfirmed
        do {
            let remoteLanguage = try await languageClient.fetch()
            if pendingLanguage != nil { return await sendPendingLanguage() }
            // A pick sent while the fetch was on the wire is newer than its answer.
            guard languagePushesConfirmed == confirmedBeforeFetch else { return }
            if let remoteLanguage {
                adopt(remoteLanguage)
            } else if let explicit = prefs.explicitLanguage {
                pendingLanguage = explicit
                await sendPendingLanguage()
            }
        } catch {
            // Network error — local state is authoritative; leave prefs alone.
        }
    }

    /// Send the pending pick now, superseding any debounce, ONE push at a
    /// time: a caller arriving while another push is on the wire waits for
    /// it, and that push's loop then sends whatever is pending by then — so
    /// two PUTs never race and the latest pick is the one the account ends
    /// on. On success the account holds the sent value, and a pick made
    /// meanwhile is sent next; on a transient failure the pick stays pending,
    /// and the next reconcile (resume, country switch, next login) retries it —
    /// the same self-heal the hiddenFilms writes rely on; a permanent refusal
    /// (`LanguagePushRefused`) drops it and takes the account's pick instead.
    private func sendPendingLanguage() async {
        pendingDebounce = nil
        // Wait out the send on the wire, then send what is still pending: that
        // send's loop picks up a pick made meanwhile only when it succeeds — a
        // failure was the older pick's, not this one's. Android's waiters do the
        // same through `languageSendMutex`.
        while let running = languageSendTask { await running.value }
        guard isLoggedIn, pendingLanguage != nil else { return }
        let task = Task { @MainActor [weak self] in
            while let self, self.isLoggedIn, let pending = self.pendingLanguage {
                let picksBeforeSend = self.languagePicks
                do {
                    try await self.languageClient.push(pending)
                } catch is LanguagePushRefused {
                    // Refused for good: it can never land, so stop owing it, and
                    // take the account's pick instead — never pushing this one back.
                    self.accountLanguage = nil
                    if self.pendingLanguage == pending {
                        self.pendingLanguage = nil
                        if let remote = try? await self.languageClient.fetch() { self.adopt(remote) }
                    }
                    break
                } catch {
                    // It may or may not have landed: the account's value is
                    // unknown until the next fetch, so no pick is a no-op.
                    self.accountLanguage = nil
                    break
                }
                self.accountLanguage = pending
                self.languagePushesConfirmed += 1
                // A pick made meanwhile is newer than whatever else reached the
                // account during this push, even when it is the value just sent.
                if self.languagePicks == picksBeforeSend, self.pendingLanguage == pending { self.pendingLanguage = nil; break }
            }
            self?.languageSendTask = nil
        }
        languageSendTask = task
        await task.value
    }

    /// Take the account's pick as this device's.
    private func adopt(_ remoteLanguage: String) {
        accountLanguage = remoteLanguage
        guard remoteLanguage != prefs.selectedLanguage else { return }
        // Not a pick: `languageChanged` hears it synchronously and must not
        // queue it — even from inside a send, where a pick back to the
        // account's value is otherwise kept pending.
        adopting = true
        defer { adopting = false }
        prefs.setLanguage(remoteLanguage)
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

    /// Queue the edit (persisted — see `UserPreferences.pendingHiddenFilmsChanges`)
    /// and send the queue. One that fails stays queued; the next reconcile
    /// re-sends it before fetching.
    private func push(_ change: HiddenFilmsChange) {
        guard isLoggedIn else { return }
        let country = prefs.selectedCountry.code
        prefs.setPendingHiddenFilmsChanges(prefs.pendingHiddenFilmsChanges(country: country) + [change], country: country)
        Task { @MainActor [weak self] in _ = await self?.sendPendingChanges(country: country) }
    }

    /// Send `country`'s queued edits in order, one flush at a time (a flush
    /// already running is awaited first, so none is sent twice). Returns
    /// whether the queue is now empty. After each accepted edit the
    /// response's validators are kept only when its set is exactly the local
    /// bucket — otherwise (another device changed the set, or more edits are
    /// still queued) they would vouch for a set this device doesn't hold, and
    /// are dropped so the next fetch is unconditional.
    private func sendPendingChanges(country: String) async -> Bool {
        let previous = flushTask
        let task = Task { @MainActor [weak self] () -> Bool in
            _ = await previous?.value
            guard let self else { return false }
            let startedIn = self.session
            while let change = self.prefs.pendingHiddenFilmsChanges(country: country).first {
                guard self.isLoggedIn else { return false }
                let result: HiddenFilmsResult
                do {
                    switch change {
                    case .hidden(let title):   result = try await self.client.hide(country: country, title: title)
                    case .unhidden(let title): result = try await self.client.unhide(country: country, title: title)
                    case .clearedAll:          result = try await self.client.clear(country: country)
                    }
                } catch {
                    return false
                }
                // A logout while it was on the wire forgot the queue it came from.
                guard self.session == startedIn else { return false }
                let remaining = Array(self.prefs.pendingHiddenFilmsChanges(country: country).dropFirst())
                self.prefs.setPendingHiddenFilmsChanges(remaining, country: country)
                if remaining.isEmpty, result.hiddenFilms == self.prefs.hiddenFilms(country: country) {
                    self.prefs.setHiddenFilmsValidators(country: country, etag: result.etag, lastModified: result.lastModified)
                } else {
                    self.prefs.clearHiddenFilmsValidators(country: country)
                }
            }
            return true
        }
        flushTask = task
        return await task.value
    }

    private func languageChanged(to language: String) {
        guard !adopting else { return }
        languagePicks += 1
        // A push on the wire may already have reached the server, so a pick
        // back to `accountLanguage` must be sent after it too.
        guard language != accountLanguage || languageSendTask != nil else {
            // Back on (or adopted) what the account already holds — nothing to push.
            pendingLanguage = nil
            pendingDebounce = nil
            return
        }
        pendingLanguage = language
        schedulePush()
    }

    /// Debounced push of `pendingLanguage` — 400ms, long enough that a rapid
    /// run of picker taps folds into one PUT.
    private func schedulePush() {
        pendingDebounce = debounceScheduler.schedule(after: 0.4) { [weak self] in
            Task { @MainActor [weak self] in
                guard let self, self.isLoggedIn else { return }
                await self.sendPendingLanguage()
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

/// The clock behind the language push's debounce: runs `action` on the main
/// actor after `delay` seconds, unless the returned token is cancelled or
/// released first. Injected so a test can fire it without waiting.
protocol DebounceScheduler {
    @MainActor func schedule(after delay: TimeInterval, _ action: @escaping @MainActor () -> Void) -> AnyCancellable
}

/// The production clock — the main queue's.
struct MainQueueDebounceScheduler: DebounceScheduler {
    @MainActor func schedule(after delay: TimeInterval, _ action: @escaping @MainActor () -> Void) -> AnyCancellable {
        let item = DispatchWorkItem { MainActor.assumeIsolated { action() } }
        DispatchQueue.main.asyncAfter(deadline: .now() + delay, execute: item)
        return AnyCancellable { item.cancel() }
    }
}
