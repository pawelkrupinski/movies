import XCTest
import Combine
@testable import KinowoAuth

/// Model-based test of `StateSyncService`: seeded random sequences of what a
/// user, the network and the account's other devices do, checked against the
/// sync's invariants. The same alphabet and invariants run against Android
/// (`StateSyncModelTest`) and the web (`HiddenFilmsSyncModelSpec`).
///
/// THE ALPHABET: switch country, hide, unhide, clear, login, logout, resume (a
/// reconcile — the server answers 304 or 200 by its own content validator),
/// another device hiding / unhiding a title, the network going down, the
/// network coming back (reconnect + resume), a local language pick, another
/// device's language pick.
///
/// THE INVARIANTS:
///  1. No title crosses countries: a title hidden in one country never reaches
///     another country's server bucket (checked after EVERY event) or local list.
///  2. Convergence: once the network is up, signed in and every country has
///     been reconciled, each country's local list IS its server list, and the
///     local language IS the account's.
///  3. No op lost: an edit made while signed in reaches the server unless
///     something later legitimately overrides it. Edits still queued at a
///     logout are forgotten with the session, so their titles are unconstrained;
///     the same holds for the last language pick made while signed in.
///
/// A failure prints the seed, the minimised sequence and the violation; paste
/// the sequence into a regression test beside `testALanguagePickQueuedAtLogout`.
@MainActor
final class StateSyncModelTests: XCTestCase {

    /// How many seeds to run: `SYNC_MODEL_SEEDS` (the nightly run sets
    /// thousands), else a push-sized default. The same knob as Android's
    /// `-PsyncModelSeeds`.
    private static let seeds: ClosedRange<UInt64> =
        1...(ProcessInfo.processInfo.environment["SYNC_MODEL_SEEDS"].flatMap(UInt64.init) ?? 200)

    func testRandomSequencesKeepTheSyncInvariants() async throws {
        for seed in Self.seeds {
            let events = SyncModel.generate(seed: seed)
            guard let violation = await SyncModel.violation(of: events) else { continue }
            let minimal = await SyncModel.minimise(events)
            let minimalViolation = await SyncModel.violation(of: minimal) ?? "?"
            XCTFail("""
                seed=\(seed) violates the sync model: \(minimalViolation)
                minimised sequence (\(minimal.count) of \(events.count) events):
                  \(minimal.map { "\($0)" }.joined(separator: ",\n  "))
                full-sequence violation: \(violation)
                """)
            return
        }
    }

    // Pinned shapes of the historical bugs, so they run whatever the seeds
    // generate: a write that failed offline (the pending-changes queue,
    // ec3af46b0), and a language pick still unsent at a logout.
    func testAFailedUnhideIsResentAfterReconnect() async {
        let violation = await SyncModel.violation(of: [.login, .hide, .networkDown, .unhide(0), .reconnect])
        XCTAssertNil(violation)
    }

    func testALanguagePickQueuedAtLogout() async {
        let violation = await SyncModel.violation(of: [.login, .networkDown, .pickLanguage("de"), .logout, .reconnect])
        XCTAssertNil(violation)
    }
}

/// One step of a generated sequence. Picks are indices resolved against the
/// state when the event runs, so a shrunk sequence stays runnable.
enum SyncEvent: CustomStringConvertible, Equatable {
    case switchCountry(String), hide, unhide(Int), clear, login, logout, resume
    case remoteHide(String), remoteUnhide(String, Int), networkDown, reconnect
    case pickLanguage(String), remoteLanguage(String)

    var description: String {
        switch self {
        case .switchCountry(let c):      return "SwitchCountry(\(c))"
        case .hide:                      return "Hide"
        case .unhide(let p):             return "Unhide(\(p))"
        case .clear:                     return "Clear"
        case .login:                     return "Login"
        case .logout:                    return "Logout"
        case .resume:                    return "Resume"
        case .remoteHide(let c):         return "RemoteHide(\(c))"
        case .remoteUnhide(let c, let p): return "RemoteUnhide(\(c), \(p))"
        case .networkDown:               return "NetworkDown"
        case .reconnect:                 return "Reconnect"
        case .pickLanguage(let l):       return "PickLanguage(\(l))"
        case .remoteLanguage(let l):     return "RemoteLanguage(\(l))"
        }
    }
}

@MainActor
enum SyncModel {
    /// Two countries that share one web origin in production (showtimes.cc).
    static let countries = ["uk", "de"]
    private static let languages = ["en", "de", "pl", "es"]
    private static let length = 30

    /// SplitMix64 — a seeded generator whose sequence is the same on every run.
    struct Random {
        var state: UInt64
        mutating func next(_ bound: Int) -> Int {
            state &+= 0x9E3779B97F4A7C15
            var z = state
            z = (z ^ (z >> 30)) &* 0xBF58476D1CE4E5B9
            z = (z ^ (z >> 27)) &* 0x94D049BB133111EB
            return Int((z ^ (z >> 31)) % UInt64(bound))
        }
    }

    static func generate(seed: UInt64) -> [SyncEvent] {
        var random = Random(state: seed)
        return (0..<length).map { _ in
            let country = countries[random.next(countries.count)]
            let pick = random.next(8)
            let language = languages[random.next(languages.count)]
            switch random.next(100) {
            case 0...19:  return .hide
            case 20...31: return .unhide(pick)
            case 32...35: return .clear
            case 36...45: return .switchCountry(country)
            case 46...52: return .login
            case 53...57: return .logout
            case 58...65: return .resume
            case 66...70: return .remoteHide(country)
            case 71...73: return .remoteUnhide(country, pick)
            case 74...80: return .networkDown
            case 81...88: return .reconnect
            case 89...95: return .pickLanguage(language)
            default:      return .remoteLanguage(language)
            }
        }
    }

    /// Greedily drop events while the sequence still violates the model.
    static func minimise(_ events: [SyncEvent]) async -> [SyncEvent] {
        var current = events
        var shrunk = true
        while shrunk {
            shrunk = false
            for i in current.indices.reversed() {
                var candidate = current
                candidate.remove(at: i)
                if await violation(of: candidate) != nil { current = candidate; shrunk = true; break }
            }
        }
        return current
    }

    /// Run `events` and then settle; the first invariant broken, or nil.
    static func violation(of events: [SyncEvent]) async -> String? {
        let run = Run()
        defer { run.tearDown() }
        return await run.play(events)
    }

    @MainActor
    private final class Run {
        private let suite = "StateSyncModel-\(UUID().uuidString)"
        private let defaults: UserDefaults
        private let prefs: UserPreferences
        private let server = FakeHiddenFilmsClient()
        private let languageClient = FakeLanguageClient()
        private let debounce = ManualDebounceScheduler()
        private let user = CurrentValueSubject<UserProfile?, Never>(nil)
        private var service: StateSyncService?

        private var signedIn = false
        private var minted = 0
        private var mustHave: [String: Set<String>] = [:]
        private var mustNotHave: [String: Set<String>] = [:]
        private var expectedLanguage: String?

        init() {
            defaults = UserDefaults(suiteName: suite)!
            prefs = UserPreferences(store: defaults)
            prefs.setCountry(Country.all.first { $0.code == SyncModel.countries[0] }!)
            session(signedIn: false)
        }

        func tearDown() { defaults.removePersistentDomain(forName: suite) }

        private var country: String { prefs.selectedCountry.code }

        func play(_ events: [SyncEvent]) async -> String? {
            service = StateSyncService(prefs: prefs, userPublisher: user.eraseToAnyPublisher(),
                                       client: server, languageClient: languageClient, debounceScheduler: debounce)
            for (index, event) in events.enumerated() {
                await apply(event)
                await quiesce()
                if let violation = isolationViolation() { return "after event #\(index) \(event): \(violation)" }
            }
            await settle()
            return isolationViolation() ?? convergenceViolation()
        }

        private func apply(_ event: SyncEvent) async {
            switch event {
            case .switchCountry(let code):
                prefs.setCountry(Country.all.first { $0.code == code }!)
            case .hide:
                minted += 1
                let title = "\(country)-\(minted)"
                prefs.hide(title)
                if signedIn { expect(country, title, hidden: true) }
            case .unhide(let pick):
                let local = prefs.hiddenFilms(country: country).sorted()
                guard !local.isEmpty else { return }
                let title = local[pick % local.count]
                prefs.unhide(title)
                if signedIn { expect(country, title, hidden: false) }
            case .clear:
                let local = prefs.hiddenFilms(country: country)
                prefs.unhideAll()
                if signedIn {
                    mustNotHave[country, default: []].formUnion(local.union(mustHave[country] ?? []))
                    mustHave[country] = []
                }
            case .login:
                session(signedIn: true)
                user.send(UserProfile(displayName: "Model", email: "model@example.com", avatarUrl: nil, provider: "google"))
                signedIn = true
            case .logout:
                // What the account is still owed is forgotten with the session.
                if signedIn {
                    for c in SyncModel.countries {
                        for change in prefs.pendingHiddenFilmsChanges(country: c) {
                            switch change {
                            case .hidden(let t), .unhidden(let t): unconstrain(c, t)
                            case .clearedAll: mustHave[c] = []; mustNotHave[c] = []
                            }
                        }
                    }
                    if prefs.pendingLanguagePush != nil { expectedLanguage = nil }
                }
                session(signedIn: false)
                user.send(nil)
                signedIn = false
            case .resume:
                await service?.reconcileCurrentCountry()
            case .remoteHide(let c):
                minted += 1
                let title = "\(c)-r\(minted)"
                server.remote[c, default: []].insert(title)
                // A clear this device still owes the account lands after it.
                if !prefs.pendingHiddenFilmsChanges(country: c).contains(.clearedAll) { expect(c, title, hidden: true) }
            case .remoteUnhide(let c, let pick):
                let remote = (server.remote[c] ?? []).sorted()
                guard !remote.isEmpty else { return }
                let title = remote[pick % remote.count]
                server.remote[c]?.remove(title)
                // A first sync's union may legitimately bring it back.
                unconstrain(c, title)
            case .networkDown:
                network(up: false)
            case .reconnect:
                network(up: true)
                await service?.reconcileCurrentCountry()
            case .pickLanguage(let language):
                // Picking the language already on screen changes nothing.
                guard prefs.selectedLanguage != language else { return }
                prefs.setLanguage(language)
                // Signed out, the account's own pick wins at the next login.
                expectedLanguage = signedIn ? language : nil
            case .remoteLanguage(let language):
                languageClient.remote = language
                // A pick this device still owes the account is newer.
                expectedLanguage = prefs.pendingLanguagePush != nil ? nil : language
            }
        }

        /// Let every Task, main-queue hop and in-flight fake call the event
        /// started run out, firing the language debounce whenever one is
        /// armed. No wall clock: everything here runs on the main actor, whose
        /// queue is FIFO, so yielding until nothing is in flight for a good
        /// run of turns is both deterministic and fast.
        private func quiesce() async {
            var idleTurns = 0
            var turns = 0
            while idleTurns < 20, turns < 100_000 {
                turns += 1
                await Task.yield()
                if debounce.hasPending {
                    debounce.fireAll()
                    idleTurns = 0
                } else {
                    idleTurns = (server.inFlight == 0 && languageClient.inFlight == 0) ? idleTurns + 1 : 0
                }
            }
        }

        /// What the server makes of this device's session cookie.
        private func session(signedIn: Bool) {
            server.signedIn = signedIn
            languageClient.signedIn = signedIn
        }

        private func network(up: Bool) {
            server.shouldFailFetch = !up
            server.shouldFailWrite = !up
            languageClient.shouldFailFetch = !up
            languageClient.shouldFailPush = !up
        }

        private func expect(_ c: String, _ title: String, hidden: Bool) {
            if hidden { mustHave[c, default: []].insert(title); mustNotHave[c]?.remove(title) }
            else { mustNotHave[c, default: []].insert(title); mustHave[c]?.remove(title) }
        }

        private func unconstrain(_ c: String, _ title: String) {
            mustHave[c]?.remove(title)
            mustNotHave[c]?.remove(title)
        }

        /// Network up, signed in, every country reconciled — twice, so a first
        /// sync's union pushes have landed before anything is compared.
        private func settle() async {
            network(up: true)
            if !signedIn { await apply(.login) }
            await quiesce()
            for _ in 0..<2 {
                for c in SyncModel.countries {
                    await apply(.switchCountry(c))
                    await quiesce()
                    await apply(.resume)
                    await quiesce()
                }
            }
        }

        private func isolationViolation() -> String? {
            for c in SyncModel.countries {
                let foreignRemote = (server.remote[c] ?? []).filter { !$0.hasPrefix("\(c)-") }
                if !foreignRemote.isEmpty { return "server bucket '\(c)' holds another country's titles \(foreignRemote.sorted())" }
                let foreignLocal = prefs.hiddenFilms(country: c).filter { !$0.hasPrefix("\(c)-") }
                if !foreignLocal.isEmpty { return "local list '\(c)' holds another country's titles \(foreignLocal.sorted())" }
            }
            return nil
        }

        private func convergenceViolation() -> String? {
            for c in SyncModel.countries {
                let local = prefs.hiddenFilms(country: c)
                let remote = server.remote[c] ?? []
                if local != remote { return "after settling, '\(c)' local \(local.sorted()) != server \(remote.sorted())" }
                let lost = (mustHave[c] ?? []).subtracting(remote)
                if !lost.isEmpty { return "after settling, '\(c)' lost hides \(lost.sorted()) (server \(remote.sorted()))" }
                let resurrected = (mustNotHave[c] ?? []).intersection(remote)
                if !resurrected.isEmpty { return "after settling, '\(c)' resurrected unhidden \(resurrected.sorted())" }
            }
            if let remote = languageClient.remote, prefs.selectedLanguage != remote {
                return "after settling, local language \(prefs.selectedLanguage) != account's \(remote)"
            }
            if let expectedLanguage, languageClient.remote != expectedLanguage {
                return "after settling, the account's language is \(languageClient.remote ?? "nil"), expected the last pick \(expectedLanguage)"
            }
            return nil
        }
    }
}
