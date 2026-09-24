import Foundation
@testable import KinowoAuth

// In-memory stand-ins for StateSyncService's collaborators, shared by its
// example-based tests and its model-based one. The same shapes as Android's
// `StateSyncFakes.kt`.

/// The per-country hidden-films endpoints as the server behaves: each bucket
/// is a set, every write applies to it and answers with the resulting set and
/// a validator derived from its content, a conditional fetch naming the
/// current validator is a 304, a signed-out session is refused, and an offline
/// one fails. Tests change the account by editing `remote` — as another device
/// would — never by scripting responses.
@MainActor
final class FakeHiddenFilmsClient: HiddenFilmsClient {
    var remote: [String: Set<String>] = [:]
    var shouldFailFetch = false
    /// Fail every hide/unhide/clear AFTER recording the call.
    var shouldFailWrite = false
    /// Whether the session this client sends is signed in; a signed-out one is
    /// refused, as the server answers it 401.
    var signedIn = true
    /// Hold a fetch this long AFTER it read the server's set, per country —
    /// its response is on the wire while the set may change underneath it.
    var fetchDelay: [String: Duration] = [:]
    /// Awaited after a fetch has read the server's set, before it answers.
    var beforeFetchResponse: (() async -> Void)?
    /// Awaited once a hide/unhide/clear has been APPLIED, before it answers.
    var beforeWriteResponse: (() async -> Void)?

    private(set) var fetchedCountries: [String] = []
    private(set) var hideCalls: [(country: String, title: String)] = []
    private(set) var unhideCalls: [(country: String, title: String)] = []
    private(set) var clearCalls: [String] = []
    /// Requests that have started and not yet answered.
    private(set) var inFlight = 0

    var onHide: (() -> Void)?
    var onUnhide: (() -> Void)?
    var onClear: (() -> Void)?

    /// The validator the server hands out for `country`'s current set:
    /// derived from the content, as `UserStateController`'s strong ETag is.
    func etagOf(_ country: String) -> String { "\"etag-\(country)-\((remote[country] ?? []).sorted().joined(separator: "|"))\"" }

    private func state(_ country: String) -> HiddenFilmsResult {
        HiddenFilmsResult(hiddenFilms: remote[country] ?? [], etag: etagOf(country), lastModified: "lm-\(country)")
    }

    func fetch(country: String, etag: String?, lastModified: String?) async throws -> HiddenFilmsFetchResult {
        fetchedCountries.append(country)
        inFlight += 1
        defer { inFlight -= 1 }
        await Task.yield()
        if shouldFailFetch { throw URLError(.notConnectedToInternet) }
        if !signedIn { throw URLError(.userAuthenticationRequired) }
        let answer: HiddenFilmsFetchResult = etag != nil && etag == etagOf(country) ? .notModified : .current(state(country))
        if let delay = fetchDelay[country] { try await Task.sleep(for: delay) }
        if let beforeFetchResponse { await beforeFetchResponse() }
        return answer
    }

    func hide(country: String, title: String) async throws -> HiddenFilmsResult {
        hideCalls.append((country, title))
        defer { onHide?() }
        return try await write(country) { $0.insert(title) }
    }

    func unhide(country: String, title: String) async throws -> HiddenFilmsResult {
        unhideCalls.append((country, title))
        defer { onUnhide?() }
        return try await write(country) { $0.remove(title) }
    }

    func clear(country: String) async throws -> HiddenFilmsResult {
        clearCalls.append(country)
        defer { onClear?() }
        return try await write(country) { $0.removeAll() }
    }

    /// One write: in flight across a suspension point, like a real request.
    private func write(_ country: String, _ change: (inout Set<String>) -> Void) async throws -> HiddenFilmsResult {
        inFlight += 1
        defer { inFlight -= 1 }
        await Task.yield()
        if shouldFailWrite { throw URLError(.notConnectedToInternet) }
        if !signedIn { throw URLError(.userAuthenticationRequired) }
        change(&remote[country, default: []])
        let answer = state(country)
        if let beforeWriteResponse { await beforeWriteResponse() }
        return answer
    }
}

@MainActor
final class FakeLanguageClient: LanguageClient {
    /// The account's stored pick — a successful push updates it, as the
    /// server does.
    var remote: String?
    var shouldFailPush = false
    /// Refuse every push for good, as the server answers a language it does
    /// not know (400).
    var refusePush = false
    var shouldFailFetch = false
    /// Whether the session is signed in; a signed-out one is refused, as the
    /// server answers it 401.
    var signedIn = true
    /// Calls that have started and not yet answered.
    private(set) var inFlight = 0
    /// Every push that SUCCEEDED, in order.
    private(set) var pushes: [String] = []
    var lastPushed: String? { pushes.last }
    /// Called on every push attempt, failed or not.
    var onPush: ((String) -> Void)?
    /// Awaited at the start of a push — holds it "in flight".
    var beforePush: (() async -> Void)?
    /// Awaited at the start of a fetch — holds it "in flight".
    var beforeFetch: (() async -> Void)?
    /// Awaited once a push has REACHED the server (the account holds its
    /// value), before the response — throwing from it models a response lost
    /// after the server applied the push.
    var beforePushResponse: (() async throws -> Void)?
    private(set) var pushesStarted = 0
    private var pushesInFlight = 0
    /// The most pushes ever on the wire at once.
    private(set) var maxPushesInFlight = 0
    private(set) var fetchesStarted = 0

    func fetch() async throws -> String? {
        fetchesStarted += 1
        inFlight += 1
        defer { inFlight -= 1 }
        if let beforeFetch { await beforeFetch() }
        await Task.yield()
        if shouldFailFetch { throw URLError(.notConnectedToInternet) }
        if !signedIn { throw URLError(.userAuthenticationRequired) }
        return remote
    }

    func push(_ language: String) async throws {
        pushesStarted += 1
        inFlight += 1
        pushesInFlight += 1
        maxPushesInFlight = max(maxPushesInFlight, pushesInFlight)
        defer { inFlight -= 1; pushesInFlight -= 1 }
        if let beforePush { await beforePush() }
        defer { onPush?(language) }
        if shouldFailPush { throw URLError(.badServerResponse) }
        if refusePush { throw LanguagePushRefused(statusCode: 400) }
        if !signedIn { throw URLError(.userAuthenticationRequired) }
        remote = language
        if let beforePushResponse { try await beforePushResponse() }
        pushes.append(language)
    }
}

/// A one-shot latch: `wait()` suspends until `open()`.
actor AsyncGate {
    private var isOpen = false
    private var waiters: [CheckedContinuation<Void, Never>] = []

    func wait() async {
        if isOpen { return }
        await withCheckedContinuation { waiters.append($0) }
    }

    func open() {
        isOpen = true
        waiters.forEach { $0.resume() }
        waiters.removeAll()
    }
}
