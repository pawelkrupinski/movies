import Foundation
import Network
import os

/// A minimal HTTP server the UI-test runner hosts on the simulator's loopback, so a suite can
/// drive the app's REAL network path (URLSession, conditional fetch, JSON decode, pruning)
/// without production: launch the app with `KINOWO_UITEST_SERVER = server.baseURL` and every
/// deployment's base URL becomes this server (`Country.uiTestServer`).
///
/// It answers `GET <path>` from `routes`; anything else is a 404, which the app treats as it
/// treats an unreachable optional endpoint (the catalog and client-support checks keep what they
/// have). One request per connection (`Connection: close`), so there is no keep-alive state.
/// A `.png` path is served as `image/png` (a poster), anything else as JSON.
final class FixtureServer: @unchecked Sendable {
    let baseURL: URL
    /// The paths the app asked for, in order -- so a test can tell "the app never fetched" from
    /// "the app fetched and did not show it".
    var requestedPaths: [String] { requested.withLock { $0 } }

    private let listener: NWListener
    private let routes: OSAllocatedUnfairLock<[String: Data]>
    private let requested = OSAllocatedUnfairLock<[String]>(initialState: [])
    private let queue = DispatchQueue(label: "FixtureServer")

    /// Starts listening on an ephemeral loopback port and returns once it is ready.
    init(routes: [String: Data] = [:]) throws {
        let routes = OSAllocatedUnfairLock(initialState: routes)
        self.routes = routes
        let parameters = NWParameters.tcp
        parameters.requiredLocalEndpoint = .hostPort(host: .ipv4(.loopback), port: .any)
        listener = try NWListener(using: parameters)
        let ready = DispatchSemaphore(value: 0)
        let failure = OSAllocatedUnfairLock<NWError?>(initialState: nil)
        listener.stateUpdateHandler = { state in
            switch state {
            case .ready: ready.signal()
            case .failed(let error): failure.withLock { $0 = error }; ready.signal()
            default: break
            }
        }
        listener.newConnectionHandler = { [routes, requested, queue] connection in
            Self.serve(connection, routes: routes, requested: requested, queue: queue)
        }
        listener.start(queue: queue)
        guard ready.wait(timeout: .now() + 10) == .success else { throw ServerError.notReady }
        if let error = failure.withLock({ $0 }) { throw error }
        guard let port = listener.port?.rawValue else { throw ServerError.notReady }
        baseURL = URL(string: "http://127.0.0.1:\(port)")!
    }

    deinit { listener.cancel() }

    /// Adds (or replaces) routes once the server listens -- for a body that names the server's own
    /// `baseURL`, such as a listing whose poster links point back here.
    func addRoutes(_ more: [String: Data]) {
        routes.withLock { $0.merge(more) { _, new in new } }
    }

    enum ServerError: Error { case notReady }

    private static func serve(_ connection: NWConnection, routes: OSAllocatedUnfairLock<[String: Data]>,
                              requested: OSAllocatedUnfairLock<[String]>, queue: DispatchQueue) {
        connection.start(queue: queue)
        receiveHead(connection, buffer: Data()) { head in
            let path = head.flatMap(requestPath) ?? ""
            requested.withLock { $0.append(path) }
            let body = routes.withLock { $0[path] }
            let type = path.hasSuffix(".png") ? "image/png" : "application/json; charset=utf-8"
            var response = Data("HTTP/1.1 \(body == nil ? "404 Not Found" : "200 OK")\r\n".utf8)
            response += Data("Content-Type: \(type)\r\n".utf8)
            response += Data("Content-Length: \(body?.count ?? 0)\r\nConnection: close\r\n\r\n".utf8)
            response += body ?? Data()
            connection.send(content: response, completion: .contentProcessed { _ in connection.cancel() })
        }
    }

    /// Reads until the end of the request head (a GET carries no body).
    private static func receiveHead(_ connection: NWConnection, buffer: Data, _ done: @escaping @Sendable (String?) -> Void) {
        connection.receive(minimumIncompleteLength: 1, maximumLength: 64 * 1024) { data, _, isComplete, error in
            let buffer = buffer + (data ?? Data())
            if let end = buffer.range(of: Data("\r\n\r\n".utf8)) {
                done(String(data: buffer[..<end.lowerBound], encoding: .utf8))
            } else if isComplete || error != nil || buffer.count > 64 * 1024 {
                done(nil)
            } else {
                receiveHead(connection, buffer: buffer, done)
            }
        }
    }

    /// `GET /warszawa/api/repertoire?x=1 HTTP/1.1` -> `/warszawa/api/repertoire`.
    private static func requestPath(_ head: String) -> String? {
        let target = head.split(separator: "\r\n").first?.split(separator: " ").dropFirst().first
        return target.map { String($0.split(separator: "?", maxSplits: 1).first ?? $0) }
    }
}

/// A captured `/warszawa/api/repertoire` listing (`repertoire-warszawa.json`, six real films, a
/// week of showings each) with its days written as `DAY+n` tokens, dated here relative to
/// `today` in the city's zone: a listing captured on a fixed date would be all past showings,
/// pruned to an empty grid, a week later. `today` is a parameter so the dating is the caller's
/// choice of clock, and the test can pin it.
enum RelativeListing {
    static func dated(_ template: Data, today: Date, zone: TimeZone = TimeZone(identifier: "Europe/Warsaw")!) -> Data {
        var calendar = Calendar(identifier: .gregorian)
        calendar.timeZone = zone
        let iso = DateFormatter()
        iso.calendar = calendar
        iso.timeZone = zone
        iso.locale = Locale(identifier: "en_US_POSIX")
        iso.dateFormat = "yyyy-MM-dd"
        let label = DateFormatter()
        label.calendar = calendar
        label.timeZone = zone
        label.locale = Locale(identifier: "pl_PL")
        label.dateFormat = "EEEE d MMMM"
        var text = String(decoding: template, as: UTF8.self)
        for offset in (0...9).reversed() {
            guard let day = calendar.date(byAdding: .day, value: offset, to: today) else { continue }
            let name = label.string(from: day)
            text = text
                .replacingOccurrences(of: "\"date\": \"DAY+\(offset)\"", with: "\"date\": \"\(iso.string(from: day))\"")
                .replacingOccurrences(of: "\"label\": \"DAY+\(offset)\"",
                                      with: "\"label\": \"\(name.prefix(1).uppercased() + name.dropFirst())\"")
        }
        return Data(text.utf8)
    }
}
