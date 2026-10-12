import Foundation
import Combine
// See `RepertoireClient.swift`: a no-op in the Xcode app's flat module, true
// only in the `KinowoNetworking` SPM target that lets `swift test` drive this.
#if canImport(KinowoCore)
@testable import KinowoCore
#endif
#if canImport(KinowoAuth)
@testable import KinowoAuth
#endif

/// The minimum-supported-version gate: on launch, asks the deployment's
/// `/api/client-support` whether this build is still served, and publishes the
/// App Store URL to send the user to when it isn't.
///
/// Fails OPEN: offline, a non-2xx, or a body that doesn't decode all leave
/// `requiredUpdate` nil — an outage of ours must never lock anyone out of the
/// app. Nothing is persisted; the answer is a few hundred bytes and asked
/// once per launch, so an offline launch simply isn't gated.
@MainActor
final class ClientSupportStore: ObservableObject {
    /// Non-nil when this build is below the server's minimum: where to update.
    @Published private(set) var requiredUpdate: URL?

    private let session: URLSession
    private let appVersion: String

    init(session: URLSession = .shared,
         appVersion: String = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "") {
        self.session = session
        self.appVersion = appVersion
    }

    func check(baseURL: URL) async {
        var request = URLRequest(url: baseURL.appendingPathComponent("api").appendingPathComponent("client-support"))
        request.setValue(KinowoUserAgent.value, forHTTPHeaderField: "User-Agent")
        guard let (data, response) = try? await session.data(for: request),
              let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode),
              let support = ClientSupport.decode(data)
        else { return }
        requiredUpdate = support.ios.updateURL(forVersion: appVersion)
    }
}
