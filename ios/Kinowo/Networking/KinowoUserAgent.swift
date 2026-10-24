import Foundation

/// The User-Agent every request from the iOS app carries:
/// `KinowoIOS/<marketing version> (iOS <os version>)`, e.g. `KinowoIOS/2.1.3 (iOS 18.2)`.
/// The server counts a call as the app's only by its `KinowoIOS/` prefix
/// (`WebRouteUsageMetrics.clientKind`), which is how the per-client route-usage table tells an
/// endpoint the app still calls from one only browsers or bots do; the version and OS behind it
/// say which builds and iOS releases still call it.
enum KinowoUserAgent {
    static let value = value(
        appVersion: Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String,
        osVersion: ProcessInfo.processInfo.operatingSystemVersion)

    /// The header for `appVersion` on `osVersion`: the OS as iOS writes it (`18.2`, `18.2.1` — a
    /// zero patch left off), and `0` for a bundle without a marketing version (a test runner).
    static func value(appVersion: String?, osVersion: OperatingSystemVersion) -> String {
        var os = "\(osVersion.majorVersion).\(osVersion.minorVersion)"
        if osVersion.patchVersion != 0 { os += ".\(osVersion.patchVersion)" }
        let version = appVersion.flatMap { $0.isEmpty ? nil : $0 } ?? "0"
        return "KinowoIOS/\(version) (iOS \(os))"
    }
}
