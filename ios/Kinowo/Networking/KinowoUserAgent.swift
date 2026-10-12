import Foundation

/// The User-Agent every request from the iOS app carries. The server counts a call as the app's
/// only by this prefix (`WebRouteUsageMetrics.clientKind`), which is how the per-client route-usage
/// table tells an endpoint the app still calls from one only browsers or bots do.
enum KinowoUserAgent {
    static let value = "KinowoIOS/1.0"
}
