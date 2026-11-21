import Foundation
#if canImport(KinowoCore)
@testable import KinowoCore
#endif

/// Sends `DELETE /api/me` and says whether the account is gone. Outside the app-only `AuthService`
/// (which needs AuthenticationServices) so `swift test` can drive it.
enum AccountDeletion {
    /// True only for a 2xx. The server answers 503 when its user store is unreachable and the
    /// account was not deleted (the session is kept); that, any other status and a network error
    /// all mean the account is still there, so the caller keeps the user signed in.
    static func delete(session: URLSession, base: URL) async -> Bool {
        guard let (_, response) = try? await session.data(for: AuthRequests.deleteAccount(base: base)),
              let http = response as? HTTPURLResponse else { return false }
        return (200..<300).contains(http.statusCode)
    }
}
