import Foundation
import CryptoKit
import Security

/// The PKCE-style secret behind a web sign-in (RFC 7636, S256 only).
///
/// `AuthService.signInWithWeb` makes a fresh verifier, keeps it for the
/// length of the `ASWebAuthenticationSession`, and sends only its `challenge`
/// to `/auth/:provider/start`; the server stamps that challenge on the one-shot
/// code the `kinowo://auth-done` callback carries, and `/auth/exchange` spends
/// the code only for this verifier. A code from anybody else's flow — another
/// app claiming the `kinowo://` scheme, or an attacker's own code — is worthless
/// here. Same scheme as Android's `PkceVerifier`.
enum PkceVerifier {

    /// 32 bytes from the system CSPRNG, base64url: 43 characters of `[A-Za-z0-9_-]`.
    static func newVerifier() -> String {
        var bytes = [UInt8](repeating: 0, count: 32)
        let status = SecRandomCopyBytes(kSecRandomDefault, bytes.count, &bytes)
        precondition(status == errSecSuccess, "SecRandomCopyBytes failed: \(status)")
        return base64URL(Data(bytes))
    }

    /// base64url(SHA-256(verifier)), unpadded — what the server compares.
    static func challenge(for verifier: String) -> String {
        base64URL(Data(SHA256.hash(data: Data(verifier.utf8))))
    }

    private static func base64URL(_ data: Data) -> String {
        data.base64EncodedString()
            .replacingOccurrences(of: "+", with: "-")
            .replacingOccurrences(of: "/", with: "_")
            .replacingOccurrences(of: "=", with: "")
    }
}

/// The two requests of the native web sign-in that carry the verifier — pure,
/// so the challenge/verifier pairing is unit-tested without a browser.
enum NativeSignIn {

    /// `/auth/{provider}/start?platform=ios&challenge=…` for `verifier`.
    static func startURL(base: URL, provider: String, verifier: String) -> URL? {
        var components = URLComponents(url: base.appendingPathComponent("auth/\(provider)/start"), resolvingAgainstBaseURL: false)
        components?.queryItems = [
            URLQueryItem(name: "platform", value: "ios"),
            URLQueryItem(name: "challenge", value: PkceVerifier.challenge(for: verifier)),
        ]
        return components?.url
    }

    /// The `/auth/exchange` body: the deep-link code and the verifier behind it.
    static func exchangeBody(code: String, verifier: String) throws -> Data {
        try JSONEncoder().encode(["code": code, "verifier": verifier])
    }
}
