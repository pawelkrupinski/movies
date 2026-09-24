import XCTest
@testable import KinowoAuth

/// The native sign-in's PKCE-style verifier must hash exactly as the server
/// does, or every new-app sign-in would be refused at `/auth/exchange`.
final class PkceVerifierTests: XCTestCase {

    func testChallengeIsTheRfc7636S256OfTheVerifier() {
        // RFC 7636 appendix B — the same vector the server and Android use.
        XCTAssertEqual(PkceVerifier.challenge(for: "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"),
                       "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
    }

    func testVerifiersAreFreshAndServerShaped() {
        let first = PkceVerifier.newVerifier()
        XCTAssertEqual(first.count, 43)
        XCTAssertNil(first.range(of: "[^A-Za-z0-9_-]", options: .regularExpression))
        XCTAssertNotEqual(first, PkceVerifier.newVerifier())
    }

    func testStartURLCarriesTheChallengeAndExchangeBodyTheVerifier() throws {
        let verifier = PkceVerifier.newVerifier()
        let url = try XCTUnwrap(NativeSignIn.startURL(base: URL(string: "https://kinowo.net")!, provider: "google", verifier: verifier))
        let items = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems ?? []
        XCTAssertEqual(url.path, "/auth/google/start")
        XCTAssertEqual(items.first { $0.name == "platform" }?.value, "ios")
        XCTAssertEqual(items.first { $0.name == "challenge" }?.value, PkceVerifier.challenge(for: verifier))

        let body = try JSONDecoder().decode([String: String].self, from: NativeSignIn.exchangeBody(code: "c", verifier: verifier))
        XCTAssertEqual(body, ["code": "c", "verifier": verifier])
    }
}
