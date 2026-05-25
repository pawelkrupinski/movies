import Foundation
import AuthenticationServices
import Combine

struct UserProfile: Codable {
    let displayName: String?
    let email: String?
    let avatarUrl: URL?
    let provider: String
}

let kinowoBaseURL = URL(string: "https://kinowo.fly.dev")!

@MainActor
final class AuthService: NSObject, ObservableObject {
    @Published var user: UserProfile?
    @Published var isLoading = false

    private let session: URLSession

    override init() {
        self.session = URLSession.shared
        super.init()
    }

    // MARK: - Apple Sign-In

    private nonisolated(unsafe) var appleSignInContinuation: CheckedContinuation<ASAuthorization, Error>?

    func signInWithApple() async {
        isLoading = true
        defer { isLoading = false }
        do {
            let authorization = try await withCheckedThrowingContinuation { (cont: CheckedContinuation<ASAuthorization, Error>) in
                appleSignInContinuation = cont
                let request = ASAuthorizationAppleIDProvider().createRequest()
                request.requestedScopes = [.fullName, .email]
                let controller = ASAuthorizationController(authorizationRequests: [request])
                controller.delegate = self
                controller.performRequests()
            }
            guard let credential = authorization.credential as? ASAuthorizationAppleIDCredential,
                  let tokenData = credential.identityToken,
                  let token = String(data: tokenData, encoding: .utf8) else {
                return
            }
            var fullName: String?
            if let fn = credential.fullName {
                let parts = [fn.givenName, fn.familyName].compactMap { $0 }.filter { !$0.isEmpty }
                if !parts.isEmpty { fullName = parts.joined(separator: " ") }
            }
            try await sendToken(provider: "apple", token: token, fullName: fullName)
        } catch {
            // User cancelled or auth failed
        }
    }

    // MARK: - Google Sign-In (native SDK)

    func signInWithGoogle(presenting: Any? = nil) async {
        // Placeholder — requires GoogleSignIn SDK integration.
        // When the SDK is added:
        // 1. GIDSignIn.sharedInstance.signIn(withPresenting: viewController)
        // 2. Extract user.idToken.tokenString
        // 3. Call sendToken(provider: "google", token: idToken)
    }

    // MARK: - Facebook Login (native SDK)

    func signInWithFacebook() async {
        // Placeholder — requires FacebookLogin SDK integration.
        // When the SDK is added:
        // 1. LoginManager().logIn(permissions: ["public_profile", "email"])
        // 2. Extract AccessToken.current.tokenString
        // 3. Call sendToken(provider: "facebook", token: accessToken)
    }

    // MARK: - Server communication

    private func sendToken(provider: String, token: String, fullName: String? = nil) async throws {
        var body: [String: String] = ["provider": provider, "token": token]
        if let name = fullName { body["fullName"] = name }
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("auth/token"))
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(body)
        let (data, response) = try await session.data(for: request)
        guard let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else {
            throw URLError(.userAuthenticationRequired)
        }
        self.user = try JSONDecoder().decode(UserProfile.self, from: data)
    }

    func checkSession() async {
        do {
            var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me"))
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse, http.statusCode == 200 else { return }
            self.user = try JSONDecoder().decode(UserProfile.self, from: data)
        } catch {
            // Not logged in or network error — stay logged out
        }
    }

    func signOut() async {
        do {
            var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("auth/logout"))
            request.httpMethod = "POST"
            _ = try? await session.data(for: request)
        }
        user = nil
        if let cookies = HTTPCookieStorage.shared.cookies(for: kinowoBaseURL) {
            cookies.forEach { HTTPCookieStorage.shared.deleteCookie($0) }
        }
    }

    func deleteAccount() async {
        do {
            var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me"))
            request.httpMethod = "DELETE"
            _ = try? await session.data(for: request)
        }
        user = nil
        if let cookies = HTTPCookieStorage.shared.cookies(for: kinowoBaseURL) {
            cookies.forEach { HTTPCookieStorage.shared.deleteCookie($0) }
        }
    }
}

// MARK: - ASAuthorizationControllerDelegate

extension AuthService: ASAuthorizationControllerDelegate {
    nonisolated func authorizationController(controller: ASAuthorizationController, didCompleteWithAuthorization authorization: ASAuthorization) {
        appleSignInContinuation?.resume(returning: authorization)
        appleSignInContinuation = nil
    }

    nonisolated func authorizationController(controller: ASAuthorizationController, didCompleteWithError error: Error) {
        appleSignInContinuation?.resume(throwing: error)
        appleSignInContinuation = nil
    }
}
