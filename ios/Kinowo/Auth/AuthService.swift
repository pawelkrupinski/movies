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
    private let googleClientId: String
    private let facebookAppId: String

    override init() {
        self.session = URLSession.shared
        self.googleClientId = Bundle.main.object(forInfoDictionaryKey: "GIDClientID") as? String ?? ""
        self.facebookAppId = Bundle.main.object(forInfoDictionaryKey: "FacebookAppID") as? String ?? ""
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

    // MARK: - Google Sign-In

    func signInWithGoogle() async {
        guard !googleClientId.isEmpty else { return }
        isLoading = true
        defer { isLoading = false }
        let redirectScheme = googleClientId.components(separatedBy: ".").reversed().joined(separator: ".")
        let redirectUri = "\(redirectScheme):/oauth2callback"
        var components = URLComponents(string: "https://accounts.google.com/o/oauth2/v2/auth")!
        components.queryItems = [
            URLQueryItem(name: "client_id", value: googleClientId),
            URLQueryItem(name: "redirect_uri", value: redirectUri),
            URLQueryItem(name: "response_type", value: "code"),
            URLQueryItem(name: "scope", value: "openid email profile"),
            URLQueryItem(name: "prompt", value: "select_account"),
        ]
        guard let authURL = components.url else { return }
        do {
            let callbackURL = try await openWebAuth(url: authURL, scheme: redirectScheme)
            guard let code = URLComponents(url: callbackURL, resolvingAgainstBaseURL: false)?
                    .queryItems?.first(where: { $0.name == "code" })?.value else { return }
            try await sendToken(provider: "google", token: code, redirectUri: redirectUri)
        } catch {
            // User cancelled
        }
    }

    // MARK: - Facebook Login

    func signInWithFacebook() async {
        guard !facebookAppId.isEmpty else { return }
        isLoading = true
        defer { isLoading = false }
        let redirectUri = "fb\(facebookAppId)://authorize"
        var components = URLComponents(string: "https://www.facebook.com/v18.0/dialog/oauth")!
        components.queryItems = [
            URLQueryItem(name: "client_id", value: facebookAppId),
            URLQueryItem(name: "redirect_uri", value: redirectUri),
            URLQueryItem(name: "response_type", value: "code"),
            URLQueryItem(name: "scope", value: "email,public_profile"),
        ]
        guard let authURL = components.url else { return }
        do {
            let callbackURL = try await openWebAuth(url: authURL, scheme: "fb\(facebookAppId)")
            guard let code = URLComponents(url: callbackURL, resolvingAgainstBaseURL: false)?
                    .queryItems?.first(where: { $0.name == "code" })?.value else { return }
            try await sendToken(provider: "facebook", token: code, redirectUri: redirectUri)
        } catch {
            // User cancelled
        }
    }

    // MARK: - ASWebAuthenticationSession helper

    private func openWebAuth(url: URL, scheme: String) async throws -> URL {
        try await withCheckedThrowingContinuation { cont in
            let session = ASWebAuthenticationSession(url: url, callbackURLScheme: scheme) { url, error in
                if let error { cont.resume(throwing: error); return }
                guard let url else { cont.resume(throwing: URLError(.cancelled)); return }
                cont.resume(returning: url)
            }
            session.prefersEphemeralWebBrowserSession = false
            session.start()
        }
    }

    // MARK: - Server communication

    private func sendToken(provider: String, token: String, fullName: String? = nil, redirectUri: String? = nil) async throws {
        var body: [String: String] = ["provider": provider, "token": token]
        if let name = fullName { body["fullName"] = name }
        if let uri = redirectUri { body["redirectUri"] = uri }
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
        } catch {}
    }

    func signOut() async {
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("auth/logout"))
        request.httpMethod = "POST"
        _ = try? await session.data(for: request)
        user = nil
        if let cookies = HTTPCookieStorage.shared.cookies(for: kinowoBaseURL) {
            cookies.forEach { HTTPCookieStorage.shared.deleteCookie($0) }
        }
    }

    func deleteAccount() async {
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me"))
        request.httpMethod = "DELETE"
        _ = try? await session.data(for: request)
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
