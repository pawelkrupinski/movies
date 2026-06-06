import Foundation
import AuthenticationServices
import Combine

@MainActor
final class AuthService: NSObject, ObservableObject {
    @Published var user: UserProfile?
    @Published var isLoading = false

    /// Armed when a sign-in starts so the post-OAuth foreground doesn't re-fire
    /// the nearer-city prompt the user already answered. Read by `ContentView`.
    let citySwitchSuppressor = CitySwitchSuppressor()

    private let session: URLSession
    private var webAuthSession: ASWebAuthenticationSession?

    override init() {
        self.session = URLSession.shared
        super.init()
    }

    // MARK: - Web OAuth (Google / Facebook)

    func signInWithGoogle() async {
        await signInWithWeb(provider: "google")
    }

    func signInWithFacebook() async {
        await signInWithWeb(provider: "facebook")
    }

    private func signInWithWeb(provider: String) async {
        // The auth sheet briefly backgrounds the app; skip the nearer-city
        // check that the return-to-foreground would otherwise re-fire.
        citySwitchSuppressor.suppressNextCheck()
        isLoading = true
        defer { isLoading = false }
        let startURL = kinowoBaseURL.appendingPathComponent("auth/\(provider)/start")
        var components = URLComponents(url: startURL, resolvingAgainstBaseURL: false)!
        components.queryItems = [URLQueryItem(name: "platform", value: "ios")]
        guard let url = components.url else { return }
        do {
            let callbackURL = try await withCheckedThrowingContinuation { (cont: CheckedContinuation<URL, Error>) in
                let webAuth = ASWebAuthenticationSession(url: url, callbackURLScheme: "kinowo") { [weak self] url, error in
                    self?.webAuthSession = nil
                    if let error { cont.resume(throwing: error); return }
                    guard let url else { cont.resume(throwing: URLError(.cancelled)); return }
                    cont.resume(returning: url)
                }
                webAuth.presentationContextProvider = self
                webAuth.prefersEphemeralWebBrowserSession = false
                self.webAuthSession = webAuth
                webAuth.start()
            }
            if let code = URLComponents(url: callbackURL, resolvingAgainstBaseURL: false)?
                .queryItems?.first(where: { $0.name == "code" })?.value {
                try await exchangeCode(code)
            }
        } catch {
            // User cancelled
        }
    }

    // MARK: - Apple Sign-In

    private nonisolated(unsafe) var appleSignInContinuation: CheckedContinuation<ASAuthorization, Error>?

    func signInWithApple() async {
        // The Apple sheet briefly backgrounds the app; skip the nearer-city
        // check that the return-to-foreground would otherwise re-fire.
        citySwitchSuppressor.suppressNextCheck()
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
        } catch {}
    }

    // MARK: - Server communication

    private func exchangeCode(_ code: String) async throws {
        var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("auth/exchange"))
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(["code": code])
        let (data, response) = try await session.data(for: request)
        guard let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else {
            throw URLError(.userAuthenticationRequired)
        }
        self.user = try JSONDecoder().decode(UserProfile.self, from: data)
    }

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

    func fetchMe() async {
        do {
            var request = URLRequest(url: kinowoBaseURL.appendingPathComponent("api/me"))
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse, http.statusCode == 200 else { return }
            self.user = try JSONDecoder().decode(UserProfile.self, from: data)
        } catch {}
    }

    func checkSession() async {
        await fetchMe()
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

// MARK: - ASWebAuthenticationPresentationContextProviding

extension AuthService: ASWebAuthenticationPresentationContextProviding {
    func presentationAnchor(for session: ASWebAuthenticationSession) -> ASPresentationAnchor {
        UIApplication.shared.connectedScenes
            .compactMap { $0 as? UIWindowScene }
            .first?.windows.first { $0.isKeyWindow } ?? ASPresentationAnchor()
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
