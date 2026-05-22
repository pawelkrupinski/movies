import Foundation

/// Fetches `/film?title=…` from the kinowo backend and parses it into
/// a `FilmDetail`. One instance per detail-screen lifetime — the view
/// owns it via `@StateObject` so navigating away and back re-fetches
/// (showings, trailer state, cinema-links can change between visits).
@MainActor
final class FilmDetailStore: ObservableObject {
    @Published var detail: FilmDetail? = nil
    @Published var isLoading: Bool = false
    @Published var error: Error? = nil

    private let baseURL: URL
    private let session: URLSession

    init(baseURL: URL = kinowoProductionURL, session: URLSession = .shared) {
        self.baseURL = baseURL
        self.session = session
    }

    func load(title: String) async {
        isLoading = true
        error = nil
        defer { isLoading = false }
        do {
            // Mirror `controllers.FilmHref.apply` server-side: percent-
            // encode the title as a single query value. We use a
            // URLComponents query item so URLSession does the encoding
            // (handles Polish diacritics, spaces, ampersands).
            guard var components = URLComponents(url: baseURL, resolvingAgainstBaseURL: false) else {
                throw URLError(.badURL)
            }
            components.path = "/film"
            components.queryItems = [URLQueryItem(name: "title", value: title)]
            guard let url = components.url else { throw URLError(.badURL) }

            var request = URLRequest(url: url)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            let (data, response) = try await session.data(for: request)
            if let http = response as? HTTPURLResponse,
               !(200..<300).contains(http.statusCode) {
                throw URLError(.badServerResponse)
            }
            guard let html = String(data: data, encoding: .utf8) else {
                throw URLError(.cannotDecodeContentData)
            }
            self.detail = FilmDetailParser.parse(html: html, fallbackTitle: title)
        } catch {
            self.error = error
        }
    }
}
