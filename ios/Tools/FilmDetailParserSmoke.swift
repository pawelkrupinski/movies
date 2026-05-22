// FilmDetailParser smoke-test driver — compiles the parser against a
// live `/film?title=…` page (or a local HTML file) and prints what it
// pulled out so a human can verify Opis/Reżyseria/Obsada/cinema-links
// /trailers/showings parse correctly. Mirrors HTMLParserSmoke for the
// listing parser.
//
// Usage:
//   ./Tools/run_film_smoke.sh                     # fetches a representative title
//   ./Tools/run_film_smoke.sh "Title with spaces" # fetches that title
//   ./Tools/run_film_smoke.sh path.html           # uses local HTML

import Foundation

@main
enum FilmDetailParserSmoke {
    static func main() {
        let args = CommandLine.arguments
        let (html, sourceTitle): (String, String) = {
            if args.count >= 2 {
                let arg = args[1]
                if FileManager.default.fileExists(atPath: arg) {
                    do {
                        return (try String(contentsOfFile: arg, encoding: .utf8), arg)
                    } catch {
                        FileHandle.standardError.write(Data("Failed to read \(arg): \(error)\n".utf8))
                        exit(1)
                    }
                }
                // Treat as a title to fetch.
                guard let s = fetchFilmHTML(title: arg) else { exit(1) }
                return (s, arg)
            }
            // Default: fetch the listing page, pick its first film, fetch /film for it.
            let title = pickAFilmFromListing() ?? "Mandalorian i Grogu"
            guard let s = fetchFilmHTML(title: title) else { exit(1) }
            return (s, title)
        }()

        print("source:   \(sourceTitle)")
        print("HTML size: \(html.count) chars")
        let detail = FilmDetailParser.parse(html: html, fallbackTitle: sourceTitle)
        print(separator)
        printDetail(detail)
        print(separator)

        var failures = 0
        func check(_ name: String, _ cond: Bool, _ extra: String = "") {
            if cond { print("✓ \(name)") }
            else { print("✗ \(name) \(extra)"); failures += 1 }
        }

        check("title non-empty", !detail.title.isEmpty, "(got '\(detail.title)')")
        check("poster URL present", detail.posterURL != nil)
        check("at least one cinema-link", !detail.cinemaLinks.isEmpty,
              "(got \(detail.cinemaLinks.count))")
        check("at least one showing day", !detail.showings.isEmpty,
              "(got \(detail.showings.count))")
        let totalShowtimes = detail.showings.flatMap { $0.cinemas }.reduce(0) { $0 + $1.showtimes.count }
        check("at least one showtime in total", totalShowtimes > 0, "(got \(totalShowtimes))")
        let hasAnyMeta = (detail.synopsis ?? "").isEmpty == false
                      || (detail.director ?? "").isEmpty == false
                      || (detail.cast ?? "").isEmpty == false
        check("at least one meta block (Opis/Reżyseria/Obsada)", hasAnyMeta)
        // The meta-value regex must not eat past the meta-value div —
        // a runaway match would slurp the trailer button, the
        // showtimes section, the closing body, etc. Cap synopsis length
        // generously (~3 KB) — real synopses run a few hundred chars,
        // never thousands.
        if let s = detail.synopsis {
            check("synopsis under 5 KB (regex didn't run away)", s.count < 5000,
                  "(got \(s.count) chars)")
        }

        if failures > 0 {
            print("FAILED with \(failures) check failures")
            exit(2)
        }
        print("ALL OK")
    }

    static let separator = String(repeating: "─", count: 80)

    static func printDetail(_ d: FilmDetail) {
        print("title:    \(d.title)")
        print("poster:   \(d.posterURL?.absoluteString.prefix(100).description ?? "—")")
        print("fallbacks:\(d.fallbackPosterURLs.count)")
        let r = d.ratings
        let ratings = [
            r.imdb.map { "IMDb \(String(format: "%.1f", $0))" },
            r.metascore.map { "MC \($0)" },
            r.rottenTomatoes.map { "RT \($0)%" },
            r.filmweb.map { "FW \(String(format: "%.1f", $0))" }
        ].compactMap { $0 }.joined(separator: " · ")
        print("ratings:  \(ratings.isEmpty ? "—" : ratings)")
        print("cinema-links (\(d.cinemaLinks.count)):")
        for link in d.cinemaLinks.prefix(5) {
            print("  • \(link.cinema) -> \(link.url.absoluteString.prefix(80))")
        }
        if let s = d.synopsis {
            let preview = s.prefix(160).replacingOccurrences(of: "\n", with: " ")
            print("Opis:     \(preview)\(s.count > 160 ? "…" : "")")
        }
        print("Reżyseria: \(d.director ?? "—")")
        print("Obsada:    \(d.cast?.prefix(160).description ?? "—")")
        print("trailers: \(d.trailerURLs.count)")
        for url in d.trailerURLs.prefix(3) {
            print("  • \(url.absoluteString)")
        }
        print("days:     \(d.showings.count)")
        for day in d.showings.prefix(3) {
            let cinemaSummary = day.cinemas.map { "\($0.cinema)(\($0.showtimes.count))" }
                .joined(separator: ", ")
            print("  \(day.date) – \(day.label): \(cinemaSummary)")
        }
    }

    /// Hit the production listing and steal a `data-title=...` so the
    /// smoke test isn't pinned to a film that may leave the schedule.
    static func pickAFilmFromListing() -> String? {
        let url = URL(string: "https://kinowo.fly.dev/")!
        var request = URLRequest(url: url)
        request.setValue("KinowoSmoke/1.0", forHTTPHeaderField: "User-Agent")
        var result: String?
        let sem = DispatchSemaphore(value: 0)
        URLSession.shared.dataTask(with: request) { data, _, _ in
            if let data, let s = String(data: data, encoding: .utf8) { result = s }
            sem.signal()
        }.resume()
        sem.wait()
        guard let html = result else { return nil }
        // First `data-title="…"` value on the page. The listing emits
        // these per film card; the first card is good enough.
        guard let re = try? NSRegularExpression(pattern: #"data-title="([^"]+)""#) else { return nil }
        let ns = html as NSString
        guard let m = re.firstMatch(in: html, range: NSRange(location: 0, length: ns.length)),
              m.numberOfRanges >= 2 else { return nil }
        return ns.substring(with: m.range(at: 1)).htmlDecoded()
    }

    static func fetchFilmHTML(title: String) -> String? {
        var components = URLComponents(string: "https://kinowo.fly.dev/film")!
        components.queryItems = [URLQueryItem(name: "title", value: title)]
        guard let url = components.url else { return nil }
        var request = URLRequest(url: url)
        request.setValue("KinowoSmoke/1.0", forHTTPHeaderField: "User-Agent")
        var result: String?
        let sem = DispatchSemaphore(value: 0)
        URLSession.shared.dataTask(with: request) { data, _, _ in
            if let data, let s = String(data: data, encoding: .utf8) { result = s }
            sem.signal()
        }.resume()
        sem.wait()
        if result == nil {
            FileHandle.standardError.write(Data("Failed to fetch \(url)\n".utf8))
        }
        return result
    }
}
