// HTMLParser smoke-test driver — compiles the parser against the kinowo.fly.dev
// HTML and prints a per-film summary so a human can tell at a glance whether
// the parse still works. Intended as the iOS-side equivalent of the Scala
// "scripts must print what they did" rule: re-run it whenever the web
// templates change and confirm the numbers + sample films still look sane.
//
// Usage:
//   ./Tools/run_smoke.sh              # fetches https://kinowo.fly.dev/
//   ./Tools/run_smoke.sh path.html    # uses local HTML

import Foundation

@main
enum HTMLParserSmoke {
    static func main() {
        let args = CommandLine.arguments
        let html: String
        if args.count >= 2 {
            do {
                html = try String(contentsOfFile: args[1], encoding: .utf8)
            } catch {
                FileHandle.standardError.write(Data("Failed to read \(args[1]): \(error)\n".utf8))
                exit(1)
            }
        } else {
            guard let s = fetchProductionHTML() else { exit(1) }
            html = s
        }

        print("HTML size: \(html.count) chars")
        let films = HTMLParser.parse(html: html)
        print("Parsed films: \(films.count)")
        print(separator)

        let totalShowtimes = films.flatMap { $0.showings }
            .flatMap { $0.cinemas }
            .reduce(0) { $0 + $1.showtimes.count }
        let withPoster   = films.filter { $0.posterURL != nil }.count
        let withFallback = films.filter { !$0.fallbackPosterURLs.isEmpty }.count
        let withImdb     = films.filter { $0.ratings.imdb != nil }.count
        let withRuntime  = films.filter { $0.runtimeMinutes != nil }.count
        let withShowings = films.filter { !$0.showings.isEmpty }.count

        let avgFallbacks = films.isEmpty ? 0.0
            : Double(films.reduce(0) { $0 + $1.fallbackPosterURLs.count }) / Double(films.count)
        print("with poster:    \(withPoster) / \(films.count)")
        print(String(format: "with fallback:  %d / %d (avg %.1f URLs per chain)",
                     withFallback, films.count, avgFallbacks))
        print("with runtime:   \(withRuntime) / \(films.count)")
        print("with IMDb:      \(withImdb) / \(films.count)")
        print("with showings:  \(withShowings) / \(films.count)")
        print("total showtimes: \(totalShowtimes)")
        print(separator)

        for f in films.prefix(5) {
            printSample(f)
        }
        print(separator)

        var failures = 0
        func check(_ name: String, _ cond: Bool, _ extra: String = "") {
            if cond { print("✓ \(name)") }
            else { print("✗ \(name) \(extra)"); failures += 1 }
        }

        check("at least 30 films parsed",     films.count >= 30, "(got \(films.count))")
        check("at least 80% have posters",    !films.isEmpty && Double(withPoster)   / Double(films.count) >= 0.8)
        check("at least 90% have showings",   !films.isEmpty && Double(withShowings) / Double(films.count) >= 0.9)
        check("at least 100 showtimes total", totalShowtimes >= 100, "(got \(totalShowtimes))")
        check("at least one IMDb rating",     withImdb > 0)
        // _movieCard emits data-fallbacks on the majority of films
        // (every non-primary cinema/TMDB/IMDb poster). If this drops
        // to ~zero the parser's data-fallbacks regex has rotted
        // against a template change — the iOS side then can't recover
        // from the cinema-side 4xxs the web template handles via
        // onerror.
        check("at least 50% have a fallback poster chain", !films.isEmpty && Double(withFallback) / Double(films.count) >= 0.5,
              "(got \(withFallback) / \(films.count))")
        // Twirl HTML-escapes `&` to `&amp;` in attribute values, so any URL
        // we extract via the raw-regex path must be HTML-decoded before
        // hitting `URL(string:)` — otherwise multi-param query strings
        // (e.g. the `&w=480&output=webp` chain on the PosterProxy URL)
        // get fed to URLSession with literal `amp;` keys and the proxy
        // ignores them. Guards against future URL extractions skipping
        // the decode step.
        let dirtyPrimaries = films.compactMap { $0.posterURL?.absoluteString }
            .filter { $0.contains("&amp;") || $0.contains("amp;") }
        let dirtyFallbacks = films.flatMap { $0.fallbackPosterURLs.map(\.absoluteString) }
            .filter { $0.contains("&amp;") || $0.contains("amp;") }
        check("no HTML-escaped &amp; in parsed poster URLs", dirtyPrimaries.isEmpty && dirtyFallbacks.isEmpty,
              "(\(dirtyPrimaries.count) dirty primaries, \(dirtyFallbacks.count) dirty fallbacks, first: \((dirtyPrimaries + dirtyFallbacks).first ?? "?"))")

        if failures > 0 {
            print("FAILED with \(failures) check failures")
            exit(2)
        }
        print("ALL OK")
    }

    static let separator = String(repeating: "─", count: 80)

    static func printSample(_ f: Film) {
        print("• \(f.title)")
        print("   poster:  \(f.posterURL?.absoluteString.prefix(80).description ?? "—")")
        print("   runtime: \(f.runtimeMinutes.map { "\($0)min" } ?? "—")")
        let r = f.ratings
        let ratings = [
            r.imdb.map { "IMDb \(String(format: "%.1f", $0))" },
            r.metascore.map { "MC \($0)" },
            r.rottenTomatoes.map { "RT \($0)%" },
            r.filmweb.map { "FW \(String(format: "%.1f", $0))" }
        ].compactMap { $0 }.joined(separator: " · ")
        print("   ratings: \(ratings.isEmpty ? "—" : ratings)")
        print("   days:    \(f.showings.count)")
        for day in f.showings.prefix(2) {
            let cinemaSummary = day.cinemas.map { "\($0.cinema)(\($0.showtimes.count))" }
                .joined(separator: ", ")
            print("     \(day.date) – \(day.label): \(cinemaSummary)")
        }
    }

    static func fetchProductionHTML() -> String? {
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
        if result == nil {
            FileHandle.standardError.write(Data("Failed to fetch \(url)\n".utf8))
        }
        return result
    }
}
