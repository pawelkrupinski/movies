import Foundation

/// Extracts `Film` rows from the kinowo `/` HTML.
///
/// The grid is a stable, server-rendered structure:
///
/// ```
/// <div class="col" data-title="...">
///   <img src="...">                              (poster)
///   <span class="pill runtime">2h 37min</span>
///   <div class="ratings"> ... rating-{imdb,meta,rt,fw}-* ... </div>
///   <div class="date-group" data-date="YYYY-MM-DD">
///     <div class="date-label">Czwartek 21 maja</div>
///     <div class="cinema-group" data-cinema="...">
///       <a class="cinema-label-link" href="...">Cinema ↗</a>
///       <a class="badge-time" href="..." data-room="..." data-format="..." data-time="HH:MM">
///         HH:MM <span class="badge-fmt">FMT</span><span class="fav-star">★</span>
///       </a>
///     </div>
///   </div>
/// </div>
/// ```
///
/// We slice the document by anchor markers (`<div class="col" data-title="`,
/// `<div class="date-group" data-date="`, `<div class="cinema-group"
/// data-cinema="`) and then regex out the attributes within each slice.
/// No nested DOM walking — siblings live at the same indentation level, so
/// "everything from this anchor to the next sibling anchor" reliably bounds
/// each slice.
enum HTMLParser {

    static func parse(html: String) -> [Film] {
        let starts = ranges(of: "<div class=\"col\" data-title=\"", in: html)
        var films: [Film] = []
        films.reserveCapacity(starts.count)
        for (i, start) in starts.enumerated() {
            let end = (i + 1 < starts.count) ? starts[i + 1] : html.endIndex
            let chunk = String(html[start..<end])
            if let film = parseFilm(chunk) {
                films.append(film)
            }
        }
        return films
    }

    // MARK: – film

    private static func parseFilm(_ chunk: String) -> Film? {
        guard let title = capture(chunk, #"data-title="([^"]+)""#) else { return nil }
        // Posters are now wrapped through the `images.weserv.nl` proxy
        // (server-side change in commit 39f23c3), so the src carries
        // `&w=480&output=webp` query params. Twirl HTML-escapes those
        // `&` to `&amp;` before they hit the wire — decode them back
        // before handing the string to `URL(string:)` so the query
        // weserv sees has the intended keys (otherwise weserv either
        // ignores w/output or rejects the URL outright, leaving
        // AsyncImage with nothing to render). Booking URL below does
        // the same dance.
        let poster   = capture(chunk, #"<img src="([^"]+)""#)
                          .flatMap { URL(string: $0.htmlDecoded()) }
        // `_movieCard` emits `data-fallbacks="url1|url2|..."` — a chain
        // of every non-primary poster URL we know about, in
        // source-priority order (Cinema City after Multikino, then
        // other cinemas, then TMDB, then IMDb). The web's inline
        // `onerror` pops the next URL until the chain is empty; iOS
        // does the same in PosterImage. Without this, films whose
        // primary URL 4xxs (Multikino's CDN today; Cinema City's
        // missing-upload case historically) show "Brak plakatu" on
        // iOS while the web swaps through cinema fallbacks.
        let fallbacks = (capture(chunk, #"data-fallbacks="([^"]+)""#) ?? "")
                          .htmlDecoded()
                          .split(separator: "|", omittingEmptySubsequences: true)
                          .compactMap { URL(string: String($0)) }
        let runtime  = capture(chunk, #"<span class="pill runtime">([^<]+)</span>"#)
                          .flatMap(parseRuntime)
        let ratings  = parseRatings(chunk)
        let showings = parseShowings(chunk)
        return Film(
            title: title.htmlDecoded(),
            posterURL: poster,
            fallbackPosterURLs: fallbacks,
            runtimeMinutes: runtime,
            ratings: ratings,
            showings: showings
        )
    }

    private static func parseRuntime(_ s: String) -> Int? {
        var minutes = 0
        var rest = Substring(s)
        if let hRange = rest.range(of: "h") {
            let h = Int(rest[..<hRange.lowerBound].trimmingCharacters(in: .whitespaces))
            if let h = h { minutes += h * 60 }
            rest = rest[hRange.upperBound...]
        }
        if let mRange = rest.range(of: "min") {
            let m = Int(rest[..<mRange.lowerBound].trimmingCharacters(in: .whitespaces))
            if let m = m { minutes += m }
        }
        return minutes > 0 ? minutes : nil
    }

    // MARK: – ratings

    private static func parseRatings(_ chunk: String) -> Film.Ratings {
        // Each rating <a> has its `class="rating-…"` AFTER the href; both
        // orderings of attributes occur in HTML in general, but `_ratingBadges`
        // always emits `href` first, so the cheap regex is fine.
        let imdbURL = capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-imdb""#)
                          .flatMap { URL(string: $0) }
        let imdb    = capture(chunk, #"class="rating-imdb-value">([^<]+)</span>"#)
                          .flatMap { Double($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        let mcURL   = capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-meta""#)
                          .flatMap { URL(string: $0) }
        // rating-meta wraps the score in the anchor body directly.
        let mc      = capture(chunk, #"class="rating-meta"[^>]*onclick="event.stopPropagation\(\)">([^<]+)</a>"#)
                          .flatMap { Int($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        let rtURL   = capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-rt[^"]*""#)
                          .flatMap { URL(string: $0) }
        let rt      = capture(chunk, #"class="rating-rt-value">([0-9]+)%</span>"#)
                          .flatMap { Int($0) }
        let fwURL   = capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-fw""#)
                          .flatMap { URL(string: $0) }
        let fw      = capture(chunk, #"class="rating-fw-value">([^<]+)</span>"#)
                          .flatMap { Double($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        return Film.Ratings(
            imdb: imdb, imdbURL: imdbURL,
            metascore: mc, metacriticURL: mcURL,
            rottenTomatoes: rt, rottenTomatoesURL: rtURL,
            filmweb: fw, filmwebURL: fwURL
        )
    }

    // MARK: – showings

    private static func parseShowings(_ chunk: String) -> [DayShowings] {
        let dayStarts = ranges(of: "<div class=\"date-group\" data-date=\"", in: chunk)
        var days: [DayShowings] = []
        days.reserveCapacity(dayStarts.count)
        for (i, start) in dayStarts.enumerated() {
            let end = (i + 1 < dayStarts.count) ? dayStarts[i + 1] : chunk.endIndex
            let dayChunk = String(chunk[start..<end])
            guard let date = capture(dayChunk, #"data-date="([^"]+)""#) else { continue }
            let label = capture(dayChunk, #"<div class="date-label">([^<]+)</div>"#)?
                .trimmingCharacters(in: .whitespacesAndNewlines) ?? date
            let cinemas = parseCinemas(dayChunk)
            if cinemas.isEmpty { continue }
            days.append(DayShowings(date: date, label: label, cinemas: cinemas))
        }
        return days
    }

    private static func parseCinemas(_ dayChunk: String) -> [CinemaShowings] {
        let cinemaStarts = ranges(of: "<div class=\"cinema-group\" data-cinema=\"", in: dayChunk)
        var out: [CinemaShowings] = []
        out.reserveCapacity(cinemaStarts.count)
        for (i, start) in cinemaStarts.enumerated() {
            let end = (i + 1 < cinemaStarts.count) ? cinemaStarts[i + 1] : dayChunk.endIndex
            let chunk = String(dayChunk[start..<end])
            guard let raw = capture(chunk, #"data-cinema="([^"]+)""#) else { continue }
            let name = raw.htmlDecoded()
            let url = capture(chunk, #"<a href="([^"]+)"[^>]*class="cinema-label-link""#)
                          .flatMap { URL(string: $0) }
            let showtimes = parseShowtimes(chunk)
            if showtimes.isEmpty { continue }
            out.append(CinemaShowings(cinema: name, cinemaURL: url, showtimes: showtimes))
        }
        return out
    }

    private static func parseShowtimes(_ chunk: String) -> [Showtime] {
        // `_filmShowings` emits each badge as one of:
        //   <a  ... class="badge-time" target="_blank" data-room="..." data-format="..." data-time="HH:MM">
        //   <span        class="badge-time"                          data-format="..." data-time="HH:MM">
        // We grab the whole opening tag (`<a ... >` or `<span ... >`) and pull
        // attributes by name, so attribute order doesn't matter.
        let pattern = #"<(?:a|span)\s+([^>]*?\bclass="badge-time"[^>]*)>"#
        guard let re = try? NSRegularExpression(pattern: pattern, options: [.dotMatchesLineSeparators]) else { return [] }
        let ns = chunk as NSString
        let matches = re.matches(in: chunk, range: NSRange(location: 0, length: ns.length))
        var out: [Showtime] = []
        out.reserveCapacity(matches.count)
        for m in matches {
            guard m.numberOfRanges >= 2 else { continue }
            let attrs = ns.substring(with: m.range(at: 1))
            guard let time = attribute(attrs, "data-time") else { continue }
            let format = attribute(attrs, "data-format") ?? ""
            let room   = attribute(attrs, "data-room")
            let booking = attribute(attrs, "href").flatMap { URL(string: $0.htmlDecoded()) }
            out.append(Showtime(time: time, format: format, room: room, bookingURL: booking))
        }
        return out
    }

    // MARK: – primitives

    private static func ranges(of needle: String, in haystack: String) -> [String.Index] {
        var result: [String.Index] = []
        var searchStart = haystack.startIndex
        while searchStart < haystack.endIndex,
              let r = haystack.range(of: needle, range: searchStart..<haystack.endIndex) {
            result.append(r.lowerBound)
            searchStart = r.upperBound
        }
        return result
    }

    private static func capture(_ string: String, _ pattern: String) -> String? {
        guard let re = try? NSRegularExpression(
            pattern: pattern,
            options: [.dotMatchesLineSeparators]
        ) else { return nil }
        let ns = string as NSString
        guard let m = re.firstMatch(in: string, range: NSRange(location: 0, length: ns.length)),
              m.numberOfRanges >= 2 else { return nil }
        let r = m.range(at: 1)
        if r.location == NSNotFound { return nil }
        return ns.substring(with: r)
    }

    private static func attribute(_ attrs: String, _ name: String) -> String? {
        guard let re = try? NSRegularExpression(pattern: "\\b\(name)=\"([^\"]*)\"") else { return nil }
        let ns = attrs as NSString
        guard let m = re.firstMatch(in: attrs, range: NSRange(location: 0, length: ns.length)),
              m.numberOfRanges >= 2 else { return nil }
        return ns.substring(with: m.range(at: 1))
    }
}
