import Foundation

/// Parses `kinowo.fly.dev/film?title=…` into a `FilmDetail`.
///
/// The detail page (`film.scala.html`) is laid out as:
///
/// ```
/// <div class="poster-wrap" data-title="...">
///   <button class="fav-poster-btn">★</button>
///   <img class="poster-img" src="..." data-fallbacks="...|...">
/// </div>
/// <div class="film-title">Title</div>
/// <div class="ratings"> ... rating-{imdb,meta,rt,fw}-* ... </div>
/// <a class="cinema-link" href="...">Cinema ↗</a>...
/// <div class="meta-label">Opis</div><div class="meta-value">...</div>
/// <div class="meta-label">Reżyseria</div><div class="meta-value">...</div>
/// <div class="meta-label">Obsada</div><div class="meta-value">...</div>
/// <div class="meta-label">Zwiastuny</div>
///   <button class="trailer-link" onclick="playTrailer(this, 'EMBED_URL')">Zwiastun 1</button>...
/// <div class="showtimes-section" data-title="...">
///   <h2>Seanse</h2>
///   <div class="date-group" data-date="...">...</div>...
/// </div>
/// ```
///
/// We re-use `RatingsParser` / `ShowingsParser` for the blocks both
/// pages share, and handle the detail-only fields (poster-img,
/// cinema-link, meta-* sections, trailer-link) here.
enum FilmDetailParser {

    static func parse(html: String, fallbackTitle: String) -> FilmDetail {
        let title = HTMLPrimitives.capture(html, #"<div class="film-title">([^<]+)</div>"#)?
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .htmlDecoded()
            ?? fallbackTitle

        // Poster carries `class="poster-img"` on /film (vs the listing's
        // unclassed `<img>`), so we anchor on that. The attribute order
        // in the emitted HTML is `src` → `alt` → `class="poster-img"`,
        // but to be tolerant we grab the whole opening tag and pull
        // attributes by name.
        let imgTag = HTMLPrimitives.capture(html, #"<img\s+([^>]*?class="poster-img"[^>]*)>"#)
        let poster = imgTag
            .flatMap { HTMLPrimitives.attribute($0, "src") }
            .flatMap { URL(string: $0.htmlDecoded()) }
        let fallbacks = imgTag
            .flatMap { HTMLPrimitives.attribute($0, "data-fallbacks") }
            .map { $0.htmlDecoded() }
            .map { raw in
                raw.split(separator: "|", omittingEmptySubsequences: true)
                    .compactMap { URL(string: String($0)) }
            } ?? []

        let ratings = RatingsParser.parseRatings(in: html)
        let cinemaLinks = parseCinemaLinks(html)
        let synopsis = parseMetaValue(html, label: "Opis")
        let director = parseMetaValue(html, label: "Reżyseria")
        let cast     = parseMetaValue(html, label: "Obsada")
        let trailers = parseTrailerURLs(html)
        let showings = ShowingsParser.parseShowings(in: html)

        return FilmDetail(
            title: title,
            posterURL: poster,
            fallbackPosterURLs: fallbacks,
            ratings: ratings,
            cinemaLinks: cinemaLinks,
            synopsis: synopsis,
            director: director,
            cast: cast,
            trailerURLs: trailers,
            showings: showings
        )
    }

    // MARK: – cinema links

    private static func parseCinemaLinks(_ html: String) -> [FilmDetail.CinemaLink] {
        // <a href="..." target="_blank" class="cinema-link">Helios ↗</a>
        // Trailing " ↗" suffix is decorative; strip it for the display name.
        let pattern = #"<a\s+href="([^"]+)"[^>]*class="cinema-link"[^>]*>([^<]+)</a>"#
        guard let re = try? NSRegularExpression(pattern: pattern, options: [.dotMatchesLineSeparators]) else { return [] }
        let ns = html as NSString
        let matches = re.matches(in: html, range: NSRange(location: 0, length: ns.length))
        var out: [FilmDetail.CinemaLink] = []
        out.reserveCapacity(matches.count)
        for m in matches where m.numberOfRanges >= 3 {
            guard let url = URL(string: ns.substring(with: m.range(at: 1)).htmlDecoded()) else { continue }
            let name = ns.substring(with: m.range(at: 2))
                .replacingOccurrences(of: "↗", with: "")
                .trimmingCharacters(in: .whitespacesAndNewlines)
                .htmlDecoded()
            if name.isEmpty { continue }
            out.append(FilmDetail.CinemaLink(cinema: name, url: url))
        }
        return out
    }

    // MARK: – meta values

    private static func parseMetaValue(_ html: String, label: String) -> String? {
        // The template emits:
        //   <div class="meta-label">LABEL</div>
        //   <div class="meta-value">VALUE</div>
        // VALUE can be multi-paragraph (newlines, blank lines) — Opis
        // commonly is — so we use `.dotMatchesLineSeparators` and a
        // non-greedy `(.*?)` to stop at the first closing `</div>`.
        // Labels are literal ASCII / Polish text, safe to embed in the
        // regex without escaping.
        let pattern = #"<div class="meta-label">\#(label)</div>\s*<div class="meta-value">(.*?)</div>"#
        return HTMLPrimitives.capture(html, pattern)
            .map(HTMLPrimitives.stripTags)
            .map { $0.htmlDecoded() }
            .flatMap { $0.isEmpty ? nil : $0 }
    }

    // MARK: – trailers

    private static func parseTrailerURLs(_ html: String) -> [URL] {
        // <button ... class="trailer-link" onclick="playTrailer(this, 'URL')">Zwiastun N</button>
        // The onclick body is the only place the embed URL appears.
        let pattern = #"class="trailer-link"\s+onclick="playTrailer\(this,\s*'([^']+)'\)""#
        guard let re = try? NSRegularExpression(pattern: pattern, options: [.dotMatchesLineSeparators]) else { return [] }
        let ns = html as NSString
        let matches = re.matches(in: html, range: NSRange(location: 0, length: ns.length))
        var out: [URL] = []
        out.reserveCapacity(matches.count)
        for m in matches where m.numberOfRanges >= 2 {
            if let url = URL(string: ns.substring(with: m.range(at: 1)).htmlDecoded()) {
                out.append(url)
            }
        }
        return out
    }
}
