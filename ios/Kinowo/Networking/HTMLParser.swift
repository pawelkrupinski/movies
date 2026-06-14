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
///         HH:MM <span class="badge-fmt">FMT</span>
///       </a>
///     </div>
///   </div>
/// </div>
/// ```
///
/// We slice the document by the `<div class="col" data-title="` anchor
/// and then delegate to `RatingsParser` / `ShowingsParser` for the
/// sub-blocks. Primitive regex helpers live in `HTMLPrimitives`.
enum HTMLParser {

    static func parse(html: String) -> [Film] {
        let starts = HTMLPrimitives.ranges(of: "<div class=\"col\" data-title=\"", in: html)
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
        guard let title = HTMLPrimitives.capture(chunk, #"data-title="([^"]+)""#) else { return nil }
        // Posters are now wrapped through the `images.weserv.nl` proxy
        // (server-side change in commit 39f23c3), so the src carries
        // `&w=480&output=webp` query params. Twirl HTML-escapes those
        // `&` to `&amp;` before they hit the wire — decode them back
        // before handing the string to `URL(string:)` so the query
        // weserv sees has the intended keys (otherwise weserv either
        // ignores w/output or rejects the URL outright, leaving
        // AsyncImage with nothing to render). Booking URL below does
        // the same dance.
        let poster   = HTMLPrimitives.capture(chunk, #"<img src="([^"]+)""#)
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
        let fallbacks = (HTMLPrimitives.capture(chunk, #"data-fallbacks="([^"]+)""#) ?? "")
                          .htmlDecoded()
                          .split(separator: "|", omittingEmptySubsequences: true)
                          .compactMap { URL(string: String($0)) }
        let runtime  = HTMLPrimitives.capture(chunk, #"<span class="pill runtime">([^<]+)</span>"#)
                          .flatMap(parseRuntime)
        let countries = (HTMLPrimitives.capture(chunk, #"data-countries="([^"]*)""#) ?? "")
                          .split(separator: "|", omittingEmptySubsequences: true)
                          .map { String($0).htmlDecoded() }
        let directors = (HTMLPrimitives.capture(chunk, #"data-director="([^"]*)""#) ?? "")
                          .split(separator: ",", omittingEmptySubsequences: true)
                          .map { String($0).trimmingCharacters(in: .whitespaces).htmlDecoded() }
        let cast = (HTMLPrimitives.capture(chunk, #"data-cast="([^"]*)""#) ?? "")
                          .split(separator: ",", omittingEmptySubsequences: true)
                          .map { String($0).trimmingCharacters(in: .whitespaces).htmlDecoded() }
        let ratings  = RatingsParser.parseRatings(in: chunk)
        let showings = ShowingsParser.parseShowings(in: chunk)
        return Film(
            title: title.htmlDecoded(),
            posterURL: poster,
            fallbackPosterURLs: fallbacks,
            runtimeMinutes: runtime,
            // The legacy home-page HTML this parser handles carries no
            // year/genre attributes; the JSON `/api/repertoire` path
            // (RepertoireStore) is the one that populates them.
            releaseYear: nil,
            genres: [],
            ratings: ratings,
            countries: countries,
            directors: directors,
            cast: cast,
            showings: showings
        )
    }

    private static func parseRuntime(_ runtimeText: String) -> Int? {
        var totalMinutes = 0
        var rest = Substring(runtimeText)
        if let hourRange = rest.range(of: "h") {
            let hours = Int(rest[..<hourRange.lowerBound].trimmingCharacters(in: .whitespaces))
            if let hours = hours { totalMinutes += hours * 60 }
            rest = rest[hourRange.upperBound...]
        }
        if let minuteRange = rest.range(of: "min") {
            let minutes = Int(rest[..<minuteRange.lowerBound].trimmingCharacters(in: .whitespaces))
            if let minutes = minutes { totalMinutes += minutes }
        }
        return totalMinutes > 0 ? totalMinutes : nil
    }
}
