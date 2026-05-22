import Foundation

/// Parses the `<div class="ratings">` block emitted by
/// `_ratingBadges.scala.html`. Identical markup on the listing card
/// and on `/film`, so the same parser serves both pages.
enum RatingsParser {

    static func parseRatings(in chunk: String) -> Film.Ratings {
        // Each rating <a> has its `class="rating-…"` AFTER the href; both
        // orderings of attributes occur in HTML in general, but `_ratingBadges`
        // always emits `href` first, so the cheap regex is fine.
        let imdbURL = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-imdb""#)
                          .flatMap { URL(string: $0) }
        let imdb    = HTMLPrimitives.capture(chunk, #"class="rating-imdb-value">([^<]+)</span>"#)
                          .flatMap { Double($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        let mcURL   = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-meta""#)
                          .flatMap { URL(string: $0) }
        // rating-meta wraps the score in the anchor body directly.
        let mc      = HTMLPrimitives.capture(chunk, #"class="rating-meta"[^>]*onclick="event.stopPropagation\(\)">([^<]+)</a>"#)
                          .flatMap { Int($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        let rtURL   = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-rt[^"]*""#)
                          .flatMap { URL(string: $0) }
        let rt      = HTMLPrimitives.capture(chunk, #"class="rating-rt-value">([0-9]+)%</span>"#)
                          .flatMap { Int($0) }
        let fwURL   = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-fw""#)
                          .flatMap { URL(string: $0) }
        let fw      = HTMLPrimitives.capture(chunk, #"class="rating-fw-value">([^<]+)</span>"#)
                          .flatMap { Double($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        return Film.Ratings(
            imdb: imdb, imdbURL: imdbURL,
            metascore: mc, metacriticURL: mcURL,
            rottenTomatoes: rt, rottenTomatoesURL: rtURL,
            filmweb: fw, filmwebURL: fwURL
        )
    }
}
