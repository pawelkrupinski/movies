import Foundation

/// Parses the `<div class="ratings">` block emitted by
/// `_ratingBadges.scala.html` on the listing card. Used by `HTMLParser`
/// for the home listing; ratings on the detail screen come from the
/// listing `Film`'s `ratings` field (populated by `/api/repertoire`).
enum RatingsParser {

    static func parseRatings(in chunk: String) -> Film.Ratings {
        // Each rating <a> has its `class="rating-…"` AFTER the href; both
        // orderings of attributes occur in HTML in general, but `_ratingBadges`
        // always emits `href` first, so the cheap regex is fine.
        let imdbURL = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-imdb""#)
                          .flatMap { URL(string: $0) }
        let imdb    = HTMLPrimitives.capture(chunk, #"class="rating-imdb-value">([^<]+)</span>"#)
                          .flatMap { Double($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        let metacriticURL   = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-meta""#)
                          .flatMap { URL(string: $0) }
        // rating-meta wraps the score in the anchor body directly. Match up
        // to the closing `>` of the `<a>` open tag, then take the body.
        // The earlier version anchored on `onclick="event.stopPropagation()"`
        // — that attribute was removed from the template (commit 6372afc),
        // so the regex silently stopped matching and MC pills disappeared
        // from every card.
        let metascore      = HTMLPrimitives.capture(chunk, #"class="rating-meta"[^>]*>([^<]+)</a>"#)
                          .flatMap { Int($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        let rottenTomatoesURL   = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-rt[^"]*""#)
                          .flatMap { URL(string: $0) }
        let rottenTomatoes      = HTMLPrimitives.capture(chunk, #"class="rating-rt-value">([0-9]+)%</span>"#)
                          .flatMap { Int($0) }
        let filmwebURL   = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="rating-fw""#)
                          .flatMap { URL(string: $0) }
        let filmweb      = HTMLPrimitives.capture(chunk, #"class="rating-fw-value">([^<]+)</span>"#)
                          .flatMap { Double($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        return Film.Ratings(
            imdb: imdb, imdbURL: imdbURL,
            metascore: metascore, metacriticURL: metacriticURL,
            rottenTomatoes: rottenTomatoes, rottenTomatoesURL: rottenTomatoesURL,
            filmweb: filmweb, filmwebURL: filmwebURL
        )
    }
}
