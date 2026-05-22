import Foundation

/// Parses the `<div class="date-group">` → `<div class="cinema-group">`
/// → `<a class="badge-time">` tree that both the listing card
/// (`_filmShowings` inside a film card) and the detail page
/// (`/film`'s `showtimes-section`) emit verbatim. Identical DOM on
/// both pages, so one parser serves both.
enum ShowingsParser {

    /// Every `date-group` in the chunk, in source order. The chunk can
    /// be a per-film card slice (listing) or the whole `/film` body
    /// (detail) — `ranges(of:)` finds the anchors regardless.
    static func parseShowings(in chunk: String) -> [DayShowings] {
        let dayStarts = HTMLPrimitives.ranges(of: "<div class=\"date-group\" data-date=\"", in: chunk)
        var days: [DayShowings] = []
        days.reserveCapacity(dayStarts.count)
        for (i, start) in dayStarts.enumerated() {
            let end = (i + 1 < dayStarts.count) ? dayStarts[i + 1] : chunk.endIndex
            let dayChunk = String(chunk[start..<end])
            guard let date = HTMLPrimitives.capture(dayChunk, #"data-date="([^"]+)""#) else { continue }
            let label = HTMLPrimitives.capture(dayChunk, #"<div class="date-label">([^<]+)</div>"#)?
                .trimmingCharacters(in: .whitespacesAndNewlines) ?? date
            let cinemas = parseCinemas(dayChunk)
            if cinemas.isEmpty { continue }
            days.append(DayShowings(date: date, label: label, cinemas: cinemas))
        }
        return days
    }

    private static func parseCinemas(_ dayChunk: String) -> [CinemaShowings] {
        let cinemaStarts = HTMLPrimitives.ranges(of: "<div class=\"cinema-group\" data-cinema=\"", in: dayChunk)
        var out: [CinemaShowings] = []
        out.reserveCapacity(cinemaStarts.count)
        for (i, start) in cinemaStarts.enumerated() {
            let end = (i + 1 < cinemaStarts.count) ? cinemaStarts[i + 1] : dayChunk.endIndex
            let chunk = String(dayChunk[start..<end])
            guard let raw = HTMLPrimitives.capture(chunk, #"data-cinema="([^"]+)""#) else { continue }
            let name = raw.htmlDecoded()
            let url = HTMLPrimitives.capture(chunk, #"<a href="([^"]+)"[^>]*class="cinema-label-link""#)
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
            guard let time = HTMLPrimitives.attribute(attrs, "data-time") else { continue }
            let format = HTMLPrimitives.attribute(attrs, "data-format") ?? ""
            let room   = HTMLPrimitives.attribute(attrs, "data-room")
            let booking = HTMLPrimitives.attribute(attrs, "href").flatMap { URL(string: $0.htmlDecoded()) }
            out.append(Showtime(time: time, format: format, room: room, bookingURL: booking))
        }
        return out
    }
}
