import Foundation

/// Per-film detail payload from `GET /api/details`. The backend only
/// includes a film here when it has a synopsis or at least one trailer,
/// so the details map is sparse — most lookups for a given listing
/// title legitimately return `nil`.
///
/// `title` is the merge key: it equals `Film.title` exactly, so the
/// detail screen joins by title rather than carrying an id.
///
/// `trailerURLs` arrive as ready-to-embed YouTube/Vimeo embed URLs
/// (the same shape iOS previously scraped from the `/film` page's
/// `playTrailer(...)` onclick), so they flow straight into
/// `TrailerEmbedHTML` for inline playback.
struct FilmDetails: Hashable, Codable {
    let title: String
    /// Original (production-language) title, e.g. the English name of a film
    /// listed under a Polish cinema title. The backend only sends this when
    /// it genuinely differs from `title`, so it can be shown verbatim.
    let originalTitle: String?
    let synopsis: String?
    let trailerURLs: [URL]

    enum CodingKeys: String, CodingKey {
        case title, originalTitle, synopsis, trailerURLs
    }

    init(title: String, originalTitle: String? = nil, synopsis: String?, trailerURLs: [URL]) {
        self.title = title
        self.originalTitle = originalTitle
        self.synopsis = synopsis
        self.trailerURLs = trailerURLs
    }

    init(from decoder: Decoder) throws {
        let c = try decoder.container(keyedBy: CodingKeys.self)
        self.title = try c.decode(String.self, forKey: .title)
        self.originalTitle = try c.decodeIfPresent(String.self, forKey: .originalTitle)
        self.synopsis = try c.decodeIfPresent(String.self, forKey: .synopsis)
        // Mirror how `Film` decodes URLs: drop any string that isn't a
        // valid URL rather than failing the whole row. A malformed
        // trailer link shouldn't sink the synopsis alongside it.
        let raw = try c.decodeIfPresent([String].self, forKey: .trailerURLs) ?? []
        self.trailerURLs = raw.compactMap { URL(string: $0) }
    }
}

extension Array where Element == FilmDetails {
    /// Build the title → details lookup used by `DetailsStore`. The last
    /// row wins on a duplicate title; the backend doesn't emit dupes,
    /// but a stable rule keeps the fake and real stores in agreement.
    func keyedByTitle() -> [String: FilmDetails] {
        Dictionary(map { ($0.title, $0) }, uniquingKeysWith: { _, new in new })
    }
}
