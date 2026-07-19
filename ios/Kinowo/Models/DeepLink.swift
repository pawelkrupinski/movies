import Foundation

/// A parsed deep link into the app.
///
/// The grammar mirrors the web URLs one-for-one so the SAME links — on any
/// country deployment (`kinowo.fly.dev`, `showtimes-uk.fly.dev`,
/// `showtimes-de.fly.dev`) — open the app via Universal Links, including the
/// copy-to-clipboard filter links, whose query string we decode back into
/// `DeepLinkFilters`. The `kinowo://` custom scheme is accepted too (host =
/// city slug), so an internal or fallback link works without the
/// associated-domain round trip.
///
///   https://kinowo.fly.dev/poznan/                     → city
///   https://kinowo.fly.dev/poznan/?dim=2D&genre=Komedia → city + filters
///   https://kinowo.fly.dev/poznan/film?title=Oppenheimer → city + film detail
///   https://showtimes-uk.fly.dev/london/                → city (UK deployment)
///   kinowo://poznan/                                    → city (custom scheme)
///   kinowo://poznan/film?title=Oppenheimer              → film (custom scheme)
///
/// Pure Foundation so it lives in `KinowoCore` and is unit-tested on Linux CI.
struct DeepLink: Equatable {
    let citySlug: String
    /// Set for `/:city/film?title=…`; the title is the film's identity (its
    /// `id`), matched against the loaded repertoire once it arrives.
    let filmTitle: String?
    let filters: DeepLinkFilters

    /// Every country deployment's host — a link on any of them opens the app.
    /// Mirrors the `baseURL` hosts of `Country.all` (PL/UK/DE); keep in sync
    /// when a country is added. (`Country` lives in a different SPM target, so
    /// this can't derive from `Country.all` directly.)
    static let webHosts: Set<String> = [
        "kinowo.fly.dev", "www.kinowo.fly.dev",
        "showtimes-uk.fly.dev",
        "showtimes-de.fly.dev",
    ]
    /// Reserved custom-scheme host already used for the OAuth callback — never a
    /// city, so never a navigation deep link.
    static let reservedSchemeHosts: Set<String> = ["auth-done"]

    /// Parse a Universal Link / custom-scheme URL into a destination, or `nil`
    /// when it isn't a recognisable city link. Returning `nil` (rather than a
    /// best guess) lets `onOpenURL` no-op on anything unexpected instead of
    /// navigating somewhere wrong.
    ///
    /// `knownCitySlugs` defaults to the app's `City.all`; an unknown first
    /// segment (`/auth/…`, `/uptime`, a city the build doesn't know) is rejected
    /// so we never treat a non-city path as a city.
    static func parse(_ url: URL, knownCitySlugs: Set<String> = Set(City.all.map(\.slug))) -> DeepLink? {
        guard let components = URLComponents(url: url, resolvingAgainstBaseURL: false),
              let scheme = components.scheme?.lowercased() else { return nil }

        // Path segments that follow the city slug (e.g. ["film"]).
        let city: String
        let trailing: [String]
        switch scheme {
        case "https", "http":
            guard let host = components.host?.lowercased(), webHosts.contains(host) else { return nil }
            let segments = pathSegments(components.path)
            guard let first = segments.first else { return nil }
            city = first
            trailing = Array(segments.dropFirst())
        case "kinowo":
            guard let host = components.host?.lowercased(), !reservedSchemeHosts.contains(host) else { return nil }
            city = host
            trailing = pathSegments(components.path)
        default:
            return nil
        }

        guard knownCitySlugs.contains(city) else { return nil }

        let filmTitle: String? = trailing.first == "film"
            ? components.queryItems?.first(where: { $0.name == "title" })?.value
            : nil

        return DeepLink(
            citySlug: city,
            filmTitle: filmTitle.flatMap { $0.isEmpty ? nil : $0 },
            filters: DeepLinkFilters(queryItems: components.queryItems ?? [])
        )
    }

    private static func pathSegments(_ path: String) -> [String] {
        path.split(separator: "/").map(String.init)
    }
}

/// The filter state a link can carry, decoded from the query string the web's
/// `buildShareURL()` emits. Scalar axes (`date`, `q`, `dim`, `lang`, `imax`,
/// `from`, `sort`) map straight onto the app's filter state. The multi-value
/// axes (`country` / `genre` / `director` / `cast` / `room`) and `cinema` are
/// stored as the web emits them — the INCLUSION set of values to KEEP — and
/// converted to the app's exclusion model via `excluded(_:universe:)` once the
/// repertoire (hence the value universe) is known.
struct DeepLinkFilters: Equatable {
    var date: DateFilter?
    var query: String?
    var dimension: String?     // "2D" | "3D"
    var language: String?      // "NAP" | "DUB"
    var imax: Bool?
    var fromHour: Int?
    var fromMinute: Int?
    var sort: SortOption?

    var includedCountries: [String] = []
    var includedGenres: [String] = []
    var includedDirectors: [String] = []
    var includedCast: [String] = []
    /// `nil` = the `cinema` param was absent (leave the user's cinema choice
    /// alone); non-nil = the explicit set of ENABLED cinemas to keep.
    var enabledCinemas: [String]?

    static let empty = DeepLinkFilters()

    var isEmpty: Bool { self == DeepLinkFilters.empty }

    init() {}

    init(queryItems: [URLQueryItem]) {
        func first(_ key: String) -> String? {
            queryItems.first(where: { $0.name == key })?.value.flatMap { $0.isEmpty ? nil : $0 }
        }
        // Repeated params AND legacy comma-lists both flatten to one set, so an
        // old `?genre=A,B` link narrows the same as a fresh `?genre=A&genre=B`.
        func all(_ key: String) -> [String] {
            queryItems.filter { $0.name == key }
                .compactMap(\.value)
                .flatMap { $0.split(separator: ",").map(String.init) }
        }

        date = first("date").flatMap(DeepLinkFilters.parseDate)
        query = first("q")
        dimension = first("dim").flatMap { ["2D", "3D"].contains($0) ? $0 : nil }
        language = first("lang").flatMap { ["NAP", "DUB"].contains($0) ? $0 : nil }
        imax = queryItems.contains { $0.name == "imax" } ? (first("imax") == "1") : nil
        if let from = first("from"), let parsed = DeepLinkFilters.parseFrom(from) {
            fromHour = parsed.hour
            fromMinute = parsed.minute
        }
        sort = first("sort").flatMap(SortOption.init(rawValue:))

        includedCountries = all("country")
        includedGenres = all("genre")
        includedDirectors = all("director")
        includedCast = all("cast")
        enabledCinemas = queryItems.contains { $0.name == "cinema" } ? all("cinema") : nil
    }

    /// The `FormatFilter` these scalar axes describe (the multi-value axes are
    /// applied separately as exclusions). Absent axes fall back to the supplied
    /// `base` so a link that sets only `dim` doesn't wipe an unrelated axis.
    func formatFilter(base: FormatFilter = .empty) -> FormatFilter {
        var f = base
        if let dimension { f.dimension = dimension }
        if let language { f.language = language }
        if let imax { f.imax = imax }
        if let fromHour {
            f.fromHour = fromHour
            f.fromMinute = fromMinute ?? 0
        }
        return f
    }

    /// Convert one INCLUSION list (values to keep) into the app's EXCLUSION set
    /// given the full universe of values in the loaded repertoire: exclude every
    /// value that isn't in the keep-list. An empty keep-list means "no
    /// constraint" → no exclusions, matching the web's all-checked default.
    func excluded(_ included: [String], universe: Set<String>) -> Set<String> {
        included.isEmpty ? [] : universe.subtracting(included)
    }

    /// The set of cinemas to DISABLE: every known cinema not in the enabled
    /// list. `nil` when the `cinema` param was absent (don't touch the choice).
    func disabledCinemas(allCinemas: Set<String>) -> Set<String>? {
        enabledCinemas.map { allCinemas.subtracting($0) }
    }

    private static func parseDate(_ value: String) -> DateFilter? {
        switch value {
        case "today": return .today
        case "tomorrow": return .tomorrow
        case "week": return .week
        case "anytime": return .anytime
        default:
            let iso = #/^\d{4}-\d{2}-\d{2}$/#
            return value.wholeMatch(of: iso) != nil ? .specific(value) : nil
        }
    }

    private static func parseFrom(_ value: String) -> (hour: Int, minute: Int)? {
        let parts = value.split(separator: ":").map(String.init)
        guard parts.count == 2, let h = Int(parts[0]), let m = Int(parts[1]),
              (0...23).contains(h), (0...59).contains(m) else { return nil }
        return (h, m)
    }
}

/// Title matching for deep links, mirroring the server's
/// `TitleNormalizer.normalize`. The web's film page matches a URL's `?title=`
/// against schedules by normalized title (Arabic→Roman numeral fold,
/// "…Prady 2" ⇄ "…Prady II"), NOT byte-for-byte — the linked title and the
/// stored title diverge for numbered films. The app must match the same way or
/// a deep link to a sequel opens the app but never the film page.
///
/// Keep in sync with `TitleNormalizer.normalize` / its `ArabicToRoman` map.
enum DeepLinkTitle {
    private static let arabicToRoman: [String: String] = [
        "1": "I", "2": "II", "3": "III", "4": "IV", "5": "V",
        "6": "VI", "7": "VII", "8": "VIII", "9": "IX", "10": "X",
        "11": "XI", "12": "XII", "13": "XIII", "14": "XIV", "15": "XV",
        "16": "XVI", "17": "XVII", "18": "XVIII", "19": "XIX", "20": "XX",
    ]

    /// Standalone Arabic numerals → Roman, word by word.
    static func normalize(_ title: String) -> String {
        title.split(separator: " ", omittingEmptySubsequences: false)
            .map { arabicToRoman[String($0)] ?? String($0) }
            .joined(separator: " ")
    }

    /// Whether two titles refer to the same film under the server's normalize.
    static func matches(_ a: String, _ b: String) -> Bool { normalize(a) == normalize(b) }
}
