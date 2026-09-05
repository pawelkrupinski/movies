import Foundation

/// On-disk cache of one endpoint's list payload, together with the DEPLOYMENT
/// and CITY it was fetched for and that response's `Last-Modified`. A cold
/// start paints instantly off disk, and a warm reload of the same
/// deployment+city issues a conditional GET.
///
/// The cached `Last-Modified` is bound to BOTH halves of its origin. The server
/// now stamps one timestamp PER CITY rather than a single global one, which
/// makes this binding more load-bearing rather than less: two cities' stamps
/// genuinely differ and are freely ordered against each other, so a replayed
/// one draws a 304 whenever it happens to be the later of the two. Neither
/// stamp says anything about which deployment answered:
///
/// - **City.** Replaying poznań's timestamp while fetching warszawa draws a 304
///   and strands the grid on the old city's films.
/// - **Deployment.** The same slug can exist on two deployments, and asking the
///   wrong one is not an error: `kinowo.net/berlin/api/repertoire` answers
///   `200 []` because Berlin simply isn't a Polish city. Cache that empty body,
///   then replay its timestamp against `de.showtimes.cc`, and Germany
///   answers 304 — leaving the grid empty on a city that has a full listing.
///   That is what made a deep link into another country's city come up as
///   "no screenings" until the entry aged out.
///
/// So a switch along EITHER axis sends no conditional header and takes a fresh
/// 200, and `load` hands back a body only for the pair that produced it.
///
/// The meta file is three lines — deployment, city, `Last-Modified`. A file
/// written by an older build has only two, which no longer matches any pair, so
/// it reads as "nothing cached" and the next fetch is unconditional: stale
/// entries cost one full response, never a wrong one.
struct ConditionalPayloadCache<Payload: Codable> {
    private let bodyFile: String
    private let metaFile: String

    init(bodyFile: String, metaFile: String) {
        self.bodyFile = bodyFile
        self.metaFile = metaFile
    }

    private var cacheDir: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    }
    private var bodyURL: URL { cacheDir.appendingPathComponent(bodyFile) }
    private var metaURL: URL { cacheDir.appendingPathComponent(metaFile) }

    /// Persist the freshly-fetched `payload` for `deployment` + `city` together
    /// with its `lastModified` header, so a later reload of that same pair can
    /// revalidate.
    func save(_ payload: [Payload], deployment: URL, city: String, lastModified: String?) {
        if let data = try? JSONEncoder().encode(payload) {
            try? data.write(to: bodyURL, options: .atomic)
        }
        let meta = [deployment.absoluteString, city, lastModified ?? ""].joined(separator: "\n")
        try? meta.write(to: metaURL, atomically: true, encoding: .utf8)
    }

    /// The cached body, but only when it belongs to `deployment` + `city` —
    /// otherwise nil, so a switch shows nothing rather than another country's
    /// (or another city's) films while the real fetch is in flight.
    func load(deployment: URL, city: String) -> [Payload]? {
        guard matches(deployment: deployment, city: city),
              let data = try? Data(contentsOf: bodyURL) else { return nil }
        return try? JSONDecoder().decode([Payload].self, from: data)
    }

    /// The body to adopt when the server answers **304 Not Modified**, or nil
    /// to keep what the caller already has.
    ///
    /// A 304 vouches for the CACHED entry, which is only the same thing as
    /// "the caller's copy is current" if the caller actually read that entry
    /// in. It may not have: the disk read happens at launch, before a deep
    /// link re-points the store, so it can be skipped for the wrong city and
    /// leave the caller empty. Taking 304 at face value then strands an empty
    /// listing on a city that has a full one — so hand back the entry the
    /// conditional header spoke for whenever the caller is holding nothing.
    func bodyForNotModified(callerIsEmpty: Bool, deployment: URL, city: String) -> [Payload]? {
        guard callerIsEmpty else { return nil }
        return load(deployment: deployment, city: city)
    }

    /// The `Last-Modified` to replay as `If-Modified-Since`, but only when the
    /// cached body belongs to `deployment` + `city`; nil for any other pair.
    func lastModified(deployment: URL, city: String) -> String? {
        guard matches(deployment: deployment, city: city) else { return nil }
        let value = meta().count > 2 ? meta()[2] : ""
        return value.isEmpty ? nil : value
    }

    private func matches(deployment: URL, city: String) -> Bool {
        let lines = meta()
        guard lines.count >= 2 else { return false }
        return lines[0] == deployment.absoluteString && lines[1] == city
    }

    private func meta() -> [String] {
        guard let text = try? String(contentsOf: metaURL, encoding: .utf8) else { return [] }
        return text.components(separatedBy: "\n")
    }
}
