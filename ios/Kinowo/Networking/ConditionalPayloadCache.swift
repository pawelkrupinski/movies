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
/// The entry is ONE file, written atomically: a header line (deployment, city,
/// `Last-Modified` as JSON) followed by the payload. A body and its origin can
/// therefore never come from two different saves — the separate body + meta
/// files older builds wrote could, when two saves interleaved or the app died
/// between the two writes, and then served one city's films under another's
/// stamp. Those files are no longer read (and are deleted on the next save),
/// so they cost one full response, never a wrong one. Every read and write
/// runs on this cache's own serial queue in the order it was issued, so an
/// older city's background save can't land after a newer one's, and a read
/// sees every save issued before it.
///
/// That ordering holds per INSTANCE, which is all it needs to: each file has
/// one owner — the store the app root builds holds its endpoint's cache for
/// its whole life — and caches of different files have nothing to order
/// against each other, so one's slow write never holds up another's read.
/// Two instances over the same file would not be ordered against each other.
///
/// The DIRECTORY is the caller's: the app root passes `defaultDirectory` (the
/// user's caches), a test a fresh directory of its own. It has no default, so
/// no test can reach the real caches — and the entry a concurrent run, or the
/// developer's own simulator session, keeps there — by leaving it out.
struct ConditionalPayloadCache<Payload: Codable> {
    private let directory: URL
    private let file: String
    private let legacyFiles: [String]
    /// Every file access of this cache, in issue order. A reference, so copies
    /// of the struct share it.
    let queue: DispatchQueue

    init(directory: URL, file: String, legacyFiles: [String] = []) {
        self.directory = directory
        self.file = file
        self.legacyFiles = legacyFiles
        queue = DispatchQueue(label: "kinowo.conditional-payload-cache.\(file)", qos: .utility)
    }

    private struct Header: Codable {
        let deployment: String
        let city: String
        let lastModified: String?

        func matches(deployment: URL, city: String) -> Bool {
            self.deployment == deployment.absoluteString && self.city == city
        }
    }

    /// Where the app keeps its entries: the user's caches directory.
    static var defaultDirectory: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    }
    private var url: URL { directory.appendingPathComponent(file) }

    /// Persist `body` — the payload exactly as the server sent it, already
    /// encoded, so nothing is re-encoded and the queue holds only the file
    /// write — for `deployment` + `city` with its `lastModified`, so a later
    /// reload of that same pair can revalidate. Off the caller's thread, after
    /// every save issued before it; a read issued after it sees it.
    func saveInBackground(body: Data, deployment: URL, city: String, lastModified: String?) {
        guard let entry = Self.entry(body: body, deployment: deployment, city: city, lastModified: lastModified)
        else { return }
        queue.async { write(entry) }
    }

    /// Forget the entry.
    func remove() {
        queue.sync { try? FileManager.default.removeItem(at: url) }
    }

    /// The one-file entry: header line, newline, encoded payload.
    private static func entry(body: Data, deployment: URL, city: String, lastModified: String?) -> Data? {
        let header = Header(deployment: deployment.absoluteString, city: city, lastModified: lastModified)
        guard let headerData = try? JSONEncoder().encode(header) else { return nil }
        return headerData + Data("\n".utf8) + body
    }

    /// Only ever on `queue`.
    private func write(_ entry: Data) {
        try? entry.write(to: url, options: .atomic)
        for legacy in legacyFiles {
            try? FileManager.default.removeItem(at: directory.appendingPathComponent(legacy))
        }
    }

    /// The cached body, but only when it belongs to `deployment` + `city` —
    /// otherwise nil, so a switch shows nothing rather than another country's
    /// (or another city's) films while the real fetch is in flight.
    func load(deployment: URL, city: String) -> [Payload]? {
        guard let (header, body) = entry(), header.matches(deployment: deployment, city: city) else { return nil }
        return try? JSONDecoder().decode([Payload].self, from: body)
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
    /// Asked before every fetch, so it reads only the header line, and on this
    /// cache's queue without holding the caller (the main actor) while a
    /// save of the previous response is still being written.
    func lastModified(deployment: URL, city: String) async -> String? {
        await withCheckedContinuation { (cont: CheckedContinuation<String?, Never>) in
            queue.async {
                guard let header = readHeader(), header.matches(deployment: deployment, city: city),
                      let value = header.lastModified, !value.isEmpty else { return cont.resume(returning: nil) }
                cont.resume(returning: value)
            }
        }
    }

    /// The header and the (still encoded) payload: the whole file is read, but
    /// only the header line is decoded here, the payload left to the caller.
    private func entry() -> (Header, Data)? {
        guard let data = queue.sync(execute: { try? Data(contentsOf: url) }),
              let newline = data.firstIndex(of: UInt8(ascii: "\n")),
              let header = try? JSONDecoder().decode(Header.self, from: data[..<newline]) else { return nil }
        return (header, data[data.index(after: newline)...])
    }

    /// Just the header line, read a chunk at a time up to its newline — never
    /// the listing behind it. Only ever on `queue`.
    private func readHeader() -> Header? {
        guard let handle = try? FileHandle(forReadingFrom: url) else { return nil }
        defer { try? handle.close() }
        var line = Data()
        while line.count < Self.maxHeaderBytes, let chunk = try? handle.read(upToCount: 4096), !chunk.isEmpty {
            if let newline = chunk.firstIndex(of: UInt8(ascii: "\n")) {
                line.append(chunk[..<newline])
                return try? JSONDecoder().decode(Header.self, from: line)
            }
            line.append(chunk)
        }
        return nil
    }

    /// A header is a deployment URL, a city slug and a date — far under this.
    /// Past it the file is not an entry this cache wrote.
    private static var maxHeaderBytes: Int { 64 * 1024 }
}

// The two endpoints' caches. A file per endpoint so their conditional-GET
// state never collides. Factories, not shared values: each call builds a new
// cache with its own queue, for the one store that owns it.
extension ConditionalPayloadCache where Payload == Film {
    /// `/{city}/api/repertoire`, kept in `directory`.
    static func repertoire(in directory: URL) -> Self {
        .init(directory: directory, file: "repertoire-entry.json", legacyFiles: ["repertoire.json", "repertoire-meta.txt"])
    }
}

extension ConditionalPayloadCache where Payload == FilmDetails {
    /// `/{city}/api/details`, kept in `directory`.
    static func details(in directory: URL) -> Self {
        .init(directory: directory, file: "details-entry.json", legacyFiles: ["details.json", "details-meta.txt"])
    }
}
