import Foundation

/// Tiny regex-based HTML helpers shared by `HTMLParser` (listing page)
/// and `FilmDetailParser` (`/film` page). Both pages are slices of the
/// same Twirl-templated site, so they share the same primitive needs:
/// find anchor offsets to slice on, pull a first-group capture out of a
/// chunk, read a named attribute out of an opening tag.
///
/// Extracted here so the two parsers don't carry parallel copies of
/// these (per CLAUDE.md "extract repeated patterns at two uses").
enum HTMLPrimitives {

    /// Every starting index where `needle` occurs in `haystack`, in
    /// source order. Used to split a document into per-anchor chunks
    /// (one film, one day, one cinema) by treating "this anchor to the
    /// next" as the slice.
    static func ranges(of needle: String, in haystack: String) -> [String.Index] {
        var result: [String.Index] = []
        var searchStart = haystack.startIndex
        while searchStart < haystack.endIndex,
              let r = haystack.range(of: needle, range: searchStart..<haystack.endIndex) {
            result.append(r.lowerBound)
            searchStart = r.upperBound
        }
        return result
    }

    /// First regex match's first capture group, or nil if the pattern
    /// doesn't match. `.dotMatchesLineSeparators` so multi-line chunks
    /// (e.g. a `<div>` that wraps lines) still match.
    static func capture(_ string: String, _ pattern: String) -> String? {
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

    /// Named attribute value from a tag's attribute-blob (e.g. the
    /// substring between `<a ` and `>`). Attribute order doesn't
    /// matter; the regex is anchored on the attribute name.
    static func attribute(_ attrs: String, _ name: String) -> String? {
        guard let re = try? NSRegularExpression(pattern: "\\b\(name)=\"([^\"]*)\"") else { return nil }
        let ns = attrs as NSString
        guard let m = re.firstMatch(in: attrs, range: NSRange(location: 0, length: ns.length)),
              m.numberOfRanges >= 2 else { return nil }
        return ns.substring(with: m.range(at: 1))
    }
}
