import Foundation

/// Tiny regex-based HTML helpers backing `HTMLParser` (the `/` listing
/// page parser). Common primitive needs: find anchor offsets to slice
/// on, pull a first-group capture out of a chunk, read a named
/// attribute out of an opening tag.
///
/// Shared with `ShowingsParser` / `RatingsParser` (the per-block
/// sub-parsers `HTMLParser` delegates to), so they don't carry parallel
/// copies of these (per CLAUDE.md "extract repeated patterns").
enum HTMLPrimitives {

    /// Every starting index where `needle` occurs in `haystack`, in
    /// source order. Used to split a document into per-anchor chunks
    /// (one film, one day, one cinema) by treating "this anchor to the
    /// next" as the slice.
    static func ranges(of needle: String, in haystack: String) -> [String.Index] {
        var result: [String.Index] = []
        var searchStart = haystack.startIndex
        while searchStart < haystack.endIndex,
              let matchRange = haystack.range(of: needle, range: searchStart..<haystack.endIndex) {
            result.append(matchRange.lowerBound)
            searchStart = matchRange.upperBound
        }
        return result
    }

    /// First regex match's first capture group, or nil if the pattern
    /// doesn't match. `.dotMatchesLineSeparators` so multi-line chunks
    /// (e.g. a `<div>` that wraps lines) still match.
    static func capture(_ string: String, _ pattern: String) -> String? {
        guard let regex = try? NSRegularExpression(
            pattern: pattern,
            options: [.dotMatchesLineSeparators]
        ) else { return nil }
        let ns = string as NSString
        guard let match = regex.firstMatch(in: string, range: NSRange(location: 0, length: ns.length)),
              match.numberOfRanges >= 2 else { return nil }
        let matchRange = match.range(at: 1)
        if matchRange.location == NSNotFound { return nil }
        return ns.substring(with: matchRange)
    }

    /// Named attribute value from a tag's attribute-blob (e.g. the
    /// substring between `<a ` and `>`). Attribute order doesn't
    /// matter; the regex is anchored on the attribute name.
    static func attribute(_ attrs: String, _ name: String) -> String? {
        guard let regex = try? NSRegularExpression(pattern: "\\b\(name)=\"([^\"]*)\"") else { return nil }
        let ns = attrs as NSString
        guard let match = regex.firstMatch(in: attrs, range: NSRange(location: 0, length: ns.length)),
              match.numberOfRanges >= 2 else { return nil }
        return ns.substring(with: match.range(at: 1))
    }
}
