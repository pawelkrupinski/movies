import Foundation

extension String {
    /// Cheap HTML-entity decoder. Covers what kinowo's template can emit:
    /// `&amp;`, `&quot;`, `&#39;`, `&lt;`, `&gt;`, `&nbsp;` plus numeric
    /// `&#N;` / `&#xH;` entities. Skips the full HTML decoder
    /// (NSAttributedString + WebKit) — overkill for plain attribute values
    /// and noticeably slower on launch.
    func htmlDecoded() -> String {
        guard self.contains("&") else { return self }
        var out = String()
        out.reserveCapacity(self.count)
        var i = self.startIndex
        while i < self.endIndex {
            let c = self[i]
            if c != "&" {
                out.append(c)
                i = self.index(after: i)
                continue
            }
            // Look ahead for `;` within a short window — entities are
            // small. If absent, treat the `&` literally.
            let limit = self.index(i, offsetBy: 12, limitedBy: self.endIndex) ?? self.endIndex
            guard let semi = self.range(of: ";", range: i..<limit) else {
                out.append(c)
                i = self.index(after: i)
                continue
            }
            let entity = String(self[i...semi.lowerBound])
            if let replacement = HTMLEntities.decode(entity) {
                out.append(replacement)
                i = semi.upperBound
            } else {
                out.append(c)
                i = self.index(after: i)
            }
        }
        return out
    }
}

private enum HTMLEntities {
    static let named: [String: String] = [
        "&amp;": "&",
        "&lt;": "<",
        "&gt;": ">",
        "&quot;": "\"",
        "&apos;": "'",
        "&#39;": "'",
        "&nbsp;": " "
    ]

    static func decode(_ entity: String) -> String? {
        if let s = named[entity] { return s }
        // Numeric `&#NNN;` or hex `&#xHH;`.
        guard entity.hasPrefix("&#"), entity.hasSuffix(";") else { return nil }
        let inside = entity.dropFirst(2).dropLast()
        let scalarValue: UInt32?
        if inside.first == "x" || inside.first == "X" {
            scalarValue = UInt32(inside.dropFirst(), radix: 16)
        } else {
            scalarValue = UInt32(inside)
        }
        guard let v = scalarValue, let scalar = Unicode.Scalar(v) else { return nil }
        return String(scalar)
    }
}
