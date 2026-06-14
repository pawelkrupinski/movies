import SwiftUI

/// Rating pills styled to match the web's two-tone shape: a colored
/// label-tab on the left ("IMDb", "FW", "RT") and a dark value-tab
/// on the right with the score in the brand colour. Metacritic is a
/// single solid square (no label tab) since web does the same.
///
/// Colours mirror `_ratingStyles.scala.html`:
///   IMDb    label #f5c518/black, value #2a2a3e/#f5c518
///   FW      label #ff6c00/white, value #2a2a3e/#ff9c4a
///   MC      single tone, bg #66cc66, fg #002200
///   RT fresh (≥60)  label #fa320a/white, value #2a2a3e/#ff7c5a
///   RT rotten      label #1a8f1a/white, value #2a2a3e/#6cd06c
struct RatingBadgesView: View {
    let ratings: Film.Ratings

    /// Pill rendering parameters. Defaults to the shipping values; the non-prod
    /// `ShowtimeTuningScreen` overrides it through the environment.
    @Environment(\.ratingPillStyle) private var style

    var body: some View {
        FlowLayout(spacing: style.interPillGap, lineSpacing: 4) {
            if let imdb = ratings.imdb {
                Badge.twoTone(
                    label:    "IMDb",
                    value:    Film.Ratings.scoreText(imdb),
                    url:      ratings.imdbURL,
                    labelBg:  Color(red: 0.961, green: 0.773, blue: 0.094),  // #f5c518
                    labelFg:  .black,
                    valueFg:  Color(red: 0.961, green: 0.773, blue: 0.094),
                    style:    style
                )
            }
            if let metascore = ratings.metascore {
                Badge.solid(
                    value: "\(metascore)",
                    url:   ratings.metacriticURL,
                    bg:    metacriticColor(metascore),
                    fg:    Color(red: 0.0, green: 0.13, blue: 0.0),           // #002200
                    style: style
                )
            }
            if let rottenTomatoes = ratings.rottenTomatoes {
                let fresh = rottenTomatoes >= 60
                Badge.twoTone(
                    label:    "RT",
                    value:    "\(rottenTomatoes)%",
                    url:      ratings.rottenTomatoesURL,
                    labelBg:  fresh ? Color(red: 0.980, green: 0.196, blue: 0.039)
                                    : Color(red: 0.102, green: 0.561, blue: 0.102),  // #fa320a / #1a8f1a
                    labelFg:  .white,
                    valueFg:  fresh ? Color(red: 1.0,   green: 0.486, blue: 0.353)
                                    : Color(red: 0.424, green: 0.816, blue: 0.424),  // #ff7c5a / #6cd06c
                    style:    style
                )
            }
            if let filmweb = ratings.filmweb {
                Badge.twoTone(
                    label:    "FW",
                    value:    Film.Ratings.scoreText(filmweb),
                    url:      ratings.filmwebURL,
                    labelBg:  Color(red: 1.0,   green: 0.424, blue: 0.0),    // #ff6c00
                    labelFg:  .white,
                    valueFg:  Color(red: 1.0,   green: 0.612, blue: 0.290),  // #ff9c4a
                    style:    style
                )
            }
        }
    }

    private func metacriticColor(_ score: Int) -> Color {
        // Mirror the rating-meta scale used on the web (`#66cc66` for
        // "must-see" >60, falling off through yellow to red below 40).
        switch score {
        case 61...:   return Color(red: 0.40, green: 0.80, blue: 0.40)        // #66cc66
        case 40...60: return Color(red: 0.95, green: 0.85, blue: 0.30)
        default:      return Color(red: 0.95, green: 0.45, blue: 0.30)
        }
    }
}

private enum Badge {
    /// Two-tab pill: colored label on the left, dark value on the
    /// right. `clipShape(RoundedRectangle)` is what gives the outer
    /// rounded corners while keeping the boundary between the two
    /// inner backgrounds straight — the same effect `.rating-imdb
    /// { border-radius: 3px; overflow: hidden }` produces on the web.
    @ViewBuilder
    static func twoTone(
        label:   String,
        value:   String,
        url:     URL?,
        labelBg: Color,
        labelFg: Color,
        valueFg: Color,
        style:   RatingPillStyle
    ) -> some View {
        let body = HStack(spacing: 0) {
            Text(label)
                .font(.system(size: style.labelFontSize, weight: style.labelWeight))
                .foregroundColor(labelFg)
                .lineLimit(1)
                .padding(.horizontal, style.labelHInset)
                .padding(.vertical, style.vInset)
                .background(labelBg)
            Text(value)
                .font(.system(size: style.valueFontSize, weight: style.valueWeight))
                .foregroundColor(valueFg)
                .lineLimit(1)
                .padding(.horizontal, style.valueHInset)
                .padding(.vertical, style.vInset)
                .background(Color(red: 0.165, green: 0.165, blue: 0.243))    // #2a2a3e
        }
        .clipShape(RoundedRectangle(cornerRadius: style.cornerRadius))
        // Pills render at their intrinsic width — never shrink the
        // text to the proposal, which is what produced "IMD…" /
        // "7…" tails on narrow grid cells. FlowLayout wraps overlong
        // pills to a new line instead.
        .fixedSize(horizontal: true, vertical: false)

        if let url {
            Link(destination: url) { body }.buttonStyle(.plain)
        } else {
            body
        }
    }

    /// Single-tab pill (Metacritic-shape) — one background, value
    /// only. Matches web's `.rating-meta { background: #66cc66 }`
    /// with no label tab.
    @ViewBuilder
    static func solid(value: String, url: URL?, bg: Color, fg: Color, style: RatingPillStyle) -> some View {
        let body = Text(value)
            .font(.system(size: style.valueFontSize, weight: style.solidWeight))
            .foregroundColor(fg)
            .lineLimit(1)
            .padding(.horizontal, style.valueHInset)
            .padding(.vertical, style.vInset)
            .background(bg, in: RoundedRectangle(cornerRadius: style.cornerRadius))
            .fixedSize(horizontal: true, vertical: false)

        if let url {
            Link(destination: url) { body }.buttonStyle(.plain)
        } else {
            body
        }
    }
}
