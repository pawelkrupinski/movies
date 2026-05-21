import SwiftUI

struct RatingBadgesView: View {
    let ratings: Film.Ratings

    var body: some View {
        FlowLayout(spacing: 4, lineSpacing: 4) {
            if let imdb = ratings.imdb {
                Badge(label: "IMDb",
                      value: String(format: "%.1f", imdb),
                      url: ratings.imdbURL,
                      background: Color(red: 0.96, green: 0.78, blue: 0.0).opacity(0.18),
                      foreground: Color(red: 0.96, green: 0.78, blue: 0.0))
            }
            if let mc = ratings.metascore {
                Badge(label: nil,
                      value: "\(mc)",
                      url: ratings.metacriticURL,
                      background: metacriticColor(mc).opacity(0.22),
                      foreground: metacriticColor(mc))
            }
            if let rt = ratings.rottenTomatoes {
                let fresh = rt >= 60
                Badge(label: "RT",
                      value: "\(rt)%",
                      url: ratings.rottenTomatoesURL,
                      background: (fresh ? Color.red : Color.green).opacity(0.20),
                      foreground: fresh ? Color(red: 0.95, green: 0.45, blue: 0.30)
                                        : Color(red: 0.55, green: 0.85, blue: 0.40))
            }
            if let fw = ratings.filmweb {
                Badge(label: "FW",
                      value: String(format: "%.1f", fw),
                      url: ratings.filmwebURL,
                      background: Color(red: 0.20, green: 0.40, blue: 0.80).opacity(0.22),
                      foreground: Color(red: 0.60, green: 0.85, blue: 1.0))
            }
        }
    }

    private func metacriticColor(_ score: Int) -> Color {
        switch score {
        case 61...:   return Color(red: 0.40, green: 0.85, blue: 0.50)
        case 40...60: return Color(red: 0.95, green: 0.85, blue: 0.30)
        default:      return Color(red: 0.95, green: 0.45, blue: 0.30)
        }
    }
}

private struct Badge: View {
    let label: String?
    let value: String
    let url: URL?
    let background: Color
    let foreground: Color

    var body: some View {
        let body = HStack(spacing: 3) {
            if let l = label {
                Text(l).font(.system(size: 9, weight: .heavy))
            }
            Text(value).font(.system(size: 11, weight: .semibold))
        }
        .foregroundStyle(foreground)
        .padding(.horizontal, 6)
        .padding(.vertical, 2)
        .background(background, in: RoundedRectangle(cornerRadius: 4))

        if let url {
            Link(destination: url) { body }
                .buttonStyle(.plain)
        } else {
            body
        }
    }
}
