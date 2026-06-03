import SwiftUI
import UIKit

/// Non-prod screen for dialling in the showtime-pill look. It renders six real
/// `FilmCardView`s in the production grid and drives their pills from a live
/// `ShowtimePillStyle` injected through the environment, so every slider /
/// picker in the draggable bottom sheet redraws the cards instantly. Nothing
/// here ships in a normal run — it's reached only via the `KINOWO_TUNING` launch
/// env var (DEBUG) or the Xcode preview at the bottom of this file.
///
/// The first two films cover the awkward cases on purpose: empty / short / long
/// / multi-token formats, a pill with a room (long-press), common-token
/// stripping (a cinema whose every showtime shares "2D NAP" renders bare
/// times), and enough showtimes to wrap across several rows.
struct ShowtimeTuningScreen: View {
    @State private var style = ShowtimePillStyle()
    @State private var sheetHeight: CGFloat = 360
    @State private var copied = false
    @GestureState private var dragTranslation: CGFloat = 0

    private let films = ShowtimeTuningData.films
    private let columns = [GridItem(.adaptive(minimum: 160, maximum: 220), spacing: 12, alignment: .top)]

    var body: some View {
        GeometryReader { geo in
            let maxH = geo.size.height
            let minH: CGFloat = 96
            let height = min(maxH, max(minH, sheetHeight - dragTranslation))
            ZStack(alignment: .bottom) {
                cards(bottomInset: minH + 16)
                sheet(height: height, maxH: maxH, minH: minH)
            }
        }
        .background(Color(red: 0.07, green: 0.07, blue: 0.10).ignoresSafeArea())
        .preferredColorScheme(.dark)
    }

    private func cards(bottomInset: CGFloat) -> some View {
        ScrollView {
            LazyVGrid(columns: columns, alignment: .leading, spacing: 12) {
                ForEach(films) { film in
                    FilmCardView(film: film, truncatable: false)
                }
            }
            .padding(12)
            .padding(.bottom, bottomInset)
        }
        .environment(\.showtimePillStyle, style)
    }

    // MARK: – draggable sheet

    private func sheet(height: CGFloat, maxH: CGFloat, minH: CGFloat) -> some View {
        VStack(spacing: 0) {
            handle(maxH: maxH, minH: minH)
            header
            ScrollView {
                VStack(alignment: .leading, spacing: 14) {
                    group("Czas (time)") {
                        weightRow("Grubość", get: { style.timeWeight }, set: { style.timeWeight = $0 })
                        slider("Rozmiar", $style.timeFontSize, 7...20)
                    }
                    group("Format") {
                        weightRow("Grubość", get: { style.formatWeight }, set: { style.formatWeight = $0 })
                        slider("Rozmiar", $style.formatFontSize, 5...16)
                    }
                    group("Padding (pigułki)") {
                        slider("Poziomy", $style.horizontalInset, 0...14)
                        slider("Pionowy", $style.verticalInset, 0...14)
                    }
                    group("Odstępy") {
                        slider("Czas ↔ format", $style.internalGap, 0...12)
                        slider("Między pigułkami", $style.interPillGap, 0...16)
                    }
                }
                .padding(.horizontal, 16)
                .padding(.bottom, 32)
            }
        }
        .frame(maxWidth: .infinity)
        .frame(height: height, alignment: .top)
        .background(.ultraThinMaterial)
        .clipShape(RoundedRectangle(cornerRadius: 16))
        .overlay(RoundedRectangle(cornerRadius: 16).stroke(Color.white.opacity(0.08)))
        .ignoresSafeArea(edges: .bottom)
    }

    /// The grab area. The drag gesture lives only here so the sliders and the
    /// controls scroll-view keep their own vertical gestures.
    private func handle(maxH: CGFloat, minH: CGFloat) -> some View {
        Capsule()
            .fill(Color.secondary)
            .frame(width: 40, height: 5)
            .frame(maxWidth: .infinity)
            .padding(.vertical, 8)
            .contentShape(Rectangle())
            .gesture(
                DragGesture()
                    .updating($dragTranslation) { value, state, _ in state = value.translation.height }
                    .onEnded { value in
                        sheetHeight = min(maxH, max(minH, sheetHeight - value.translation.height))
                    }
            )
    }

    private var header: some View {
        HStack(alignment: .top) {
            VStack(alignment: .leading, spacing: 2) {
                Text("Showtime tuning").font(.system(size: 13, weight: .semibold))
                Text(fitReadout)
                    .font(.system(size: 11))
                    .foregroundStyle(fitOK ? Color.green : Color.orange)
            }
            Spacer()
            Button {
                UIPasteboard.general.string = valuesText
                copied = true
                DispatchQueue.main.asyncAfter(deadline: .now() + 1.2) { copied = false }
            } label: {
                Label(copied ? "Skopiowano" : "Kopiuj", systemImage: copied ? "checkmark" : "doc.on.doc")
                    .font(.system(size: 13, weight: .medium))
            }
            .buttonStyle(.bordered)
            Button("Reset") { style = ShowtimePillStyle() }
                .font(.system(size: 13, weight: .medium))
                .buttonStyle(.bordered)
        }
        .padding(.horizontal, 16)
        .padding(.bottom, 8)
    }

    // MARK: – clipboard

    /// Plain-text dump of every value, in the same format Android's tuning
    /// screen emits — paste it back to compare or to ask for an adjustment.
    private var valuesText: String {
        func w(_ x: Font.Weight) -> String { WeightOption(x).rawValue }
        return """
        time: size=\(f(style.timeFontSize)) weight=\(w(style.timeWeight))
        format: size=\(f(style.formatFontSize)) weight=\(w(style.formatWeight))
        padding: h=\(f(style.horizontalInset)) v=\(f(style.verticalInset))
        gaps: internal=\(f(style.internalGap)) interPill=\(f(style.interPillGap))
        """
    }

    private func f(_ v: CGFloat) -> String { String(format: "%.1f", v) }

    // MARK: – live two-per-row readout

    /// Estimated width of one pill at the current style, using the same CoreText
    /// measurement the fit test does. Approximate (the weight axis is mapped
    /// from `Font.Weight`), but it tracks what the cards above actually draw.
    private func pillWidth(time: String, format: String) -> CGFloat {
        let tw = ShowtimePillMetrics.textWidth(time, size: style.timeFontSize, weight: Self.axis(style.timeWeight))
        let fw = format.isEmpty ? 0
            : style.internalGap + ShowtimePillMetrics.textWidth(format, size: style.formatFontSize, weight: Self.axis(style.formatWeight))
        return 2 * style.horizontalInset + tw + fw
    }

    private var twoUpWidth: CGFloat {
        pillWidth(time: "12:55", format: "2D DUB") + style.interPillGap + pillWidth(time: "22:55", format: "3D NAP")
    }

    private var fitOK: Bool {
        twoUpWidth <= ShowtimePillMetrics.cardShowingsWidth(screenWidth: ShowtimePillMetrics.narrowestSupportedWidth)
    }

    private var fitReadout: String {
        let margin = ShowtimePillMetrics.cardShowingsWidth(screenWidth: ShowtimePillMetrics.narrowestSupportedWidth) - twoUpWidth
        // Always measured against the 390 pt floor (not this device), so the
        // readout is honest about the smallest phone we guarantee two-up on.
        return fitOK
            ? String(format: "✓ dwie pigułki w rzędzie @390pt (zapas %.1f pt)", margin)
            : String(format: "✗ druga pigułka się zawija @390pt (brakuje %.1f pt)", -margin)
    }

    /// SwiftUI `.weight` → CoreText normalised weight axis, matching
    /// `ShowtimePillMetrics`'s private constants for the shipping weights.
    private static func axis(_ w: Font.Weight) -> CGFloat {
        switch w {
        case .regular:  return 0.0
        case .medium:   return 0.23
        case .semibold: return 0.3
        case .bold:     return 0.4
        case .heavy:    return 0.56
        default:        return 0.0
        }
    }

    // MARK: – control builders

    @ViewBuilder
    private func group(_ title: String, @ViewBuilder _ content: () -> some View) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            Text(title.uppercased())
                .font(.system(size: 10, weight: .semibold))
                .foregroundStyle(.secondary)
                .tracking(0.5)
            content()
        }
    }

    private func slider(_ label: String, _ value: Binding<CGFloat>, _ range: ClosedRange<CGFloat>) -> some View {
        HStack(spacing: 10) {
            Text(label).font(.system(size: 12)).frame(width: 110, alignment: .leading)
            Slider(value: value, in: range, step: 0.5)
            Text(String(format: "%.1f", value.wrappedValue))
                .font(.system(size: 12, weight: .medium).monospacedDigit())
                .frame(width: 38, alignment: .trailing)
        }
    }

    private func weightRow(_ label: String, get: @escaping () -> Font.Weight, set: @escaping (Font.Weight) -> Void) -> some View {
        HStack(spacing: 10) {
            Text(label).font(.system(size: 12)).frame(width: 110, alignment: .leading)
            Picker(label, selection: Binding(get: { WeightOption(get()) }, set: { set($0.weight) })) {
                ForEach(WeightOption.allCases) { Text($0.short).tag($0) }
            }
            .pickerStyle(.segmented)
        }
    }
}

/// The `Font.Weight` choices the tuning picker offers, with a reverse map so the
/// segmented control can show the current style's weight. The `rawValue` is the
/// token used in the copied-values text.
private enum WeightOption: String, CaseIterable, Identifiable {
    case regular, medium, semibold, bold, heavy
    var id: String { rawValue }
    var short: String {
        switch self {
        case .regular: return "Reg"
        case .medium: return "Med"
        case .semibold: return "Semi"
        case .bold: return "Bold"
        case .heavy: return "Heavy"
        }
    }
    var weight: Font.Weight {
        switch self {
        case .regular: return .regular
        case .medium: return .medium
        case .semibold: return .semibold
        case .bold: return .bold
        case .heavy: return .heavy
        }
    }
    init(_ w: Font.Weight) {
        switch w {
        case .medium: self = .medium
        case .semibold: self = .semibold
        case .bold: self = .bold
        case .heavy: self = .heavy
        default: self = .regular
        }
    }
}

/// Deterministic sample repertoire — identical every launch. The first two
/// films exercise every showtime-rendering case; films 3–6 are ordinary so the
/// grid looks like a real screen.
enum ShowtimeTuningData {
    static let films: [Film] = [edgeCases, manyShowtimes, drama, kids, latNight, festival]

    private static func ratings(_ imdb: Double?, _ mc: Int?, _ rt: Int?, _ fw: Double?) -> Film.Ratings {
        Film.Ratings(imdb: imdb, imdbURL: nil, metascore: mc, metacriticURL: nil,
                     rottenTomatoes: rt, rottenTomatoesURL: nil, filmweb: fw, filmwebURL: nil)
    }

    private static func film(_ title: String, _ runtime: Int?, _ year: Int?, _ ratings: Film.Ratings, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [], runtimeMinutes: runtime,
             releaseYear: year, genres: [], ratings: ratings, countries: [], directors: [],
             cast: [], showings: days)
    }

    private static func st(_ time: String, _ format: String = "", room: String? = nil) -> Showtime {
        Showtime(time: time, format: format, room: room, bookingURL: nil)
    }

    // 1 — every case: empty/short/long/multi-token formats, a room, common-token
    // stripping (Multikino's showtimes all share "2D NAP" → rendered bare), two
    // days, two cinemas.
    private static let edgeCases = film("1 — Wszystkie przypadki", 128, 2025, ratings(7.8, 81, 92, 7.4), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: URL(string: "https://example.com"), showtimes: [
                st("10:00"),
                st("12:30", "2D"),
                st("14:45", "2D DUB", room: "Sala 1"),
                st("18:00", "IMAX 3D NAP", room: "IMAX"),
                st("20:15", "3D NAP"),
                st("22:30", "2D NAP"),
                st("23:59", "4DX 3D DUB"),
            ]),
            CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
                st("11:00", "2D NAP"), st("13:00", "2D NAP"), st("15:00", "2D NAP"),
            ]),
        ]),
        DayShowings(date: "2026-06-09", label: "Wtorek 9 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: nil, showtimes: [
                st("16:00", "2D"), st("19:30", "VOSE"),
            ]),
        ]),
    ])

    // 2 — many showtimes → wraps across several rows.
    private static let manyShowtimes = film("2 — Dużo seansów", 95, 2026, ratings(6.2, nil, 55, nil), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Cinema City", cinemaURL: nil, showtimes: [
                st("09:15", "2D"), st("10:30", "2D DUB"), st("11:45", "3D"), st("13:00", "2D NAP"),
                st("14:15", "2D"), st("15:30", "3D DUB"), st("16:45", "2D"), st("18:00", "IMAX 2D"),
                st("19:15", "2D NAP"), st("20:30", "3D NAP"), st("21:45", "2D"), st("23:00", "2D DUB"),
            ]),
        ]),
    ])

    private static let drama = film("3 — Dramat", 142, 2024, ratings(8.1, 88, nil, 7.9), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: nil, showtimes: [
                st("17:00", "2D NAP"), st("20:00", "2D NAP"),
            ]),
        ]),
    ])

    private static let kids = film("4 — Dla dzieci", 88, 2026, ratings(nil, nil, 78, 6.8), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
                st("10:00", "2D DUB"), st("12:00", "2D DUB"), st("14:00", "3D DUB"),
            ]),
        ]),
    ])

    private static let latNight = film("5 — Nocny seans", 116, 2025, ratings(7.0, 64, 71, nil), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Cinema City", cinemaURL: nil, showtimes: [
                st("22:00", "2D NAP"), st("23:30", "2D NAP", room: "Sala 7"),
            ]),
        ]),
    ])

    private static let festival = film("6 — Festiwal", 101, 2023, ratings(7.5, nil, nil, 8.2), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: nil, showtimes: [
                st("15:00", "VOSE"), st("18:30", "OV"), st("21:00", "VOSE"),
            ]),
        ]),
    ])
}

#Preview {
    ShowtimeTuningScreen()
        .environmentObject(UserPreferences())
}
