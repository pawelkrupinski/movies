import SwiftUI
import UIKit

/// Non-prod developer screen for dialling in the look of the three main views
/// against their real renderers. A full-screen horizontal pager swaps between
/// pages — swipe left/right, or tap a label in the page bar:
///
///  - **Karta**  — six real `FilmCardView`s in the production grid, driven by a
///    live `ShowtimePillStyle` / `RatingPillStyle` / `CardSpacingStyle`.
///  - **Kina**   — the real `CinemaSectionedGridView`, driven by a live
///    `CinemaHeaderStyle` (cinema section header font / underline / spacing).
///  - **Film**   — the real `FilmDetailView`, driven by a live `FilmDetailStyle`
///    (title / meta / showings-header fonts + the vertical gaps).
///
/// The sliders live in a draggable bottom sheet that is a *sibling* of the
/// pager, not a child of it — a horizontal slider drag must not be stolen by
/// the pager's swipe gesture (the same lesson as the top-bar pager: keep
/// interactive controls out of the paged scroll view). The sheet's content
/// switches to whichever page is showing.
///
/// Nothing here ships in a normal run — it's reached only via the
/// `KINOWO_TUNING` launch env var (DEBUG) or the Xcode preview at the bottom.
struct ShowtimeTuningScreen: View {
    @State private var page: TuningPage = .card
    @State private var style = ShowtimePillStyle()
    @State private var ratingStyle = RatingPillStyle()
    @State private var spacing = CardSpacingStyle()
    @State private var cinemaHeader = CinemaHeaderStyle()
    @State private var filmStyle = FilmDetailStyle()
    @StateObject private var details = DetailsStore.seeded(TuningSampleData.details)
    @State private var sheetHeight: CGFloat = 360
    @State private var copied = false
    @GestureState private var dragTranslation: CGFloat = 0

    private let columns = [GridItem(.adaptive(minimum: 160, maximum: 220), spacing: 12, alignment: .top)]

    var body: some View {
        GeometryReader { geo in
            let maxH = geo.size.height
            let minH: CGFloat = 96
            let height = min(maxH, max(minH, sheetHeight - dragTranslation))
            ZStack(alignment: .bottom) {
                previews(bottomInset: minH + 16)
                sheet(height: height, maxH: maxH, minH: minH, avail: geo.size)
            }
        }
        .background(Color(red: 0.07, green: 0.07, blue: 0.10).ignoresSafeArea())
        .preferredColorScheme(.dark)
    }

    // MARK: – paged previews (the real views behind the sheet)

    private func previews(bottomInset: CGFloat) -> some View {
        TabView(selection: $page) {
            cardsPreview(bottomInset: bottomInset).tag(TuningPage.card)
            kinaPreview.tag(TuningPage.kina)
            filmPreview.tag(TuningPage.film)
        }
        .tabViewStyle(.page(indexDisplayMode: .never))
        .ignoresSafeArea(edges: .bottom)
        // Every page reads the card-level styles too (kina + film embed real
        // cards / pills), so inject them once for the whole pager.
        .environment(\.showtimePillStyle, style)
        .environment(\.ratingPillStyle, ratingStyle)
        .environment(\.cardSpacingStyle, spacing)
    }

    private func cardsPreview(bottomInset: CGFloat) -> some View {
        ScrollView {
            LazyVGrid(columns: columns, alignment: .leading, spacing: 12) {
                ForEach(Array(TuningSampleData.films.enumerated()), id: \.element.id) { index, film in
                    FilmCardView(film: film, truncatable: false)
                        // The XCUITest measures the first card's height before
                        // and after dragging a spacing slider.
                        .accessibilityIdentifier("\(A11y.Tuning.cardPrefix).\(index)")
                }
            }
            .padding(12)
            .padding(.bottom, bottomInset)
        }
    }

    private var kinaPreview: some View {
        CinemaSectionedGridView(sections: TuningSampleData.sections)
            .environment(\.cinemaHeaderStyle, cinemaHeader)
    }

    private var filmPreview: some View {
        FilmDetailView(film: TuningSampleData.detailFilm)
            .environmentObject(details)
            .environment(\.filmDetailStyle, filmStyle)
    }

    // MARK: – draggable sheet

    private func sheet(height: CGFloat, maxH: CGFloat, minH: CGFloat, avail: CGSize) -> some View {
        VStack(spacing: 0) {
            handle(maxH: maxH, minH: minH)
            pageBar
            header(avail: avail)
            ScrollView {
                VStack(alignment: .leading, spacing: 14) {
                    switch page {
                    case .card: cardControls
                    case .kina: kinaControls
                    case .film: filmControls
                    }
                }
                .padding(.horizontal, 16)
                .padding(.bottom, 32)
            }
            .accessibilityIdentifier(A11y.Tuning.controlsScroll)
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

    /// Tappable page indicator doubling as the page switcher — swipe is the
    /// primary gesture, but tapping a label jumps straight there (and gives the
    /// UITests a reliable page hook that doesn't depend on a swipe distance).
    private var pageBar: some View {
        HStack(spacing: 6) {
            ForEach(TuningPage.allCases) { p in
                let active = p == page
                Button {
                    withAnimation(.easeInOut(duration: 0.25)) { page = p }
                } label: {
                    Text(p.title)
                        .font(.system(size: 12, weight: active ? .semibold : .regular))
                        .foregroundColor(active ? Color(red: 0.10, green: 0.10, blue: 0.18) : .white)
                        .frame(maxWidth: .infinity)
                        .padding(.vertical, 6)
                        .background(
                            active ? Color(red: 0.42, green: 0.67, blue: 0.87) : Color.white.opacity(0.08),
                            in: RoundedRectangle(cornerRadius: 7)
                        )
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier("\(A11y.Tuning.pageTabPrefix).\(p.rawValue)")
            }
        }
        .padding(.horizontal, 16)
        .padding(.bottom, 8)
    }

    private func header(avail: CGSize) -> some View {
        HStack(alignment: .top) {
            VStack(alignment: .leading, spacing: 2) {
                Text("\(page.title) tuning").font(.system(size: 13, weight: .semibold))
                // Live readout of the area the layout actually gets (points),
                // plus the scale and the pixels it works out to — the available
                // size matters more here than the raw panel resolution.
                Text(DisplayInfo.tuningReadout(
                    pointWidth: avail.width, pointHeight: avail.height, scale: UIScreen.main.scale
                ))
                .font(.system(size: 10))
                .foregroundStyle(.secondary)
                // The two-per-row fit readout only makes sense for the card
                // page (the showtime pills) — hide it elsewhere.
                if page == .card {
                    Text(fitReadout)
                        .font(.system(size: 11))
                        .foregroundStyle(fitOK ? Color.green : Color.orange)
                }
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
            Button("Reset", action: reset)
                .font(.system(size: 13, weight: .medium))
                .buttonStyle(.bordered)
        }
        .padding(.horizontal, 16)
        .padding(.bottom, 8)
    }

    /// Reset only the styles the current page edits — switching pages and
    /// hitting Reset shouldn't wipe another page's work.
    private func reset() {
        switch page {
        case .card:
            style = ShowtimePillStyle(); ratingStyle = RatingPillStyle(); spacing = CardSpacingStyle()
        case .kina:
            cinemaHeader = CinemaHeaderStyle()
        case .film:
            filmStyle = FilmDetailStyle()
        }
    }

    // MARK: – per-page controls

    @ViewBuilder
    private var cardControls: some View {
        group("Odstępy karty") {
            slider("Sekcje", $spacing.sectionSpacing, 0...24, id: A11y.Tuning.sectionSpacingSlider)
            slider("Pod ocenami", $spacing.ratingsBottom, 0...32)
            slider("Blok seansów", $spacing.showingsBlock, 0...24, id: A11y.Tuning.showingsBlockSlider)
            slider("Dzień → kino", $spacing.dayToCinema, 0...24, id: A11y.Tuning.dayToCinemaSlider)
            slider("Nad dniem", $spacing.dayLabelTop, 0...16)
            slider("Kino → seanse", $spacing.cinemaToPills, 0...16)
            slider("Rzędy pigułek", $spacing.pillRowSpacing, 0...16)
        }
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
        group("Ocena: etykieta (IMDb/FW/RT)") {
            weightRow("Grubość", get: { ratingStyle.labelWeight }, set: { ratingStyle.labelWeight = $0 })
            slider("Rozmiar", $ratingStyle.labelFontSize, 6...18)
        }
        group("Ocena: wartość") {
            weightRow("Grubość", get: { ratingStyle.valueWeight }, set: { ratingStyle.valueWeight = $0 })
            slider("Rozmiar", $ratingStyle.valueFontSize, 6...18)
        }
        group("Ocena: Metacritic (solid)") {
            weightRow("Grubość", get: { ratingStyle.solidWeight }, set: { ratingStyle.solidWeight = $0 })
        }
        group("Ocena: padding") {
            slider("Etykieta poziomy", $ratingStyle.labelHInset, 0...12)
            slider("Wartość poziomy", $ratingStyle.valueHInset, 0...12)
            slider("Pionowy", $ratingStyle.vInset, 0...10)
        }
        group("Ocena: kształt") {
            slider("Zaokrąglenie", $ratingStyle.cornerRadius, 0...12)
            slider("Między pigułkami", $ratingStyle.interPillGap, 0...16)
        }
    }

    @ViewBuilder
    private var kinaControls: some View {
        group("Nagłówek kina") {
            weightRow("Grubość", get: { cinemaHeader.fontWeight }, set: { cinemaHeader.fontWeight = $0 })
            slider("Rozmiar", $cinemaHeader.fontSize, 10...24, id: A11y.Tuning.cinemaHeaderFontSlider)
            slider("Grubość linii", $cinemaHeader.underlineThickness, 0...4)
            slider("Tytuł → linia", $cinemaHeader.titleBottomPadding, 0...16)
        }
        group("Odstępy") {
            slider("Między kinami", $cinemaHeader.sectionSpacing, 0...40)
            slider("Nagłówek → siatka", $cinemaHeader.headerToGrid, 0...24)
        }
    }

    @ViewBuilder
    private var filmControls: some View {
        group("Układ") {
            slider("Sekcje", $filmStyle.sectionSpacing, 0...32)
            slider("Kolumna nagłówka", $filmStyle.headerColumnSpacing, 0...20)
            slider("Bloki meta", $filmStyle.metaBlockSpacing, 0...24)
        }
        group("Tytuł") {
            weightRow("Grubość", get: { filmStyle.titleWeight }, set: { filmStyle.titleWeight = $0 })
            slider("Rozmiar", $filmStyle.titleFontSize, 14...34, id: A11y.Tuning.detailTitleFontSlider)
            slider("Tytuł oryg.", $filmStyle.originalTitleFontSize, 10...22)
        }
        group("Bloki meta (tekst)") {
            slider("Etykieta", $filmStyle.metaLabelFontSize, 8...16)
            slider("Wartość", $filmStyle.metaValueFontSize, 10...20)
            slider("Etykieta → wartość", $filmStyle.metaLabelToValue, 0...12)
        }
        group("Seanse") {
            slider("Nagłówek", $filmStyle.showingsHeaderFontSize, 12...26)
        }
    }

    // MARK: – clipboard

    /// Plain-text dump of the current page's values, in the same `key=value`
    /// format Android's tuning screen emits — paste it back to compare or to
    /// ask for an adjustment.
    private var valuesText: String {
        func w(_ x: Font.Weight) -> String { WeightOption(x).rawValue }
        switch page {
        case .card:
            return """
            time: size=\(f(style.timeFontSize)) weight=\(w(style.timeWeight))
            format: size=\(f(style.formatFontSize)) weight=\(w(style.formatWeight))
            padding: h=\(f(style.horizontalInset)) v=\(f(style.verticalInset))
            gaps: internal=\(f(style.internalGap)) interPill=\(f(style.interPillGap))
            rating-label: size=\(f(ratingStyle.labelFontSize)) weight=\(w(ratingStyle.labelWeight))
            rating-value: size=\(f(ratingStyle.valueFontSize)) weight=\(w(ratingStyle.valueWeight))
            rating-solid: weight=\(w(ratingStyle.solidWeight))
            rating-pad: labelH=\(f(ratingStyle.labelHInset)) valueH=\(f(ratingStyle.valueHInset)) v=\(f(ratingStyle.vInset))
            rating-shape: corner=\(f(ratingStyle.cornerRadius)) interPill=\(f(ratingStyle.interPillGap))
            card-gaps: section=\(f(spacing.sectionSpacing)) ratingsBottom=\(f(spacing.ratingsBottom)) showingsBlock=\(f(spacing.showingsBlock)) dayToCinema=\(f(spacing.dayToCinema)) dayLabelTop=\(f(spacing.dayLabelTop)) cinemaToPills=\(f(spacing.cinemaToPills)) pillRow=\(f(spacing.pillRowSpacing))
            """
        case .kina:
            return """
            cinema-header: size=\(f(cinemaHeader.fontSize)) weight=\(w(cinemaHeader.fontWeight))
            cinema-underline: thickness=\(f(cinemaHeader.underlineThickness)) titleBottom=\(f(cinemaHeader.titleBottomPadding))
            cinema-gaps: section=\(f(cinemaHeader.sectionSpacing)) headerToGrid=\(f(cinemaHeader.headerToGrid))
            """
        case .film:
            return """
            film-layout: section=\(f(filmStyle.sectionSpacing)) headerColumn=\(f(filmStyle.headerColumnSpacing)) metaBlock=\(f(filmStyle.metaBlockSpacing))
            film-title: size=\(f(filmStyle.titleFontSize)) weight=\(w(filmStyle.titleWeight)) original=\(f(filmStyle.originalTitleFontSize))
            film-meta: label=\(f(filmStyle.metaLabelFontSize)) value=\(f(filmStyle.metaValueFontSize)) labelToValue=\(f(filmStyle.metaLabelToValue))
            film-showings: header=\(f(filmStyle.showingsHeaderFontSize))
            """
        }
    }

    private func f(_ v: CGFloat) -> String { String(format: "%.1f", v) }

    // MARK: – live two-per-row readout (card page)

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

    private func slider(_ label: String, _ value: Binding<CGFloat>, _ range: ClosedRange<CGFloat>, id: String? = nil) -> some View {
        let step: CGFloat = 0.5
        return HStack(spacing: 8) {
            Text(label).font(.system(size: 12)).frame(width: 92, alignment: .leading)
            stepButton(systemName: "minus") {
                value.wrappedValue = max(range.lowerBound, value.wrappedValue - step)
            }
            Slider(value: value, in: range, step: step)
                .accessibilityIdentifier(id ?? "")
            stepButton(systemName: "plus") {
                value.wrappedValue = min(range.upperBound, value.wrappedValue + step)
            }
            Text(String(format: "%.1f", value.wrappedValue))
                .font(.system(size: 12, weight: .medium).monospacedDigit())
                .frame(width: 34, alignment: .trailing)
        }
    }

    /// Small −/+ button flanking a slider, nudging it by one 0.5 step (clamped
    /// to the slider's range) for precise dialling without dragging.
    private func stepButton(systemName: String, _ action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: systemName)
                .font(.system(size: 11, weight: .bold))
                .foregroundColor(.white)
                .frame(width: 24, height: 26)
                .background(Color.white.opacity(0.10), in: RoundedRectangle(cornerRadius: 5))
        }
        .buttonStyle(.plain)
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

/// The pages the tuning pager swipes between, in display order.
enum TuningPage: Int, CaseIterable, Identifiable {
    case card, kina, film
    var id: Int { rawValue }
    var title: String {
        switch self {
        case .card: return "Karta"
        case .kina: return "Kina"
        case .film: return "Film"
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
/// grid looks like a real screen. `sections` regroups them by cinema for the
/// Kina page, and `detailFilm` (+ `details`) feeds the Film page.
enum TuningSampleData {
    static let films: [Film] = [edgeCases, manyShowtimes, drama, kids, latNight, festival]

    /// Cinema-grouped view of the sample films for the Kina page. Uses real
    /// cinema display names (so `CinemaSection.pillName` shortens them the way
    /// the live Kina tab does).
    static let sections: [CinemaSection] = [
        CinemaSection(cinema: "Cinema City Kinepolis", films: [edgeCases, drama, festival]),
        CinemaSection(cinema: "Multikino Stary Browar", films: [manyShowtimes, kids]),
        CinemaSection(cinema: "Helios Posnania", films: [latNight]),
    ]

    /// One rich film for the Film-detail page — genres / directors / cast /
    /// countries populated (the listing samples leave them empty) so every
    /// meta block renders and its fonts are tunable.
    static let detailFilm = Film(
        title: "Wszystkie przypadki", posterURL: nil, fallbackPosterURLs: [],
        runtimeMinutes: 128, releaseYear: 2025,
        genres: ["Dramat", "Thriller"], ratings: ratings(7.8, 81, 92, 7.4),
        countries: ["Polska", "Francja"], directors: ["Jan Kowalski"],
        cast: ["Anna Nowak", "Piotr Wiśniewski", "Maria Lewandowska"],
        showings: edgeCases.showings
    )

    /// Synopsis + trailers for `detailFilm` (matched by title), so the Opis
    /// block and Zwiastuny section show real content to tune.
    static let details: [FilmDetails] = [
        FilmDetails(
            title: "Wszystkie przypadki",
            originalTitle: "All The Cases",
            synopsis: "Wciągający dramat o splątanych losach kilku rodzin w powojennej "
                + "Europie. Reżyser prowadzi widza przez kolejne odsłony tajemnicy, "
                + "stopniowo odsłaniając, jak jeden wybór potrafi zaważyć na życiu "
                + "wszystkich bohaterów. Tekst celowo długi, by dało się dostroić "
                + "rozmiar i odstępy opisu na ekranie szczegółów.",
            trailerURLs: [URL(string: "https://www.youtube.com/embed/aqz-KE-bpKQ")!]
        )
    ]

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
