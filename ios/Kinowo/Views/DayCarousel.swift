import SwiftUI

/// Finger-following, wrap-around day carousel — the iOS counterpart of the
/// web's day-swipe on the listing. Replaces the old paged `TabView` (Filmy /
/// Kina) with a real carousel over the four `DateFilter` presets: drag the grid
/// and the adjacent day's grid is revealed from the screen edge 1:1 with the
/// finger; release past a threshold (or with a flick) commits to that day,
/// otherwise it snaps back.
///
/// Why a hand-rolled carousel and not a `TabView`/`UIPageViewController`:
///  - **Shared vertical scroll.** All three panes read ONE scroll offset, so
///    the revealed neighbour appears already scrolled to the same Y as the
///    current day — a paged TabView gives each page its own independent scroll.
///  - **Wrap-around.** The window is always `[prev, current, next]` with the
///    neighbours computed by `previousPreset` / `nextPreset` (which wrap), so
///    dragging left past Wszystkie reveals Dziś and right before Dziś reveals
///    Wszystkie — endless in both directions.
///  - **NavigationStack push.** A `UIViewRepresentable` bridge would sever the
///    `NavigationLink` push `FilmGridView` relies on; staying pure-SwiftUI keeps
///    the per-film detail navigation working.
///
/// The current pane is the one interactive `FilmGridView` (its ScrollView owns
/// the vertical pan and reports its offset up here); the two neighbours are
/// read-only mirrors offset by the same shared Y, so they look scrolled in
/// lockstep during the drag. On commit the new day animates its scroll back to
/// the top (via `FilmGridView.scrollResetToken`, keyed on `dateFilter`); on a
/// snap-back the scroll stays put.
struct DayCarousel: View {
    @Binding var dateFilter: DateFilter
    /// Films for a given day filter — generalises ContentView's
    /// `filmsForFilmsTab` so each of the three panes can be populated.
    let films: (DateFilter) -> [Film]
    /// Whether the per-card cinema label shows (computed by the caller from the
    /// current day's cinema count). Applied to all three panes — close enough
    /// during the brief drag, and correct for the committed day.
    let showCinemaHeaders: Bool
    /// Fired once per committed day change (after `dateFilter` advances), so the
    /// caller can flash the day-name label and retire the swipe hint.
    let onCommit: () -> Void

    /// Live horizontal drag translation; 0 at rest. The three-pane strip is
    /// offset by `-width + dragX`, so at rest the centre (current) pane fills
    /// the screen and a drag slides a neighbour in 1:1 with the finger.
    @GestureState private var dragX: CGFloat = 0
    /// Shared vertical scroll offset (points scrolled down, ≥ 0). Reported by
    /// the current pane's ScrollView; mirrored read-only by the neighbours so
    /// the revealed day sits at the same Y.
    @State private var sharedScrollY: CGFloat = 0

    /// Fraction of the width past which a release commits to the neighbour.
    private let commitFraction: CGFloat = 0.3
    /// A fast flick commits even on a short drag: predicted end-translation past
    /// this many points in the drag direction counts as intent to change day.
    private let flickThreshold: CGFloat = 80

    var body: some View {
        GeometryReader { geo in
            let width = geo.size.width
            let height = geo.size.height
            // Top-aligned, with every pane clamped to the GeometryReader's
            // height. The interactive (centre) pane is a ScrollView, whose own
            // height is the viewport (~screen), while the read-only mirror panes
            // render their grid at its FULL natural content height (thousands of
            // points). Without clamping, the tallest mirror inflated the HStack
            // row to that natural height and the default `.center` alignment then
            // dropped the shorter ScrollView's frame far below the screen
            // (origin y ≈ (rowHeight − viewport)/2) — so the centre pane's cards
            // mounted off the bottom and the grid read blank at rest. Pinning
            // every pane to `height` keeps the row exactly one screen tall.
            HStack(alignment: .top, spacing: 0) {
                mirrorPane(for: dateFilter.previousPreset, width: width, height: height)
                currentPane(width: width, height: height)
                mirrorPane(for: dateFilter.nextPreset, width: width, height: height)
            }
            .frame(width: width * 3, height: height, alignment: .topLeading)
            // Park the centre pane on screen at rest, slide with the finger.
            .offset(x: -width + dragX)
            .contentShape(Rectangle())
            .simultaneousGesture(dragGesture(width: width))
        }
        // Resolve NavigationLink(value: Film) from the current pane to the
        // per-film detail screen (the carousel container, not the inner panes,
        // owns the destination so the push survives a recenter).
        .navigationDestination(for: Film.self) { film in
            FilmDetailView(film: film)
        }
        .ignoresSafeArea(edges: [.bottom, .horizontal])
    }

    /// The single interactive pane: a real `FilmGridView` whose ScrollView owns
    /// the vertical pan and reports its offset into `sharedScrollY`. Keyed on
    /// `dateFilter` so a committed day change animates its scroll back to top.
    private func currentPane(width: CGFloat, height: CGFloat) -> some View {
        FilmGridView(
            films: films(dateFilter),
            showCinemaHeaders: showCinemaHeaders,
            scrollResetToken: AnyHashable(dateFilter),
            pagedInTabView: false,
            scrollOffsetReporter: { sharedScrollY = $0 }
        )
        .frame(width: width, height: height, alignment: .top)
    }

    /// A read-only neighbour pane: the same grid content with no ScrollView,
    /// translated by the shared scroll offset so it mirrors the current day's
    /// vertical position during the drag.
    private func mirrorPane(for filter: DateFilter, width: CGFloat, height: CGFloat) -> some View {
        FilmGridView(
            films: films(filter),
            showCinemaHeaders: showCinemaHeaders,
            pagedInTabView: false,
            mirroredOffset: sharedScrollY
        )
        // Clamp to the viewport and clip: the mirror renders its grid at full
        // natural height, but it must occupy only one screen in the strip so it
        // can't inflate the HStack row (see the carousel body comment).
        .frame(width: width, height: height, alignment: .top)
        .clipped()
    }

    private func dragGesture(width: CGFloat) -> some Gesture {
        DragGesture(minimumDistance: 12)
            .updating($dragX) { value, state, _ in
                // Only follow predominantly-horizontal drags so the grid's
                // vertical scroll keeps working; a vertical drag leaves the
                // strip parked and the inner ScrollView handles it.
                guard abs(value.translation.width) > abs(value.translation.height) else { return }
                state = value.translation.width
            }
            .onEnded { value in
                guard width > 0,
                      abs(value.translation.width) > abs(value.translation.height)
                else { return }
                let tx = value.translation.width
                let predicted = value.predictedEndTranslation.width
                let committed = abs(tx) > width * commitFraction || abs(predicted) > flickThreshold
                guard committed else { return }          // snap-back: GestureState resets to 0
                // Drag left (tx < 0) reveals the NEXT day; drag right the PREVIOUS.
                let target = tx < 0 ? dateFilter.nextPreset : dateFilter.previousPreset
                guard target != dateFilter else { return }
                // Recenter instantly on the committed neighbour: advancing
                // `dateFilter` makes the new day the centre pane, so the strip
                // is correct at -width with no animation needed (dragX has
                // already snapped back to 0 as the gesture ended).
                dateFilter = target
                // New day starts at the top; the keyed scrollResetToken animates
                // the current pane up, so zero the shared mirror to match.
                withAnimation(.easeOut(duration: 0.25)) { sharedScrollY = 0 }
                onCommit()
            }
    }
}
