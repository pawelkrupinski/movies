import SwiftUI

/// One native paged `TabView` page per day preset (Dziś / Jutro /
/// 7 dni / Wszystkie); a horizontal swipe pages between them. This
/// restores the hardware-smooth `UIPageViewController` swipe the app
/// had with the old Filmy / Kina tabs. The hand-rolled carousel that
/// briefly replaced it re-filtered the whole repertoire and rebuilt
/// three grids on every drag frame (`@GestureState` invalidates the
/// body per touch-move), which dropped frames and felt choppy. The
/// trade-off of going back to the native pager is no wrap-around and
/// no mid-drag day-pill preview (the carousel's extras).
///
/// Every page's `scrollResetToken` is keyed on the *selected* day, so
/// changing day (swipe or pill tap) snaps each grid back to row one —
/// preserving the "land at the top of the new day" behaviour the
/// single grid had (guarded by DayChangeScrollResetUITests), rather
/// than stranding the user mid-scroll on the day they swiped to.
struct DayPager: View {
    @Binding var dateFilter: DateFilter
    /// The listing of one day page.
    let films: (DateFilter) -> [Film]
    /// The days a running search brought onto a page, drawn lighter; `nil` for none.
    let outsideDays: (DateFilter) -> OutsideDays?
    /// The safe-area edges the pages run under (`ListingSafeArea`).
    let ignoredEdges: Edge.Set
    @Environment(RepertoireStore.self) private var store

    var body: some View {
        TabView(selection: $dateFilter) {
            ForEach(DateFilter.presets, id: \.self) { preset in
                // The per-card cinema label is always shown on the main
                // listing (FilmGridView defaults `showCinemaHeaders` true);
                // only the grouped-by-cinema section view suppresses it.
                FilmGridView(films: films(preset),
                             scrollResetToken: AnyHashable(dateFilter))
                    // While a search shows every day, the days it brought in are drawn lighter.
                    .environment(\.outsideDays, outsideDays(preset))
                    .refreshable { await store.reload() }
                    .tag(preset)
            }
        }
        // `.page(indexDisplayMode: .never)` gives the horizontal swipe with
        // no dot indicator — the date-pill row is the "which day" affordance.
        .tabViewStyle(.page(indexDisplayMode: .never))
        // Ignore only bottom (and symmetric horizontal insets, see
        // `ignoredEdges`) — NOT the top: the TabView sits below
        // the bar in the VStack, so its paged scroll view (and pan gesture)
        // starts at the bar's bottom edge and never reaches up under the pill
        // row to steal the day-pill taps. See ContentView's VStack comment.
        .ignoresSafeArea(edges: ignoredEdges)
        // Resolve NavigationLink(value: Film) from the grids to the per-film
        // detail screen — the container owns the destination so the push
        // survives a page change.
        //
        // The grids hand us a `filteredFor` copy whose showings were pruned
        // to the tapped day-page; the detail screen is a full-schedule view,
        // so re-resolve the complete, all-days film by title from the
        // unfiltered `store.films` (matching the deep-link push and Android).
        .navigationDestination(for: Film.self) { film in
            FilmDetailView(film: store.films.fullFilm(for: film))
        }
    }
}
