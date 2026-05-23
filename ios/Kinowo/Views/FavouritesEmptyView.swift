import SwiftUI

/// Empty state for the Ulubione tab. Distinguishes two reasons the
/// grid came back empty:
///
/// - `hasAnyFavourites == false`: the user hasn't favourited a single
///   film yet. Walk them through how to add one.
/// - `hasAnyFavourites == true`: they have favourites, just none in
///   the current date window. Nudge them to pick a different day.
///
/// Both messages live in this one view so the empty-state shell
/// (icon, vertical centering, padding) doesn't fork.
struct FavouritesEmptyView: View {
    let hasAnyFavourites: Bool

    var body: some View {
        VStack(spacing: 14) {
            Image(systemName: hasAnyFavourites ? "calendar.badge.exclamationmark" : "star")
                .font(.system(size: 48))
                .foregroundStyle(.secondary)
            Text(hasAnyFavourites
                 ? "Brak ulubionych na ten dzień"
                 : "Brak ulubionych filmów")
                .font(.title3.weight(.semibold))
            Text(hasAnyFavourites
                 ? "Wybierz inną datę u góry ekranu, albo dodaj kolejne filmy do ulubionych ⭐ na karcie filmu."
                 : "Stuknij ⭐ na karcie filmu lub na pojedynczym seansie, aby zacząć budować listę ulubionych.")
                .font(.callout)
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal, 32)
                .fixedSize(horizontal: false, vertical: true)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .accessibilityIdentifier(A11y.Favourites.emptyState)
    }
}
