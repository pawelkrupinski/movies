import SwiftUI

extension EnvironmentValues {
    /// Live width of the listing viewport, published by `ContentView`. The
    /// default (iPhone 17) is only seen by views hosted outside `ContentView`
    /// (previews, the tuning screen).
    @Entry var listingViewportWidth: CGFloat = 393
    /// The app's one `StateSyncService`, injected at the root by `KinowoApp`;
    /// nil only in views hosted outside it (previews, the tuning screen).
    @Entry var stateSync: StateSyncService? = nil
}

extension ListingSafeArea {
    /// The safe-area edges the paged grid ignores: always the bottom (the grid
    /// scrolls under the home indicator), the horizontal ones only when it
    /// bleeds into them.
    var ignoredEdges: Edge.Set {
        bleedsIntoHorizontalInsets ? [.bottom, .horizontal] : .bottom
    }
}
