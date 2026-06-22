import Foundation
import Combine

/// Holds the most recent inbound deep link until the UI consumes it.
///
/// `KinowoApp`'s `.onOpenURL` parses the URL, switches the city eagerly (so the
/// CityGate flips to the linked city on a cold launch) and parks the parsed
/// `DeepLink` here; `ContentView` watches `pending`, applies the filters and
/// film push, then clears it. Kept as a tiny observable so the link survives
/// the gate flip and a not-yet-loaded repertoire.
@MainActor
final class DeepLinkCoordinator: ObservableObject {
    @Published var pending: DeepLink?
}
