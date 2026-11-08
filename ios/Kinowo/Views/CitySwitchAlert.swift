import SwiftUI

/// "Jesteś bliżej miasta X" — offer to switch the repertoire to the nearer
/// supported city. `suggestion` is set (and the prompt-key remembered) only by
/// `ContentView`'s authorized-only location check, so it never fires without an
/// existing location grant. Accepting re-points the stores at the new city.
struct CitySwitchAlert: ViewModifier {
    @Binding var suggestion: City.CitySwitchSuggestion?
    @Environment(UserPreferences.self) private var prefs
    @Environment(RepertoireStore.self) private var store
    @Environment(DetailsStore.self) private var details
    @Environment(\.locale) private var locale

    func body(content: Content) -> some View {
        content.alert(
            suggestion.map { String(format: localizedString("switch.nearer_title", locale: locale), $0.target.name) } ?? "",
            isPresented: Binding(
                get: { suggestion != nil },
                set: { if !$0 { suggestion = nil } }
            ),
            presenting: suggestion
        ) { suggestion in
            Button("switch.confirm") {
                prefs.setCity(suggestion.target.slug)
                store.use(citySlug: suggestion.target.slug,
                          timeZone: suggestion.target.zone(fallback: prefs.selectedCountry.timeZone))
                details.use(citySlug: suggestion.target.slug)
            }
            Button("switch.not_now", role: .cancel) {}
        } message: { suggestion in
            Text(String(format: localizedString("switch.question", locale: locale), suggestion.target.name))
        }
    }
}
