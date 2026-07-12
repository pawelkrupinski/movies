import SwiftUI

/// First-visit sheet for a SPLIT city (e.g. London): asks which areas to show,
/// all pre-selected (so tapping straight through shows everything — the flat
/// default). Unchecking an area excludes its cinemas on confirm. Presented once
/// per city (see `UserPreferences.markAreaPickerSeen`). Mirrors the web's
/// first-visit area picker.
struct AreaPickerSheet: View {
    let catalog: CinemaCatalog
    /// The cinemas to disable (every cinema in an unchecked area), then dismiss.
    let onConfirm: (_ disabledCinemas: [String]) -> Void

    @Environment(\.dismiss) private var dismiss
    /// Slugs currently checked. Seeded to ALL areas on appear (pre-selected).
    @State private var kept: Set<String> = []

    var body: some View {
        NavigationStack {
            List {
                Section {
                    ForEach(catalog.areas) { area in
                        Button { toggle(area.slug) } label: {
                            HStack(spacing: 12) {
                                Image(systemName: kept.contains(area.slug) ? "checkmark.square.fill" : "square")
                                    .foregroundColor(kept.contains(area.slug) ? .accentColor : .secondary)
                                Text(area.name)
                                Spacer()
                                Text("\(area.cinemas.count)").foregroundStyle(.secondary)
                            }
                            .contentShape(Rectangle())
                        }
                        .buttonStyle(.plain)
                        .accessibilityIdentifier("areapicker.area.\(area.slug)")
                    }
                } header: {
                    Text("Choose your areas")
                } footer: {
                    Text("Show cinemas only in the areas you pick. You can change this anytime.")
                }
            }
            .navigationTitle("London")
            .navigationBarTitleDisplayMode(.inline)
            .safeAreaInset(edge: .bottom) {
                Button(action: confirm) {
                    Text("Show listings").frame(maxWidth: .infinity)
                }
                .buttonStyle(.borderedProminent)
                .padding()
                .accessibilityIdentifier("areapicker.confirm")
            }
        }
        .onAppear { if kept.isEmpty { kept = Set(catalog.areas.map(\.slug)) } }
    }

    private func toggle(_ slug: String) {
        if kept.contains(slug) { kept.remove(slug) } else { kept.insert(slug) }
    }

    private func confirm() {
        onConfirm(catalog.cinemasToDisable(keepingAreas: kept))
        dismiss()
    }
}
