import SwiftUI

/// First-visit sheet for a SPLIT city (e.g. London): asks which areas to show,
/// all pre-selected (so tapping straight through shows everything — the flat
/// default). Unchecking an area excludes its cinemas on confirm. Presented once
/// per city (see `UserPreferences.markAreaPickerSeen`). Mirrors the web's
/// first-visit area picker.
///
/// A master row heads the list — select all / deselect all in one control, the
/// same shape the web picker and Android's dialog use. A metro like the Bay
/// Area has enough areas that "clear them all, then tick the one I want" is the
/// fastest way through the sheet.
struct AreaPickerSheet: View {
    let catalog: CinemaCatalog
    /// The cinemas to disable (every cinema in an unchecked area), then dismiss.
    let onConfirm: (_ disabledCinemas: [String]) -> Void

    @Environment(\.dismiss) private var dismiss
    /// Which areas are checked. Seeded to ALL areas on appear (pre-selected).
    @State private var selection = AreaSelection(areas: [])

    var body: some View {
        NavigationStack {
            List {
                Section {
                    Button { selection.setAll(!selection.allKept) } label: {
                        HStack(spacing: 12) {
                            Image(systemName: masterIcon)
                                .foregroundColor(selection.kept.isEmpty ? .secondary : .accentColor)
                            Text("areapicker.all").fontWeight(.semibold)
                            Spacer()
                        }
                        .contentShape(Rectangle())
                    }
                    .buttonStyle(.plain)
                    .accessibilityIdentifier("areapicker.all")

                    ForEach(catalog.areas) { area in
                        Button { selection.toggle(area.slug) } label: {
                            HStack(spacing: 12) {
                                Image(systemName: selection.isKept(area.slug) ? "checkmark.square.fill" : "square")
                                    .foregroundColor(selection.isKept(area.slug) ? .accentColor : .secondary)
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
                    Text("areapicker.title")
                } footer: {
                    Text("areapicker.subtitle")
                }
            }
            .navigationTitle(Text("areapicker.title"))
            .navigationBarTitleDisplayMode(.inline)
            .safeAreaInset(edge: .bottom) {
                Button(action: confirm) {
                    Text("areapicker.confirm").frame(maxWidth: .infinity)
                }
                .buttonStyle(.borderedProminent)
                .padding()
                .accessibilityIdentifier("areapicker.confirm")
            }
        }
        .onAppear { if selection.all.isEmpty { selection = AreaSelection(areas: catalog.areas) } }
    }

    /// Checked / mixed / empty — the three states of the master row. SwiftUI has
    /// no tri-state checkbox, so the mixed one is the dash SF Symbol.
    private var masterIcon: String {
        if selection.allKept { return "checkmark.square.fill" }
        return selection.partial ? "minus.square.fill" : "square"
    }

    private func confirm() {
        onConfirm(catalog.cinemasToDisable(keepingAreas: selection.kept))
        dismiss()
    }
}
