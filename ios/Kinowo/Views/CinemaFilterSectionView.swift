import SwiftUI

/// The Filtry sheet's "Kina" section — the one cinema filter, over
/// `UserPreferences.disabledCinemas`.
///
/// A **flat** city lists every cinema as its own checkbox under a "Wszystkie
/// kina" master. A **split** city (e.g. London) groups the same universe into
/// collapsible areas, each with a tri-state checkbox that (de)selects the whole
/// area. All the state arithmetic — which boxes read on/off/mixed, and what the
/// excluded set becomes on a tap — lives in `CinemaFilterSection`; this view
/// only draws it. Mirrors Android's `CinemasSection` and the web's Filtry
/// cinema panel.
///
/// Renders nothing until the city catalog has loaded, so the sheet never shows
/// an empty "Kina" header.
struct CinemaFilterSectionView: View {
    let catalog: CinemaCatalog
    @ObservedObject var prefs: UserPreferences

    @State private var openAreas: Set<String> = []

    private var section: CinemaFilterSection {
        CinemaFilterSection(catalog: catalog, disabled: prefs.disabledCinemas)
    }

    var body: some View {
        if !catalog.cinemas.isEmpty {
            Section(header: Text("Kina"), footer: footer) {
                let all = section.allCheck
                checkboxRow(
                    check: all,
                    label: String(localized: "cinemafilter.all_cinemas"),
                    bold: true,
                    identifier: A11y.CinemaFilter.all
                ) {
                    prefs.setDisabledCinemas(section.settingAll(enabled: all != .on))
                }

                if section.isSplit {
                    ForEach(catalog.areas) { area in areaGroup(area) }
                } else {
                    ForEach(catalog.cinemas, id: \.self) { cinema in
                        cinemaRow(cinema, indent: false)
                    }
                }
            }
        }
    }

    /// Reads out how much of the city is still shown — the same count the old
    /// bar's collapsed handle carried, now that there's no handle.
    private var footer: some View {
        Text(String(format: String(localized: "cinemafilter.count"),
                    section.enabledCount, catalog.cinemas.count))
    }

    private func areaGroup(_ area: CinemaArea) -> some View {
        let areaCheck = section.check(ofArea: area)
        let open = openAreas.contains(area.slug)
        return Group {
            HStack(spacing: 8) {
                Button {
                    prefs.setDisabledCinemas(section.setting(area: area, enabled: areaCheck != .on))
                } label: {
                    Image(systemName: glyph(areaCheck))
                        .foregroundColor(areaCheck == .off ? .secondary : .accentColor)
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier("\(A11y.CinemaFilter.areaTogglePrefix).\(area.slug)")

                Button {
                    withAnimation(.easeInOut(duration: 0.18)) {
                        if open { openAreas.remove(area.slug) } else { openAreas.insert(area.slug) }
                    }
                } label: {
                    HStack {
                        Text(area.name)
                        Spacer(minLength: 0)
                        Text("\(area.cinemas.count)")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                        Image(systemName: "chevron.down")
                            .font(.caption.weight(.semibold))
                            .rotationEffect(.degrees(open ? 180 : 0))
                            .foregroundStyle(.secondary)
                    }
                    .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier("\(A11y.CinemaFilter.areaHeaderPrefix).\(area.slug)")
            }

            if open {
                ForEach(area.cinemas, id: \.self) { cinema in
                    cinemaRow(cinema, indent: true)
                }
            }
        }
    }

    private func cinemaRow(_ cinema: String, indent: Bool) -> some View {
        checkboxRow(
            check: section.check(ofCinema: cinema),
            label: CinemaSection.pillName(for: cinema),
            bold: false,
            indent: indent,
            identifier: "\(A11y.CinemaFilter.cinemaPrefix).\(cinema)"
        ) {
            prefs.setDisabledCinemas(
                section.setting(cinema: cinema, enabled: section.check(ofCinema: cinema) == .off)
            )
        }
    }

    private func checkboxRow(
        check: CinemaFilterSection.Check,
        label: String,
        bold: Bool,
        indent: Bool = false,
        identifier: String,
        action: @escaping () -> Void
    ) -> some View {
        Button(action: action) {
            HStack(spacing: 8) {
                Image(systemName: glyph(check))
                    .foregroundColor(check == .off ? .secondary : .accentColor)
                Text(label).fontWeight(bold ? .semibold : .regular)
                Spacer(minLength: 0)
            }
            .padding(.leading, indent ? 26 : 0)
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .accessibilityIdentifier(identifier)
    }

    private func glyph(_ c: CinemaFilterSection.Check) -> String {
        switch c {
        case .on:    return "checkmark.square.fill"
        case .off:   return "square"
        case .mixed: return "minus.square.fill"
        }
    }
}
