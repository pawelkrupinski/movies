import SwiftUI

/// Multi-select cinema picker for SPLIT cities (e.g. London). A slim handle,
/// like `CinemaPillBar`'s, unfolds into an "all cinemas" master over collapsible
/// AREA groups — each area a (de)select checkbox plus a fold revealing its
/// cinemas' own checkboxes. Mirrors the web's area-grouped Filtry list. Flat
/// cities keep the single-select `CinemaPillBar`.
///
/// Selection state lives in `UserPreferences.disabledCinemas` (the excluded
/// set); this view is stateless about it — it renders from `disabled` and calls
/// back to mutate. Checkbox glyphs: filled = on, empty = off, minus = mixed.
struct CinemaAreaBar: View {
    let catalog: CinemaCatalog
    /// Excluded cinema names (from `UserPreferences.disabledCinemas`).
    let disabled: Set<String>
    /// Toggle one cinema on/off.
    let onSetCinema: (_ cinema: String, _ enabled: Bool) -> Void
    /// (De)select every cinema in an area.
    let onSetArea: (_ cinemas: [String], _ enabled: Bool) -> Void
    /// (De)select every cinema in the city (the master).
    let onSetAll: (_ cityCinemas: [String], _ enabled: Bool) -> Void

    @State private var expanded = false
    @State private var openAreas: Set<String> = []

    private var scale: CGFloat { TopBar.viewportScale }
    private var cityCinemas: [String] { catalog.cinemas }
    private var enabledCount: Int { cityCinemas.filter { !disabled.contains($0) }.count }

    private enum Check { case on, off, mixed }

    private func check(of cinemas: [String]) -> Check {
        let off = cinemas.reduce(0) { $0 + (disabled.contains($1) ? 1 : 0) }
        if off == 0 { return .on }
        if off == cinemas.count { return .off }
        return .mixed
    }

    private func glyph(_ c: Check) -> String {
        switch c {
        case .on:    return "checkmark.square.fill"
        case .off:   return "square"
        case .mixed: return "minus.square.fill"
        }
    }

    var body: some View {
        VStack(spacing: 0) {
            handleRow
            if expanded {
                panel.transition(.move(edge: .top).combined(with: .opacity))
            }
        }
        .clipped()
    }

    private var handleLabel: String {
        enabledCount == cityCinemas.count
            ? String(localized: "areabar.all_cinemas")
            : String(format: String(localized: "areabar.count"), enabledCount, cityCinemas.count)
    }

    private var handleRow: some View {
        Button {
            withAnimation(.easeInOut(duration: 0.22)) { expanded.toggle() }
        } label: {
            HStack(spacing: 6 * scale) {
                Text(handleLabel)
                    .font(.system(size: 13 * scale, weight: .medium))
                    .lineLimit(1)
                Spacer(minLength: 0)
                Image(systemName: "chevron.down")
                    .font(.system(size: 11 * scale, weight: .semibold))
                    .rotationEffect(.degrees(expanded ? 180 : 0))
                    .accessibilityLabel(Text(expanded ? "areabar.collapse" : "areabar.expand"))
            }
            .foregroundStyle(.secondary)
            .padding(.horizontal, 14 * scale)
            .padding(.vertical, 6 * scale)
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .accessibilityIdentifier(A11y.CinemaBar.areaHandle)
        .accessibilityLabel(Text(String(format: String(localized: "areabar.a11y"), handleLabel)))
    }

    private var panel: some View {
        ScrollView(.vertical, showsIndicators: true) {
            VStack(alignment: .leading, spacing: 0) {
                let all = check(of: cityCinemas)
                checkboxRow(check: all, label: String(localized: "areabar.all_cinemas"), bold: true,
                            identifier: A11y.CinemaBar.areaAll) {
                    onSetAll(cityCinemas, all != .on)
                }
                Divider().opacity(0.3)
                ForEach(catalog.areas) { area in areaGroup(area) }
            }
            .padding(.horizontal, 14 * scale)
            .padding(.bottom, 8 * scale)
        }
        .frame(maxHeight: 320 * scale)
    }

    private func areaGroup(_ area: CinemaArea) -> some View {
        let areaCheck = check(of: area.cinemas)
        let open = openAreas.contains(area.slug)
        return VStack(alignment: .leading, spacing: 0) {
            HStack(spacing: 8 * scale) {
                Button {
                    onSetArea(area.cinemas, areaCheck != .on)
                } label: {
                    Image(systemName: glyph(areaCheck))
                        .font(.system(size: 18 * scale))
                        .foregroundColor(areaCheck == .off ? .secondary : .accentColor)
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier("\(A11y.CinemaBar.areaTogglePrefix).\(area.slug)")

                Button {
                    withAnimation(.easeInOut(duration: 0.18)) {
                        if open { openAreas.remove(area.slug) } else { openAreas.insert(area.slug) }
                    }
                } label: {
                    HStack(spacing: 6 * scale) {
                        Text(area.name).font(.system(size: 15 * scale, weight: .medium))
                        Spacer(minLength: 0)
                        Text("\(area.cinemas.count)")
                            .font(.system(size: 12 * scale)).foregroundStyle(.secondary)
                        Image(systemName: "chevron.down")
                            .font(.system(size: 11 * scale, weight: .semibold))
                            .rotationEffect(.degrees(open ? 180 : 0))
                            .foregroundStyle(.secondary)
                    }
                    .contentShape(Rectangle())
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier("\(A11y.CinemaBar.areaHeaderPrefix).\(area.slug)")
            }
            .padding(.vertical, 8 * scale)

            if open {
                ForEach(area.cinemas, id: \.self) { cinema in
                    checkboxRow(check: disabled.contains(cinema) ? .off : .on,
                                label: CinemaSection.pillName(for: cinema),
                                bold: false, indent: true,
                                identifier: "\(A11y.CinemaBar.areaCinemaPrefix).\(cinema)") {
                        onSetCinema(cinema, disabled.contains(cinema))
                    }
                }
            }
            Divider().opacity(0.3)
        }
    }

    private func checkboxRow(check c: Check, label: String, bold: Bool, indent: Bool = false,
                             identifier: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            HStack(spacing: 8 * scale) {
                Image(systemName: glyph(c))
                    .font(.system(size: 18 * scale))
                    .foregroundColor(c == .off ? .secondary : .accentColor)
                Text(label).font(.system(size: 15 * scale, weight: bold ? .semibold : .regular))
                Spacer(minLength: 0)
            }
            .padding(.leading, indent ? 26 * scale : 0)
            .padding(.vertical, 7 * scale)
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .accessibilityIdentifier(identifier)
    }
}
