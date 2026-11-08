import SwiftUI

/// One Filtry row (country / genre / director / cast) that pushes a
/// `NameFilterList` and shows an "included/total" count badge once the
/// user has excluded anything. Renders nothing when there are no entries
/// to filter, so the four call sites stay declarative one-liners.
struct NameFilterSection: View {
    let title: LocalizedStringKey
    let allEntries: [(name: String, count: Int)]
    @Binding var excluded: Set<String>

    var body: some View {
        if !allEntries.isEmpty {
            Section {
                NavigationLink {
                    NameFilterList(title: title, allEntries: allEntries, excluded: $excluded)
                } label: {
                    HStack {
                        Text(title)
                        Spacer()
                        if !excluded.isEmpty {
                            Text("\(allEntries.count - excluded.count)/\(allEntries.count)")
                                .foregroundStyle(.secondary)
                        }
                    }
                }
            }
        }
    }
}

struct NameFilterList: View {
    let title: LocalizedStringKey
    let allEntries: [(name: String, count: Int)]
    @Binding var excluded: Set<String>

    private var allNames: Set<String> { Set(allEntries.map(\.name)) }

    var body: some View {
        Form {
            Section {
                Toggle("filtersheet.all", isOn: Binding(
                    get: { excluded.isEmpty },
                    set: { on in
                        excluded = on ? [] : allNames
                    }
                ))
            }
            Section {
                ForEach(allEntries, id: \.name) { entry in
                    Toggle(isOn: Binding(
                        get: { !excluded.contains(entry.name) },
                        set: { on in
                            if on { excluded.remove(entry.name) }
                            else  { excluded.insert(entry.name) }
                        }
                    )) {
                        HStack {
                            Text(entry.name)
                            Spacer()
                            Text("(\(entry.count))")
                                .foregroundStyle(.secondary)
                        }
                    }
                }
            }
        }
        .navigationTitle(title)
        .navigationBarTitleDisplayMode(.inline)
    }
}
