import SwiftUI

@main
struct KinowoApp: App {
    @StateObject private var store = RepertoireStore()
    @StateObject private var prefs = UserPreferences()

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(store)
                .environmentObject(prefs)
                .preferredColorScheme(.dark)
                .tint(Color(red: 0.42, green: 0.67, blue: 0.87))
        }
    }
}
