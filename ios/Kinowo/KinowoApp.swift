import SwiftUI

@main
struct KinowoApp: App {
    @StateObject private var store = RepertoireStore()
    @StateObject private var prefs = UserPreferences()
    @StateObject private var authService = AuthService()

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(store)
                .environmentObject(prefs)
                .environmentObject(authService)
                .environmentObject(StateSyncService(prefs: prefs, auth: authService))
                .preferredColorScheme(.dark)
                .tint(Color(red: 0.42, green: 0.67, blue: 0.87))
                .task { await authService.checkSession() }
        }
    }
}
