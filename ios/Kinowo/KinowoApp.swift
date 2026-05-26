import SwiftUI

@main
struct KinowoApp: App {
    @StateObject private var store = RepertoireStore()
    @StateObject private var prefs: UserPreferences
    @StateObject private var authService: AuthService
    @StateObject private var sync: StateSyncService

    init() {
        let p = UserPreferences()
        let a = AuthService()
        _prefs = StateObject(wrappedValue: p)
        _authService = StateObject(wrappedValue: a)
        _sync = StateObject(wrappedValue: StateSyncService(
            prefs: p,
            userPublisher: a.$user.eraseToAnyPublisher(),
            client: HttpUserStateClient()
        ))
    }

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(store)
                .environmentObject(prefs)
                .environmentObject(authService)
                .environmentObject(sync)
                .preferredColorScheme(.dark)
                .tint(Color(red: 0.42, green: 0.67, blue: 0.87))
                .task { await authService.checkSession() }
        }
    }
}
