import SwiftUI

@main
struct KinowoApp: App {
    @StateObject private var store = RepertoireStore()
    @StateObject private var details = DetailsStore()
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
            root
                .environmentObject(store)
                .environmentObject(details)
                .environmentObject(prefs)
                .environmentObject(authService)
                .environmentObject(sync)
                .preferredColorScheme(.dark)
                .tint(Color(red: 0.42, green: 0.67, blue: 0.87))
                .task { await authService.checkSession() }
        }
    }

    /// Normally `ContentView`. In DEBUG, setting the `KINOWO_TUNING` launch
    /// env var swaps in the non-prod `ShowtimeTuningScreen` instead — a quick
    /// way to dial in the showtime-pill look on a real device without adding
    /// any UI to the shipping app.
    @ViewBuilder
    private var root: some View {
        #if DEBUG
        if ProcessInfo.processInfo.environment["KINOWO_TUNING"] != nil {
            ShowtimeTuningScreen()
        } else {
            ContentView()
        }
        #else
        ContentView()
        #endif
    }
}
