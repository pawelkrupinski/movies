// swift-tools-version: 5.9
import PackageDescription

// SPM view of the Kinowo iOS app. The Xcode app (`Kinowo.xcodeproj`)
// remains the production build for the .ipa; this manifest gives us a
// second view of the same files so we can run XCTest cases via
// `swift test` on Linux/Docker without a full Xcode install.
//
// Three targets:
// - `KinowoCore` — Foundation-only sources (Models + parser layer of
//   Networking). The Views/, Storage/, ContentView, KinowoApp,
//   RepertoireClient, DetailsStore files are excluded because they
//   import SwiftUI/UIKit/Combine, none of which exist on
//   swift-corelibs-foundation. The Xcode app still compiles those
//   files for the real device build — they live in `Kinowo/` and
//   `xcodebuild` picks them up through the `.pbxproj`.
// - `KinowoAuth` — Combine-dependent auth/sync layer (macOS/iOS only).
//   Compiled as a separate module so StateSyncService can be tested via
//   `swift test` on macOS without pulling SwiftUI into KinowoCore.
// - `KinowoCoreTests` — unit + integration + smoke XCTest cases,
//   resourced with captured production HTML fixtures.
// - `KinowoAuthTests` — sync-service tests (Combine; macOS/iOS only).
#if canImport(Combine)
let authTargets: [Target] = [
    .target(
        name: "KinowoAuth",
        path: "Kinowo",
        sources: [
            "Auth/UserProfile.swift",
            "Auth/UserStateClient.swift",
            "Auth/StateSyncService.swift",
            "Storage/UserPreferences.swift",
        ]
    ),
    .testTarget(
        name: "KinowoAuthTests",
        dependencies: ["KinowoAuth"],
        path: "Tests/KinowoAuthTests"
    ),
]
#else
let authTargets: [Target] = []
#endif

let package = Package(
    name: "Kinowo",
    platforms: [
        // The Foundation APIs we use (URLSession async, DateFormatter)
        // are universally available on these floors; the floor exists
        // mainly so `swift test` on macOS picks a sane toolchain.
        .macOS(.v13),
        .iOS(.v16),
    ],
    products: [
        .library(name: "KinowoCore", targets: ["KinowoCore"]),
    ],
    targets: [
        .target(
            name: "KinowoCore",
            path: "Kinowo",
            exclude: [
                // SwiftUI / UIKit — not on Linux. Listed per-file
                // rather than excluding all of `Views/` so the
                // SwiftUI-free `FlowLayoutMath.swift` (pure
                // CoreGraphics) still ships in `KinowoCore` and is
                // reachable from the test target.
                "Views/CachedAsyncImage.swift",
                "Views/FilmCardView.swift",
                "Views/FilmDetailView.swift",
                "Views/FilmGridView.swift",
                "Views/FiltersBar.swift",
                "Views/FlowLayout.swift",
                "Views/RatingBadgesView.swift",
                "Views/RatingPillStyle.swift",
                "Views/ShowingsView.swift",
                "Views/ShowtimePillStyle.swift",
                "Views/ShowtimeTuningScreen.swift",
                "ContentView.swift",
                "KinowoApp.swift",
                "Auth",
                // Combine (`ObservableObject` / `@Published`) — not on
                // Linux. Logic in these files is a thin URLSession +
                // parser-delegation shim; the parser layer below is
                // what carries the test-worthy behaviour.
                "Networking/RepertoireClient.swift",
                "Networking/DetailsStore.swift",
                "Storage",
                // Asset catalog — not a Swift source.
                "Assets.xcassets",
            ]
        ),
        .testTarget(
            name: "KinowoCoreTests",
            dependencies: ["KinowoCore"],
            path: "Tests/KinowoCoreTests",
            resources: [
                // Captured production HTML, replayed by the parser
                // tests so we don't hit the network on every PR. Each
                // file lives at `Tests/KinowoCoreTests/Fixtures/<x>.html`
                // and is loaded via `Bundle.module.url(forResource:…)`.
                .copy("Fixtures"),
            ]
        ),
    ] + authTargets
)
