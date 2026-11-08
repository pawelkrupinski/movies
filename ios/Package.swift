// swift-tools-version: 6.0
import PackageDescription

// Swift 6 language mode for every target (tools 6.0's default, spelled out in `swiftLanguageModes`
// below): complete concurrency checking with data races as errors, on the Mac and on Linux alike —
// swift-corelibs-foundation 6.2+ annotates URL, DispatchQueue, Calendar and URLSession as Sendable,
// so the Linux CI image (swift:6.2, ios.yml) checks the same sources the Mac does.
// `scripts/ios-compile-check.sh` fails on any concurrency diagnostic, so the count stays at zero.

// SPM view of the Kinowo iOS app. The Xcode app (`Kinowo.xcodeproj`)
// remains the production build for the .ipa; this manifest gives us a
// second view of the same files so we can run XCTest cases via
// `swift test` on Linux/Docker without a full Xcode install.
//
// Targets:
// - `KinowoCore` — Foundation-only sources (Models + the cache /
//   poster layer of Networking). The Views/, Storage/, ContentView, KinowoApp,
//   RepertoireClient, DetailsStore files are excluded because they
//   import SwiftUI/UIKit, which don't exist on swift-corelibs-foundation, or
//   need `KinowoAuth` (below). The Xcode app still compiles those
//   files for the real device build — they live in `Kinowo/` and
//   `xcodebuild` picks them up through the `.pbxproj`.
//   `Location/` is IN the target but wrapped in `#if canImport(CoreLocation)`,
//   so it compiles to nothing on Linux and its resolver's ordering and
//   deadlines are unit-tested on the macOS leg.
// - `KinowoAuth` — the auth/sync layer, macOS/iOS only: `PkceVerifier` hashes
//   with CryptoKit, which Linux lacks (the `@Observable` state and the
//   `observedValues` stream are plain Observation, on Linux too).
//   Compiled as a separate module so StateSyncService can be tested via
//   `swift test` on macOS without pulling SwiftUI into KinowoCore.
// - `KinowoCoreTests` — unit + LocalServer XCTest cases, resourced
//   with a captured production `/api/details` JSON fixture.
// - `KinowoAuthTests` — sync-service tests (macOS/iOS only, with `KinowoAuth`).
// - `KinowoTestSupport` — test doubles shared across test targets
//   (`URLProtocolStub`, `LockedValue`).
#if canImport(CryptoKit)
let authTargets: [Target] = [
    .target(
        name: "KinowoAuth",
        dependencies: ["KinowoCore"],
        path: "Kinowo",
        sources: [
            "Auth/UserProfile.swift",
            "Auth/HiddenFilmsClient.swift",
            "Auth/LanguageClient.swift",
            "Auth/StateSyncService.swift",
            "Auth/ObservedValues.swift",
            "Auth/PkceVerifier.swift",
            "Auth/AuthRequests.swift",
            "Storage/UserPreferences.swift",
            // The country registry + persisted selection: `kinowoBaseURL`
            // (UserProfile) and `UserPreferences` route the API base URL and
            // forced language through these. `Country` also compiles into
            // `KinowoCore` (where it's unit-tested); the Xcode app target
            // compiles each file exactly once via the pbxproj.
            "Models/Country.swift",
            "Storage/CountrySelection.swift",
            // The independent UI-language preference: `UserPreferences`
            // resolves/persists `selectedLanguage` through it, same reasoning
            // as `CountrySelection` above.
            "Storage/LanguageSelection.swift",
        ]
    ),
    .testTarget(
        name: "KinowoAuthTests",
        dependencies: ["KinowoAuth", "KinowoTestSupport"],
        path: "Tests/KinowoAuthTests"
    ),
]
#else
let authTargets: [Target] = []
#endif

// - `KinowoNetworking` — the `@Observable` store layer, macOS/iOS only because it
//   depends on `KinowoAuth`: compiled as its own module so
//   `RepertoireStore` can be driven directly by `swift test` on macOS (a
//   stubbed `URLSession` in, real reload/cache/prune logic exercised)
//   instead of only through the decoder-only line `LocalServerRepertoireTests`
//   exercises. `RepertoireClient.swift` has no `import` statements for
//   `Film`/`City`/`ConditionalPayloadCache`/etc — the Xcode app compiles it into one
//   flat module alongside those types, with no module boundary at all. This
//   target instead depends on `KinowoCore`/`KinowoAuth` properly and the file
//   itself guards a `@testable import` of each behind `#if canImport(...)`,
//   which is false (so a no-op) in the Xcode build where no such module
//   exists, and true only here — `@testable` because none of those types are
//   `public`, and widening that surface just for this target isn't worth it.
//   `DetailsStore.swift`, `CountrySwitch.swift`, `ConditionalListEndpoint.swift` and `ClientSupportStore.swift` ride along under the same `#if canImport` guard.
// - `KinowoNetworkingTests` — `RepertoireStore` / `DetailsStore` / `ClientSupportStore` unit tests
//   (macOS/iOS only).
#if canImport(CryptoKit)
let networkingTargets: [Target] = [
    .target(
        name: "KinowoNetworking",
        dependencies: ["KinowoCore", "KinowoAuth"],
        path: "Kinowo",
        sources: [
            "Networking/RepertoireClient.swift",
            "Networking/DetailsStore.swift",
            "Networking/CountrySwitch.swift",
            "Networking/ConditionalListEndpoint.swift",
            "Networking/ClientSupportStore.swift",
        ]
    ),
    .testTarget(
        name: "KinowoNetworkingTests",
        dependencies: ["KinowoCore", "KinowoNetworking", "KinowoTestSupport"],
        path: "Tests/KinowoNetworkingTests"
    ),
]
#else
let networkingTargets: [Target] = []
#endif

let package = Package(
    name: "Kinowo",
    platforms: [
        // The app's floor, iOS 18 (`IPHONEOS_DEPLOYMENT_TARGET` in the
        // pbxproj; `scripts/ios-compile-check.sh` fails when the two
        // disagree), and its macOS twin for `swift test`: both carry the
        // Observation framework (`@Observable`) and `Synchronization.Mutex`.
        .macOS(.v15),
        .iOS(.v18),
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
                "Views/CardSpacingStyle.swift",
                "Views/CinemaHeaderStyle.swift",
                "Views/CinemaFilterSectionView.swift",
                "Views/AreaPickerSheet.swift",
                "Views/CityGate.swift",
                "Views/FilmCardView.swift",
                "Views/FilmDetailStyle.swift",
                "Views/FilmDetailView.swift",
                "Views/FilmGridView.swift",
                "Views/FiltersSheet.swift",
                "Views/HiddenFilmsList.swift",
                "Views/ListingOverlays.swift",
                "Views/LocalizedCaptions.swift",
                "Views/NameFilterList.swift",
                "Views/SearchBar.swift",
                "Views/TopBar.swift",
                "Views/FlowLayout.swift",
                "Views/Palette.swift",
                "Views/PosterChainImage.swift",
                "Views/InterfaceOrientationReader.swift",
                "Views/RatingBadgesView.swift",
                "Views/RatingPillStyle.swift",
                "Views/ShowingsView.swift",
                "Views/ShowtimePillStyle.swift",
                "Views/ShowtimeTuningScreen.swift",
                "Views/UpdateRequiredView.swift",
                "ContentView.swift",
                "KinowoApp.swift",
                "DeepLinkCoordinator.swift",
                "Auth",
                // The `@Observable` stores — `KinowoNetworking`'s, which
                // needs `KinowoAuth`. Logic in these files is a thin
                // URLSession + parser-delegation shim; the parser layer
                // below is what carries the test-worthy behaviour.
                "Networking/RepertoireClient.swift",
                "Networking/DetailsStore.swift",
                "Networking/CountrySwitch.swift",
                "Networking/ConditionalListEndpoint.swift",
                "Networking/ClientSupportStore.swift",
                // App-target catalog store: URLSession + Bundle.main + UserDefaults.
                "Networking/CatalogStore.swift",
                "Storage",
                // Asset catalog — not a Swift source.
                "Assets.xcassets",
                // String Catalog — an app-target resource; KinowoCore is
                // Foundation-only and ships no localized bundle.
                "Localizable.xcstrings",
                // Bundled catalog seed — an app-target resource read from
                // Bundle.main at launch; not a KinowoCore source.
                "catalog-seed.json",
                // Country registry — Foundation-only, but it's owned by the
                // `KinowoAuth` SPM target (below) because `kinowoBaseURL` +
                // `UserPreferences` there route the base URL / language through
                // it, and SPM forbids one file in two targets. Tested in
                // `KinowoAuthTests`. (The Xcode app compiles it once via pbxproj.)
                "Models/Country.swift",
            ]
        ),
        .testTarget(
            name: "KinowoCoreTests",
            dependencies: ["KinowoCore", "KinowoTestSupport"],
            path: "Tests/KinowoCoreTests",
            resources: [
                // Captured production HTML, replayed by the parser
                // tests so we don't hit the network on every PR. Each
                // file lives at `Tests/KinowoCoreTests/Fixtures/<x>.html`
                // and is loaded via `Bundle.module.url(forResource:…)`.
                .copy("Fixtures"),
            ]
        ),
        // Test doubles more than one test target needs (the canned-HTTP
        // `URLProtocolStub`, the `LockedValue` box). A plain target, so its API
        // is `public`; on Linux too, for `KinowoCoreTests`.
        .target(
            name: "KinowoTestSupport",
            path: "Tests/KinowoTestSupport"
        ),
    ] + authTargets + networkingTargets,
    swiftLanguageModes: [.v6]
)
