# Kinowo iOS

SwiftUI client for [kinowo.net](https://kinowo.net) — the same repertuar
("/" page) the web app shows, on an iPhone.

The app calls `/{city}/api/repertoire` (film grid), `/{city}/api/details`
(synopsis, trailers), `/{city}/api/cinemas` (cinema universe + areas) and
`/api/catalog` (countries + cities) as JSON. There is no HTML path: the models
in `Models/` are `Codable` and the stores decode the API bodies directly.

## Open & run

1. `open ios/Kinowo.xcodeproj`
2. Pick an iPhone simulator (iOS 16+) and ⌘R.
3. Set a development team if you want to run on a real device:
   Project → Kinowo target → *Signing & Capabilities* → *Team*.

The default bundle id is `dev.kinowo.Kinowo`; change it to something under
your team's namespace before signing.

## Features

* Film grid (poster, title, runtime, IMDb / Metacritic / RT / Filmweb badges)
* Showings grouped by day → cinema → time, with booking-URL deep-link
* Search by title, date filter (anytime / today / tomorrow / week)
* Hide a film from the grid (✕ on the poster); manage hidden films via the
  toolbar `eye.slash` button
* Pull-to-refresh re-fetches repertoire from the JSON API

## Layout

```
ios/
├── Kinowo.xcodeproj/
├── Kinowo/
│   ├── KinowoApp.swift         @main App
│   ├── ContentView.swift       Root + filter wiring
│   ├── Models/
│   │   ├── Film.swift          Film, DayShowings, CinemaShowings, Showtime, Ratings
│   │   └── Filters.swift       DateFilter (anytime/today/tomorrow/week)
│   ├── Networking/
│   │   ├── RepertoireClient.swift  URLSession fetcher + @Published store
│   │   ├── DetailsStore.swift      `/api/details` fetcher (synopsis, trailers)
│   │   └── CatalogStore.swift      `/api/catalog` fetcher (countries, cities)
│   ├── Storage/
│   │   └── UserPreferences.swift   UserDefaults-backed hidden-films state
│   ├── Views/
│   │   ├── FilmGridView.swift      LazyVGrid container
│   │   ├── FilmCardView.swift      Poster + title + actions
│   │   ├── ShowingsView.swift      Per-day / per-cinema / per-time layout
│   │   ├── RatingBadgesView.swift  IMDb / Metacritic / RT / Filmweb pills
│   │   ├── FiltersBar.swift        Search field + date pills
│   │   └── FlowLayout.swift        Wrapping-row `Layout` (flex-wrap)
│   └── Assets.xcassets/
├── Package.swift                  SPM manifest — KinowoCore library + tests
├── Tests/KinowoCoreTests/         XCTest cases (Foundation-only)
│   ├── Unit/                      Per-component tests (decoders, filters, prune, metrics)
│   ├── LocalServer/               JSON-contract tests against a live fixture server (env-gated)
│   ├── Fixtures/                  Captured production `/api/details` JSON
│   └── Support/Fixtures.swift     Bundle-resource loader
└── KinowoUITests/                 XCUITest target — drives the simulator
```

## Tests

Three lanes:

```sh
# Unit (fast, offline). Runs in a few seconds on macOS.
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer xcrun swift test

# LocalServer: the JSON contract against a live FixtureServerMain boot
# (opt-in — skipped when KINOWO_LOCAL_URL is unset). Boot the fixture
# server from the repo root in one shell —
#   sbt 'web/PageTest/runMain tools.FixtureServerMain <port-file>'
# — then point the suite at the port it wrote:
KINOWO_LOCAL_URL=http://127.0.0.1:$(cat <port-file>) \
    DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer \
    xcrun swift test --filter LocalServer

# UI tests on a booted simulator (requires full Xcode).
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer xcodebuild test \
    -project Kinowo.xcodeproj -scheme Kinowo \
    -destination 'platform=iOS Simulator,name=iPhone 17' \
    CODE_SIGNING_ALLOWED=NO
```

CI runs the unit and UI lanes from `.github/workflows/ios.yml` (Linux Docker
swift:5.10 and a macos-latest runner, on every PR) and the LocalServer lane
from the `mobile-local-server` job in `ci.yml`, which boots one fixture
server for the iOS and Android suites together.

The LocalServer tests catch server-side JSON drift on the PR that introduces
it; the unit tests catch decoder regressions against pinned fixtures so the
client doesn't quietly rot between server changes.

## Known gaps

* The app loads the full grid in one fetch (≈1.5 MB) and re-parses it on every
  pull-to-refresh; no incremental updates.
* Hidden-film state syncs server-side when signed in; local-device state
  is the fallback when not authenticated.
* Tapping a poster opens a per-film detail screen (synopsis + trailers from
  `/api/details`). Tap a showtime badge to open the booking URL; tap a rating
  badge to open the rating page.
* iPad runs the same layout; not optimised for iPad split-view yet.
* No format/time/from-time filters that the web app has — date + search only.
* No cinema-pin (the web `/kina/:cinema` view).
