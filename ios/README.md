# Kinowo iOS

SwiftUI client for [kinowo.fly.dev](https://kinowo.fly.dev) — the same repertuar
("/" page) the web app shows, on an iPhone.

The app calls `/{city}/api/repertoire` (film grid) and `/{city}/api/details`
(synopsis, trailers) as JSON. `HTMLParser` is still used for the LocalServer
fixture tests (it parses listing HTML produced by the fixture server), but the
production app fetches JSON, not HTML.

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
│   │   ├── HTMLParser.swift        Slice-and-regex extractor
│   │   └── HTMLDecoding.swift      Cheap `&amp;`/`&#39;`/… decoder
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
│   ├── Unit/                      Per-component tests (parsers, filters, prune)
│   ├── Integration/               Pipeline tests against captured fixtures
│   ├── Smoke/                     Live-network tests (env-gated)
│   ├── Fixtures/                  Captured production HTML
│   └── Support/Fixtures.swift     Bundle-resource loader
└── KinowoUITests/                 XCUITest target — drives the simulator
```

## Tests

Three lanes:

```sh
# Unit + integration (fast, offline). ~125 tests, runs in ~2s on macOS.
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer xcrun swift test

# Smoke tests against the live site (opt-in — hits kinowo.fly.dev).
RUN_SMOKE_TESTS=1 DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer \
    xcrun swift test --filter Smoke

# UI tests on a booted simulator (requires full Xcode).
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer xcodebuild test \
    -project Kinowo.xcodeproj -scheme Kinowo \
    -destination 'platform=iOS Simulator,name=iPhone 17' \
    CODE_SIGNING_ALLOWED=NO
```

CI runs the same three lanes (`.github/workflows/ios.yml`): unit + integration
on every PR (Linux Docker, swift:5.10), UI tests on every PR (macos-latest
runner), smoke nightly at 04:00 UTC against production.

The smoke tests catch API drift against the live site; the unit/integration
tests catch parser/decoder regressions against pinned fixtures so the client
doesn't quietly rot between live-site changes.

## Known gaps

* The app loads the full grid in one fetch (≈1.5 MB) and re-parses it on every
  pull-to-refresh; no incremental updates.
* Hidden-film state syncs server-side when signed in; local-device state
  is the fallback when not authenticated.
* No per-film detail page; tapping a poster does nothing (yet). Tap a showtime
  badge to open the booking URL; tap a rating badge to open the rating page.
* iPad runs the same layout; not optimised for iPad split-view yet.
* No format/time/from-time filters that the web app has — date + search only.
* No cinema-pin (the web `/kina/:cinema` view).
