# Kinowo iOS

SwiftUI client for [kinowo.fly.dev](https://kinowo.fly.dev) — the same repertuar
("/" page) the web app shows, on an iPhone.

The app fetches the production HTML and parses the film grid directly. There is
no JSON API yet — if the web template changes shape (`<div class="col"
data-title=…>`, `data-date=`, `data-cinema=`, `data-time=`, `data-format=`,
`class="rating-…"`), the parser will need to be updated to match.

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
* Favourite a whole film (★ on the poster) or a single screening (★ on the
  time badge) — persists in `UserDefaults` per device
* Hide a film from the grid (✕ on the poster); manage hidden films via the
  toolbar `eye.slash` button
* Pull-to-refresh re-fetches the kinowo HTML

Favourite-screening ids match the web app's format
(`title|cinema|YYYY-MM-DDTHH:MM`) so a future server-sync feature can reuse
them without translation.

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
│   │   └── UserPreferences.swift   UserDefaults-backed favourites/hidden state
│   ├── Views/
│   │   ├── FilmGridView.swift      LazyVGrid container
│   │   ├── FilmCardView.swift      Poster + title + actions
│   │   ├── ShowingsView.swift      Per-day / per-cinema / per-time layout
│   │   ├── RatingBadgesView.swift  IMDb / Metacritic / RT / Filmweb pills
│   │   ├── FiltersBar.swift        Search field + date pills
│   │   ├── HiddenFilmsView.swift   Manage hidden films
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

The HTML parser is fragile by construction — any change to the kinowo template
markup can break it. The smoke tests catch that drift; the unit/integration
tests catch parser regressions against pinned fixtures so the parser doesn't
quietly rot between live-site changes.

## Known gaps

* The app loads the full grid in one fetch (≈1.5 MB) and re-parses it on every
  pull-to-refresh; no incremental updates.
* No authenticated login (the web app's Google/Facebook OAuth is server-side
  only). Favourites/hidden are local-device only.
* No per-film detail page; tapping a poster does nothing (yet). Tap a showtime
  badge to open the booking URL; tap a rating badge to open the rating page.
* iPad runs the same layout; not optimised for iPad split-view yet.
* No format/time/from-time filters that the web app has — date + search only.
* No cinema-pin (the web `/kina/:cinema` view).
