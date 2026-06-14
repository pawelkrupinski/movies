# Kinowo

A Polish cinema repertoire aggregator. Scrapes showtimes from several
Polish cinema chains and independent cinemas, enriches each film with
metadata and ratings from TMDB / IMDb / Filmweb / Metacritic / Rotten
Tomatoes, and serves the combined view as a website and a native iOS
app.

Live at **<https://kinowo.fly.dev>**.

## What it does

- **Scrapes showtimes** from Cinema City, Multikino, Helios, Kino
  Apollo, Kino Bułgarska, Kino Muza, Kino Pałacowe, Rialto, and
  Charlie Monroe. Each cinema has its own client under
  `worker/src/main/scala/services/cinemas/`.
- **Enriches** every film with posters, synopses, trailers, and
  ratings from TMDB, IMDb, Filmweb, Metacritic, Rotten Tomatoes,
  OMDb and Cinemeta (`worker/src/main/scala/services/enrichment/`).
- **Caches** the resolved film records in memory (Caffeine) with
  MongoDB as the write-through store, so a cold start rehydrates
  from Mongo and a refresh tick fans out to the cinemas.
- **Authenticates** users via Google or Facebook OAuth2 and stores
  per-user hidden films and disabled cinemas server-side. Anonymous
  visitors fall back to `localStorage`.
- **iOS and Android apps** (`ios/Kinowo/`, `android/`) render the
  same data — they call the `/api/repertoire` and `/api/details`
  JSON endpoints.

## Stack

| Layer       | Technology                                  |
|-------------|---------------------------------------------|
| Language    | Scala 3                                     |
| Web         | Play Framework (Pekko-based, sbt-plugin 3)  |
| Frontend    | Twirl templates + vanilla JS                |
| Database    | MongoDB (official Scala driver 5)           |
| Cache       | Caffeine (in-process, write-through to Mongo) |
| Scraping    | jsoup, with Zyte proxy for bot-blocked sources |
| DI          | Guice                                       |
| Build       | sbt 1.12, JDK 25 → Java 21 bytecode         |
| iOS         | SwiftUI                                     |
| Hosting     | Fly.io (`kinowo` + `kinowo-worker` apps, region `arn`) |

## Repository layout

```
common/                   # Shared models, utilities
worker/                   # Scrape + enrich app (kinowo-worker Fly app)
│   └── src/main/scala/
│       └── services/
│           ├── cinemas/      # One client per cinema chain / venue
│           └── enrichment/   # TMDB / IMDb / Filmweb / Metacritic / RT clients + ratings
web/                      # Play serving app (kinowo Fly app)
│   └── src/main/
│       ├── scala/
│       │   ├── controllers/  # MovieController, AuthController, UserStateController, ...
│       │   ├── models/       # Movie, MovieRecord, Showtime, Cinema, User, UserState
│       │   ├── modules/      # Guice wiring
│       │   └── services/     # movies/, events/, auth/, users/, lock/
│       ├── twirl/views/      # Twirl templates
│       └── assets/js/        # Vanilla JS assets
testkit/                  # Shared test helpers
e2e/                      # End-to-end specs
android/                  # Native Compose Android app
ios/Kinowo/               # SwiftUI iOS app (uses /api/repertoire + /api/details)
conf/
├── application.conf
├── logback.xml
└── routes
fly.toml, fly.worker.toml, Dockerfile   # Fly.io deploy (two apps, one image)
```

## Running locally

Prereqs: **JDK 17+** (25 recommended), **sbt 1.12**, a running
**MongoDB** instance.

```bash
# Point at your Mongo. The app reads MONGODB_URI; if unset the cache
# falls back to in-memory-only (no rehydrate, no write-through).
# MONGODB_DB picks the database name; defaults to `kinowo`.
export MONGODB_URI=mongodb://localhost:27017
export MONGODB_DB=kinowo

sbt run
# → http://localhost:9000
```

The first start may take a minute as the scrapers fan out across every
cinema and enrich each film. Subsequent starts rehydrate the in-memory
cache from Mongo in a few seconds via 4-way parallel cursors (see
`MongoMovieRepo.findAll` and `MeasureStartup`), then begin serving
immediately.

### Useful local endpoints

- `/` — main repertoire view
- `/film?title=...` — single-film detail page
- `/kina` — view grouped by cinema, `/kina/:cinema` to pin one
- `/plan` — pick movies + cinemas + rooms, get an availability summary
- `/debug` — dev page exposing the cache contents
- `POST /debug/reenrich?title=...` — drop one row and re-fetch every source
- `POST /debug/rehydrate` — reload the in-memory cache from Mongo
- `/health` — Fly health check

## Tests

Three separate sbt configurations so CI can fan them out:

```bash
sbt testUnit                 # unit tests          → <module>/src/test/scala/
sbt itAll                    # integration tests   → <module>/src/it/scala/
sbt web/PageTest/test        # browser/page tests  → web/src/page/scala/ (needs Chrome)
```

`PageTest` drives a real Chrome over CDP, so it lives in its own
configuration and stays out of the default unit run so non-browser
CI runners don't need a Chrome install.

## Deploying

Fly.io. Two apps in `arn` (Stockholm — nearest Fly region to Polish
users, no `waw` exists): `kinowo` (serving, 1 GB shared-CPU) and
`kinowo-worker` (scrape/enrich). Mongo is self-hosted on Fly too —
`kinowo-mongo` app, same region, `fly/mongo/`.

```bash
flyctl deploy
```

The Dockerfile builds the Play distribution via sbt-native-packager;
`fly.toml` pins the runtime config (memory, ALPN, health check). Brief
downtime during a redeploy is acceptable per project conventions —
this is a hobby-traffic app, not a 24/7 SLA.

## Conventions

Project-specific conventions for working on this codebase (script
discipline, backfill rules, license posture, etc.) live in
[`CLAUDE.md`](./CLAUDE.md).

## License

Source-available under the
[PolyForm Noncommercial License 1.0.0](./LICENSE). Free for personal,
research, educational, and other noncommercial use.
**Commercial use requires a separate license** — contact
pawel.krupinski@gmail.com.
