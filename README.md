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
  `app/services/cinemas/`.
- **Enriches** every film with posters, synopses, trailers, and
  ratings from TMDB, IMDb, Filmweb, Metacritic, Rotten Tomatoes,
  OMDb and Cinemeta (`app/services/enrichment/`).
- **Caches** the resolved film records in memory (Caffeine) with
  MongoDB as the write-through store, so a cold start rehydrates
  from Mongo and a refresh tick fans out to the cinemas.
- **Authenticates** users via Google or Facebook OAuth2 and stores
  per-user favourites, hidden films, and disabled-cinema lists
  server-side. Anonymous visitors fall back to `localStorage`.
- **iOS app** (`ios/Kinowo/`) renders the same data — talks to the
  website directly and parses its HTML rather than calling a
  separate JSON API.

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
| Hosting     | Fly.io (`kinowo` app, region `arn`)         |

## Repository layout

```
app/
├── controllers/          # MovieController, AuthController, UserStateController, ...
├── models/               # Movie, MovieRecord, Showtime, Cinema, User, UserState
├── modules/              # Guice wiring
├── services/
│   ├── cinemas/          # One client per cinema chain / venue
│   ├── enrichment/       # TMDB / IMDb / Filmweb / Metacritic / RT clients + ratings
│   ├── movies/           # MovieCache, MovieRepo, MovieService, merge/patch logic
│   ├── events/           # In-process event bus (cache → enrichment listeners)
│   ├── auth/             # OAuth2 providers
│   ├── users/            # User + per-user state persistence
│   └── lock/             # Mongo-backed distributed lock
├── views/                # Twirl templates
└── tools/                # Test wiring + dev utilities
conf/
├── application.conf
├── logback.xml
└── routes
ios/Kinowo/               # SwiftUI iOS app (scrapes the website's HTML)
test/scala/               # Unit tests (sbt test)
it/scala/                 # Integration tests (sbt IntegrationTest/test)
page/scala/               # Browser-driven page-regression tests (sbt PageTest/test)
fly.toml, Dockerfile      # Fly.io deploy
```

## Running locally

Prereqs: **JDK 17+** (25 recommended), **sbt 1.12**, a running
**MongoDB** instance.

```bash
# Point at your Mongo. The app reads MONGO_URI; if unset it defaults to
# mongodb://localhost:27017 with database `movies`.
export MONGO_URI=mongodb://localhost:27017

sbt run
# → http://localhost:9000
```

The first start may take a minute as the scrapers fan out across every
cinema and enrich each film. Subsequent starts rehydrate from Mongo and
are near-instant.

### Useful local endpoints

- `/` — main repertoire view
- `/film?title=...` — single-film detail page
- `/kina` — view grouped by cinema, `/kina/:cinema` to pin one
- `/ulubione` — favourites (requires login)
- `/debug` — dev page exposing the cache contents
- `POST /debug/reenrich?title=...` — drop one row and re-fetch every source
- `POST /debug/rehydrate` — reload the in-memory cache from Mongo
- `/health` — Fly health check

## Tests

Three separate sbt configurations so CI can fan them out:

```bash
sbt test                     # unit tests          → test/scala/
sbt IntegrationTest/test     # integration tests   → it/scala/
sbt PageTest/test            # browser/page tests  → page/scala/ (needs Chrome)
```

`PageTest` drives a real Chrome over CDP, so it lives in its own
configuration and stays out of the default `sbt test` so non-browser
CI runners don't need a Chrome install.

## Deploying

Fly.io. One machine in `arn` (Stockholm), 1 GB shared-CPU, with a
write-through Mongo elsewhere.

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
