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
  `worker/src/main/scala/services/cinemas/`, in a per-country
  subpackage (`pl/`, `uk/`, `de/`); the country-agnostic scraping
  machinery lives in `common/`.
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
| DI          | Compile-time (Play `BuiltInComponents`, `modules.AppLoader`) |
| Build       | sbt 1.12, JDK 25 → Java 21 bytecode         |
| iOS         | SwiftUI                                     |
| Hosting     | Fly.io (`kinowo` + `kinowo-worker` apps, region `arn`) |

## Repository layout

```
common/                   # Shared domain used by both apps
│   └── src/main/scala/
│       ├── models/           # Movie, MovieRecord, Showtime, Cinema, User, ...
│       └── services/         # movies/ (cache/repository/merge), events/, cinemas/,
│                             #   readmodel/, titlerules/, freshness/, staging/, tasks/
worker/                   # Scrape + enrich app (kinowo-worker Fly app)
│   └── src/main/scala/services/
│       ├── cinemas/          # One client per cinema chain / venue
│       ├── enrichment/       # TMDB / IMDb / Filmweb / Metacritic / RT clients + ratings
│       └── tasks/, staging/, schedule/, alerts/   # scrape loop + read-model projection
web/                      # Play serving app (kinowo Fly app)
│   ├── src/main/scala/
│   │   ├── controllers/      # MovieController, AuthController, UserStateController, ...
│   │   ├── modules/          # AppLoader composition root (compile-time DI)
│   │   └── services/         # auth/ (OAuth2), users/ (per-user state)
│   ├── src/main/twirl/views/ # Twirl templates
│   ├── src/main/assets/js/   # Vanilla JS assets
│   └── src/main/resources/   # application.conf, logback.xml, routes
testkit/                  # Shared test helpers
e2e/                      # End-to-end specs
android/                  # Native Compose Android app
ios/Kinowo/               # SwiftUI iOS app (uses /api/repertoire + /api/details)
fly.toml, fly.worker.toml, Dockerfile   # Fly.io deploy (two apps, one image)
```

## Running locally

Prereqs: **JDK 17+** (25 recommended), **sbt 1.12**, and a local
**MongoDB** (the local stack expects it on port **27018** as a
single-node replica set — see below).

The repository is two apps: **worker** scrapes + enriches into Mongo and
projects a read model; **web** serves that read model. `sbt localStack`
boots both against one local Mongo — the worker replays the checked-in
fixtures (`test/resources/fixtures/today`) so you get a full corpus
offline, and web serves it on :9000:

```bash
# One-time: a local Mongo on 27018 (replica set → live change streams)
docker run -d -p 27018:27017 --name kinowo-local-mongo mongo --replSet rs0
docker exec kinowo-local-mongo mongosh --quiet --eval 'rs.initiate()'

sbt localStack
# → http://localhost:9000  (web serving; worker replaying fixtures in the background)
```

To run just the serving app against an existing Mongo, use `sbt web/run`
— it reads `MONGODB_URI` / `MONGODB_DB` (db defaults to `kinowo`) and
serves whatever a worker has already projected into the read model. Web
rehydrates its in-memory cache from Mongo via 4-way parallel cursors
(see `MongoMovieRepository.findAll` and `MeasureStartup`).

### Useful local endpoints (city-scoped routes take a `:city` slug, e.g. `poznan`)

- `/` — landing page; `/:city/` — main repertoire view
- `/:city/film?title=...` — single-film detail page
- `/:city/filmy` — browse / filter the full film catalogue
- `/:city/plan` — pick movies + cinemas + rooms, get an availability summary
- `/:city/api/repertoire`, `/:city/api/details` — the JSON feeds the mobile apps read
- `/debug` — dev page exposing the source `movies` corpus;
  `/debug/readmodel` — the projected read model web actually serves
- `POST /debug/reenrich?title=...` — drop one row and re-fetch every source
- `POST /:city/debug/rehydrate` — reload the in-memory cache from Mongo
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
users, no `waw` exists): `kinowo` (serving, 1 GB shared-CPU, `fly.toml`)
and `kinowo-worker` (scrape/enrich, `fly.worker.toml`), built from **one
image** whose `BIN` build-arg selects the launcher (`web` or `worker`).
Mongo is self-hosted on Fly too — `kinowo-mongo` app, same region,
`fly/mongo/`.

Every push to `main` deploys both legs via GitHub Actions
(`.github/workflows/deploy.yml`): it `sbt "web/stage" "worker/stage"`s
each app, then `flyctl deploy --build-arg BIN=<bin>` per app. To deploy
one app by hand after staging it:

```bash
flyctl deploy -c fly.toml         --build-arg BIN=web
flyctl deploy -c fly.worker.toml  --build-arg BIN=worker
```

`fly.toml` / `fly.worker.toml` pin each app's runtime config (memory,
ALPN, health check). Brief downtime during a redeploy is acceptable per
project conventions — this is a hobby-traffic app, not a 24/7 SLA.

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
