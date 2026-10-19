# Kinowo

A multi-country cinema repertoire aggregator. Scrapes showtimes from
cinema chains, aggregators and independent cinemas in Poland, the UK,
Germany, the US and Spain, resolves each listing to one film, enriches it
with metadata and ratings from TMDB / IMDb / Filmweb (Poland only) /
Metacritic / Rotten Tomatoes, and serves the combined view as a website
and native iOS and Android apps.

Live at **<https://kinowo.net>** (Poland) and
**<https://showtimes.cc>** (`/uk`, `/de`, `/us`, `/es`).

## What it does

- **Scrapes showtimes** from chains (Cinema City, Multikino, Helios,
  Odeon, Vue, AMC, …), aggregator platforms (kinoheld, Flicks, …) and
  independent venues. Each source has its own client under
  `scrapers/src/main/scala/services/cinemas/`, in a per-country
  subpackage (`pl/`, `uk/`, `de/`, `us/`, `es/`); the country-agnostic
  scraping machinery (shared platform clients, fallbacks, retries) lives
  in its `common/`, and the venue rosters in `roster/`.
- **Resolves identity**: every scraped listing is matched to one film by
  the identity resolver (the `identity/` sbt module, projected by
  `worker/.../services/identity/`); see
  [`docs/design/identity-resolver.md`](docs/design/identity-resolver.md).
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
  same data — they call the `/:city/api/repertoire`, `/:city/api/details`
  and `/:city/api/cinemas` JSON endpoints.

## Stack

| Layer       | Technology                                  |
|-------------|---------------------------------------------|
| Language    | Scala 3                                     |
| Web         | Play Framework (Pekko-based, sbt-plugin 3)  |
| Frontend    | Twirl templates + vanilla JS                |
| Database    | MongoDB (official Scala driver 5)           |
| Cache       | Caffeine (in-process, write-through to Mongo) |
| Scraping    | jsoup, with a Decodo residential proxy for bot-blocked sources (Zyte for the one the proxy cannot reach) |
| DI          | Compile-time (Play `BuiltInComponents`, `modules.AppLoader`) |
| Build       | sbt 1.12, JDK 27 → Java 21 bytecode         |
| iOS         | SwiftUI                                     |
| Android     | Jetpack Compose                             |
| Hosting     | k3s on Hetzner (web + worker pods, `infra/`)  |

## Repository layout

```
common/                   # Shared domain used by both apps
│   └── src/main/scala/
│       ├── models/           # Movie, MovieRecord, Showtime, Cinema, User, ...
│       └── services/         # movies/ (cache/repository), events/, cinemas/, readmodel/,
│                             #   titlerules/, freshness/, tasks/
identity/                 # The identity resolver (listing → film), its own sbt module
worker/                   # Scrape + enrich app (one pod per country)
│   └── src/main/scala/services/
│       ├── cinemas/          # One client per cinema chain / venue
│       ├── enrichment/       # TMDB / IMDb / Filmweb / Metacritic / RT clients + ratings
│       ├── identity/         # identity projection: listings → films → read model
│       └── tasks/, schedule/, alerts/   # scrape loop, task queue, alerting
web/                      # Play serving app (one pod per country)
│   ├── src/main/scala/
│   │   ├── controllers/      # MovieController, AuthController, UserStateController, ...
│   │   ├── modules/          # AppLoader composition root (compile-time DI)
│   │   └── services/         # auth/ (OAuth2), users/ (per-user state)
│   ├── src/main/twirl/views/ # Twirl templates
│   ├── src/main/assets/js/   # Vanilla JS assets
│   └── src/main/resources/   # application.conf, logback.xml, routes
testkit/                  # Shared test helpers
e2e/                      # End-to-end specs (per-country convergence suites)
page-tests-playwright/    # Playwright browser tests (mobile + desktop)
scripts/                  # Agent/CI tooling: identity-slice, sbt-warm, hooks, ... (self-tested)
data/                     # Per-country roster data pipelines
tools/devpanel/           # macOS developer palette
android/                  # Native Compose Android app
ios/Kinowo/               # SwiftUI iOS app (uses /api/repertoire + /api/details)
Dockerfile                # One image, BIN build-arg selects web or worker
infra/                    # NixOS fleet, Prometheus + Grafana (k8s manifests: movies-gitops)
docs/                     # Runbooks, design notes, investigation logs (index: docs/README.md)
```

## Running locally

Prereqs: **JDK 27** (the build emits Java 21 bytecode), **sbt 1.12**, and a local
**MongoDB** (the native brew single-node replica set on port **28017** —
`scripts/local-mirror/start-local-mongo.sh`, see `scripts/local-mirror/README.md`).

The repository is two apps: **worker** scrapes + enriches into Mongo and
projects a read model; **web** serves that read model. Only **web** runs
locally: `scripts/dev-server.sh` runs `sbt web/run` on :9000 against the local
Mongo (starting it if nothing answers), reading `MONGODB_URI` / `MONGODB_DB`
and serving whatever a worker has already projected into the read model
(`web_movies` / `web_screenings`, loaded into memory by `WebReadModel`). The
worker runs only in the cluster and in the test layers.

### Useful local endpoints (city-scoped routes take a `:city` slug, e.g. `poznan`)

- `/` — landing page; `/:city/` — main repertoire view
- `/:city/movie/:slug` — single-film detail page (the older
  `/:city/movie?title=...` form still resolves, 301-ing onto the slug, and so
  do the pre-rename `/:city/film…` / `/:city/filmy` addresses)
- `/:city/movies` — browse / filter the full film catalogue
- `/:city/api/repertoire`, `/:city/api/details`, `/:city/api/cinemas` — the JSON feeds the mobile apps read
- `/debug` — dev page exposing the source `movies` corpus;
  `/debug/readmodel` — the projected read model web actually serves
- `POST /:city/debug/rehydrate` — reload the in-memory cache from Mongo
- `/health` — liveness/readiness probe

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

**k3s on Hetzner.** One web pod and one worker pod per country, all from
**one image** whose `BIN` build-arg selects the launcher (`web` or
`worker`). Mongo is self-hosted on `mongo-1`; `monitoring-1` carries the
k3s control plane, Prometheus and Grafana. The whole fleet is NixOS,
declared under [`infra/`](./infra); the Kubernetes manifests live in a
separate repository (`movies-gitops`, fetched by `infra/bin/fetch-gitops`).

Every push to `main` builds and pushes the two images to GHCR
(`.github/workflows/main.yml`, jobs `publish-web` / `publish-worker`), each
path-gated so a web-only push does not restart the workers. **Flux's
image automation rolls them out** — CI does not deploy to the cluster.

**Nothing from this repository deploys to Fly.io any more.** The last
Fly app, the `kinowo.fly.dev` redirect host (`docs/domain-cutover.md`), lost
its config, its CI deploy and its retirement composition root on 2026-10-02.

Worker downtime is acceptable; the web tier should stay answering (a
brief 5xx window during a rolling deploy is fine) — see the downtime
policy in [`.claude/skills/agent-ops/SKILL.md`](.claude/skills/agent-ops/SKILL.md).
Dashboards: <https://grafana.kinowo.net> (sources under
`infra/nix/files/monitoring/grafana/dashboards/apps/`).

## Conventions

Project conventions for working on this codebase (test layers, the
commit gate, snapshots, worktree and landing flow, design rules) live in
[`CLAUDE.md`](./CLAUDE.md), with task-specific detail in the skills
under [`.claude/skills/`](.claude/skills). Every other document is
indexed in [`docs/README.md`](docs/README.md).

## License

Source-available under the
[PolyForm Noncommercial License 1.0.0](./LICENSE). Free for personal,
research, educational, and other noncommercial use.
**Commercial use requires a separate license** — contact
pawel.krupinski@gmail.com.
