# Local `movies` mirror for a fast `/debug`

`/debug` renders the **whole `movies` corpus** (`movieRepo.findAll()` — a full
collection scan, 1200+ docs). In local dev that read goes over the prod
`flyctl proxy` tunnel, where it takes **30–60s** and intermittently hits
`MongoMovieRepo.findAll`'s 60s timeout — which is swallowed to `Seq.empty`, so
the page silently renders an **empty table** ("sometimes shows no results").

This sets up a **local single-node replica-set Mongo** that mirrors prod's
`movies` collection, and points the web app's `movieRepo` at it, so `/debug`
reads the corpus from a LAN hop (~100ms) instead.

Everything else stays on prod, unchanged: the served repertoire (read model),
auth, and **all writes — including the `/debug` re-enrich (↻) button and the
`/tasks` buttons**, which enqueue onto prod's `tasks` collection where the prod
worker consumes them. The enriched result flows back to the local mirror via
the change-stream tailer, so re-enrich works end-to-end and the `/debug` live
SSE view updates within seconds.

## One-time setup

1. **Point the web app at the mirror.** Add to `.env.local` (leave
   `MONGODB_URI` as-is — it stays the prod tunnel for everything else):

   ```
   MONGODB_MOVIES_MIRROR_URI=mongodb://127.0.0.1:28017/kinowo?directConnection=true
   ```

2. **Start the mirror + sync** in its own terminal (long-running). It starts
   the local Mongo (Docker), seeds `movies` from prod once, then tails prod's
   change stream into the mirror:

   ```
   scripts/local-mirror/mirror.sh
   ```

   It's a self-healing daemon: it brings up its **own** `flyctl proxy` tunnel
   when nothing already serves `:27017` (and uses an existing one — e.g. `sbt
   run`'s — when there is, never fighting it), re-ensures the local Mongo if its
   container stops, re-seeds if the mirror is wiped, and reconnects the tunnel /
   change stream on every drop. So a dropped tunnel, a Docker restart, or a
   stale resume token all recover on their own instead of leaving `/debug`
   silently empty. Leave it running; `Ctrl-C` stops it (and kills the proxy it
   started).

3. **Run the web app** (`sbt run`) and open `/debug` — it now reads the mirror.

When `MONGODB_MOVIES_MIRROR_URI` is unset, the app reads `movies` from prod
exactly as before, so this is opt-in and prod is never affected.

## How it stays in sync

`mirror.sh` tails prod's `movies` change stream (`tail.js`) and applies every
insert/update/replace/delete to the local mirror, persisting a resume token on
the local side so a restart resumes without re-seeding. If the token ages out
of prod's oplog (rare — the oplog is ~1GB, `movies` churn is tiny), the stream
can't resume — `tail.js` detects this (on stream open **or** first `getMore`)
and exits 2, and `mirror.sh` does a full re-seed.

- Force a fresh full copy: `scripts/local-mirror/mirror.sh --reseed`
- The initial seed is a zlib-compressed cursor copy over the tunnel (~50s for
  ~1200 docs — same path the app's findAll uses; a plain `mongodump` cursor
  drops mid-transfer here); the continuous tailer is incremental and cheap.

## Teardown

```
docker rm -f kinowo-local-mongo          # stop + remove the container
docker volume rm kinowo-mirror-data      # drop the mirrored data
```

Then remove `MONGODB_MOVIES_MIRROR_URI` from `.env.local`.

## Ports

| Port  | What                                                              |
|-------|-------------------------------------------------------------------|
| 27017 | prod tunnel (`flyctl proxy`) — everything except `/debug` reads, and the sync **source** |
| 28017 | local mirror Mongo (Docker) — `/debug` reads + the sync **target** |

Override the mirror port with `LOCAL_MIRROR_PORT` (and match it in
`MONGODB_MOVIES_MIRROR_URI`).
