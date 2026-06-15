# Local Mongo for dev — `/debug` mirror + web/worker playground

A single **native (Homebrew) single-node replica-set Mongo on `:28017`** backs
local development. It's a replica set (not a standalone) because both the
`/debug` live SSE view and the local web+worker stack read **change streams**,
which a standalone rejects (error 40573) — the same reason prod runs `--replSet
rs0`. One instance, two databases:

| DB           | Role | Written by |
|--------------|------|------------|
| `kinowo_prod_mirror` | The `/debug` corpus mirror — prod's `movies` synced in, read-only locally | `mirror.sh` tailer |
| `kinowo_local` | The local **web + worker** read/write playground (its own change streams) | a locally-run worker |

Why the `kinowo_prod_mirror` mirror exists: `/debug` renders the whole `movies`
corpus (`movieRepo.findAll()` — a full scan, 1200+ docs). Read over the prod
`flyctl` tunnel that takes **8–30s** and intermittently times out to an empty
table; from the LAN mirror it's **~200ms**.

> One source of truth per collection. The tailer **replaces**
> `kinowo_prod_mirror.movies` from prod, so don't also write it from a local
> worker — point the worker at `kinowo_local`. That's the whole reason for two
> databases.

## One-time setup

1. **Install the server** (you likely only have `mongosh` + tools):

   ```
   brew tap mongodb/brew
   brew install mongodb-community@7.0     # matches prod 7.0.x
   ```

2. **Point the web app's `/debug` at the mirror.** Add to `.env.local` (leave
   `MONGODB_URI` as-is — it stays the prod tunnel for everything else):

   ```
   MONGODB_MOVIES_MIRROR_URI=mongodb://127.0.0.1:28017/kinowo_prod_mirror?directConnection=true
   ```

3. **Start the mirror + sync** — either in a terminal (`scripts/local-mirror/mirror.sh`)
   or, better, as a login service (next section). It brings up the native Mongo
   via `start-local-mongo.sh` (writing a replica-set config to
   `$(brew --prefix)/etc/mongod.conf` and `rs.initiate()`-ing once), seeds
   `kinowo_prod_mirror.movies` from prod, then tails prod's change stream into it.

4. **Run the web app** (`sbt run`) and open `/debug` — it now reads the mirror.

When `MONGODB_MOVIES_MIRROR_URI` is unset, the app reads `movies` from prod
exactly as before, so the mirror is opt-in and prod is never affected.

## Run it as a service (no terminal to babysit)

Install `mirror.sh` as a **macOS launchd user agent** — starts at login,
restarts on failure:

```
scripts/local-mirror/service.sh install     # install + start (runs at login)
scripts/local-mirror/service.sh status      # show state + pid
scripts/local-mirror/service.sh logs        # tail the agent log
scripts/local-mirror/service.sh uninstall   # stop + remove the agent
```

The agent runs `mirror.sh`, a self-healing daemon: it brings up its **own**
`flyctl proxy` tunnel when nothing already serves `:27017` (and uses an existing
one — e.g. `sbt run`'s — when there is, never fighting it), re-ensures the native
Mongo via `brew services` if it's stopped, re-seeds if `kinowo_prod_mirror` is empty, and
reconnects the tunnel / change stream on every drop. So a dropped tunnel, a
stopped Mongo, or a stale resume token all recover on their own instead of
leaving `/debug` empty. The Mongo itself also restarts at login (it's a
`brew services` agent). Logs: `~/Library/Logs/kinowo-local-mirror.log`. Prereqs:
`MONGODB_MOVIES_MIRROR_URI` set (above) and `flyctl auth login` done.

## Running the web + worker stack locally (`kinowo_local`)

Point both apps at the native Mongo, using the **`kinowo_local`** database so the
local worker's writes never collide with the prod-synced `kinowo_prod_mirror`:

```
MONGODB_URI=mongodb://127.0.0.1:28017/?directConnection=true
MONGODB_DB=kinowo_local
```

`kinowo_local` is a normal database on the same replica set, so the read-model
projector, the staging fold (transactions), and the Filmweb-fallback watcher all
get the change streams they need. `/debug` keeps reading the prod-synced
`kinowo_prod_mirror` (via `MONGODB_MOVIES_MIRROR_URI`); the rest of the local
site serves from whatever the local worker projects into `kinowo_local`.

## How the mirror stays in sync

`mirror.sh` tails prod's `movies` change stream (`tail.js`) and applies every
insert/update/replace/delete to `kinowo_prod_mirror`, persisting a resume token locally so a
restart resumes without re-seeding. If the token ages out of prod's oplog
(rare), `tail.js` detects it (on stream open **or** first `getMore`) and exits 2,
and `mirror.sh` does a full re-seed.

- Force a fresh full copy: `scripts/local-mirror/mirror.sh --reseed`
- The initial seed is a zlib-compressed cursor copy over the tunnel (~50s for
  ~1300 docs); the continuous tailer is incremental and cheap.

## Sync the admin-curated `titleRules` into `kinowo_local`

`titleRules` is the live rule set `TitleNormalizer` runs (edited from the admin
UI). It's admin-curated, so `reset-corpus.sh` deliberately leaves it alone and
nothing else seeds it — a fresh `kinowo_local` starts with an **empty**
collection and the local stack falls back to the frozen `TitleRuleDefaults`,
diverging from prod's normalisation. Pull the live prod set across:

```
scripts/local-mirror/sync-title-rules.sh            # one-way prod → kinowo_local
scripts/local-mirror/sync-title-rules.sh --dry-run  # dump + count only, change nothing
```

One-shot and on-demand (title rules change rarely) — re-run after admin edits.
It `mongodump`s the one collection from prod over the tunnel, guards on the
record count (≥10, same floor as `scripts.DumpTitleRules`), then
`mongorestore --drop`s it into `kinowo_local` — a true one-way mirror, so a rule
deleted in prod also disappears locally. The local web+worker watch
`kinowo_local`'s `titleRules` change stream, so re-running against a **live**
local stack hot-swaps the rules. Reads `MONGODB_URI`/`MONGODB_DB` (prod source)
and `LOCAL_MONGO_URI`/`LOCAL_MONGO_DB` (default `kinowo_local`) from `.env.local`.

> This is the DB→DB (prod → local) direction. Two other syncs touch the same
> collection and aren't this: `scripts.DumpTitleRules` (DB→**code**, refreshing
> the `GeneratedTitleRules.scala` test mirror via
> `.github/workflows/sync-title-rules.yml`), and `scripts.ApplyExtraTitleRules`
> (code→prod-DB, proposing new rules).

## Teardown

```
scripts/local-mirror/service.sh uninstall   # stop the sync agent
brew services stop mongodb-community@7.0     # stop the Mongo
```

Then remove `MONGODB_MOVIES_MIRROR_URI` from `.env.local`. To wipe the data,
`rm -rf "$(brew --prefix)/var/mongodb"` while it's stopped.

## Ports

| Port  | What                                                              |
|-------|-------------------------------------------------------------------|
| 27017 | prod tunnel (`flyctl proxy`) — everything except `/debug` reads, and the sync **source** |
| 28017 | native local Mongo — `/debug` mirror reads + the sync **target**, and the web+worker `kinowo_local` |

Override the Mongo port with `LOCAL_MIRROR_PORT` (and match it in
`MONGODB_MOVIES_MIRROR_URI`).
