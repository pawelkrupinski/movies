# Local Mongo for dev — `/debug` mirror + web/worker playground

A single **native (Homebrew) single-node replica-set Mongo on `:28017`** backs
local development. It's a replica set (not a standalone) because both the
`/debug` live SSE view and the local web+worker stack read **change streams**,
which a standalone rejects (error 40573) — the same reason prod runs `--replSet
rs0`. One instance, two databases:

| DB           | Role | Written by |
|--------------|------|------------|
| `<prod db>_prod_mirror` | The `/debug` mirror — prod's data synced in, read-only locally. One per country: `kinowo_prod_mirror`, `kinowo_uk_prod_mirror`, `kinowo_de_prod_mirror` | `mirror.sh` tailers |
| `kinowo_local` | The local **web + worker** read/write playground (its own change streams) | a locally-run worker |

Why the mirror exists: everything `/debug` reads is otherwise a prod-tunnel
round-trip, and the tunnel charges **~110ms per round-trip** on top of queries
that themselves run in single-digit ms. Two shapes of pain follow. The corpus
table is a full `movies` scan (`movieRepo.findAll()`, 1200+ docs) that takes
**8–30s** and intermittently times out to an empty table. And expanding one row
costs three sequential reads — the row, its enrichment attempts, its rating
cadence — i.e. **~340ms of pure latency**. Against the LAN mirror the ping is
**~0.5ms**: the corpus lands in ~200ms and a row expand in single-digit ms.

**What's mirrored** — `scripts/local-mirror/mirror-targets.js`, the single
source of truth for both halves:

- **Collections:** `movies`, `screenings`, `movie_slots`,
  `enrichment_attempts`, `rating_cadence`, `pending_movies`, `web_movies`,
  `web_screenings` — exactly what a `/debug` load reads. A collection this list
  omits reads as permanently **empty**, not slowly-from-prod: the `/debug`
  country stacks read the mirror unconditionally. `MongoConnectionSpec` fails
  if the app reads one the list omits, diffing it against
  `services.DebugMirror` rather than a literal — which is how `web_movies` /
  `web_screenings` (a blank `/debug/readmodel` for every non-boot country) and
  `pending_movies` (a blank staging table) turned up missing.
- **Databases:** every `kinowo*` database the tunnel exposes, discovered at
  startup (override with `KINOWO_MIRROR_DBS`). So the navbar's country switch
  (`/debug?country=uk`) is LAN-fast too, not just the boot country.

`MongoConnection.mirrorDbFor` derives the same `_prod_mirror` names on the Scala
side, and `MongoConnectionSpec` fails if the two drift apart or if `/debug`
starts reading a collection the sync doesn't carry.

> One source of truth per collection. The tailers **replace** the mirrored
> collections from prod, so don't also write them from a local worker — point
> the worker at `kinowo_local`. That's the whole reason for the suffix.

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

3. **Check you can reach prod Mongo over ssh.** The tunnel is an ssh local
   forward, opened by `scripts/local-mirror/prod-tunnel.sh` — the single
   definition every prod-sourced local script shares. It must work
   NON-INTERACTIVELY, because launchd has no terminal to answer a passphrase
   prompt with:

   ```
   ssh -o BatchMode=yes root@178.105.221.61 true      # must exit 0, silently
   ```

   Override the target with `KINOWO_MONGO_SSH=<user>@<host>` in `.env.local`
   (or in the environment) if the database is moved or a rescue host stands in.

4. **Start the mirror + sync** — either in a terminal (`scripts/local-mirror/mirror.sh`)
   or, better, as a login service (next section). It brings up the native Mongo
   via `start-local-mongo.sh` (writing a replica-set config to
   `$(brew --prefix)/etc/mongod.conf` and `rs.initiate()`-ing once), discovers
   prod's `kinowo*` databases, seeds each one's mirrored collections, then tails
   a change stream per database into them. One supervised tailer process per
   database — a change stream is per-database here (a deployment-wide one is
   `Unauthorized` for these credentials) and mongosh is single-threaded, so N
   databases means N children rather than one loop.

5. **Run the web app** (`sbt run`) and open `/debug` — it now reads the mirror.

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
ssh tunnel when nothing already serves `:27017` (and uses an existing
one — e.g. `sbt run`'s — when there is, never fighting it), re-ensures the native
Mongo via `brew services` if it's stopped, re-seeds when a mirror is empty **or
has drifted** (below), and reconnects the tunnel / change stream on every drop.
So a dropped tunnel, a stopped Mongo, or a stale resume token all recover on
their own instead of leaving `/debug` empty. The Mongo itself also restarts at
login (it's a `brew services` agent). Logs:
`~/Library/Logs/kinowo-local-mirror.log`, trimmed to its last 2000 lines
whenever it passes 8MB (in place — launchd holds an append fd on it, so renaming
would strand the agent writing to the old inode). Prereqs:
`MONGODB_MOVIES_MIRROR_URI` set (above) and non-interactive ssh to the Mongo
host working.

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

### The seed → tail handover — why "start from now" loses writes

A seed copies the mirrored collections **one at a time** and only then hands
over to the tailer, so there is a window — the rest of the copy — in which a
write to an already-copied collection belongs to neither. `seed.js` used to
clear the resume token on its way out and `tail.js` started *from now*, i.e.
from the moment the LAST collection finished, which made that window a permanent
hole. Nothing repaired it: the staleness gate above measures lag from
`updatedAt` (a delete carries none) and drift from `movies` only, so a stale row
in any other collection reads green forever.

Found 2026-08-29: a re-seed copied DE's `pending_movies` while three films were
still incubating, the staging fold deleted those rows during the copy's
remaining collections, and `/debug` showed three films **stuck in staging** that
prod had folded an hour earlier — three ghosts in UK too.

So `seed.js` now captures prod's `operationTime` **before** it reads the first
collection and persists it as the mirror's start point, and `tail.js` opens
there rather than at now (`stream-start.js` owns the choice: resume token first,
then the seed's time, then now). The cost is replaying the copy's own window,
which is free — every apply is idempotent (`replaceOne` upsert / `deleteOne`).

The decision is a pure function, asserted with no Mongo:

```
mongosh --nodb --quiet --file scripts/local-mirror/stream-start.js \
                       --file scripts/local-mirror/stream-start-spec.js
```

and the handover itself is asserted end-to-end against the local replica set —
seed, delete a row, start the tailer, and the mirror must still lose the row
(`mirror-sync-spec.sh`, which also covers the ghost signal below):

```
scripts/local-mirror/mirror-sync-spec.sh
```

### The staleness gate — why a mirror can't quietly rot

Resuming is only the right recovery while the saved token still tracks prod. A
tailer that crash-loops on a token it can never advance leaves a mirror that is
**non-empty and arbitrarily out of date**, and the old "re-seed only when
`movies` is empty" check called that healthy — so `/debug` served weeks-old data
with nothing to notice (found 2026-07-27: ~1000 restarts logged while `kinowo_uk`
sat at 406 of prod's 1555 movies).

So every supervision cycle asks `staleness.js` first, and re-seeds when any of
these trips — and asks again every ten minutes WHILE the tailer streams
(`AUDIT_POLLS`), because a healthy tailer never ends and the drift these catch
appears mid-stream, not at startup:

- **lag** — prod's newest `updatedAt` (across `movies` + `screenings`) minus the
  mirror's, over 30 minutes. Measured mirror-vs-prod, never against the wall
  clock, so an idle prod at 04:00 stays "fresh" at lag 0 rather than triggering
  a pointless re-seed every night.
- **count drift** — `movies` off by more than 2%. Deletes carry no `updatedAt`,
  so a mirror that missed only deletions keeps pace on lag while over-reporting.
- **documents prod deleted** — any mirrored collection where the MIRROR holds
  more rows than prod. The two signals above read `movies` alone, at a ratio
  three stray rows never reach, so a missed delete anywhere else was invisible:
  three folded DE films sat in the mirror's `pending_movies` while `/debug`
  showed them stuck in staging (2026-08-29). An excess only counts once it has
  STOOD for 15 minutes — a single sample cannot tell a missed delete from a
  mirror still catching up, and measured on prod, UK showed an excess with lag
  reading 0ms that was gone on the next run (the read-model projector rewrites a
  collection by deleting and re-inserting, and the two counts are a round-trip
  apart). The gate keeps its own note of when each collection was first seen
  ahead, drops the ones that come back into line, and a seed clears it.
- **a torn snapshot** — the previous seed left its unfinished mark behind.
  `seed.js` copies collection by collection, dropping each before refilling it,
  so a seed killed partway leaves `movies` (copied first) complete and current
  while a later collection is empty or truncated: lag, drift and the
  missing-collection check all read healthy on a mirror that is a fragment. The
  mark is the only thing that sees it (found 2026-08-04: `/debug` listing 934
  films with **zero cinemas apiece** because `movie_slots` had been dropped and
  never refilled).

A check that itself fails (tunnel blip) means *tail anyway* and re-judge next
cycle — a broken gate must never block the sync. The thresholds live in
`staleness-rule.js` as a pure function, asserted with no Mongo at all:

```
mongosh --nodb --quiet --file scripts/local-mirror/staleness-rule.js \
                       --file scripts/local-mirror/staleness-rule-spec.js
```

### The page states its own age

Every gate above is the sync judging itself, and a sync that has stopped judges
nothing. Whatever the cause, the visible result is the same: `/debug` renders a
SNAPSHOT — right-looking numbers, a live-looking page, silently hours or days
behind. It has been mistaken for a data bug three times, most recently on
2026-08-30, when a frozen mirror made `/debug/cadence?country=us` show nothing
but the 2h base interval (the copy had stopped during that country's FIRST HOUR
of rating checks, before any film had backed off) and read as a cadence bug.

So the debug navbar carries the age of what it is showing — the newest
`updatedAt` across that country's mirrored `movies` + `screenings`
(`services.MirrorFreshness`, read per page load; ~12–26ms against the loopback
Mongo). Past the same 30 minutes the re-seed gate uses, it turns amber and says
`⚠ mirror 26h behind` instead of `mirror 12s behind`. Nothing is rendered in
prod, where the pages read the source and there is no copy to be behind.

### Picking up its own code

The tailers re-read the `.js` files each time one starts, but the supervisor is
a long-lived bash process holding the command lines it was written with — so a
`git pull` lands half-applied: new scripts, old invocations. That is not
hypothetical: the commit that added `stream-start.js` to the tailer's `--file`
list left the running daemon launching `tail.js` without it, which then died on
an undefined function every time, and only a human noticed.

So the supervisor digests the files it actually executes — derived from its own
`$HERE/...` references, so a `--file` added later is covered without anyone
remembering, and editing a spec or this README churns nothing — and exits when
that digest changes. launchd (`KeepAlive`) brings it straight back on the new
code; its EXIT trap takes the tailers down on the way out, so nothing is
orphaned alongside the replacement.

### How the tunnel reaches prod — and why a TCP probe isn't enough

Prod Mongo used to be the Fly app `kinowo-mongo`, reached with
`flyctl proxy 27017:27017`. On **2026-08-29** it moved to the Hetzner host
`mongo-1`, and the Fly machine is now **stopped**. `mongod` there listens on
`127.0.0.1`, on `10.20.0.10` (a private Hetzner subnet no laptop can route to)
and on a Fly 6PN WireGuard address (how the deployed apps reach it) — so from a
laptop the only way in is ssh, and the only far-side address that answers inside
that session is the loopback one:

```
ssh -N -L 27017:127.0.0.1:27017 root@178.105.221.61
```

That is a **drop-in** for what `flyctl proxy` published on the same port, which
is why nothing downstream changed: `seed.js`, `tail.js`, `staleness.js`,
`mongodump`, `mongorestore` and the app's own `MONGODB_URI` all still dial
`127.0.0.1:27017`. Dumping on `mongo-1` and streaming the result back would not
work here anyway — the mirror's steady state is `tail.js` holding a **change
stream** open for days, which is a live cursor, not a dump.

All of it lives in **`prod-tunnel.sh`**, sourced by `mirror.sh`,
`sync-title-rules.sh`, `sync-enrichment-cache.sh` and `../reset-corpus.sh`.
Before, those four carried four copies of the same `nc -z || flyctl proxy`
block, and a move like this had to find every one of them — the copy that got
missed would go on dialling a stopped machine and fail like a network blip
rather than a wrong destination.

**The health check is an authenticated `ping`, not `nc -z`.** That is the
lesson the cutover taught: a `flyctl proxy` left running from before the move
kept *accepting* connections on `:27017` while the machine behind it was
stopped, so every supervision cycle read "tunnel healthy" and every change
stream then died with `ECONNRESET` — 420 in a row in the log, with nothing
naming the tunnel as the cause. A liveness check a dead backend passes is worse
than none, because it routes the failure somewhere it can't be diagnosed. When
the port is held by a process that isn't ours and prod doesn't answer through
it, the scripts say so and stop rather than killing a tunnel that may be
someone's `sbt run` — the fix is `pkill -f 'flyctl proxy 27017'`.

The tunnel is also `ExitOnForwardFailure=yes` (ssh otherwise holds a session
open with *no* forward when the local bind fails, which reads as live and fails
later), `BatchMode=yes` (launchd has nobody to answer a passphrase prompt) and
`ServerAliveInterval=15` (a slept laptop leaves a half-open session that
neither errors nor carries data). Only a tunnel the script *started* is ever
killed, and it is killed on every exit path — no strays.

### Recovery paths are the daemon — so they're tested

`mirror.sh` runs under `set -euo pipefail` with each database's `supervise_db`
in a **background subshell**, so any bare non-zero exit inside one kills that
supervisor silently, and the parent goes on `wait`ing — which launchd reads as a
healthy process and never restarts. That is exactly what a dropped tunnel did on
2026-08-02: the seed's `mongosh` exited non-zero, all three supervisors died at
once, and the mirror sat frozen mid-seed for two days. Every fallible step
therefore wraps itself (`set +e` … `set -e`, or `|| continue`), and the shape is
asserted against stubs — no Mongo, no tunnel:

```
scripts/local-mirror/mirror-resilience-spec.sh
```

It covers the tunnel too: that a dead backend does not read as healthy, that a
tunnel held by another process is never adopted or killed, and that the ssh
target defaults to `mongo-1` and is overridable.

Wrapping every fallible step is not enough on its own: it makes a supervisor
*likely* to survive, and the parent's `wait` still cannot tell a dead one from a
busy one (the log rotator and the targets poller never end, so `wait` never
returns). The tunnel dropping wedged the daemon again on 2026-08-30 — a live
pid, KeepAlive never firing, /debug a day behind. So the parent watches its
supervisors' pids (`await_supervisor_exit`, every `KINOWO_MIRROR_LIVENESS_POLL`
seconds) and takes the whole process down as soon as one is gone; launchd starts
a clean one. `launchctl kickstart -k gui/$(id -u)/pl.kinowo.local-mirror` stays
the manual lever, but a wedge no longer needs it.

`mirror.sh` is sourceable for that spec: everything below the
`[ "${BASH_SOURCE[0]}" = "${0}" ] || return 0` guard runs only when it is
executed, so sourcing defines the functions and reads no `.env.local`.

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
It `mongodump`s the one collection from prod over the same ssh tunnel, guards on the
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

> **CI was repointed too, and no longer tunnels via Fly.**
> `.github/workflows/record-scrape-fixtures.yml` (through
> `scripts/ci/wait-for-mongo-tunnel.sh`) reaches prod Mongo over an ssh key
> pinned to a forced command on `mongo-1` — the narrowest route a driver-side
> read can use; `infra/nix/modules/roles/mongo-ci-read.nix` argues the case. That
> end needed a deploy key in GitHub secrets, which is why it landed separately
> from this local repoint; this one needs no new secret at all.

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
| 27017 | prod tunnel (`ssh -N -L 27017:127.0.0.1:27017`) — everything except `/debug` reads, and the sync **source** |
| 28017 | native local Mongo — `/debug` mirror reads + the sync **target**, and the web+worker `kinowo_local` |

Override the Mongo port with `LOCAL_MIRROR_PORT` (and match it in
`MONGODB_MOVIES_MIRROR_URI`).
