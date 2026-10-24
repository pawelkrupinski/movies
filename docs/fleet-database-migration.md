# Moving the fleet database out of Poland's corpus

Status (2026-10-24): code side landed (`MONGODB_FLEET_DB`, default still `kinowo`). Prod steps 1–9 below
NOT run yet. They are for a human with the root credentials; no agent runs them.

## Why

`services.FleetDatabase` is the database every country's web and worker share. Until now it was Poland's own
corpus database, `kinowo`, because the application user had rights only on the country databases. On 2026-10-24
a cleanup of a fleet collection almost became a delete of Poland's live data, because both live in one database.
The fleet moves to its own database, `kinowo_fleet`.

## What moves and what does not

Every collection opened through `fleetConnection` (web) or `fleetMongoConnection` (worker):

| Collection | Writers / readers | Moves |
|---|---|---|
| `facebook_rescrapes` | worker `MongoFacebookRescrapeStore` (fleet queue + `_id: "quota"` pacing doc); web `MongoFacebookSlots` (holds the queue when a sign-in reads Facebook's X-App-Usage high) | yes |
| `facebook_fetches` | web `FacebookFetchRecorder` writes; worker `MongoFacebookFetches` reads (TTL on `fetchedAt`, 30 d) | yes |
| `fleet_host_pace` | worker `MongoFleetHostPace` (one doc per fleet-paced host) | yes |

Shared across countries but NOT in the fleet database, so not part of this move:

- `users`, `userStates`, `authExchangeCodes`: the web's shared accounts database, chosen by `MONGODB_USERS_DB`
  (gitops `web/base/all.yaml`: `kinowo`). They sit beside the Polish corpus for the same historical reason. Once
  step 1 below has granted `kinowo_fleet`, they can follow with the existing `tools.SharedUsersMigration`
  (`MONGODB_USERS_DB=kinowo_fleet`). That move is session-critical and is a separate change;
  `SharedUsersDatabaseConfigSpec` pins today's value.

Per-country (each country's own database, opened through `mongoConnection`), so they stay:
`env_overrides`, `env_registry`, `uptimeBuckets`, `uptimeServiceTags`, `database_owner` and the whole corpus.

## Users and roles

- **`kinowo_app`** (the app's user, `authSource=kinowo`, so the user record lives in `kinowo`): needs `readWrite` on
  `kinowo_fleet`. Without it the fleet connection comes up (it is `Optional`, and a ping needs no grant), then every
  re-scrape write fails `Unauthorized`. Facebook re-scrapes stop and fleet pacing falls back to per-worker. The
  grant goes in BEFORE anything points at the new database.
- **`kinowo-backup`** holds the cluster-wide `backup` role, and the nightly `mongodump --oplog` dumps the whole
  instance. No change: `kinowo_fleet` is in the next dump.
- **`kinowo-ci-corpus`** reads the country corpora for fixture recording and never reads fleet collections. No change.
- **`kinowo_monitor`** has `clusterMonitor`. No change.

## The design: switch by setting, copy before and after

- `MONGODB_FLEET_DB` names the fleet database (`settings.FleetDatabaseName`, read by
  `ProcessConfiguration.fleetDatabase`, resolved by `FleetDatabase.name`).
- Unset or blank means `kinowo`. That is why the code can deploy before any data is copied.
- A country's corpus database other than `kinowo` is refused at boot.
- The operator flips the setting once, in gitops, for every web and worker together. Reloader rolls every pod.

Why there is no dual-read with a fallback to `kinowo`:

- The three collections are coordination state: a queue, a pacing slot and a hold.
- A read that falls back to `kinowo` while writes go to `kinowo_fleet` makes two queues and two quotas. That is a
  split brain for as long as the fallback lives, not just during a rollout.

What the switch costs:

- Coordination is split only while pods roll, which is minutes.
- In that window, old pods pace and queue in `kinowo` and new pods in `kinowo_fleet`. At worst, Facebook gets
  about twice the fleet's one request per 20 s, and a fleet-paced host gets about twice its pace. That is well
  inside the X-App-Usage hold (75 %).
- Entries that old pods queue in the window are picked up by copy pass 2.

Downtime: none. Web pods roll as on any deploy. Workers restart (Reloader), which the downtime policy accepts.
Rollback: unset `MONGODB_FLEET_DB`. Every pod returns to `kinowo`, which still holds the pre-flip copy
(step 9 drops it only after the soak).

## Operator steps, in order

Run everything from a checkout of `forgejo/main` that contains this change. `scripts/with-prod-mongo.sh` opens the
tunnel and fetches the credential (one Touch ID). `--root` gives `MONGO_ROOT_URI`; without it, the app's
`MONGODB_URI` (as `kinowo_app`). mongosh: `/opt/homebrew/bin/mongosh`, or `nix shell nixpkgs#mongosh`.

### 0. Deploy the code (any time)

Land the commit that adds `MONGODB_FLEET_DB` (no setting yet). It changes nothing in prod: every pod still
resolves `kinowo`. Wait for image automation to roll it out. Every web and worker must run an image containing it
BEFORE step 5, or an old pod ignores the setting and stays on `kinowo` after the flip.

### 1. Grant `kinowo_app` readWrite on `kinowo_fleet`

```sh
scripts/with-prod-mongo.sh --root bash -c 'mongosh "$MONGODB_URI" --quiet --eval "
  db.getSiblingDB(\"kinowo\").grantRolesToUser(\"kinowo_app\", [{ role: \"readWrite\", db: \"kinowo_fleet\" }]);
  printjson(db.getSiblingDB(\"kinowo\").getUser(\"kinowo_app\").roles)"'
```

The roles printed must include `{ role: 'readWrite', db: 'kinowo_fleet' }`. Use `getSiblingDB("kinowo")`, not
`admin` (see `adding-a-country.md`, "User kinowo_app@admin not found").

### 2. Prove the grant as the app user

```sh
scripts/with-prod-mongo.sh bash -c 'mongosh "$MONGODB_URI" --quiet --eval "
  const f = db.getSiblingDB(\"kinowo_fleet\");
  f.grant_probe.insertOne({ _id: \"probe\", at: new Date() });
  f.grant_probe.drop();
  print(\"kinowo_app can write kinowo_fleet\")"'
```

An `Unauthorized` / `not authorized on kinowo_fleet` error means step 1 did not take. Stop.

### 3. Dry run: what will be copied

Read-only. Write the copy script once. It is used again in steps 4 and 6.

```sh
cat > /tmp/fleet-copy.js <<'EOF'
// Copies the fleet collections from kinowo into kinowo_fleet. Indexes first, so TTL and claim
// indexes hold from the first document. Never overwrites a document already in the target
// (whenMatched: keepExisting): after the flip, kinowo_fleet is the truth. Idempotent.
// DRY=1 only prints.
const Collections = ["facebook_rescrapes", "facebook_fetches", "fleet_host_pace"];
const dry = process.env.DRY === "1";
const src = db.getSiblingDB("kinowo"), dst = db.getSiblingDB("kinowo_fleet");
for (const c of Collections) {
  const indexes = src.getCollection(c).getIndexes().filter(ix => ix.name !== "_id_");
  print(`${c}: kinowo ${src.getCollection(c).countDocuments({})} docs, kinowo_fleet ${dst.getCollection(c).countDocuments({})} docs; ` +
        `indexes ${indexes.map(ix => ix.name).join(", ") || "(none)"}`);
  if (dry) continue;
  for (const ix of indexes) {
    const { key, v, ns, ...options } = ix;
    dst.getCollection(c).createIndex(key, options);
  }
  src.getCollection(c).aggregate([{ $merge: {
    into: { db: "kinowo_fleet", coll: c }, on: "_id", whenMatched: "keepExisting", whenNotMatched: "insert" } }]).toArray();
  print(`  -> kinowo_fleet ${dst.getCollection(c).countDocuments({})} docs`);
}
EOF
DRY=1 scripts/with-prod-mongo.sh --root bash -c 'mongosh "$MONGODB_URI" --quiet --file /tmp/fleet-copy.js'
```

Expect three lines with non-zero `kinowo` counts. `facebook_rescrapes` holds the `quota` document; `facebook_fetches`
is up to 30 days of fetches. `kinowo_fleet` should show 0 docs everywhere. Non-zero there means someone has
already started; read before going on.

### 4. Copy pass 1

```sh
scripts/with-prod-mongo.sh --root bash -c 'mongosh "$MONGODB_URI" --quiet --file /tmp/fleet-copy.js'
```

Each `->` count should equal the `kinowo` count printed above it, give or take what the live fleet wrote in
between.

### 5. Flip the setting (movies-gitops)

In `movies-gitops`, add the same line to BOTH ConfigMaps, `web-env` in `web/base/all.yaml` and `worker-env` in
`worker/base/all.yaml`:

```yaml
  # The fleet's shared state (Facebook re-scrape queue and quota, Facebook fetches, fleet host paces):
  # services.FleetDatabase, docs/fleet-database-migration.md in the movies repo. Unset = Poland's kinowo.
  MONGODB_FLEET_DB: kinowo_fleet
```

Commit and push movies-gitops `main`. Flux applies it, and Reloader rolls every web and worker Deployment. The
web tier rolls one pod at a time.

Verify, once the rollout finishes:

- every worker log has `MongoConnection connected to kinowo_fleet`:
  `kubectl -n kinowo logs deploy/worker-pl | grep kinowo_fleet`, and the same for uk, de, us, es;
- no worker logs `Facebook re-scrapes are OFF`;
- `kinowo_worker_share_cards_rescrape_total{outcome="sent"}` keeps rising (worker-diagnostics dashboard);
- nothing new reaches the old copy. Run this twice, a few minutes apart; the number must not change:
  `scripts/with-prod-mongo.sh --root bash -c 'mongosh "$MONGODB_URI" --quiet --eval "db.getSiblingDB(\"kinowo\").facebook_fetches.find().sort({fetchedAt:-1}).limit(1).toArray()"'`

### 6. Copy pass 2 (right after step 5's rollout)

Picks up what old pods queued in `kinowo` while the pods rolled. Same script, same command as step 4. It never
overwrites. A page sent from `kinowo_fleet` in the window may be queued once more; a second re-scrape of one page
is harmless.

### 7. Soak

At least one release, a few days. Rollback in this period: delete the gitops line. The pods return to `kinowo`,
whose copy is stale since step 5. Run the step 3 script with `kinowo` and `kinowo_fleet` swapped first if the queue
matters.

### 8. Make `kinowo_fleet` the default (code, any time after step 5 is verified)

A movies commit:

- `FleetDatabase.name` falls back to `Dedicated` instead of `Legacy`;
- `Legacy` is no longer accepted (it is a country's corpus);
- `FleetDatabaseSpec` changes to match;
- a deploy spec beside `SharedUsersDatabaseConfigSpec` pins the gitops value;
- the `kinowo.facebook_rescrapes` wording goes from `infra/nix/files/monitoring/rules/share-cards.rules`.

With step 5 live, deploying it changes nothing in prod. After it, the gitops line is optional. Keeping it is fine.

### 9. Drop the old copies from `kinowo` (after the soak; list first, explicit OK)

Confirm the nightly dump after step 5 contains `kinowo_fleet`, then:

```sh
scripts/with-prod-mongo.sh --root bash -c 'mongosh "$MONGODB_URI" --quiet --eval "
  const k = db.getSiblingDB(\"kinowo\");
  for (const c of [\"facebook_rescrapes\", \"facebook_fetches\", \"fleet_host_pace\"]) print(c, k.getCollection(c).countDocuments({}))"'
# then, by exact name, never by pattern, and only these three:
#   db.getSiblingDB("kinowo").facebook_rescrapes.drop()
#   db.getSiblingDB("kinowo").facebook_fetches.drop()
#   db.getSiblingDB("kinowo").fleet_host_pace.drop()
```

Never run a drop against `kinowo` without the collection name spelled out: everything else there is Poland's corpus.
