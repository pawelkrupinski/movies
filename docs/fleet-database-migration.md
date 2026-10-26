# Moving the fleet database out of Poland's corpus

Status (2026-10-25): steps 0–6 DONE in prod; copy pass 2 was running when this was written. Every web and worker
runs on `kinowo_fleet` (movies-gitops `eb9db81`). Left: the soak (7), the default flip in code (8), and the drop of
the old copies (9, `scripts/fleet-db-finish`). Steps 8 and 9 wait for the user's word that the soak is over.

## Why

`services.FleetDatabase` is the database every country's web and worker share. Until now it was Poland's own
corpus database, `kinowo`, because the application user had rights only on the country databases. On 2026-10-24
a cleanup of a fleet collection almost became a delete of Poland's live data, because both live in one database.
The fleet moves to its own database, `kinowo_fleet`.

## What moves and what does not

Every collection opened through `fleetConnection` (web) or `fleetMongoConnection` (worker), the ones
`services.MongoCollections` registers in the fleet database (`RegisteredCollectionListsSpec` holds this table to them):

| Collection | Writers / readers | Moves |
|---|---|---|
| `facebook_rescrapes` | worker `MongoFacebookRescrapeStore` (fleet queue + `_id: "quota"` pacing doc); web `MongoFacebookSlots` (holds the queue when a sign-in reads Facebook's X-App-Usage high) | yes |
| `facebook_fetches` | web `FacebookFetchRecorder` writes; worker `MongoFacebookFetches` reads (TTL on `fetchedAt`, 30 d) | yes |
| `fleet_host_pace` | worker `MongoFleetHostPace` (one doc per fleet-paced host) | yes |
| `omdb_budget` | worker `MongoOmdbBudget` (one doc per OMDb day: permits granted, by every country's worker) | no: new, starts empty |

Shared across countries but NOT in the fleet database, so not part of this move:

- `users`, `userStates`, `authExchangeCodes`: the web's shared accounts database, chosen by `MONGODB_USERS_DB`
  (gitops `web/base/all.yaml`: `kinowo`). They sit beside the Polish corpus for the same historical reason. Once
  step 1 has granted `kinowo_fleet`, they can follow with the existing `tools.SharedUsersMigration`
  (`MONGODB_USERS_DB=kinowo_fleet`). That move is session-critical and is a separate change;
  `SharedUsersDatabaseConfigSpec` pins today's value.

Per-country (each country's own database, opened through `mongoConnection`), so they stay:
`env_overrides`, `env_registry`, `uptimeBuckets`, `uptimeServiceTags`, `database_owner` and the whole corpus.

## Users and roles

- **The app's user** — the one in the app's `MONGODB_URI`: on 2026-10-25 that is **`kinowo_app_2`, `authSource=kinowo`**
  (so the user record lives in `kinowo`), with `readWrite` on kinowo, kinowo_uk, kinowo_de, kinowo_es, kinowo_us and
  kinowo_fleet. Every doc before then said `kinowo_app`, a user prod does not have (the first grant attempt failed
  `Could not find user "kinowo_app" for db "kinowo"`). It needs `readWrite` on `kinowo_fleet`.
  Without it the fleet connection comes up (it is `Optional`, and a ping needs no grant), then every
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

## What was run (2026-10-25)

Steps 1–6 ran as one scratch script (phases `before`, `flip`, `after`) by the user, from a laptop, against prod.
That script is not checked in: those steps are done and are not to be repeated. What it did, for the record:

0. **Code deployed** before anything else: the `MONGODB_FLEET_DB` setting, default still `kinowo`.
1. **Grant.** The app's user and auth database were read off its own `MONGODB_URI` (that is how `kinowo_app_2`
   was found), then `grantRolesToUser(<user>, [{ role: "readWrite", db: "kinowo_fleet" }])` on that auth database,
   as root. Today this is `scripts/mongo-grants --apply`, whose list includes `kinowo_fleet`.
2. **Proof as the app**: an insert into and drop of `kinowo_fleet.grant_probe`, as the app's user.
3. **Dry run**: per collection, the counts in both databases and the indexes; `kinowo_fleet` was empty.
4. **Copy pass 1**, as root: the indexes first (except `_id_`, so TTL and claim indexes hold from the first
   document), then `$merge` into `kinowo_fleet` on `_id` with `whenMatched: "keepExisting"`. It never overwrites,
   and it can be run again.
5. **Flip**: `MONGODB_FLEET_DB: kinowo_fleet` added to `web-env` (`web/base/all.yaml`) and `worker-env`
   (`worker/base/all.yaml`) in movies-gitops, pushed as `eb9db81`. Flux applied it and Reloader rolled every pod.
6. **Copy pass 2**, once two looks at `kinowo` three minutes apart showed no new writes there: the same `$merge`,
   picking up what old pods queued while the pods rolled.

Rollback until step 8 lands: delete the gitops line and push. The pods return to `kinowo`, whose copy is stale
since step 5; copy back first (the same `$merge` with the two databases swapped) if the queue matters.

## What is left

### 7. Soak

At least one release, a few days. Watch: every worker logs `MongoConnection connected to kinowo_fleet`; none logs
`Facebook re-scrapes are OFF`; `kinowo_worker_share_cards_rescrape_total{outcome="sent"}` keeps rising.
`scripts/fleet-db-finish` (read-only) shows `kinowo`'s newest `facebook_fetches.fetchedAt` frozen at the flip and
`kinowo_fleet`'s moving.

### 8. Make `kinowo_fleet` the default (a movies commit, after the soak)

- `FleetDatabase.name` falls back to `Dedicated` instead of `Legacy`;
- `kinowo` (`Legacy`) is refused like every other country's corpus;
- `FleetDatabaseSpec` changes to match, and a deploy spec beside `SharedUsersDatabaseConfigSpec` pins the gitops
  value;
- the `kinowo.facebook_rescrapes` wording goes from `infra/nix/files/monitoring/rules/share-cards.rules`.

With step 5 live, deploying it changes nothing in prod. After it, the gitops line is optional; keeping it is fine.
After it, rollback is no longer "delete the line": `MONGODB_FLEET_DB=kinowo` would be refused at boot.

### 9. Drop the old copies from `kinowo` (after step 8 is deployed, explicit OK)

Confirm a nightly dump after step 5 contains `kinowo_fleet`. Then:

```sh
scripts/fleet-db-finish                                                    # read-only: counts in both databases
scripts/fleet-db-finish --drop facebook_rescrapes facebook_fetches fleet_host_pace
```

`--drop` takes exactly those three names, never a pattern, and drops them from `kinowo` only. It refuses when
`kinowo_fleet` lacks one of them or holds nothing in one, or when the app's user cannot read `kinowo_fleet`.
Everything else in `kinowo` is Poland's live corpus: never drop there by hand without the name spelled out.

## Rule for migration scripts that edit another repository

A script that edits or pushes another repository (here: movies-gitops) takes the target checkout's path as an
explicit, required argument. No default (`${GITOPS:-$HOME/projects/movies-gitops}`), and no environment variable
that falls back to the real checkout. A test of such a script once ran against the real movies-gitops because its
fake path was not passed and the default took over. Its tests pass a scratch repository, every time.
