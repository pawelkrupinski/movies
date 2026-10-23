# Dependency, API and flag audit (2026-10-23)

What the libraries, SDKs, servers and tools we already use offer in their current or newer versions that we don't
use yet: APIs that would delete or speed up our code, and switches/parameters left at a worse value. Research only;
nothing here has landed.

**How much to trust each line.** Every `file:line` comes from a grep of `main` at `f470029f2`. "Available since X"
and "gains N%" claims come mostly from the researchers' knowledge, not from release notes read that day (web
search came back thin). Lines marked **[checked]** were confirmed against the repo; **[unverified]** marks a claim
that must be confirmed before acting. Measure before landing any performance item.

## Summary: best payoff per hour

1. **MongoDB:** confirm prod's feature-compatibility version (FCV) is 8.3, then add `appName` (done) so mongod's slow-op log lines name their sender.
2. **JVM:** retire `infra/jvm/jdk25-parity.options`, starting with compact object headers; canary on one worker.
3. **Android:** a Baseline Profile; posters on the app's shared OkHttp client; `AsyncImage` over
   `SubcomposeAsyncImage` in the grid.
4. **Hosts:** zram swap and kubelet memory reserves on the 4 GB nodes.
5. **iOS:** `@Observable` with a deployment-target bump to iOS 17.
6. **Page tests:** replace the remaining `waitForTimeout` sleeps.

## Versions: mostly already current

Scala Steward (`scala-steward.yml`), the weekly `nix-update.yml` and Renovate (Gradle/Android only) keep most
versions fresh.

| Area | Current | Newer | Note |
|---|---|---|---|
| Play | 3.0.12 | 3.1.0-M9 (milestone only) | stay |
| mongo-scala-driver, jsoup, Caffeine, Prometheus client | 5.13.0, 1.23.2, 3.3.1, 1.10.0 | — | latest |
| sentry-logback / icu4j | 8.61.0 / 78.3 | 8.61.1 / 79.1 | patch / data refresh |
| Scala | 3.9.0 | 3.10.1-RC1 | see JDK target below |
| sbt | 1.13.0 | sbt 2.x status unverified | migration high effort |
| MongoDB server | **8.3.11** (ahead of nixpkgs' 8.2.11) | no 8.4/9.0 found [unverified] | see below |
| Playwright / axe-core | 1.63 / 4.13 | 1.64.1 / 4.14.0 | one-line bump |
| ESLint / globals | 9.17 / 15 | 10.13.0 / 17.13.0 | low payoff |
| Coil (Android) | 2.7.0 | 3.x | see Android |
| Haze (Android) | 1.7.3 (deprecated API names) | 2.0 beta | see Android |
| ML script (numpy, scikit-learn) | pinned by the Python 3.9 floor (`scripts/run-script-tests:42`) | — | raise the floor only if another script wants it |
| Nix-managed infra (k3s 1.35.8, Grafana 13.0.9, Prometheus 3.12, VictoriaLogs 1.52, Forgejo 16.0.5, Caddy 2.11.4) | current with nixpkgs | — | |
| Flux 1.9.4, Headlamp 0.45.0, Reloader 1.0.28, kube-state-metrics 2.14.0, base-image tags | not auto-bumped | not compared upstream | candidate for Renovate |

## MongoDB

**Server version: nothing to upgrade.** Prod mongo-1 runs 8.3.11 [checked in `infra/nix/modules/roles/mongodb.nix`]
as a single-node replica set `rs0` (2 vCPU, 4 GB, WiredTiger cache 1 GB, oplog 4 GB). The binary was moved from
8.2.11 to 8.3 on 2026-09-27. CI's `mongo:8.3.11` is held to it by `MongoServerVersionParitySpec`.

- **Open: FCV.** The role calls `setFeatureCompatibilityVersion: "8.3"` "a separate, later step", and nothing
  records it was done. Check with `db.adminCommand({getParameter: 1, featureCompatibilityVersion: 1})`. Setting it
  is one-way: an 8.2 binary can no longer open the data. Take an **off-box** dump first, because backups are a
  local `mongodump` on the same volume ("the largest known gap", per the role).
- **The local prod mirror (`:28017`) runs 7.0.43, not prod's version.** Behaviour checked there can differ from
  prod's 8.3.
- 8.3 is on MongoDB's minor-release track, so it gets no patches once the next release ships. Keep following each
  release via `nix-update`.

**Server parameters** (all unset [checked]):

| Parameter | Proposed | Gain | Cost |
|---|---|---|---|
| `operationProfiling` slow-op threshold | leave: mongod already logs ops over 100 ms by default, profiler off | — | raising it would log less |
| `oplogMinRetentionHours` | 24 (via `replSetResizeOplog`) | explicit change-stream resume window (4 GB ≈ 12 h today) | oplog may outgrow 4 GB |
| `blockCompressor` | `zstd` instead of snappy | ~30–40 % smaller on disk and in cache [unverified] | restart; existing collections need a rebuild |
| `defaultMaxTimeMS` (8.0+ cluster parameter) | reads ~30 s | stops runaway reads | test against the initial hydration first |
| `cacheSizeGB`, connection limits, `minSnapshotHistoryWindowInSeconds` | leave | data is ~70 MB | — |
| Transparent huge pages | check `/sys/kernel/mm/transparent_hugepage/enabled`; 8.x guidance changed [unverified] | latency/RSS | measure before/after |

**Client** (`common/src/main/scala/.../MongoConnection.scala:450-478`):

- **`appName` per tier and country** — done: `kinowo-<tier>-<countries>`, e.g. `kinowo-web-pl`, `kinowo-worker-pl-uk`. Makes slow-op logs and `currentOp` attributable. Cheapest,
  highest-value diagnostic item.
- **Client-side operation timeout (CSOT)** — done: web and worker clients carry a 60 s `timeoutMS`
  (`MongoConnection.DefaultOperationTimeout`). Tools get none. 60 s is the longest budget an ordinary call site already
  awaits, so it never cuts an operation short. Before, an `Await` that ran out left the operation running in mongod;
  now mongod stops it (`maxTimeMS`). Change streams and index builds opt out (`OperationTimeoutOptOut`). Without the
  opt-out, a change stream dies with `MongoOperationTimeoutException` after idling for the timeout, and our watchers
  then reopen it on a backoff. `MongoOperationTimeoutIntegrationSpec` covers both. The API is still
  `@Alpha(Reason.CLIENT)` in driver 5.13, with no compatibility guarantee, so we use only the one setting plus
  `withTimeout`. Follow-up: give the hot 10 s stores (`MongoTaskQueue`, `MongoChunkScrapeStore`, the
  `MongoMovieRepository` point ops) their own 10 s `withTimeout`, so mongod stops their work when their caller gives up.
- Leave as they are: pool of 25, `w:1`, and compression (zlib on loopback only).

## JVM backend

### Runtime flags: retire the JDK 25 parity

`infra/jvm/jdk25-parity.options` (2026-09-27) deliberately turns off JDK 27 behaviour, one line per difference
from the JDK 25 prod ran before, and `JdkParityOptionsSpec` holds it there. Now that 27 is settled, dropping lines
gives those defaults back. Do it in this order, measuring a worker's first 10 boot minutes (CPU, RSS, live set)
each time:

1. **`-XX:-UseCompactObjectHeaders` [done 2026-10-23].** JEP 534 made compact headers the default in 27. They
   require the monitor side table, so `-XX:-UseObjectMonitorTable` went with them (27 ignores it beside them, with a
   boot warning). Measured: `UnmatchedClustersRatchetSpec` under the worker's options on local JDK 27, peak live heap
   (`GC.class_histogram` totals every 3 s, two runs each) 600 / 583 MB without, 528 / 531 MB with -- about 10 %.
   The AOT cache is trained through the same launcher, so it shares the mode; `AotCacheOptionsSpec` keeps overlays
   from naming it. Still owed: a worker's first 10 boot minutes in prod (CPU, RSS, live set).
2. `FastLockingSpins=13`: 27's locking default.
3. `-ShortRunningLongLoop`, `-UseAutoVectorization*`: newer C2 loop and vectorization optimizations.
4. `AllocatePrefetchDistance/Lines`: pinned to one Skylake's ergonomics.

Other runtime items:

- **The AOT cache carries almost no profiles.** The training main (`tools.ClassArchiveTraining`) only loads
  classes. Training on a representative workload (a fixture resolve; a few web requests) lets JEP 515 (25+) store
  method profiles, which cuts JIT CPU at boot. Image builds get slower, and the `CompileThresholdScaling` tuning
  needs re-measuring. Check what is stored with `-Xlog:aot`.
- **`web.options` uses G1 on a ~384 MB heap.** SerialGC is likely cheaper there, as on the workers; it needs a
  canary for pause times. Not ZGC or Shenandoah at these heap sizes.
- `worker.options` `SweeperThreshold=50`: the sweeper is gone, but its comment records a measured effect on 25.
  Re-measure on 27 before deleting.
- Not worth doing: `AutoCreateSharedArchive` (conflicts with the AOT cache), `TieredStopAtLevel=1` in prod,
  always-on JFR.

### Compile target: Java 21 bytecode on a JDK 27 runtime

`build.sbt:45-47` (Scala 3.9.0 caps `-java-output-version` at 21) and `build.sbt:61` (`--release 21`). Whether
Scala 3.10 lifts the cap is unverified. Once it does, the JDK 22–25 APIs become callable:

- **`ScopedValue`** for the `ThreadLocal`s used as dynamic context: `ObservationReads.scala:20`,
  `ListingReads.scala:49`, `JsonBodies.scala:18`, `LookupGaps.scala:19`, `RuleAblation.scala:13`. Safer under
  virtual threads; not a speed win. (`CreditRelation.scala:18` is a per-thread ICU cache: keep it, because
  transliterators aren't thread-safe.)
- Stream `Gatherers`.
- `StructuredTaskScope` could replace `DaemonExecutors`' `invokeAll` prefetch once it is final (preview in 25).

Virtual threads already benefit from the 27 runtime (no pinning on `synchronized` since 24).

### Library APIs

- **HTTP/2** in `RealHttpFetch.scala:52`, which pins `HTTP_1_1`, possibly on purpose for the proxy tunnel. Try it
  per host through `HostPolicies` and measure.
- **Sentry cron check-ins** for the periodic worker loops, only if Grafana doesn't already alert on missed runs.
- Nothing to gain: Caffeine (`BoundedCache`, `CaffeineMovieCache`), jsoup (no document large enough for
  `StreamParser`), Prometheus native histograms (the server side isn't enabled), `TextNormalization` (already has
  an ASCII fast path).

### Build and test settings

- **`ThisBuild / usePipelining`** (sbt 1.4+): pipelined multi-module compile. A one-line trial; watch the `-Werror`
  and Twirl modules.
- **Short unit-test forks:** `-XX:+UseSerialGC -XX:TieredStopAtLevel=1`; keep C2 for the identity and e2e specs.
  `ForkedTests.scala` is otherwise well tuned.
- **scalac:** `-Wunused:all` (matches the dead-code rule; land module by module because of `-Werror`),
  `-Wsafe-init`, `-Wshadow:all`, and `-deprecation`/`-unchecked` for the whole build (today Play adds them to web
  only). Not `-Yexplicit-nulls`.
- No `.scalafmt.conf` exists although sbt-scalafmt is installed. Either add one (`runner.dialect = scala3`) or drop
  the plugin.

## Android

1. **Baseline Profile + `profileinstaller`: absent** [checked]. Typically 20–30 % faster cold start and less
   first-scroll jank in the poster grid (`ListScreen.kt`) [unverified for us]. Medium effort: a `:baselineprofile`
   module and an emulator generator.
2. **Posters bypass the shared OkHttp client.** There is no custom `ImageLoader`, so Coil builds its own client and
   connection pool (`MainActivity.kt:198` holds ours). Also, `SubcomposeAsyncImage` (`PosterImage.kt:53`)
   subcomposes per grid cell; `AsyncImage` with `onState`/`placeholder` is the scroll win. Both work on Coil 2;
   Coil 3 (`coil3.*`, `OkHttpNetworkFetcherFactory(callFactory = …)`) is a small follow-on touching
   `PosterPrefetch.kt` and `KinowoViewModel.kt:441-452`.
3. **Haze:** `ListScreen.kt:98-99,260,412` use the deprecated `haze`/`hazeChild` (now `hazeSource`/`hazeEffect`).
   The effect is a 1 dp blur with zero tint and noise: a per-frame offscreen render over the scrolling grid for an
   almost invisible result. Consider dropping the dependency (a design call).
4. **JSON:** `KinowoApi.kt:87,102,122,141` buffer the body into a String before `decodeFromString`.
   `decodeFromStream(body.byteStream())` lowers peak memory. Also extract the four separate
   `Json { ignoreUnknownKeys = true }` instances into one shared instance.
5. Type-safe navigation routes (`KinowoApp.kt:307-320`, navigation-compose 2.8+); shared-element list→detail
   transition (needs a stable film id rather than the title as the key).
6. `material-icons-extended` for 12 icons: R8 strips the rest, so the gain is build time, not APK size. Replace
   with vector drawables if it's convenient.

**Build settings:**

- `maxParallelForks` for the Robolectric unit tests (absent); the order-independence guards make it reasonably safe.
- CI runs Gradle three times with `--no-daemon` (`android.yml:242,258,295`). Merge the steps or keep the daemon:
  tens of seconds per job.
- `org.gradle.jvmargs=-Xmx2048m`: consider 3g with ParallelGC for Compose and R8.
- Smaller: `org.gradle.parallel`, `-repackageclasses`, `androidResources.localeFilters` (check against the app's
  real languages).

## iOS

Baseline: deployment target 16.0, `SWIFT_VERSION = 5.0`, no strict concurrency, built with Xcode 27.1.

1. **`@Observable` (iOS 17)** instead of 9 `ObservableObject` classes and about 22 `@Published` properties.
   `UserPreferences` (`Storage/UserPreferences.swift:34-88`) re-renders every observer on any change. The Combine
   pipelines in `StateSyncService.swift:99,449,460-470` become `Observations` or a cancellable `Task`. The largest
   code deletion. Needs the iOS 17 bump and test rewrites in `KinowoAuthTests`.
2. **Swift 6.2 approachable concurrency + default MainActor isolation** (no target bump). Deletes most of the 41
   manual `@MainActor` annotations and the `MainActor.assumeIsolated` workaround (`StateSyncService.swift:466`).
   First set `SWIFT_STRICT_CONCURRENCY = complete` in Swift 5 mode and fix the warnings; the
   `@unchecked Sendable`/`nonisolated(unsafe)` sites are `PosterStore.swift:36`,
   `LocationCityResolver.swift:393,400` and `AuthService.swift:67`.
3. **Actors and `Task.sleep(for:)`** instead of queue-plus-continuation code (`ConditionalPayloadCache.swift:55-61,140`,
   `PosterStore.swift:181`), `asyncAfter` (`ShowtimeTuningScreen.swift:199`, `FilmDetailView.swift:441`) and
   `Task.sleep(nanoseconds:)` (5 sites).
4. **`onGeometryChange`** instead of `GeometryReader` + `PreferenceKey` workarounds (`ShowingsView.swift:269-273,347-352`,
   `ContentView.swift:153-157`, `FiltersBar.swift:227`): fewer views per showtime pill in a lazy grid. Whether it
   back-deploys to iOS 16 is unverified.
5. **`@Entry`** for the 7 `EnvironmentKey` structs (back-deploys): about 30 lines deleted.
6. Optional: Swift Testing for new tests (needs `Package.swift` tools-version 6.0); `glassEffect` for the iOS 26
   look behind `#available`.
7. Keep: `PosterStore`/`CachedAsyncImage` (`AsyncImage` has no cache control). Skip: SwiftData, `Span`/`InlineArray`.

**Build settings:** `COMPILATION_CACHE_ENABLE_CACHING = YES` (Xcode 26+, absent) for faster warm rebuilds; it helps
CI only if the cache persists between runs. `Package.swift` has no `swiftSettings`; move it to tools 6.x with
`.swiftLanguageMode` and upcoming features alongside item 2.

## Web, page tests, tooling

- **Fixed sleeps:** `waitForTimeout` in `search-tap-dismiss.spec.ts:44` and `keyboard-nav.spec.ts:46` should become
  web-first assertions or `expect.toPass`. `history-restore.spec.ts:32,56` `waitForFunction` becomes `toBeAttached()`.
  `day-swipe.spec.ts` repeats `expect.poll(() => settled(page))` about 10 times; share the helper (`helpers.ts:217-245`).
- **Playwright config:** `trace: 'retain-on-failure'` with `retries: 0` records a trace for every test. Turning it
  off speeds up the WebKit rows (CI's long pole) at the cost of traces on failure. `fullyParallel` is absent, so
  check for shared-state flakes first.
- **Day-swipe slide** (`shared.js:3173-3200,3269-3294`): `requestAnimationFrame` + `transitionend` + a timeout
  fallback. The View Transitions API (Safari 18+) could replace the state machine; finger tracking likely stays
  custom. Medium risk to the day-swipe specs.
- **A hand-rolled modal** (hidden films): `<dialog>.showModal()` (Safari 15.4+) gives focus trap, Esc and the top
  layer [modal code unread].
- Optional: speculation rules (Chromium only, so Android/Chrome traffic) for film-page prefetch.
- Already modern: `content-visibility`, `loading=lazy`, `fetchpriority`.
- **ESLint:** `linterOptions.reportUnusedDisableDirectives: 'error'`, `ecmaVersion: 'latest'` with the ESLint 10 bump.
- **version-dashboard:** current (Node 24, TS 7); only lockfile refreshes.
- **Dockerfile:** `COPY --link` on `COPY stage/ ./` and apt cache mounts; small gains.

## Hosts, k3s, edge

- **No swap and no kubelet reserves on 4 GB nodes** [checked: no `swapDevices`/`zramSwap`, no `--kubelet-arg`].
  Add `zramSwap.enable` + `vm.swappiness=10`, plus `system-reserved`/`kube-reserved` and
  `eviction-hard=memory.available<300Mi`. Check overlap with `modules/fleet/container-image-gc.nix`.
- **CPU throttling:** pods don't set `-XX:ActiveProcessorCount`, so the JVM sizes itself to the node's cores. Audit
  with `container_cpu_cfs_throttled_periods_total`, and pin it to the pod's CPU request where limits throttle.
- Prometheus `scrape_interval` 30 s → 60 s halves series storage (`prometheus.nix:111`).
- **Already fine:** HTTP compression (gzip at the origin; brotli deliberately off because Cloudflare caches one
  variant per URL, see `infra/nix/files/monitoring/rules/web-page-weight.rules`), Caddy HTTP/3 (default), k3s with
  traefik/servicelb/metrics-server disabled, VictoriaLogs memory and disk caps.
- **Not assessed:** Forgejo runner `capacity`, cache server and pull policy (outside `infra/nix`); k3s datastore
  (kine vs etcd).
