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

Scala Steward (`scala-steward.yml`) and the weekly `nix-update.yml` keep most versions fresh; since 2026-10-23 the
nix-update PR also carries the container image pins (`infra/bin/image_pins.py`). Renovate (Gradle/Android only) has
not run since the move to Forgejo: `renovate.yml` still mints a GitHub App token and drives the github platform, so
on Forgejo it would fail at its first step (as Scala Steward did before its Forgejo adaptation); Android
dependencies are currently unwatched.

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
| Flux (distribution; controllers source/kustomize 1.9.4, image 1.2.4) | v2.9.4 | v2.9.6 | movies-gitops; listed weekly by `infra/bin/image_pins.py`, bump by hand (`flux install --export`) |
| kube-state-metrics | v2.14.0 | v2.20.0 | movies-gitops; listed weekly; check `k3s.rules`' series names first |
| Reloader | v1.0.28 | v1.4.22 | movies-gitops; listed weekly, bump by hand |
| Headlamp | v0.45.0 | v0.46.0 | movies-gitops; listed weekly, bump by hand |
| `eclipse-temurin:27-jdk@sha256` (Dockerfile) | 2026-10-17 digest | same | digest refreshed weekly in the nix-update PR |
| `ubuntu:26.04` (Dockerfile, script-test container) | 26.04 LTS | 26.10 (interim, not taken) | newer LTS listed weekly |
| `moby/buildkit` (setup-buildx) | v0.33.1 | v0.34.1 | bumped weekly in the nix-update PR (within major) |
| `swift` CI container (ios.yml) | 5.10 | 5.10.1 / 6.4.1 | listed weekly; moves with the Xcode/Swift pins by hand |

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
- **Client-side operation timeout (`timeout`, driver 5.2+ [unverified]).** About 35
  `Await.result(..., 10.seconds)` budgets in `MongoTaskQueue`, `MongoChunkScrapeStore` and `MongoMovieRepository`.
  With it the driver cancels server work instead of abandoning it. Medium effort.
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

- **The AOT cache carries almost no profiles [measured 2026-10-23, not adopted].** The training main
  (`tools.ClassArchiveTraining`) only loads classes. On local JDK 27+35 under the worker's launcher options plus PL's
  JAVA_OPTS, its cache holds 12,818 classes and 731 MethodTrainingData / 3,661 MethodCounters (`-Xlog:aot`), 125.5 MB.
  Training the same class list and then the fixture pipeline (`FixtureTestWiring("08-06-2026").bootStartup()`, ~20 s)
  stores 15,100 classes and 23,395 MethodTrainingData, 170 MB. It needs `MaxMetaspaceSize` above 128m while training;
  at 128m the training run died of `OutOfMemoryError: Metaspace`. Measured on that same workload, the best case for a
  trained cache, 10 interleaved runs each at load average 17-50:
  user CPU median 58.9 -> 53.1 s (-10 %), workload 22.5 -> 19.7 s, `CITime` total 23.1 -> 21.0 s, peak RSS the same
  (~846 MB). Tier-3 compiles drop 15k -> 2.2k because replayed profiles skip the profiling tier.
  The same trained cache with `-XX:-AOTReplayTraining` was worse than today's (user ~68 s), so the extra classes alone
  do not help; the replayed profiles do. Not adopted: the gain is an upper bound (training and measuring on one
  workload, while prod boots against Mongo). The image would also need to carry the fixture corpus and the test-side
  wiring to train hermetically, plus ~30 s of build. Revisit if a production-shaped workload can run from `src/main`
  without fixtures. The header mode and collector need no extra care: the training runs through the same launcher.
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

- **`ThisBuild / usePipelining`** (sbt 1.4+): **tried 2026-10-23, dropped.** Cold `clean; Test/compile` (compile's
  own elapsed, interleaved, load 11–40): 80/86/108 s off, 74/80/83 s on, ~7 % at comparable load. But an
  incremental edit of one `common` file went from 1–2 s (1 file) to 3–4 s: web recompiled 39 + 24 sources (Twirl /
  routes) on every upstream change. Agents' edit-compile loop and CI's restored target cache are incremental; a
  cold compile is rare (sbt-warm seeds), so the trade is a loss.
- **Short unit-test forks:** **tried 2026-10-23, dropped.** `-XX:+UseSerialGC -XX:TieredStopAtLevel=1` on the
  common / scrapers / testkit forks, `X/Test/test` wall, interleaved with the default (load 25–100):
  common 35/28/23/31/24 s default vs 50/37/64/51 s fast (C1-only is slower on its 1,713 CPU-bound specs);
  scrapers 19/11/10/11/12 vs 11/11/12/38; testkit 3–4 vs 2–4. SerialGC alone: common 37 s (and one 364 s run
  under a load spike), scrapers 11/13 s — no gain either. Startup is not where these forks spend their time.
- **scalac:** `-Wunused:all` (matches the dead-code rule; land module by module because of `-Werror`),
  `-Wsafe-init`, `-Wshadow:all`. **`-deprecation` for the whole build: landed 2026-10-23** — the build was
  already warning-clean under it (0 fixes). Before, a deprecated use outside web still failed `-Werror`, but only
  as "there was 1 deprecation warning; re-run with -deprecation" with no location. `-unchecked` not tried. Not
  `-Yexplicit-nulls`.
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

- `maxParallelForks` for the Robolectric unit tests: **tried, dropped (2026-10-23).** The test task is only ~16 s
  (638 tests, ~15 s of test time) and each fork pays its own Robolectric boot: `testDebugUnitTest --rerun` took
  18/23/31 s with 1 fork, 17/18/24 s with 2 (what a 4-core runner gets), 17/16/53 s with 4 and 28/25/83 s with 7,
  interleaved at load 25-130. No gain above the noise, and 7 forks drove the laptop's load past 600.
- CI ran Gradle three times with `--no-daemon`: **done (2026-10-23)**, the build job's three steps share one daemon
  (the steps stay separate for their reports and uploads). The job's three cache-hit invocations took 19/20 s with
  `--no-daemon` and 14/12 s with a daemon locally. ci.yml's LocalServer step runs Gradle once, so it keeps
  `--no-daemon`.
- `org.gradle.jvmargs`: **done (2026-10-23)**, `-Xmx3g -XX:+UseParallelGC -XX:MaxMetaspaceSize=512m`. GC pauses over a
  clean `assembleRelease` fell from 17.0 s (2g G1) to 3.3 s. Wall times were noisy (load 60-780): median 110 s to 95 s
  over five runs each. The CI job's worst case (sbt 4g + Gradle + Kotlin daemon + test JVM) is ~11 GB of its 16 GB.
- Smaller: `org.gradle.parallel`, `-repackageclasses`, `androidResources.localeFilters` (check against the app's
  real languages).

## iOS

Baseline: deployment target 16.0, `SWIFT_VERSION = 5.0`, no strict concurrency, built with Xcode 27.1.
**Since 2026-10-23:** complete concurrency checking with zero diagnostics (b3c47caa1), then the Swift 6 language mode
for the app, its XCUITests and the SwiftPM package (tools 6.0, `swiftLanguageModes: [.v6]`); the Linux lanes moved
from swift 5.10 to 6.2, whose corelibs Foundation no longer marks URL / DispatchQueue / Calendar / URLSession
non-Sendable (the package builds and its 400 tests pass on Linux 6.2 and 6.4 with zero diagnostics).
`scripts/ios-compile-check.sh` fails on any concurrency diagnostic on all three builds.

1. **`@Observable` (iOS 17)** instead of 9 `ObservableObject` classes and about 22 `@Published` properties.
   `UserPreferences` (`Storage/UserPreferences.swift:34-88`) re-renders every observer on any change. The Combine
   pipelines in `StateSyncService.swift:99,449,460-470` become `Observations` or a cancellable `Task`. The largest
   code deletion. Needs the iOS 17 bump and test rewrites in `KinowoAuthTests`.
2. **Swift 6.2 approachable concurrency + default MainActor isolation**: **measured and skipped
   (2026-10-23).** The app sources carry 28 `@MainActor`s, 18 of them in files the SwiftPM package also compiles
   (`StateSyncService`, `LocationCityResolver`, the `Networking/` stores), where the package has no MainActor default
   (KinowoCore is Foundation-only and runs on Linux), so they stay; at most 10 app-only ones could go. Building the
   app with `SWIFT_APPROACHABLE_CONCURRENCY = YES` + `SWIFT_DEFAULT_ACTOR_ISOLATION = MainActor` fails with 14 errors
   in the first pass (models' conformances become main-actor-isolated, `DetailsStore`/`RepertoireClient`/`PosterStore`
   call into them off the main actor), so it costs more `nonisolated` than the `@MainActor`s it deletes, and
   `NonisolatedNonsendingByDefault` moves where nonisolated async functions run (a behaviour change). Default MainActor
   isolation for the XCUITest target alone (29 class-level annotations) fails too: XCTestCase's inherited initializers
   become main-actor-isolated against their nonisolated overrides (374 errors).
3. **Actors and `Task.sleep(for:)`** instead of queue-plus-continuation code (`ConditionalPayloadCache.swift:55-61,140`,
   `PosterStore.swift:181`), `asyncAfter` (`ShowtimeTuningScreen.swift:199`, `FilmDetailView.swift:441`) and
   `Task.sleep(nanoseconds:)` (5 sites).
4. **`onGeometryChange`** instead of `GeometryReader` + `PreferenceKey` workarounds (`ShowingsView.swift:269-273,347-352`,
   `ContentView.swift:153-157`, `FiltersBar.swift:227`): fewer views per showtime pill in a lazy grid. Whether it
   back-deploys to iOS 16 is unverified.
5. **`@Entry`** for the 7 `EnvironmentKey` structs (back-deploys): about 30 lines deleted.
6. Optional: Swift Testing for new tests (`Package.swift` is tools 6.0 now); `glassEffect` for the iOS 26
   look behind `#available`.
7. Keep: `PosterStore`/`CachedAsyncImage` (`AsyncImage` has no cache control). Skip: SwiftData, `Span`/`InlineArray`.

**Build settings:** `COMPILATION_CACHE_ENABLE_CACHING = YES` (Xcode 26+, absent) for faster warm rebuilds; it helps
CI only if the cache persists between runs. `Package.swift` is on tools 6.0 / Swift 6 mode (done 2026-10-23); its
Linux lanes are pinned at 6.2 because that is the newest toolchain `swift-actions/setup-swift@v2` installs
(ci.yml's LocalServer job), and ios.yml's container stays in step.

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
