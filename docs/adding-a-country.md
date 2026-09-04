# Adding a new country

A repeatable runbook for bringing a new country online — size the source → model
→ data → worker → web → localization → mobile → store. Distilled from the UK (`uk`, Flicks) and
Germany (`de`, Filmstarts) rollouts. Each country is a **fully isolated pipeline**:
its own Mongo db, worker machine, web app, and locale — nothing is shared but the
Mongo cluster and the Docker image.

Use `<cc>` for the country code (`de`), `<lang>` for the BCP-47 language (`de`),
`<Db>` for the database (`kinowo_de`). Work in a git worktree per the repo's
standing rule; commit each phase.

## 0. Size the source, and check whose budget it spends

Do this BEFORE writing any model code. It decides the roster shape, the cadence,
and occasionally whether the country is worth doing at all — and every one of
those is expensive to change once a `Country` is switchable.

1. **Count the venues.** Some sources publish a sitemap — `sitemap-cinemas.xml/`
   on Flicks (trailing slash) — and there `grep -c '<loc>'` is the whole
   measurement. Others publish NONE: SensaCine 404s every sitemap name and names
   none in `robots.txt`, so its own province index (`/cines/provincias-<id>/`,
   paginated) is the only enumeration there is, and the count comes from sweeping
   it. Do not trust the count a listing page's `meta description` advertises —
   SensaCine's summed 6% high against the real deduplicated id count, consistently
   per province, apparently counting closed venues. Whichever way you get it, this
   number, not the country's population, is the cost driver.
2. **Work out the sweep length** and check it against a cadence you would
   actually run: `venues x requests-per-venue x pace = sweep`. Requests-per-venue
   is a property of the client AND of the market (Flicks: 1 programme page + 1
   AJAX call per advertised day, ~36; Webedia: 1 venue page + 1 per advertised
   date — 13.4 in Germany but 7.8 in Spain, measured over 30 venues drawn from
   both large and small provinces). **Measure it on a stratified sample rather
   than inheriting the sibling's number**: it is the term the cadence guard
   divides by, and Germany's stayed at a stale 5 through a horizon change,
   reporting a comfortable 3h sweep while the real one had grown to 7.9h. The UK's numbers are
   the reference point — ~500 flicks-primary venues x 36 x 200ms = ~60min sweep
   on a 420min cadence, i.e. the pacer idle ~86% of the time. **Aim for that
   duty cycle, not for a sweep that merely fits**: a pacer at ~100% duty is a
   sustained flat load against one third-party origin, which is a different
   (and less forgiving) profile than the bursts these paces are tuned against.
   An hourly UK cadence was tried on 2026-07-28 and reverted the same day for
   exactly this. If the full roster does not fit, either lengthen the cadence or
   ship a scoped `active<CC>Cities` roster with a one-line lever to widen it —
   both are legitimate; guessing a faster pace is not.
3. **Ask whether the new source SHARES a limiter with one we already depend on.**
   This is the step the US rollout added, and it has a specific failure it is
   guarding against: a new country quietly spending an existing country's request
   budget and taking a *working* pipeline down. Reuse of a client (Flicks for
   UK+US, Webedia for DE+FR/ES/TR/BR/MX, Cinema City for PL+CZ/HU/SK/BG/RO) is
   exactly when this bites. Check all three layers — the first two by reading,
   the third by measuring:
   - **The pace gate.** `RateLimitedHttpFetch` buckets by FULL LOWERCASED
     HOSTNAME, so two markets on different hostnames never share a slot queue.
     But `RealHttpFetch.HostPolicies` rows match by host SUFFIX, so a new
     market's host does NOT inherit its sibling's row (`flicks.co.uk` does not
     match `flicks.us`) — **a host with no row of its own is not paced at all**,
     which is the condition that produced the UK's self-inflicted 429 storm.
     Give the new host its own row and its own `paceKnob`, so either market can
     be retuned live without touching the other.
   - **The 429 back-off.** `ThrottledHttpFetch` keys its pause by the same full
     hostname, so a `Retry-After` earned on one host cannot stall the other.
   - **The origin.** Different hostnames may still be one quota behind the same
     CDN. MEASURE it rather than assuming, and measure it in the direction that
     matters: drive the NEW host under load while polling the EXISTING host as a
     control, and compare the control's latency and status codes against its
     idle baseline. Keep it bounded (a few hundred requests, a hard time cap,
     stop on the first control failure) — the goal is a signal, not a limit-find,
     and the site being probed is one production depends on. For Flicks this was
     run on 2026-08-30: flicks.us driven at 3-4.3 req/s across 30 workers left
     flicks.co.uk at p50 1.3s / max 2.0s / zero non-200s in 56 control polls,
     against a ~1.0s idle baseline, while the US host itself degraded to p50
     3.7s / p99 39.6s. Read that for exactly what it is — the new host degraded
     while the incumbent beside it did not, which is strong evidence of per-zone
     limiting. It is not proof: the new host only ever STALLED, never returned a
     hard 429/403, so the hard-block case stays untested. Do not go hunting for
     it either — forcing a hard block on a third party's production site is not
     a test worth running, and layers 1 and 2 are decisive on their own and are
     the ones we control.
   - **Note the throttle SHAPE while you are there.** Flicks does not answer 429
     under this load — it STALLS connections, and its throughput plateaus at
     ~3-5 req/s no matter the concurrency (10 workers -> 2.88 req/s, 30 workers
     -> 3.14 req/s). A source that behaves this way cannot be sped up with extra
     egress IPs, because the ceiling is per zone; believing otherwise is how a
     roster gets sized against a pace the origin will never serve.
4. **Check the egress the new country will share.** Residential-proxy hosts all
   egress over the ONE Decodo pool (`residential-proxy.properties`, 7 ports),
   spread by a `hostAndPath` hash with **no concurrency cap**. A new country's
   sweep lands on the same IPs as every existing one, so a ban or a Decodo
   concurrent-auth rejection earned by the newcomer is felt by the incumbents.
   Either give the new country its own ports or ship on the shared pool with the
   `Residential proxy` bar on `/uptime` (and the throttle panel) watched for the
   first few sweeps. Say which you chose and why.

## 1. Model (`common/`)

1. **`Country`** (`common/src/main/scala/models/Country.scala`) — add a
   `case object` to the `Country` enum + `all`: `code`, `displayName`,
   `language = Locale.forLanguageTag("<lang>-<REGION>")`, `mongoDb = "kinowo_<cc>"`,
   `filmwebEnabled` (Filmweb is Polish-only → false elsewhere), `brandName`
   ("Showtimes" outside PL), and `webUrl` — **start `None`** (not yet deployed),
   flip to `Some("https://<cc>.showtimes.cc")` in phase 4. `cities` reads the
   city list from phase 2. `webUrl = Some(...)` is what makes it `switchable`
   (appears in the navbar country switcher) and self-serve its OG origin.
2. **City roster** (`City.scala`) — the sealed `City` model. Two shapes:
   - **Hand-authored case objects** (like PL/UK): `case object X extends City(slug,
     CityLabels(...), lat, lon, ZoneId.of(...))` with `cinemas = Cinema.<city>`.
     Keep an `all<CC>Cities` full roster + an `active<CC>Cities` filter so
     enabling/disabling a city is a one-line edit (see UK). Add the list to
     `City.all`, `City.allModelled`.
   - **Data-driven** (like DE at scale — hundreds of cinemas): generate the roster
     from a checked-in data file (see `data/germany/`), grouped into **regions**
     (≤~200, to cap the dropdown). This needs a `case class` data subtype of the
     sealed `City`/`Cinema` fed into `Cinema.byCity`.
3. **Cinemas** (`Cinema.scala`) — each venue is a `Cinema(displayName, pillName)`
   **and a `Source`** (merge/priority). `Cinema.all` = `byCity.flatMap(_._2)`, and
   `Source.all`/`priority`/`byDisplayName` derive from it — so just add the country
   to `byCity` and it flows through. **Display names must be globally unique** (the
   `Source` key) — disambiguate chain collisions with `(City)`.
4. **Scrape catalog** (`worker/.../services/cinemas/CinemaScraperCatalog.scala`) —
   wire each cinema to its source client, keyed into `baseByCity`. Reuse an existing
   client where the source repeats (UK→`FlicksClient`, DE→`WebediaShowtimesClient`
   via `filmstarts(theaterId, cinema)`); a genuinely new source is a new
   `CinemaScraper` fitting the existing contract (no reaper change — open/closed).

   A client used by ONE country lives in a **per-country subpackage** —
   `services.cinemas.pl` / `.uk` / `.us` — so such a country gets its own
   `services/cinemas/<cc>/` directory. A client that comes to serve TWO moves up
   to `services.cinemas.common` beside `FlicksClient`; that is where
   `WebediaShowtimesClient` went when Spain joined Germany on it, and leaving it
   under `.de` would have made every Spanish scrape import a German package. Anything country-agnostic (the `CinemaScraper` contract, the
   `Retrying`/`Chunked`/`AdaptiveTimeout`/`UptimeRecording` wrappers, the
   `SlotsToMovies` fold, the Zyte egress plumbing) belongs in
   `services.cinemas.common` instead — if a new country's client wants to reuse
   a helper that currently sits under `pl`, lift the country-neutral part into
   `common` rather than importing across country packages. The catalog itself
   stays at `services.cinemas`, the one place that composes all of them.

   **A client that now serves two countries moves to `common` and grows a
   MARKET object** — `FlicksMarket` (UK + US), `WebediaMarket` (DE + ES). Do not
   parameterise it by host alone. Spain's rollout found three things hiding
   inside what looked like a host-only difference, and every one of them is
   silent when wrong:

   - **A localized PATH.** The website-JSON endpoint is uniform across the
     Webedia family, but the venue page the client reads its day list off is
     not: `/kinoprogramm/kino/<id>/` in Germany, `/cines/cine/<id>/` in Spain.
   - **Language-shaped VALUES inside the payload.** `runtime` is `"1 Std. 46
     Min."` in Germany and `"1h 46min"` in Spain; a certificate is a labelled
     `"FSK 6"` in one and a bare `"16"` in the other. These parse to `None` or
     to a wrong number rather than to an error.
   - **The badge VOCABULARY.** A source's own words never reach a badge: every
     token passes `services.movies.ScreeningTokens` at ingest, which maps each
     spelling onto the shared vocabulary and drops what is not a screen format,
     a language version or per-screening accessibility. A new country's labels
     go in that table — not into per-client code, and not through untouched. One
     token in it is the country's own to spell: `Country.voiceoverToken`, `LEK`
     in Poland and `LEC` in the English-speaking deployments.
   - **Local ABBREVIATIONS the UI shows.** The four language versions are
     `OV`/`OmU`/`OmeU`/`DF` to a German and `VO`/`VOSE`/`VOSI`/`DOB` to a
     Spaniard. The tag vocabulary is shared; the token is the market's — and the
     subtitled/dubbed pair has to be repeated on `Country.versionTokens`, which
     is what the Filtry panel's version radios filter on (a country that marks
     neither version gets no row at all).

   Match those language-shaped values **case-sensitively**. German `"Min"` and
   Spanish `"min"` are the same letters, so a loose match let the German market
   read a Spanish `"1h 46min"` as 46 minutes — an hour short, plausible, and
   invisible. Strictness makes a market applied to the wrong payload produce
   NOTHING, which something notices.

   And give the new market **its own `RealHttpFetch.HostPolicies` row.** Rows
   match by host SUFFIX, so `filmstarts.de` does not match `www.sensacine.com`
   and a market with no row of its own **is not paced at all** — the condition
   that produced the UK's self-inflicted 429 storm, and one that is much easier
   to miss here because the two markets share a client, a parser and a
   dashboard. Absent a measurement of the new origin, adopt the SIBLING's
   converged pace rather than guessing a faster one; it costs sweep length and
   risks nothing, and the `paceKnob` retunes it live.

**Tests:** `CountrySpec`, `CatalogSpec`, and any city-count spec. `Source.all`
grows — sanity-check `byDisplayName` uniqueness.

## 2. Data harvest (only if the roster isn't hand-authored)

Three things the US roster (5,031 venues) added to this phase, all of which cost
a re-harvest if missed:

- **Pick the `City` unit deliberately, and do it BEFORE generating.** The source's
  own region list is not automatically the right one, and neither is the biggest
  administrative unit: the US first grouped by state (55) because Flicks' 577
  metros are past the ~200 a picker stays usable at, and that was wrong — nobody
  asks what is on in California. It is one `City` per METRO (448, plus the nine
  states too small to have any), with the state kept as a `CityGroup` so the
  picker can still be browsed. Group by the unit a visitor NAMES, and reach for a
  grouping — not a bigger city — when the resulting list is long.
- **A metro-sized `City` unit needs a slug rule.** 457 US slugs join one global
  `City.bySlug` namespace that already held a UK Birmingham, and a metro name is
  not unique across state lines either. `City.usSlugs` qualifies with the state
  where the bare slug is taken; `CitySpec` fails on any collision that survives.
- **A country wider than one time zone needs the zone per PLACE.** `GermanRegion`
  hardcodes `Europe/Berlin` because Germany has one; `UsCity` takes it as a
  constructor parameter because the US has six, and a national default would move
  a whole coast's "today" boundary. Check this before copying `GermanRegion`.
- **Expect the source's region index to be incomplete.** The US region sweep
  missed 789 of the 5,017 venues in `sitemap-cinemas.xml`; they were recovered by
  fetching each `/cinema/<slug>/` page for its own `data-lat`/`data-lon` and
  address (788/789 succeeded, one genuine 404). Always diff the harvest against
  the sitemap and fill the gap — a venue missing here is a venue that silently
  never gets scraped.

**Guard the display names in the GENERATOR, not afterwards.** `displayName` is
the wire key every per-cinema slot is stored under and `Source.byDisplayName` is
a plain `toMap`, so two venues sharing a name collapse to whichever is built last
and the loser's stored showtimes read back as the winner's. The generator must
refuse to emit an unresolved duplicate within the new country (qualify by town,
then region — 30 US venues needed it), and the roster object must separately
check against every EXISTING country's names, which the generator cannot see
(`UsRoster.claimedElsewhere`). `CountrySpec` asserts global uniqueness across all
four countries as the backstop.

For a full-country sweep (DE), see `data/germany/README.md` + `scripts/`: crawl the
source directory for every venue + its scraper id, geocode the cities
(GeoNames bulk `DE.txt`, 100% match — no live Nominatim needed), cluster into
≤~200 regions. **Source sites rate-limit bulk crawls (429)** — route through the
Decodo residential proxy (`KINOWO_PROXY_*`, `isp.decodo.com:10001`), the same proxy
the prod worker uses. Persist the dataset into the repo (`data/<country>/`) as the
loader's input.

## 3. Worker (`worker-<cc>`)

The worker is split **per country** — each pod watches only its own db's change
stream, because `KINOWO_COUNTRIES` also selects the database. A large roster folded
into a sibling's pod will OOM or throttle it.

1. **`movies-gitops/worker/overlays/<cc>/`** — clone `overlays/de/`. The overlay
   carries only what genuinely differs: `KINOWO_COUNTRIES = "<cc>"`, the two
   scrape-rate levers, the JVM heap, and a fixed `nodePort` (30900/30901/30902 are
   taken; take the next free one). Everything else comes from `../../base`. Drop
   `<cc>` from any sibling's `KINOWO_COUNTRIES`.
2. **Almost nothing to provision** — the country reuses the existing
   `kinowo/worker-secrets` and `kinowo/ghcr-pull`. `MONGODB_URI` is the shared one
   pointing at `10.20.0.10` over the Hetzner private network, and **`MONGODB_DB` is
   never set**: `Country.mongoDb` derives the database from the country, so setting
   it would pin every country to one corpus.

   **The ONE thing that does need provisioning: the Mongo user's grant on the new
   database.** The application user's `readWrite` is granted PER DATABASE, so a
   country whose `kinowo_<cc>` has never been granted fails every read and write
   with `not authorized on kinowo_<cc>` — and because it is an auth error rather
   than a connection error, both web and worker come up, pass their health check,
   and then do nothing. Grant it with the root credentials before the first
   deploy:

   ```
   mongosh "$MONGO_ROOT_URI"   # tunnelled to mongo-1 on 127.0.0.1:27017, authSource=admin
   db.getSiblingDB("kinowo").grantRolesToUser(
     "kinowo_app", [{ role: "readWrite", db: "kinowo_<cc>" }])
   ```

   **`getSiblingDB("kinowo")`, not `admin`** — `kinowo_app` authenticates against
   `authSource=kinowo`, so the user record lives in the `kinowo` database and a
   grant issued against `admin` fails with "User kinowo_app@admin not found".
   Check it with `db.getSiblingDB("kinowo").getUser("kinowo_app")`; the roles
   should read `readWrite@kinowo, @kinowo_de, @kinowo_uk, @kinowo_us, …`.

   This bit the UK and German rollouts in exactly the same way, and was still
   missing for the US on 2026-08-30 — the failure is silent, so nothing reminds
   you: both tiers boot, pass their health checks, and do nothing.
3. **Add its scrape target** to `infra/nix/files/monitoring/scrape-kinowo-apps.yaml`
   (`10.20.0.12:<nodePort>`, labelled `country: <cc>`), and its Deployment name to
   `fleet.k8sDeploy.targets` in `infra/nix/modules/roles/k8s-deploy.nix` so CI can
   roll it.
4. **Let Flux create it**: add a `worker-<cc>-config` Kustomization to
   `movies-gitops/flux/gotk-sync.yaml`, then `kubectl apply -f flux/gotk-sync.yaml`
   once — that file is the one thing Flux does not reconcile for itself. Flux builds
   the Deployment from the overlay and keeps it thereafter, and image-automation
   moves its image with everyone else's. There is no `main.yml` matrix leg — every
   worker leg there is `enabled: false` and adding one would deploy a second copy. Fly deploys
   exactly one thing now, the Polish web app; `FlyDeployScopeSpec` holds that rule.

## 4. Web frontend (`showtimes.cc/<cc>/`)

The web tier is one k3s Deployment per country on `k3s-worker-1`, not Fly apps — see
`movies-gitops/web/README.md` for the shape and `docs/domain-cutover.md` for the
host/DNS side.

**Every Showtimes country is a PATH on the one `showtimes.cc` host** — it was a
subdomain each until 2026-08, and that move is what makes a new country cheap:
`Country.pathPrefix` (`"/es"`) mounts the router, Caddy routes the path to the
country's NodePort, and there is **no new DNS record and no new vhost**, so
neither ACME issuance nor a Caddy restart is in the critical path. Poland is the
exception and stays on its own domain, mounted at the root.

So a new country is: an overlay, one line in each of the two places that name its
NodePort by number (the Caddy PATH UPSTREAM and the Prometheus target), and the
`webUrl` flip.

1. **A kustomize overlay, `movies-gitops/web/overlays/<cc>/`** — copy
   `overlays/de/` and change **only** the three things a country is allowed to
   differ in: `nameSuffix: -<cc>` + the `country: <cc>` label in
   `kustomization.yaml`, `KINOWO_COUNTRY: "<cc>"` in the ConfigMap patch, and the
   Service's `nodePort`. `KINOWO_COUNTRY` is **singular** (the worker's is
   `KINOWO_COUNTRIES`) and it also selects the database, which is why `MONGODB_DB`
   is never set here. CPU request: `500m` unless the roster is Poland-sized —
   memory stays at the base's 1Gi request+limit, which is the sizing proven not to
   OOM. Anything else you find yourself copying belongs in `base/` instead.
2. **Allocate the next free NodePort.** The workers hold 30900–30902 and the web
   tier 30910 (pl) / 30911 (de) / 30912 (uk) / 30913 (us) / 30914 (es), so a sixth takes 30915. It is
   **fixed, never allocated**: the Caddy vhost and the Prometheus target both name
   the number, so a Service re-created with a fresh port takes the site off the
   internet and turns the scrape target red at the same moment. Keep the overlay at
   **one replica** — a second pod behind one NodePort makes kube-proxy alternate
   between two independent sets of counters and every `kinowo_web_*` alert sees
   phantom resets.
3. **A Caddy PATH UPSTREAM** in `infra/nix/hosts/k3s-worker-1/default.nix` — one
   line inside the EXISTING `"showtimes.cc"` block, not a vhost of its own:
   `"/es" = "127.0.0.1:30914";`. Nothing in the cluster terminates TLS — k3s runs
   with traefik and servicelb disabled — so Caddy on the node *is* the ingress,
   and the pod's NodePort stays unreachable from outside (the firewall opens
   22/80/443 and nothing else).

   ⚠️ **MERGING THIS DOES NOT DEPLOY IT, AND NOTHING WILL DEPLOY IT FOR YOU.** CI
   only STAGES NixOS closures (`nix-stage-closures.yaml` never activates), and
   `fleet/auto-apply.nix` activates only changes that disturb no running unit. A
   new vhost changes `caddy.service`, so auto-apply refuses it by design and logs:

   ```
   nixos-auto-apply: blocked (units_would_change): 1 systemd unit(s) differ, so this
   switch would disturb running services: caddy.service.d/overrides.conf. Deploy it by hand.
   ```

   Until someone activates it the country's host has DNS and a running pod but no
   vhost, so it answers with another site's certificate or a TLS error — and every
   other check looks green. Activate on `k3s-worker-1` (`2.28.47.31`):

   ```
   S=$(readlink -f /var/lib/nixdeploy/staged-system)
   grep -c '<cc>.showtimes.cc' $S/etc/caddy/caddy_config   # confirm you're activating the right closure
   nix-env -p /nix/var/nix/profiles/system --set $S && $S/bin/switch-to-configuration switch
   ```

   This RESTARTS Caddy, which briefly interrupts TLS for **every** country's site,
   not just the new one. It is seconds, and it is the accepted cost — but say so
   out loud afterwards rather than letting an unexplained blip pass.

   Check `journalctl -u nixos-auto-apply.service` for the classifier's verdict
   before assuming a nix change landed.
4. **No DNS record, and no certificate.** A path-mounted country arrives on a
   host that already resolves and already holds a valid certificate, so both of
   the steps that used to gate a launch are simply gone. This is the single
   biggest saving of the shared-host move, and it removes the failure that used
   to bite hardest: a name that did not resolve yet failed ACME HTTP-01 issuance,
   gave the visitor a hard TLS error rather than a degraded page, and earned a
   Let's Encrypt backoff on the *failed* validation for being early.
   `docs/domain-cutover.md` has the record list for a country that does want its
   own domain (Poland is the only one).
5. **A Prometheus target** in `infra/nix/files/monitoring/scrape-kinowo-apps.yaml`,
   under the `kinowo-web` job:
   `- targets: ["10.20.0.12:30913"]` with
   `labels: { app: kinowo, country: <cc>, tier: web, platform: k3s, instance: kinowo-web-<cc> }`.
   Prometheus runs outside the cluster with no Kubernetes credentials, so it can
   neither discover the pod nor resolve cluster DNS; the NodePort is the only
   address that survives a rollout. The same file's `kinowo-worker` job needs the
   worker's target alongside it (phase 3).
6. **Flip `Country.<Cc>.webUrl` → `Some("https://<cc>.showtimes.cc")`** — now
   `switchable`. This one flag AUTO-adds the country to (a) the navbar country
   `<select>`, (b) the debug `?country=` switcher + the dev per-country `DebugStack`
   wiring, and (c) the `/api/catalog` mobile endpoint — all three iterate
   `Country.switchable`, so no separate edits. Then update the CountrySpec /
   CatalogSpec / PageSnapshot assertions that asserted the country was excluded, and
   **regenerate the page snapshots** (the switcher gains an option) + the **mobile
   catalog seeds** (`CatalogSeedSpec` rewrites `ios/…/catalog-seed.json` +
   `android/…/catalog-seed.json` — mobile then picks up the country + cities
   automatically).
7. **Secrets need nothing new.** All three Deployments `envFrom` the one
   `kinowo/web-secrets` Secret in the cluster (`MONGODB_URI` at Mongo's private
   address, TMDB/OMDb, the OAuth client pairs, Sentry, the admin allowlist), so a
   fourth country inherits it by existing — `movies-gitops/web/README.md` lists
   the keys. Only a value genuinely specific to the new country would need adding,
   and the Secret is built from the repo-root `.env.local` and piped over SSH
   rather than passed as arguments, so no value ever reaches a process list.
   **OAuth login needs nothing either, and that is deliberate.** It used to need a
   per-country redirect URI registered by hand in both the Google and Facebook
   consoles — a manual, provider-side step that nothing in a deploy could check
   and that failed silently until a real person tried to sign in on the new site.
   Every country now hands the provider the ONE registered URL per provider
   (`models.Country.oauthCallbackOrigin` + `/auth/<provider>/callback`), and the
   deployment mounted at the apex either finishes the flow — when the new country
   shares its origin, so the browser is still sending the cookie holding the CSRF
   state — or relays it untouched to the deployment that can. A country added under
   `showtimes.cc/<cc>` is the first case, so there is nothing to register.
8. **Roll it out.** Add a `web-<cc>-config` Kustomization to
   `movies-gitops/flux/gotk-sync.yaml` and `kubectl apply -f flux/gotk-sync.yaml`
   once. Flux creates the Deployment from the overlay and gives it the image the
   base manifest names — which is a REAL build, written there by image-automation,
   so a brand-new country comes up on whatever the rest of the fleet is running.

   **This used to be the fiddliest step in the runbook, and is not any more.**
   `apply.sh` existed because `kubectl apply` and `kubectl set image` fought over
   the image field: every manifest's `image:` was a `:latest` PLACEHOLDER, so a
   plain apply silently reverted production to `latest`, and a first-ever apply had
   to be handed an explicit SHA or the new country came up on it. None of that
   survives the pin moving into git (@4def6caa1) — `apply.sh` is deleted, and there
   is no image ref to choose.

   **Two host-topology traps worth knowing before you debug anything:**
   - The k3s API is at `root@128.140.49.167`, which is
     **`monitoring-1`** — the control plane, running Grafana's Caddy. The pods,
     their NodePorts, and the PUBLIC Caddy all live on **`k3s-worker-1`**
     (`2.28.47.31` / `10.20.0.12`). Curling a web NodePort from the API host
     returns nothing for every country, including the ones serving perfectly.
   - The app containers have **no `curl`**, so `kubectl exec … curl` fails with
     `executable file not found`. Verify a new country from OUTSIDE
     (`curl https://showtimes.cc/<cc>/`) or from its logs — `MongoConnection
     connected to kinowo_<cc>` is the line that proves the grant in phase 3 worked.

## 5. Localization

- **Web** (`web/src/main/resources/`): `messages.<lang>` mirroring `messages.en`'s
  keys; add `<lang>` to `play.i18n.langs` in `application.conf` (else the deployment
  silently falls back to Polish). Fix any hardcoded literals to `messages(...)`.
  **Two things outside the bundle are language-shaped too, and neither fails
  loudly.** `controllers.JsLocale` carries the showtime PLURAL FORMS the client
  JS renders — a language that adds no entry there silently ships the English
  "showing/showings" inside an otherwise translated page. And
  `models.CityGrammar` supplies the preposition in front of a city name for the
  locative slot ("in London", "en Madrid"); it read a hardcoded English "in" for
  every non-Polish language until Spain, which would have put "in Madrid" in
  every Spanish share card and every piece of Spanish structured data.
  Generate `og-home-<cc>.jpg` (the landing share card) AND the country's
  per-city cards, and add the country to the `regenerate-og-cards` workflow's
  matrix so the weekly refresh keeps them current. This IS a launch blocker:
  `OgCardAssetsSpec` fails on a country whose landing or city cards are missing,
  because a named-but-absent card is not a graceful degrade — it points every
  share at a 404 and the link previews with no image at all. Germany and the US
  both shipped that way before the spec existed.
  **The workflow cannot produce the FIRST set** — it screenshots the live site,
  which does not exist before launch. Generate placeholders locally instead; the
  recipe and the reasoning are in phase 8.
- **iOS** (`ios/`): add a `<lang>` localization to every key in
  `Localizable.xcstrings` + `InfoPlist.xcstrings`; add `<lang>` to `knownRegions`
  in `project.pbxproj`; add a `Country(code:"<cc>", languageCode:"<lang>")`
  fallback-seed entry (locale is country-forced). The `.xcstrings` files are
  JSON, so a script is the sane way to add ~85 keys — but re-serialise them in
  the EXACT style each file already uses or the diff is the whole file instead of
  the lines you added. The two catalogs here do not agree: `Localizable.xcstrings`
  is written `"key": value` and `InfoPlist.xcstrings` `"key" : value`, both with a
  2-space indent and keys sorted. A handful of keys carry NO localizations at all
  (dev/tuning screens, where the key IS the string); leave those alone, as `de`
  and `pl` do. Then add a case to
  `KinowoUITests/LocalizationUITests` — it launches the app per country and
  asserts what actually rendered, which is the only layer that catches a key
  you forgot to translate. Three traps worth knowing:
    - Xcode's extractor only sees `LocalizedStringKey` positions. A caption
      passed as a `String` (a view's `let title: String`, `Text(someString)`)
      is invisible to it and will silently stay untranslated — type such
      parameters as `LocalizedStringKey`.
    - `KinowoCore` deliberately excludes the catalog, so `String(localized:)`
      in a file that compiles into it resolves to the bare key. Display
      captions belong in the app target (see `DateFilter.label`).
    - Plurals go in the catalog as `variations.plural` read through
      `String.localizedStringWithFormat`, not as hand-written `if count ==`
      rules — Polish needs one/few/many where English and German need two
      forms (see `showings.more_showtimes`).
- **Android** (`android/`): `res/values-<lang>/strings.xml` mirroring the base
  keys; a `Country(code="<cc>", languageTag="<lang>")` seed entry.
- **Google Play** (`android/app/src/main/play/listings/<locale>/`): `title.txt`
  (≤30), `short-description.txt` (≤80), `full-description.txt` (≤4000). Publish with
  `./gradlew :app:bootstrapReleaseListing` then `:app:publishReleaseListing` (see
  `android/PLAY_PUBLISHING.md`).
- **Title rules** (`common/src/main/scala/services/titlerules/`): audit the seed for
  rules that rewrite WORDS rather than punctuation/format, and tag each with the
  countries it belongs to (`countries = Some(Set(Country.Poland))` — the sealed
  `Country` itself, never a string code). A rule with
  `countries = None` runs everywhere, which is right for language-neutral strips
  (4K-restored suffixes, format tags) and wrong for anything language-specific.
  Getting this wrong is silent and expensive: the Polish `" & "` → `" i "`
  unification ran for every country, so a German film listed as
  "Minions & Monster" was served as "Minions i Monster" AND keyed
  `minionsimonster` — a key no German cinema slot can produce, so every settle
  re-canonicalised the row and orphaned its showtimes until the next scrape
  (a 30-minute square wave on `kinowo_worker_showtimes`, 2026-07-18).

## 6. Deep links (mobile)

The new `showtimes.cc/<cc>/` links should open the native apps — **Universal
Links** on iOS, **App Links** on Android — not the browser. The AASA /
`assetlinks.json` files are served identically on every deployment (same web
binary, one app id `CQ4YC43YDM.dev.kinowo.Kinowo` / package `pl.kinowo`), so
there is **no web change** — this is purely app-side.

**And since the countries share one host, there is no new HOST to register
either** — `showtimes.cc` is already in both entitlements and the manifest, and
already verified. What a new country adds is its PATH SEGMENT, so the parser
knows to step over `/es/` before reading the city. **Two** places, mirroring the
existing UK/DE/US entries:

1. **iOS parser** (`ios/Kinowo/Models/DeepLink.swift`) — add the code to
   `countryPathSegments`.
2. **Android parser** (`android/app/src/main/java/pl/kinowo/deeplink/DeepLink.kt`) —
   add the code to `COUNTRY_PATH_SEGMENTS`.

Both are guarded so a segment that is ALSO a city slug still resolves as the
city, which is why this is a set of country codes rather than a blanket
"drop the first segment".

Cross-country switching is automatic: both `handleDeepLink`s resolve the linked
city's country from the live catalog (`countryOf`) and switch the deployment before
loading, so a new country's cities — which arrive via `/api/catalog`, not the
compile-time roster — just work once the seed/catalog includes them (phase 4
regenerates the mobile catalog seeds).

**Tests:** `DeepLinkTests`/`DeepLinkTest` (the host parses) + `CityTests`/`CitiesTest`
(`countryOf`). The OS-level routing itself is only verifiable on a device/emulator
(the app must be signed and the host's AASA/assetlinks reachable). iOS needs the
**paid** Associated Domains entitlement (already wired); Android App Links
additionally need the **Play App Signing** SHA-256 in `assetlinks.json` before a
Play-installed build auto-verifies (see `web/src/main/resources/wellknown/README.md`).

## 7. Observability

**The live stack is the fleet's own**, on `monitoring-1`: Prometheus + Grafana at
`grafana.kinowo.net`, configured under `infra/nix/files/monitoring/`. The
`fly/grafana/` tree is the RETIRED Grafana-on-Fly stack (`kinowo-grafana` is scaled
to zero, and the deploy annotation step in `main.yml` carries the scar of pointing
at it) — but it is still committed and still read by the `Grafana*Spec`s under
`worker/src/test/scala/deploy/`, so keeping it consistent is a CI obligation rather
than a production one. Do both:

1. **`infra/nix/files/monitoring/scrape-kinowo-apps.yaml`** — the one that matters.
   Add the new worker target to the `kinowo-worker` job and the new web target to
   the `kinowo-web` job, each carrying `country: <cc>` in its own label block
   (phases 3 and 4 give the NodePorts). Prometheus runs outside the cluster with no
   Kubernetes credentials and discovers nothing, so a country missing from this file
   is simply unmonitored — no error, no red target, no panel.
2. **`fly/grafana/victoria/scrape.yml`** — add a `kinowo-worker-<cc>` target (its
   `kinowo_worker_*` series carry `country="<cc>"`) and a `showtimes-<cc>-web`
   target, so the retired stack stays internally consistent for the specs that read
   it.
3. **The throttle backstop** — `fly/grafana/provisioning/alerting/contact-points.yaml`
   needs a `WorkerThrottle<Cc>` webhook pointing at
   `http://kinowo-worker-<cc>.internal:9000/throttle`, and
   `notification-policies.yaml` a route matching `app = kinowo-worker-<cc>` to it.
   This one is unavoidable: Grafana cannot template a webhook URL from
   `$labels.app`, so the target worker must be resolved at routing time. Skipping
   it is caught by `GrafanaWorkerThrottleCoverageSpec` before it can ship. Read the
   whole mechanism as inherited from the Fly era: its partner, the worker's primary
   self-throttle `CpuCreditPoller`, watched a shared-cpu credit bucket that is a Fly
   billing concept and does not exist for a pod on a dedicated eight-core box.

Everything else follows automatically:

- **App-level panels** (`kinowo_worker_*`, `kinowo_web_*` — task flow, queue depth,
  corpus, films served) carry a `country` label and are scoped by the `Country`
  dropdown, which is a `label_values(...)` query rather than a list, so the new
  country appears in it as soon as its targets are scraped.
- **Host panels** on the fleet dashboards are node_exporter numbers for
  `k3s-worker-1`, `monitoring-1` and `mongo-1`. They are per-HOST and can never be
  per-country: every worker and web pod of every country shares one box, so read
  them as the machine and drop to the per-process JVM panels for one country.
- **Fly-host panels** (CPU load / credit / throttle / steal, memory, HTTP latency,
  instance up) exist only in the retired `fly/grafana/` tree, are fleet-wide and are
  NOT country-scoped — Fly's managed Prometheus exported no `country` label on
  `fly_instance_*` / `fly_app_*`, only `app`. They scope by app-name convention
  instead: `kinowo.*|showtimes-.*` for both roles, `kinowo|showtimes-.*` web-only,
  `kinowo-worker.*` worker-only.

Never widen those matchers by adding the new app to a list — an enumerated matcher
is how `showtimes-de` went invisible on all six Fly-host panels and three alert
rules until 2026-07-18. `GrafanaCountryBlindAppMatcherSpec`
(`worker/src/test/scala/deploy/`) derives the deployed app set from the repo's
`fly*.toml` files and fails CI if any `app=~"…"` matcher accepts one country's app
of a role while rejecting another's, so this can't regress silently.

## 8. Ship

Provision everything (phases 3–4) **before** merging, so the deploy legs have live
targets. If you cannot — the US was merged first, because its images had to exist
before its Deployments existed — then add the Kustomizations promptly afterwards
and know what the gap looks like: the country's overlay sits in the config repo
reconciled by nobody, because `gotk-sync.yaml` is the one file Flux does not apply
for itself. Nothing is red; the country simply never appears.

**The order that actually works, and why each step precedes the next:**

1. **Mongo grant** (phase 3) — nothing works without it and it fails silently.
   TWO users need one: the application user (`kinowo_app`, readWrite) and the
   CI corpus reader (`kinowo-ci-corpus`, read), whose grant list
   `ConvergenceLegWiringSpec` pins against `Country.all` — so the nix role
   documenting it has to name the new database or a test fails.

   **Naming the database in `roles/mongo-ci-read.nix` does NOT grant it.** That
   list is documentation of a state a human has to produce; `db.createUser()`
   ran once, and every country after the first is a `grantRolesToUser` issued by
   hand against the running mongod with the root credentials:

   ```
   db.getSiblingDB("admin").grantRolesToUser(
     "kinowo-ci-corpus", [ { role: "read", db: "kinowo_<cc>" } ])
   ```

   Spain merged with `kinowo_es` in the nix list and no grant on the host, so the
   first nightly `Record scrape fixtures` died with `not authorized on kinowo_es
   to execute command find: cinema_scrapes` — and the convergence leg, which only
   reads what that job recorded, was red for a day with a message about a missing
   fixture rather than a missing grant.
2. **Merge**, and wait for a GREEN image build whose SHA contains your commit.
   (No DNS step for a path-mounted country — see phase 4.4.)
3. **`kubectl apply -f flux/gotk-sync.yaml`** in `movies-gitops`, once, so Flux
   picks up the two new Kustomizations — per phase 4.8. No image ref to choose.
4. **Activate the staged NixOS closure by hand** for the Caddy path upstream
   (phase 4.3). Auto-apply will NOT do this for you — the change touches
   `caddy.service`, which the classifier refuses by design — and `/{cc}/` 404s
   until it is done, while every other indicator is already green. This is the
   step most likely to be forgotten. It restarts Caddy, which briefly interrupts
   TLS for EVERY country; that is seconds and it is accepted, but say so
   afterwards rather than letting an unexplained blip pass.
5. **Verify**: each new pod `Running` + `/health` passing, the worker's log
   line `MongoConnection connected to kinowo_<cc>` (that is what proves step 1),
   `curl https://showtimes.cc/<cc>/` serving, and the two Prometheus targets
   green. Watch the first sweep's throttle percentage on panel 14 of
   kinowo-worker-diag — that is where an unpaced or over-paced new host shows
   up, and it is the number the pace was set against.

6. **Record the country's first convergence corpus by hand**, once the worker
   has completed a sweep and `cinema_scrapes` holds something worth replaying:
   `gh workflow run "Record scrape fixtures"` (~10 minutes, all countries in
   parallel). The convergence legs replay a corpus RECORDED OUT OF BAND — they
   never reach prod Mongo themselves — and `convergence-setup` looks only at
   SUCCESSFUL runs of that workflow, so until one has published a
   `scrape-fixtures-<cc>` artifact the country's leg fails every assertion with
   "no corpus fixture for <cc>". Waiting for the nightly is a day of red on a
   country that is otherwise fine, and a nightly that fails (see step 1) is
   another day. The leg has no enrichment tree either on its first run: it
   fetches live, publishes what it recorded, and the run after it is the fast
   one — so expect the first leg to be slow, and read a timeout there as a cold
   start rather than a regression.

**The share cards are a launch-order problem, and they are the one thing that
cannot follow the normal order.** `OgCardAssetsSpec` fails while a country's
`og-home-<cc>.jpg` and per-city cards are missing, so they must be committed
BEFORE merge — but `regenerate-og-cards.yml` screenshots the LIVE site, which
does not exist yet. Break the cycle by generating placeholders locally against
an empty database: the generator renders a valid card with no films (wordmark +
city line over an empty grid), and the weekly job replaces them with real
posters once the country serves.

```
MONGODB_DB=<scratch> KINOWO_COUNTRY=<cc> sbt web/run          # local, empty corpus
KINOWO_OG_BASE=http://localhost:9000 KINOWO_COUNTRY=<cc> \
  sbt 'web/PageTest/runMain tools.OgCardGenerator home'       # then again with no arg for the cities
```

Say out loud in the commit that the cards are placeholders. Germany and the US
both shipped with NO cards at all, which is what the spec was added to stop.
