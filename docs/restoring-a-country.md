# Restoring a stopped country

> **Status: the 2026-08-02 shutdown was reversed on 2026-08-29.** UK and Germany
> are deployed again — on `uk.showtimes.cc` and `de.showtimes.cc`, on the k3s
> cluster rather than Fly (see `docs/domain-cutover.md`). Everything below is
> kept because it is the general procedure, not a description of today; read it
> as "what to do if a country is stopped again", and note that steps naming
> `flyctl` now describe a tier that no longer runs on Fly.


On **2026-08-02** the UK and German deployments were taken out of service to cut
hosting cost, leaving Poland as the only country served. Nothing about those
countries was deleted — their databases, city rosters, scrapers, title rules,
translations, fly configs and BOTH app-store listings are all intact. What
changed is a short list of switches, and this file is that list.

`docs/adding-a-country.md` is the other direction (a country that never existed).
Use this one for `uk` and `de`.

Throughout, `<cc>` is `uk` or `de` and `<CC>` is `GBR` or `DEU`.

## The one switch that matters

`models.Country.<Country>.webUrl` in `common/src/main/scala/models/Country.scala`
is `None` for both. It is the single lever the whole product hangs off:

| Reads `webUrl` (via `Country.switchable`) | Effect while `None` |
| --- | --- |
| `_navbar.scala.html` country `<select>` | not rendered (guarded on `sizeIs > 1`) |
| `models.Catalog.json` → `/api/catalog` | country + its cities absent from the payload |
| iOS `CityGate` / `FiltersBar`, Android `CountryPicker` / filters sheet | pickers hidden — they read the catalog |
| `Country.ogOrigin` | falls back to the deployed host, so share links resolve |
| `CountryConvergenceBehaviour.productionIsLive` | the ±5% production band is cancelled for that country |
| `_countrySwitch` / `_debugNavbar` (admin, debug) | render the country's name instead of a switcher |

Putting the host string back re-arms every one of those, including the
convergence band. There is no second place to remember for any of them.

## Checklist

1. **Bring the country's workloads back.** ⚠️ **Superseded by the k3s migration.** Both
   tiers now run as Kubernetes Deployments, not Fly machines, so there is nothing to
   `flyctl start` — scale the country's Deployments back up instead:

   ```
   infra/kubernetes/apply.sh worker <cc>
   infra/kubernetes/apply.sh web <cc>
   ```

   The Fly instructions this step used to give (`flyctl scale count 1 -a showtimes-<cc>`,
   `flyctl machines start -a kinowo-worker-<cc>`) would start a SECOND copy of the country
   alongside the cluster's — two workers both holding change streams and both projecting the
   read model. Do not follow them unless you are deliberately rolling the whole tier back to
   Fly.

   ⚠️ Stopping a WEB app does not hold — `auto_start_machines = true` with
   `auto_stop_machines = false` means any inbound request boots it and it never
   stops again. That is why the web tier was scaled to zero rather than stopped.
   Re-check machine state a few minutes after any future stop.

2. **Re-enable its deploy legs.** ⚠️ **Superseded.** Every row in
   `.github/workflows/main.yml` except `kinowo` is `enabled: false`, deliberately and
   permanently: both tiers ship to k3s from that same file's `build-web` /
   `build-worker` jobs, and flipping a row back to `true` deploys a second copy on Fly
   (the workflow says so in place). Nothing needs re-enabling here for a country to
   come back.

   The one enabled row is not an exception to that: `kinowo` is the RETIRED
   `kinowo.fly.dev` redirect host, not Poland's site, and it deploys so the redirects
   track `main`. Fly runs exactly one thing from this repository, and
   `FlyDeployScopeSpec` fails the build if a second row is flipped on without that
   spec being updated to say why.

3. **Give the country its `webUrl` back** (the table above):
   `webUrl = Some("https://<cc>.showtimes.cc")`.

4. **Restore the two mobile registries.** `Country.all` in
   `ios/Kinowo/Models/Country.swift` and
   `android/app/src/main/java/pl/kinowo/model/Country.kt` were trimmed to Poland.
   This is not cosmetic: `Country.byCode` resolves the PERSISTED selection through
   these lists and the API base URL is built from the result, so trimming them is
   what moved existing `uk`/`de` users onto the live host. Re-adding restores the
   country as a selectable, persistable choice. Keep the two lists mirrored.

5. **Regenerate what derives from the above** — both are guarded, so CI tells you
   if you forget:
   - mobile catalog seeds — `sbt "common/testOnly tools.CatalogSeedSpec"` rewrites
     `ios/Kinowo/catalog-seed.json` and `android/app/src/main/assets/catalog-seed.json`;
     run it twice (the first run fails by design after rewriting).
   - page snapshots — the navbar `<select>` reappears in all four
     `test/resources/fixtures/08-06-2026/expected-*.html`. See the
     `regenerate-snapshots` skill.

6. **Re-arm its "serving app down" alert.** In
   `fly/grafana/provisioning/alerting/alert-rules.yaml`, `showtimes-uk-serving-down`
   is kept but has `noDataState: OK` / `execErrState: OK` instead of `Alerting` —
   otherwise a deliberately-absent app pages critical to Telegram every 4 hours
   forever. Set both back to `Alerting` so absence is the alarm again. (There is no
   `showtimes-de` equivalent; write one from the `kinowo` rule if Germany returns.)
   Every other Grafana asset was left untouched — the `app=~` fleet rules are
   already `noDataState: OK` and the dashboard panels just render empty.

7. **Re-enable the UK OG-card leg.** `.github/workflows/regenerate-og-cards.yml`:
   delete the `if: false` on "Regenerate per-city UK cards from live prod". It
   cannot run while the country is undeployed — `ogOrigin`'s fallback would point
   it at Poland's host and it would commit a PR of 404 cards.

8. **Put the country back on sale.** Nothing about the store LISTINGS was touched:
   descriptions, keywords and screenshots for `de-DE`, `en-GB` and `pl-PL` are all
   still live in both stores and mirrored under `ios/store/listings/` and
   `android/app/src/main/play/listings/`. Only availability changed.
   - App Store: territory availability was narrowed to `POL`. `PATCH
     /v1/territoryAvailabilities/{id}` with `{"available": true}` for `<CC>` — the
     id is the base64 of `{"s":"<appId>","t":"<CC>"}`, read it off
     `/v2/appAvailabilities/{appId}/territoryAvailabilities`.
   - Play: **was never changed** — production is still available in DE, GB and PL.

9. **Re-record the corpus and its baseline once the country's data is FRESH again.**
   The convergence legs keep running for all three countries throughout the
   shutdown — only their production band is cancelled, and step 3 re-arms it. But
   the moment it is re-armed it will score the run against whatever is on disk,
   and during the shutdown both sides are stale in a specific way: the archive
   holds the last scrapes the worker took (including ones it never folded) and the
   baseline holds the read model as it was when the worker stopped, which is why
   the band was cancelled in the first place.

   So: bring the worker up (steps 1–2), **let it complete at least one full scrape
   and projection cycle** — the cadence is `KINOWO_SCRAPE_FRESHNESS_MINUTES` in
   that country's `fly.worker.<cc>.toml`, so ~3h for DE and ~7h for UK — and only
   then re-record, so corpus and baseline are captured together from a prod that is
   genuinely moving again:

   ```
   gh workflow run "Record scrape fixtures" --ref main     # ~10 min, all countries
   ```

   Re-arming the band before that re-record will fail the leg on stale inputs, and
   the failure will look like a pipeline regression when it is just an old dump.

   Nothing else about fixture recording needs touching: the job reads the
   `cinema_scrapes` archive over a Mongo tunnel (DB only, no live scraping), so it
   kept working with every machine down — the corpora simply stopped changing.

   Note the convergence suite does NOT read the wall clock: `renderAt` is derived
   from the corpus's own earliest showtime, and nothing on the projection path
   filters on "today". A frozen corpus therefore renders the same window forever
   rather than ageing out — which is why these legs stay meaningful and stable for
   as long as a country is parked, however long that is.

## What will fail until you finish

A partial restore is loud on purpose. The specs below encode "one deployed
country" and fail the moment `webUrl` comes back — that is the checklist telling
you what is left:

`CountrySpec`, `CatalogSpec`, `PageSnapshotSpec`, `NavbarDebugLinkSpec`,
`TasksViewSpec`, `UptimeViewSpec`, `DebugViewCountrySwitchSpec`,
`MovieControllerDebugCountrySpec`, `LandingCountryPreviewSpec`, iOS `CountryTests`
and `CityChoiceSearchUITests`, Android `CountryTest`, `CityChoiceSearchTest`,
`FiltersSheetOrderTest`, `LocaleStringsTest`, `UserPreferencesCountryTest`.

For a full two-country restore the cleanest starting point is
`git revert ea73ff84d` (the commit that made the product Poland-only), then work
the checklist above for anything that revert does not cover — the machines, the
deploy flags, the alert state and the store territories.
