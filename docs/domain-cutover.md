# Cutting the product over to kinowo.net / showtimes.cc

On **2026-08-29** the web tier moved off Fly.io onto the k3s cluster, and off Fly's
hostnames onto two domains this project owns:

| was | is | serves |
| --- | --- | --- |
| `kinowo.fly.dev` | `kinowo.net` | Poland |
| `showtimes-uk.fly.dev` | `uk.showtimes.cc` | United Kingdom |
| `showtimes-de.fly.dev` | `de.showtimes.cc` | Germany |
| — | `showtimes.cc` | the brand front door: a country picker, Poland included |
| `grafana.2-28-52-210.sslip.io` | `grafana.kinowo.net` | Grafana (both names serve) |

`docs/adding-a-country.md` and `docs/restoring-a-country.md` cover the country
dimension. This file covers the HOST dimension, and exists mainly for its
ordering — several of the steps below are only safe in one sequence.

## The order, and why it is not arbitrary

1. **DNS first.** Every name must resolve to its host before anything is
   deployed, because Caddy obtains certificates over ACME **HTTP-01**: a name
   that does not resolve fails issuance, and the failure a visitor sees is a hard
   TLS error, not a degraded page. Let's Encrypt also rate-limits *failed*
   validations, so deploying early buys a backoff as well as an outage.

   ```
   kinowo.net        A  2.28.47.31    # k3s-worker-1 — the web pods
   www.kinowo.net    A  2.28.47.31
   grafana.kinowo.net A 2.28.52.210   # monitoring-1 — NOT the same host
   showtimes.cc      A  2.28.47.31
   www.showtimes.cc  A  2.28.47.31
   uk.showtimes.cc   A  2.28.47.31
   de.showtimes.cc   A  2.28.47.31
   ```

   The records live at OVH. Both addresses are pinned by
   `infra/terraform/primary_ips.tf` with `auto_delete = false`, so they are safe
   to hardcode in a zone file.

2. **The cluster secret**, before any pod starts: `kinowo/web-secrets` (see
   `infra/kubernetes/web/README.md` for its keys and why it is not in git). A pod
   without it does not crash-loop, it sits in `CreateContainerConfigError`, which
   reads like an image problem rather than a missing secret.

3. **Push to main.** This builds `ghcr.io/pawelkrupinski/movies-web:<sha>` and
   stages the NixOS closures. Two things have to land before a web rollout can
   succeed, and on the FIRST push neither has:
   - the `web-pl` / `web-de` / `web-uk` Deployments do not exist yet, and
   - `monitoring-1`'s deploy endpoint has not yet learned the `movies-web`
     target (`fleet.k8sDeploy.targets`), so it refuses the image.

   So expect the first `Build web image (GHCR)` run's deploy job to fail. That is
   the documented cost of bootstrapping, not a fault.

   (`movies-web` came out **public**, like `movies-worker` — GitHub gave it the repository's
   visibility rather than defaulting it to private, so the pull needed no intervention. Noted
   because the opposite would have looked like a manifest fault: the tell is
   `ImagePullBackOff` with a 403 rather than a 404.)

4. **Let the closure activate** (`systemctl start nixos-auto-apply` on each host
   to skip the poll), which brings up Caddy's vhosts and teaches the deploy
   endpoint about `movies-web`.

5. **Create the Deployments**, now that an image exists to pull:

   ```
   infra/kubernetes/apply.sh web all
   ```

6. **Pin the build**, so nothing is left running `:latest`:

   ```
   ssh -i <k8sdeploy key> k8sdeploy@2.28.52.210 ghcr.io/pawelkrupinski/movies-web:<sha>
   ```

7. **Retire the Fly web apps.** `showtimes-uk` and `showtimes-de` have their
   `.github/workflows/main.yml` legs `enabled: false` and are scaled to zero —
   scale to zero rather than `machines stop`, because `auto_start_machines = true`
   means any inbound request boots a stopped web machine and it never stops again.

   `kinowo` is the exception and stays up as a REDIRECT HOST, because it is the
   only one of the three with published links behind it (see the last section).
   Its leg is the ONE Fly deploy this repository still does, so the redirects
   track `main` rather than drifting behind a hand-rolled deploy nobody remembers
   to run. `KINOWO_RETIRED` lives in `fly.toml`, not the workflow, so that leg
   cannot un-retire the host — it can only ship a newer build of the same
   redirects. The price is a rolling restart of the redirect host on every push.

   **Fly now hosts exactly one thing from this repository, and CI deploys only
   that.** `FlyDeployScopeSpec` enforces it across every workflow: the five other
   matrix legs stay off, and `deploy-grafana.yml` — which would start the stopped
   `kinowo-grafana` rollback machine — is `workflow_dispatch` only, no push
   trigger.

## What DNS and a deploy do not cover

- **OAuth redirect URIs.** `AuthController.callbackUrl` builds the callback from
  the REQUEST host, so no code names a domain — but Google and Facebook both
  reject an unregistered `redirect_uri`. Register
  `https://kinowo.net/auth/{google,facebook}/callback` and
  `https://showtimes.cc/{uk,de,us}/auth/{google,facebook}/callback`
  in each console, or login fails with `redirect_uri_mismatch` on the new hosts
  while continuing to work on the old ones. (The path carries the country now:
  each non-Polish deployment is MOUNTED under its country segment, and
  `callbackUrl` builds the path from the reverse route, so the registered URI has
  to include it.)
- **Universal Links / App Links.** `ios/Kinowo/Kinowo.entitlements` and
  `android/app/src/main/AndroidManifest.xml` name the new hosts, and the
  `.well-known/` files are served by the app itself — but an INSTALLED app keeps
  its old associations until it is updated, and Apple's CDN caches AASA. Deep
  links to the new domains only open the apps from a build shipped after this.
- **Search Console.** `kinowo.net` is a new property. The HTML verification meta
  tag on `/` is kept (removing a verification a property rests on un-verifies
  it), but the new domain now also supports DNS TXT verification, which
  `fly.dev` never could because Fly owned the zone.
- **`kinowo.fly.dev` is RETIRED, not switched off** (reversing the original call
  here, which was to scale it to zero and let the old links die). Fly's edge only
  routes to a running machine, so a scaled-to-zero app cannot redirect — the one
  machine stays up, running the same image with `KINOWO_RETIRED=true` in
  `fly.toml`. That boots `modules.RetiredComponents` rather than the serving
  composition root, which is what makes it cheap: no Mongo client, no change
  stream, no read model, a 512 MB machine instead of 1 GB. `/` and `/{city}/`
  render a notice — the address changed, here is the new one — and everything
  else 301/308s to the same path on `kinowo.net`.

  The notice pages carry the LIVE page's own `<title>`, Open Graph tags and
  canonical, all derived from the city and country rather than the repertoire, so
  a link shared before the move still previews exactly as it did and the indexing
  goes to `kinowo.net`. Deep links redirect rather than showing the notice
  precisely because every social scraper follows a 30x when it scrapes.

  `showtimes-uk.fly.dev` / `showtimes-de.fly.dev` were already at zero machines
  and stay there; they were never public long enough to accumulate links.
