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
   kinowo.net        A  204.168.140.213     # k3s-worker-1 — the web pods
   www.kinowo.net    A  204.168.140.213
   grafana.kinowo.net A 2.28.52.210         # monitoring-1 — NOT the same host
   showtimes.cc      A  204.168.140.213
   www.showtimes.cc  A  204.168.140.213
   uk.showtimes.cc   A  204.168.140.213
   de.showtimes.cc   A  204.168.140.213
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

   ⚠️ **Check the new GHCR package's visibility after that first build.** A package GitHub
   Actions creates for the first time is **private**, while `movies-worker` is public. The
   cluster's `ghcr-pull` secret is a read:packages PAT for the same account, so a private
   package *should* pull — but if the pods sit in `ImagePullBackOff` with a 403, that is the
   difference, and the fix is to make `movies-web` public like its sibling.

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

7. **Retire the Fly web apps.** Every leg in `.github/workflows/deploy.yml` is
   already `enabled: false`, which stops future deploys but does not stop the
   running machines. `flyctl scale count 0 -a kinowo` (and `showtimes-uk` /
   `showtimes-de`, already at zero). Scale to zero rather than `machines stop`:
   `auto_start_machines = true` means any inbound request boots a stopped web
   machine and it never stops again.

## What DNS and a deploy do not cover

- **OAuth redirect URIs.** `AuthController.callbackUrl` builds the callback from
  the REQUEST host, so no code names a domain — but Google and Facebook both
  reject an unregistered `redirect_uri`. Register
  `https://{kinowo.net,uk.showtimes.cc,de.showtimes.cc}/auth/{google,facebook}/callback`
  in each console, or login fails with `redirect_uri_mismatch` on the new hosts
  while continuing to work on the old ones.
- **Universal Links / App Links.** `ios/Kinowo/Kinowo.entitlements` and
  `android/app/src/main/AndroidManifest.xml` name the new hosts, and the
  `.well-known/` files are served by the app itself — but an INSTALLED app keeps
  its old associations until it is updated, and Apple's CDN caches AASA. Deep
  links to the new domains only open the apps from a build shipped after this.
- **Search Console.** `kinowo.net` is a new property. The HTML verification meta
  tag on `/` is kept (removing a verification a property rests on un-verifies
  it), but the new domain now also supports DNS TXT verification, which
  `fly.dev` never could because Fly owned the zone.
- **`kinowo.fly.dev` does not redirect anywhere.** Fly's edge only routes to a
  running machine, so once the app is scaled to zero the old host simply stops
  answering. Keeping one machine alive purely to serve 301s is the only way to
  preserve those links; it was judged not worth the cost.
