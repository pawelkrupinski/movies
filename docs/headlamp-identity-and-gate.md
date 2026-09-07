# Headlamp's identity, and where the gate actually is

**Status: settled 2026-09-07.** Sign-in for all three published names is the fleet's
oauth2-proxy door. Headlamp no longer authenticates anyone itself, answers the API server
with one read-only ServiceAccount, and is reachable only from Caddy and kubelet.

This page changed direction twice in one day, and both turns are kept because the second
is unreadable without the first. If you are here to revert something, read *Why the
subject went and came back* before touching either half.

## The arrangement now

| | door: who are you | inside: what may you do |
|---|---|---|
| `grafana.kinowo.net` | oauth2-proxy → Google | `auth.proxy` on `X-Auth-Request-Email` → per-user Grafana accounts |
| `logs.kinowo.net` | oauth2-proxy → Google | nothing; authorization *is* the `/select`-only path list |
| `headlamp.kinowo.net` | oauth2-proxy → Google | nothing; one shared read-only ServiceAccount, no secrets, no writes |

One oauth2-proxy, one callback at `auth.kinowo.net`, one cookie on `.kinowo.net`, one
address allow-list (`fleet.googleSso.allowedEmails`). Authentication is unified.
Authorization is not, and cannot be — the three applications have genuinely different
models, and flattening them would mean inventing one where none exists.

## Why the subject went and came back

**Removed in the morning.** While Headlamp did its own Google OIDC, the person's token
carried every API call and the backend needed no identity of its own. Measured: with
`-in-cluster` alone it makes exactly ONE call as the ServiceAccount —
`get configmaps kube-system/kubeadm-config` (`kubeconfig.go:1170`), cosmetic, to name the
context — and that call already failed on k3s, which writes no such ConfigMap. Any error
there, Forbidden as much as NotFound, falls back to `"main"` (`:1209-1213`, `:58`), which
is the context name the UI had been using all along. The binding was the only thing making
the mounted token a cluster-wide read credential, and it was one: a pod running as that
ServiceAccount pulled a full `NamespaceList` through Headlamp's own proxy, and 403'd once
the subject was gone.

**Restored in the evening,** because the premise expired. With OIDC removed there is no
user token to forward, so the backend needs `-unsafe-use-service-account-token` or every
panel 401s — and with that flag the identity it was safe to withhold is the only identity
left. This is a knowing reversal, not a correction.

**What it costs, stated rather than glossed:** Headlamp is one shared robot to Kubernetes.
The API server's audit log records `system:serviceaccount:monitoring:headlamp`, never a
person. Everyone admitted by the door sees the same thing. The grant — read-only,
`secrets` absent, no write verbs — is the entire bound, which is why it must stay narrow.

## The exposure, and what closes it

With that flag the backend answers with its own credential when a request carries no
`Authorization` header. So an unauthenticated request straight to the ClusterIP, which
used to answer 401, is **answered**. The Service is a ClusterIP and Caddy's door guards
only the public path, so without something else every pod k3s schedules could read the
cluster without signing in to anything — and the scraper workloads fetch hostile
third-party pages for a living.

`networkpolicy.yaml` closes it. NetworkPolicy **is** enforced here — verified with a
deny-all in a throwaway namespace, with a positive control showing the same probe
succeeded without the policy.

⚠️ **Every allowed address was measured, and all three obvious guesses are wrong:**

- Caddy runs in monitoring-1's **host** network namespace and reaches the pod cross-node
  over flannel, arriving as **`10.42.0.0`** — that node's flannel.1 gateway, **not** the
  node IP `10.20.0.11`. A rule written against the node IP 502s the public site.
- kubelet probes a pod on its **own** node over the cni0 bridge, arriving as
  **`10.42.2.1`** — a third kind of address. Headlamp has liveness *and* readiness probes;
  block them and the pod restart-loops.
- Neither `.0` nor `.1` is assignable to a pod (allocation starts at `.2`), so allowing
  them admits no workload.

Measured with a `hostNetwork` pod on monitoring-1 and an nginx pod carrying a probe, both
reading the source address out of the access log. Re-measure that way if the CNI or the
subnet allocation ever changes.

## Verified end to end (2026-09-07)

| probe | before | after |
|---|---|---|
| Caddy's vantage, no credential | 401 | **200 + `NamespaceList`** |
| arbitrary pod → ClusterIP | 401 (and `/config` 200) | **connection refused on every path** |
| `headlamp.kinowo.net` anonymous | 200 | **302 to Google** |
| `kubectl get nodes` | works | works (client cert, untouched) |
| pod | Ready | Ready, 0 restarts |

## Traps

- ⚠️ **`requireGoogleLogin` on the Headlamp vhost is load-bearing in a way the other two
  are not.** Delete it and Headlamp does not merely become public — it serves the whole
  read-only cluster view to anyone who asks, with nothing behind it. Grafana would still
  demand a login; VictoriaLogs would still expose only `/select`.
- ⚠️ **Restoring per-person identity takes the API server flag FIRST.** Putting a `User`
  subject back in `rbac.yaml` achieves nothing while no issuer is trusted — the name
  matches nobody. `fleet.k3sServer.oidc` was deleted from
  `infra/nix/hosts/monitoring-1/default.nix`, and the option itself from
  `modules/roles/k3s-server.nix`. If it returns, the `oidc:` **username prefix returns
  with it**: without one, a token whose email equals a client-certificate CN or a
  ServiceAccount name authenticates AS that subject.
- ⚠️ **Unmounting the token is not a tidier version of anything.**
  `rest.InClusterConfig()` needs it to exist; `automountServiceAccountToken: false` stops
  the backend booting.
- ⚠️ **`k3s.service` is in `neverDisturbUnits`,** so any change to the API server's flags
  is refused SILENTLY by the auto-applier and needs a hand switch on monitoring-1.
- ⚠️ **`kubectl` was never at risk** and still isn't: `~/.kube/kinowo-k3s.yaml` is a
  `system:masters` client certificate, not OIDC. Check that before touching issuer trust
  on any fleet where it might not be true.
- ⚠️ **The binding is NOT in this repository.** It lives in `movies-gitops`, which Flux
  applies from `./headlamp` with no kustomization file — so a new YAML there is picked up
  automatically.
- ⚠️ **The `headlamp-oidc` Secret is now referenced by nothing.** It was left in place
  deliberately rather than deleted; removing it is a separate, destructive decision.

## Related

`infra/nix/modules/roles/google-sso.nix` defines the door; `fleet.googleSso` on
monitoring-1 is its only call site, and now the only consumer of the Google client that
used to be Headlamp's. Grafana's `auth.proxy` trusts `X-Auth-Request-Email` with an IP
whitelist as its whole defence — worth re-reading in this light, since it is now the
weakest of the three.
