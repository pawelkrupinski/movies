# Headlamp's ServiceAccount and cluster read

**Status: settled 2026-09-07. The ServiceAccount subject has been removed**, in
`movies-gitops` commit `2c27bbb`, and Flux has applied it. Nothing here is open. This
page stays because the *reason* is worth more than the diff: the subject carried a
comment claiming it was needed, and the only way to retire that claim for good is to
write down what was measured instead.

The question was: `movies-gitops/headlamp/rbac.yaml`'s `headlamp-read`
ClusterRoleBinding named two subjects against one read-only ClusterRole — the pod's own
ServiceAccount, and the `oidc:`-prefixed User. The `User` subject is the point of the
whole arrangement and is untouched. The question was only ever about the first.

An earlier draft of this page called the ServiceAccount subject unused. That was wrong
in an instructive way, so it is worth stating: proving the *cluster-data* path uses the
person's token does not prove the backend uses no token of its own. Those are different
claims, and only the second one licenses removal.

## The answer

**The backend makes exactly one call as the ServiceAccount, and it is not a cluster-scope
read.** Headlamp v0.45.0, started with `-in-cluster` and with neither
`-unsafe-use-service-account-token` nor `-enable-cluster-inventory` set:

- `deriveInClusterName` (`backend/pkg/kubeconfig/kubeconfig.go:1170`) does
  `ConfigMaps("kube-system").Get(ctx, "kubeadm-config")` on a clientset built from the
  raw `rest.InClusterConfig()` — the real mounted token. It is reached from
  `GetInClusterContext` (`:1233`) only when no context name is configured, which is our
  case. Its purpose is cosmetic: to name the cluster after whatever kubeadm or KiND
  wrote at creation time.
- Every other API call is carried by the token the browser sent. The in-cluster context
  is built with an **empty** `AuthInfo` (`:1279`); the SA's `BearerTokenFile` is copied
  in only under the `unsafeUseServiceAccountToken` flag (`:1281-1283`). So
  `SetupProxy` adds no auth round-tripper and the proxy forwards the incoming
  `Authorization` header or none at all — which is why an unauthenticated request
  straight to the ClusterIP answers 401 rather than serving the cluster.
- The `SelfSubjectAccessReview` / port-forward / node-drain paths all read the bearer
  token off the incoming request, not off the pod.

**And that one call already failed, and always had.** k3s writes no `kubeadm-config`, so
it returned NotFound. `deriveInClusterContextName` maps **any** error — NotFound and
Forbidden alike — to `""` (`:1209-1213`), and `""` becomes `DefaultInClusterContextName`,
`"main"` (`:58`). `main` is the context name the UI has been using all along. Denying the
call changes one log line's error string and nothing else.

## What it was actually costing

Not hypothetical, and worse than the earlier draft supposed. Before the change, a pod
scheduled with `serviceAccountName: headlamp` — carrying nothing but its own mounted
token — got **200 and a full `NamespaceList`** through Headlamp's own proxy. The binding,
not Headlamp's behaviour, was what made that token a cluster-wide read credential. Since
the Service is a ClusterIP with no NetworkPolicy in front of it, every pod on the cluster
could reach that path.

After the change the same probe gets **403 Forbidden**. The 401 on the unauthenticated
path is now enforced by permissions rather than resting on the backend declining to fall
back.

## Evidence (measured 2026-09-07, do not re-derive)

Probes ran from a throwaway pod inside `monitoring` using its own mounted token, so no
credential passed through a command line or a laptop.

| probe | before | after |
|---|---|---|
| proxy `/clusters/main/api/v1/namespaces`, headlamp SA token | 200 + `NamespaceList` | **403 Forbidden** |
| same, a control SA bound to `headlamp-read` | — | **200 + `NamespaceList`** |
| same, no token at all | 401 | 401 |
| `/config` | 200 | 200 |
| `auth can-i get pods -A --as=…:headlamp` | yes | **no** |
| `auth can-i get pods -A --as=oidc:pawel.krupinski@gmail.com` | yes | yes |
| `auth can-i list secrets -A --as=oidc:…` | no | no |

The control SA row is the one that replaces signing in with a browser: it shows the
proxy still serves cluster data to any identity the binding permits, so the User subject's
panels are unaffected. `auth can-i` was run against an unbound ServiceAccount as a
positive control, since a bare `no` can also mean "you may not ask".

After a `rollout restart`, the startup log shows the predicted change and nothing else:

```
kubeconfig.go:1209  "deriving in-cluster context name from kubeadm-config"
  error: configmaps "kubeadm-config" is forbidden: User
         "system:serviceaccount:monitoring:headlamp" cannot get resource "configmaps"
kubeconfig.go:482   "Proxy setup"  context=main
```

Same line, same `info` level, `context=main` unchanged. No warn or error since.

## Traps

- ⚠️ **Unmounting the token is NOT the tidier version of this.**
  `rest.InClusterConfig()` needs the token to exist, so
  `automountServiceAccountToken: false` would stop the backend starting at all. The pod
  still mounts it; it simply no longer buys anything.
- ⚠️ **`kubectl` defaults to the wrong cluster.** The default context is
  `rancher-desktop`, which is local. The fleet kubeconfig is `~/.kube/kinowo-k3s.yaml`,
  reaching the cluster through a tunnel on `127.0.0.1:16443`. Set `KUBECONFIG` explicitly.
- ⚠️ **The binding is NOT in this repository.** It lives in `movies-gitops`, fetched by
  `infra/bin/fetch-gitops`. Editing `infra/` will not change it.
- ⚠️ **Two halves gate access.** This binding decides what an identity may do; the Google
  project's test-user list decides whether a token is minted at all. Removing only one
  leaves an account that signs in and then 403s.
- ⚠️ **`oidc:` is part of the User name**, not decoration — it is the API server's
  `--oidc-username-prefix`. Do not "tidy" it.
- ⚠️ **Do not remove the `User` subject.** It is what makes a person a Kubernetes subject
  rather than a shared robot, which is the property the whole OIDC arrangement exists for.
- ⚠️ **The two flags are what would reopen this.** Setting
  `-unsafe-use-service-account-token` (or `-enable-cluster-inventory`, which watches
  `ClusterProfile` CRs on the hub config) makes the backend use the pod's identity for
  real work. Either would need the subject back — deliberately, and with the reason
  written down.

## Related

`infra/nix/hosts/monitoring-1/default.nix` wires `fleet.k3sServer.oidc` (the API server
trusting Google) and publishes `headlamp.kinowo.net`. Since 2026-09-07 that vhost also
sits behind a Google sign-in at the proxy (`fleet.googleSso`) — a second, outer gate,
unrelated to this binding.
