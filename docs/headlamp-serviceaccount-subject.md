# Does Headlamp's ServiceAccount still need cluster read?

**Status: open question, not a known defect.** Raised 2026-09-07 while answering a
different question ("is there an advantage to Headlamp holding a Kubernetes-level
identity?"). Nothing is broken. The task is to establish whether one of two subjects
on a ClusterRoleBinding is still earning its place, and to remove it if not.

**Do not start by removing it.** The manifest says it is deliberate, and the first
version of this write-up wrongly called it unused. Read "What is already known"
before touching anything.

## The thing in question

`movies-gitops/headlamp/rbac.yaml`, the `headlamp-read` ClusterRoleBinding, has two
subjects against the same read-only ClusterRole:

```yaml
subjects:
  # The pod's own identity, used for the backend's cluster-independent calls.
  - kind: ServiceAccount
    name: headlamp
    namespace: monitoring
  - kind: User
    name: "oidc:pawel.krupinski@gmail.com"
```

The ClusterRole grants `get/list/watch` across the common resources — nodes, pods,
deployments, jobs, ingresses, storage classes, pod logs — and deliberately withholds
`secrets`.

The question is only about the **first** subject. The `User` subject is the point of
the whole arrangement and must stay.

## What is already known (measured 2026-09-07, do not re-derive)

**Both subjects resolve to identical permissions.** `kubectl auth can-i` with
`--as=system:serviceaccount:monitoring:headlamp` and with
`--as=oidc:pawel.krupinski@gmail.com` both answer `yes` to `get pods --all-namespaces`
and `no` to `list secrets`. (Impersonation is permitted from the operator kubeconfig,
verified as a positive control — a bare `no` from `auth can-i` can also mean "you may
not ask".)

**Cluster data is served with the USER's token, not the ServiceAccount's.** Hitting
the ClusterIP directly, bypassing Caddy and Google entirely, from monitoring-1:

```
curl http://10.43.165.84:80/clusters/main/api/v1/namespaces   -> 401 Unauthorized
curl http://10.43.165.84:80/config                            -> 200, "auth_type":"oidc"
```

The 401 is the API server's own `Status: Failure`, forwarded. If Headlamp fell back to
its mounted ServiceAccount token for that path, this would have returned namespaces.
So the Kubernetes-level identity is load-bearing: no signed-in person, no data.

**But the ServiceAccount subject is documented as intentional**, for "the backend's
cluster-independent calls". What those calls are, and whether they need cluster-scope
READ rather than no permissions at all, is **not established**. That is the open part.

**The pod does mount the token.** `deployment.yaml` sets `serviceAccountName: headlamp`
and does not set `automountServiceAccountToken: false`.

**Nothing on the network stops a pod reaching it.** The Service is a ClusterIP
(`10.43.165.84:80`) and there are no NetworkPolicies in the `monitoring` namespace —
the only ones on the cluster are in `flux-system`. So any pod k3s schedules can reach
Headlamp directly, without passing Caddy or Google.

## Why it might matter

Today the direct-to-ClusterIP path answers 401, so that reachability costs nothing. The
concern is that this holds by Headlamp's *behaviour* rather than by its *permissions*:
the binding would allow cluster read if anything ever made the backend use its own
token — a config regression that drops the OIDC env, an upstream change to the fallback,
or a future feature that reads with the pod identity. Removing the subject would make
the current behaviour the enforced one.

Against that: the subject is documented as serving real calls, and removing it may break
them in ways a smoke test would not obviously surface.

## What to do

1. **Establish what the backend actually uses the ServiceAccount for.** Headlamp v0.45.0,
   started with `-in-cluster`. Either read upstream's source for the in-cluster
   code path, or watch the API server's audit/logs for requests authenticated as
   `system:serviceaccount:monitoring:headlamp` while the UI is exercised. The second is
   the more direct evidence.
2. **If those calls need no cluster-scope read** (plausible — a backend listing its own
   configured clusters need not read pods), remove the ServiceAccount subject from
   `rbac.yaml`, commit to `movies-gitops`, and let Flux apply it.
3. **If they do need it**, leave the subject and instead record in that comment WHICH
   calls need it, so the next person does not re-open this. That is a complete and
   acceptable outcome for this task.
4. **Verify either way**: sign in at `https://headlamp.kinowo.net` and confirm the pod,
   node and log panels still render; then re-run the direct-to-ClusterIP curl above and
   confirm it still answers 401.

## Traps

- ⚠️ **`kubectl` defaults to the wrong cluster.** The default context is
  `rancher-desktop`, which is local. The fleet kubeconfig is `~/.kube/kinowo-k3s.yaml`
  and reaches the cluster through an already-running tunnel on `127.0.0.1:16443`. Set
  `KUBECONFIG` explicitly.
- ⚠️ **The binding is NOT in this repository.** It lives in `movies-gitops`, fetched by
  `infra/bin/fetch-gitops`. Editing `infra/` will not change it.
- ⚠️ **Two halves gate access, and the manifest says so.** This binding decides what an
  identity may do; the Google project's test-user list decides whether a token is minted
  at all. Removing only one leaves an account that signs in and then 403s.
- ⚠️ **`oidc:` is part of the User name**, not decoration — it is the API server's
  `--oidc-username-prefix`. Do not "tidy" it.
- ⚠️ **Do not remove the `User` subject.** It is what makes a person a Kubernetes subject
  rather than a shared robot, which is the property the whole OIDC arrangement exists for.

## Related

`infra/nix/hosts/monitoring-1/default.nix` wires `fleet.k3sServer.oidc` (the API server
trusting Google) and publishes `headlamp.kinowo.net`. Since 2026-09-07 that vhost also
sits behind a Google sign-in at the proxy (`fleet.googleSso`), which is a second, outer
gate and is unrelated to this binding.
