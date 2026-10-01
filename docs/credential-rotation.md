# Rotating a revoked or leaked credential

What breaks when each credential is revoked, which alert says so, and how to put a new one in place.
Commands carry `<placeholders>`; a real value never goes into this file, a commit, or a command line
on a remote host (pipe it over stdin instead -- an argv sits in the remote process list).

Fresh values are kept in the repo-root `.env.local` of the ROOT checkout, or the Touch ID store
(`secrets set movies <KEY>`, value on stdin) -- see `~/projects/secrets/README.md`.

`kubectl` below means the fleet cluster, never the default `rancher-desktop` context:

```
export KUBECONFIG=~/.kube/kinowo-k3s.yaml   # reaches the k3s API through the tunnel on 127.0.0.1:16443
```

## Why this page exists: the 2026-09-27 leaked-key sweep

A leaked-key sweep on 2026-09-27 revoked the operator SSH key that sat on a compromised Mac (the
fleet side is cbbf6556b and d9def8365 in `infra/nix/modules/fleet/accounts.nix`) and, with it, two
credentials no runbook named. Both took something down, and both were recreated by hand from memory:

| credential | outage | alert that now fires |
|---|---|---|
| GitHub token behind `kinowo/ghcr-pull` | ImagePullBackOff on every worker, 13:32-14:07Z | `K3sImagePullFailing` |
| Flux's `movies-gitops` deploy key (`flux-system/flux-git-write`) | image automation stopped deploying, 11:41-13:29Z | `FluxObjectNotReady` |

## 1. `kinowo/ghcr-pull` -- the image pull secret

**What it is.** A `kubernetes.io/dockerconfigjson` Secret in namespace `kinowo`, holding a GitHub
token with `read:packages` ONLY (never the token CI pushes with, never a Fly token -- see
movies-gitops `worker/README.md`). Every `web-<cc>` and `worker-<cc>` Deployment names it under
`imagePullSecrets`. It was created by hand from `.env.local`; it is not in git and must never be.

**What breaks when it is revoked.** Nothing at once: running pods keep their image. The next pod
that has to PULL -- a deploy, a reschedule, a crash on a node without the image cached -- sits in
`ErrImagePull` / `ImagePullBackOff`. On 2026-09-27 that was every worker. The web tier is in the
same position on its next rollout.

**The alert.** `K3sImagePullFailing` (`infra/nix/files/monitoring/rules/k3s.rules`) fires on a
container waiting in `ErrImagePull|ImagePullBackOff`. Confirm the cause is the credential, not the
tag:

```
kubectl -n kinowo get pods | grep -E 'ErrImagePull|ImagePullBackOff'
kubectl -n kinowo describe pod <pod> | grep -iE 'unauthorized|denied|401|403'
```

**Recreate it.**

1. github.com -> Settings -> Developer settings -> Personal access tokens -> Tokens (classic) ->
   Generate new token, scope `read:packages` and nothing else. Record it as `GHCR_PULL_TOKEN` in
   `.env.local` (or `secrets set movies GHCR_PULL_TOKEN`).
2. Replace the Secret. `--dry-run=client -o yaml | kubectl apply -f -` builds the manifest locally
   and replaces it in place; the token is read from the environment, not typed:

   ```
   GHCR_PULL_TOKEN=$(secrets get movies GHCR_PULL_TOKEN)   # or read it from .env.local
   kubectl -n kinowo create secret docker-registry ghcr-pull \
     --docker-server=ghcr.io \
     --docker-username=<github-user> \
     --docker-password="$GHCR_PULL_TOKEN" \
     --dry-run=client -o yaml | kubectl apply -f -
   unset GHCR_PULL_TOKEN
   ```

3. Kick the pods stuck in back-off rather than waiting for the back-off timer (worker downtime is
   fine; roll the web tier, do not delete all its pods at once):

   ```
   kubectl -n kinowo delete pod <pod-in-ImagePullBackOff> ...
   kubectl -n kinowo rollout status deployment/worker-pl     # and each sibling
   ```

4. Revoke the old token on GitHub if the sweep has not already.

**Pending decision.** Both packages (`ghcr.io/pawelkrupinski/movies-web`, `movies-worker`) are
public, so an anonymous pull would work and this Secret may be droppable altogether -- removing the
`imagePullSecrets` entries from movies-gitops `web/` and `worker/` bases and the Secret with them.
Not done: the decision is the owner's. Until it is made, this Secret has to exist and be valid.

## 2. Flux's `movies-gitops` deploy key -- `flux-system/flux-git-write`

**What it is.** A repo-scoped, WRITE-enabled SSH deploy key on `pawelkrupinski/movies-gitops`,
whose private half lives in the Secret `flux-git-write` (keys `identity`, `identity.pub`,
`known_hosts`) in namespace `flux-system`. Only the `movies-write` GitRepository and the image
automation use it -- the automation pushes the new image tag commits to `main`. Its creation is
also described in movies-gitops `image-automation/automation.yaml`, the authority if the two
disagree.

**What breaks when it is revoked.** DEPLOYS stop: a green build is never committed to the gitops
repo, so nothing rolls out. Config reconciliation keeps working, on purpose -- the `movies` source
in `flux/gotk-sync.yaml` reads the public repository over anonymous https and needs no key.

**The alert.** `FluxObjectNotReady` (`infra/nix/files/monitoring/rules/flux.rules`) fires after
30 minutes of a Flux object reporting `Ready=False` -- here the `movies-write` GitRepository and the
ImageUpdateAutomation `movies`. Confirm:

```
flux get sources git -A          # movies-write: "authentication required" / "permission denied"
flux get image update -A
```

**Recreate it.**

1. Generate a new key pair into the Secret, in place of the old one (`flux create secret git`
   replaces an existing Secret of the same name and prints the PUBLIC key):

   ```
   flux create secret git flux-git-write \
     --namespace=flux-system \
     --url=ssh://git@github.com/pawelkrupinski/movies-gitops \
     --ssh-key-algorithm=ed25519
   ```

2. github.com/pawelkrupinski/movies-gitops -> Settings -> Deploy keys -> Add deploy key: paste the
   printed public key, tick **Allow write access**. Delete the revoked key's entry.
3. Reconcile now rather than waiting out the 5-minute interval, and check both report Ready:

   ```
   flux reconcile source git movies-write -n flux-system
   flux reconcile image update movies -n flux-system
   flux get sources git -A && flux get image update -A
   ```

   A missing `known_hosts` reads as "host key verification failed" in the automation's events;
   `flux create secret git` writes it, a hand-built Secret often does not.
