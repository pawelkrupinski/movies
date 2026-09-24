# Heap dumps: where they are, what bounds them, how to fetch one

Every web and worker JVM runs with `-XX:+HeapDumpOnOutOfMemoryError -XX:+ExitOnOutOfMemoryError`.
On a heap OOM it writes a dump, exits, and the kubelet restarts it. The dump survives the pod
being replaced.

## Where

| | |
| --- | --- |
| Node | `k3s-worker-1` (`root@2.28.47.31`); every kinowo pod is pinned there |
| Host path | `/var/lib/kinowo/heapdumps/<app>-<country>/`, e.g. `web-pl/`, `worker-us/` |
| In the pod | `/data/heapdumps` (a hostPath, mounted through a per-app-country `subPathExpr`) |
| File name | `<app>-<country>_<pod>_<JVM start, UTC>.hprof`, e.g. `web-pl_web-pl-7c9f8d-x2k4_20260923T190112Z.hprof` |
| Also | `wedge-<millis>.hprof` from the worker's liveness watchdog; `oom-<stamp>-pid1.hprof` is a renamed legacy `java_pid1.hprof` |

The timestamp is when that JVM **started**, not when it died. The file's mtime is when it died.
After about 10 minutes the node timer gzips a dump in place (`.hprof.gz`, keeping the mtime).

Before 2026-09-24 the web tier kept dumps on an emptyDir, so a dump was lost as soon as the pod
was replaced. That is how web-pl's dump from its 2026-09-23 OOM was lost. The workers kept theirs
on the `worker-data-<cc>` PVC under `heapdumps/`. The hostPath now mounts over that directory, so
older worker dumps are still on the PVC at
`/var/lib/rancher/k3s/storage/pvc-*_kinowo_worker-data-<cc>/heapdumps/`.

## What bounds them

`infra/nix/files/heap-dumps.sh` sets the budget, and `infra/test/test_heap_dumps.sh` pins it:

- **3 dumps and 4 GiB per app-country**, oldest (by mtime) deleted first. Three dumps from
  worker-us, the worst case at about 1.4 GB each, still fit.
- **16 GiB in total** across all ten directories, again oldest first. The node has a 150G disk
  with about 129G free, so the dumps can never be what fills it.
- A file written in the **last 10 minutes is never deleted, renamed or compressed**, because it
  may be a dump still being written. It still counts toward the limits, so older files are deleted
  first.

Two things enforce the budget:

1. **Every container at start**, before the JVM runs. It prunes only its own directory and
   logs `heap-dumps: dir=/data/heapdumps files=N bytes=B ...`, which you can find in
   VictoriaLogs or `kubectl logs`.
2. **The `kinowo-heap-dumps.timer` on the node**, every 15 minutes. It compresses settled dumps,
   prunes per directory, then prunes the total across directories.

## Rolling it out, and in what order

Three independent pieces, and any order is safe:

- **Image** (app repo `main`): the boot prune checks `/proc/self/mountinfo`. If `/data/heapdumps`
  is not a mount of its own, the manifests are older than the hostPath, so it keeps **one**
  dump. That keeps the pod's sized emptyDir or PVC from overflowing.
- **Manifests** (movies-gitops `main`): the hostPath. `DirectoryOrCreate` makes it `root:root
  0755` if the node has not yet, and the JVM runs as root, so writes work either way.
- **Node** (Nix): the timer, metrics and tmpfiles. CI stages the closure. Because the units are
  new, auto-apply refuses the first switch, so apply it by hand:

```
ssh root@2.28.47.31 'staged=$(readlink -f /nix/var/nix/gcroots/auto/nixdeploy-staged) && nix-env -p /nix/var/nix/profiles/system --set "$staged" && "$staged"/bin/switch-to-configuration switch'
```

Every order keeps the dumps within a budget. The recommended order is node, then manifests,
then image, so the metrics and the `HeapDumpWritten` alert are live before the first dump lands
on the hostPath.

The `HeapDumpWritten` alert (jvm-heap.rules, sent by email and Telegram) fires when a
directory's newest dump moves forward.

## Seeing what is there

Prometheus (node_exporter textfile, from the timer):

```
kinowo_heapdumps_files{dir="web-pl"}                  # dumps kept
kinowo_heapdumps_bytes{dir="web-pl"}                  # bytes kept
kinowo_heapdumps_newest_timestamp_seconds{dir=...}    # changes when a new dump lands
kinowo_heapdumps_budget_bytes                         # the total cap
kinowo_heapdumps_prune_last_success_timestamp_seconds # the timer is alive
```

On the node:

```
ssh root@2.28.47.31 'ls -la --time-style=+%FT%TZ /var/lib/kinowo/heapdumps/*/'
ssh root@2.28.47.31 'systemctl status kinowo-heap-dumps.timer; journalctl -u kinowo-heap-dumps -n 50'
```

From a running pod:

```
kubectl -n kinowo exec deploy/web-pl -- ls -la /data/heapdumps
```

## Fetching one

Copy the file straight off the node. This is the fastest way and needs no running pod:

```
scp root@2.28.47.31:/var/lib/kinowo/heapdumps/web-pl/<file>.hprof.gz .
gunzip <file>.hprof.gz
```

Through the pod, if you have only kubectl access. `kubectl cp` needs `tar` in the image, and
the Temurin image has it:

```
kubectl -n kinowo cp web-pl-<pod>:/data/heapdumps/<file>.hprof.gz ./<file>.hprof.gz
```

Take a copy **before** you debug by restarting things. A crash loop keeps only the 3 newest dumps.

## Reading one

The images are a JRE, so they have no `jcmd`, `jmap` or `jhat`. Read the dump off the box with
Eclipse MAT or VisualVM, or with the streaming Python class histogram described in the
`reference_jvm_heap_is_the_binding_constraint` memory. That memory also has the attach protocol
for taking a live histogram from a pod.

## Changing the budget

Edit the defaults at the top of `infra/nix/files/heap-dumps.sh` and the cases in
`infra/test/test_heap_dumps.sh`. That one file reaches both callers:

- the images, through `build.sbt`'s `heapDumpScript`. The web and worker path filters in
  `main.yml` include the file, so a change redeploys both tiers.
- the node, through `modules/fleet/heap-dumps.nix`, which auto-apply stages.

The budget is deliberately not a ConfigMap value, for the reason given in the Dockerfile.
