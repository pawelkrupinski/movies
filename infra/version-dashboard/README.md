# kinowo dashboard

http://127.0.0.1:8788 — two pages from one process:

- **/nixos** — what every NixOS host in the fleet is running (roster from the flake, running state
  from Prometheus over ssh to monitoring-1), and "Bring to latest…" to activate the closure CI
  already staged (check → switch; the mongo role needs its name typed).
- **/mobile** — what's in `ios/` and `android/` that neither store has shipped yet, diffed from
  each platform's own live version.

TypeScript (Fastify + tsx, Node ≥ 24). State is read in the background and pushed to open tabs
over server-sent events; nothing builds on request. Same shape as
`~/bitcashier/version-dashboard` (port 8787).

```
npm ci
npm run service install   # launchd: com.kinowo.nixos-dashboard + its /healthz watchdog
npm run restart           # by hand: lint + test, then SIGTERM; running switches drain first
npm run service status    # launchd state + /healthz
npm run service logs      # ~/.kinowo-dashboard-logs/
npm run once              # print the fleet once as text and exit (never serves, never acts)
npm test                  # vitest; setup refuses every real command and network call
npm run lint              # tsc + type-aware oxlint
```

**Autodeploy.** launchd runs it from the root checkout with `KINOWO_AUTODEPLOY=1`, so merging to
main is the deploy (like the events app): the server notices its own source change, runs `npm ci` if
the lockfile moved, type-checks, and only then drains running checks/switches and exits for
KeepAlive to start the new code. A change that does not type-check is logged and the old code keeps
serving. Never `tsx watch` in service: a watch restart kills a switch mid-ssh.
