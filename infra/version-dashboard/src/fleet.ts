/**
 * kinowo's /nixos page: fleetkit's fleet page (~/bitcashier/fleetkit, shared with bitcashier's
 * dashboard), configured for this fleet. What is here is only what makes it THIS fleet.
 */
import { join } from "node:path";
import { localGitForge } from "fleetkit/fleet/assemble";
import type { FleetConfig } from "fleetkit/fleet/config";
import { createFleetPage as createPage, type FleetPage, type WakeSource } from "fleetkit/fleet/page";
import { CACHE_DIR, INFRA_DIR } from "./config.js";

/** Keyed on the role, so a second database host inherits it by declaring `role = "mongo"`. */
export const MONGO_WARNING = "This is the PRODUCTION DATABASE. Activating can restart mongod, and the web tier's change "
  + "streams stop with it — a dropped change stream does not reconnect by itself, so the site would serve stale "
  + "showtimes until the app is restarted.";

/** nix-stage-closures.yml's push `paths`, relative to infra/. */
export const STAGED_PATHS = [".", "../.github/workflows/nix-stage-closures.yml"] as const;

export function kinowoFleet(env: NodeJS.ProcessEnv = process.env, infraDir = INFRA_DIR): FleetConfig {
  return {
    infraDir,
    // NO FORGE TOKEN HERE (this page runs outside the vault), so the checkout is the forge: main is
    // its cached origin/main, never fetched. It cannot say how CI staged a closure. ONLY THE
    // COMMITS nix-stage-closures builds for count (its push paths): the rest are application
    // commits no closure is built for, and counting them read a current fleet as "33 behind".
    forges: [localGitForge(infraDir, { remote: "origin", paths: STAGED_PATHS })],
    // ROOT, at the PUBLIC address: this fleet has no VPN and no jump host, and root ssh is the only
    // access there is (bitcashier's hosts are the other way round: an admin over sudo, via a jump).
    ssh: { user: env.KINOWO_FLEET_SSH_USER ?? "root" },
    roster: {
      role: "fleet.role",
      env: "fleet.environment",
      privateAddress: "fleet.privateAddress",
      publicAddress: "fleet.publicAddress",
      ssh: "public",
      // `nix eval .` in infra/ resolves to this whole repository; keyed on its HEAD, every
      // application commit would re-evaluate the fleet.
      inputs: ["flake.nix", "flake.lock", "nix"],
      nixArgs: ["--extra-experimental-features", "nix-command flakes"],
      // Generous: paid rarely, off the critical path, and this laptop's load routinely kills 240s.
      timeoutMs: 900_000,
      retryMs: 300_000,
      hive: false,
      // Outside the repository: a state file inside it would be an uncommitted change.
      cacheFile: join(CACHE_DIR, "fleet-roster.json"),
    },
    // Prometheus binds monitoring-1's PRIVATE address only (its UI has no auth) and nothing routes
    // there from a laptop: ssh to its public address, curl from there.
    prometheus: { via: "ssh", host: "monitoring-1" },
    // infra/bin/rolling-reboot: fleetkit's tool (this package's pinned copy) with profile movies.
    reboot: { tool: join(infraDir, "bin", "rolling-reboot") },
    confirmRoles: [{ role: "mongo", warning: MONGO_WARNING }],
  };
}

export function createFleetPage(wake: WakeSource): FleetPage {
  return createPage(kinowoFleet(), { wake });
}
