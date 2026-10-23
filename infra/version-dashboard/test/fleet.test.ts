import { build } from "esbuild";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { CACHE_DIR, INFRA_DIR } from "../src/config.js";
import { createFleetPage, kinowoFleet, MONGO_WARNING, STAGED_PATHS } from "../src/fleet.js";

/** What makes fleetkit's page kinowo's: the page itself is tested in fleetkit. */
describe("kinowo's fleet configuration", () => {
  it("names the staging workflow's own push paths", () => {
    const workflow = readFileSync(join(INFRA_DIR, "..", ".github", "workflows", "nix-stage-closures.yml"), "utf8");
    const push = workflow.slice(workflow.indexOf("  push:"), workflow.indexOf("  workflow_dispatch:"));
    expect([...push.matchAll(/^\s+- '([^']+)'/gm)].map((match) => match[1])).toEqual(["infra/**", ".github/workflows/nix-stage-closures.yml"]);
  });

  it("is root at each host's public address, no jump, Prometheus through monitoring-1, mongo typed, no reboot", () => {
    const config = kinowoFleet({});
    expect(config.infraDir).toBe(INFRA_DIR);
    expect(config.ssh).toEqual({ user: "root" });
    expect(config.roster).toMatchObject({
      role: "fleet.role", env: "fleet.environment", privateAddress: "fleet.privateAddress", publicAddress: "fleet.publicAddress",
      ssh: "public", inputs: ["flake.nix", "flake.lock", "nix"], timeoutMs: 900_000, hive: false,
    });
    expect(config.roster.cacheFile?.startsWith(CACHE_DIR)).toBe(true);
    expect(config.prometheus).toEqual({ via: "ssh", host: "monitoring-1" });
    expect(config.confirmRoles).toEqual([{ role: "mongo", warning: MONGO_WARNING }]);
    expect(config.reboot).toEqual({ tool: `${INFRA_DIR}/bin/rolling-reboot` });
    expect(config.forges.map((forge) => [forge.remote, "stagingRuns" in forge])).toEqual([["forgejo", false]]);
    expect(STAGED_PATHS).toEqual([".", "../.github/workflows/nix-stage-closures.yml"]);
    expect(kinowoFleet({ KINOWO_FLEET_SSH_USER: "ops" }).ssh.user).toBe("ops");
  });

  it("reboots through infra/bin/rolling-reboot, which stops between hosts and runs fleetkit's tool", () => {
    const wrapper = readFileSync(join(INFRA_DIR, "bin", "rolling-reboot"), "utf8");
    // fleetkit's jobs.ts starts a run only when the tool says it stops between hosts.
    for (const marker of ["stop requested", "signal.SIGTERM"]) expect(wrapper).toContain(marker);
    expect(wrapper).toContain("node_modules/fleetkit/rolling-reboot/bin/rolling-reboot");
    expect(existsSync(join(INFRA_DIR, "version-dashboard", "node_modules", "fleetkit", "rolling-reboot", "bin", "rolling-reboot"))).toBe(true);
  });

  it("serves fleetkit's browser half and stylesheet, which bundle, and re-polls on wake", async () => {
    const listeners: (() => void)[] = [];
    const page = createFleetPage({ onWake: (listener) => listeners.push(listener) });
    expect([page.name, page.path]).toEqual(["fleet", "/nixos"]);
    expect(listeners).toHaveLength(1);
    expect(existsSync(page.stylesheet)).toBe(true);
    const bundled = await build({ entryPoints: [page.script], bundle: true, format: "esm", write: false });
    expect(bundled.outputFiles[0]?.text).toContain("fleet-apply");
  });
});
