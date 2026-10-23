import { build } from "esbuild";
import { existsSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { CACHE_DIR, INFRA_DIR } from "../src/config.js";
import { createFleetPage, kinowoFleet, MONGO_WARNING } from "../src/fleet.js";

/** What makes fleetkit's page kinowo's: the page itself is tested in fleetkit. */
describe("kinowo's fleet configuration", () => {
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
    expect(config.reboot).toBeNull();
    expect(config.forges.map((forge) => [forge.remote, "stagingRuns" in forge])).toEqual([["forgejo", false]]);
    expect(kinowoFleet({ KINOWO_FLEET_SSH_USER: "ops" }).ssh.user).toBe("ops");
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
