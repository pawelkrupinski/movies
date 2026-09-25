import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Autodeploy, isSource } from "../src/autodeploy.js";
import { setExecutor, type CommandResult, type Executor } from "../src/exec.js";

let restore: Executor | null = null;
let ran: string[] = [];
let results: Record<string, Partial<CommandResult>> = {};

beforeEach(() => {
  vi.useFakeTimers();
  ran = [];
  results = {};
  restore = setExecutor(async (argv) => {
    const name = (argv[0] ?? "").split("/").pop() ?? "";
    ran.push(`${name} ${argv.slice(1).join(" ")}`.trim());
    return { code: 0, stdout: "", stderr: "", timedOut: false, ...results[name] };
  });
});
afterEach(() => {
  vi.useRealTimers();
  if (restore) setExecutor(restore);
});

function deployer(lock = () => "lock-1") {
  const restarts: string[] = [];
  const logs: string[] = [];
  let notify: (path: string) => void = () => {};
  const autodeploy = new Autodeploy({
    root: "/dash",
    restart: (reason) => restarts.push(reason),
    watchFn: (_dir, onChange) => ((notify = onChange), { close: () => {} }),
    readLock: lock,
    settleMs: 2_000,
    log: (line) => logs.push(line),
  });
  autodeploy.start();
  const settle = async () => {
    await vi.advanceTimersByTimeAsync(2_000);
    await vi.runAllTimersAsync();
  };
  return { restarts, logs, change: (path: string) => notify(path), settle };
}

describe("what counts as a change to the running server", () => {
  it("is its source and its dependencies, not its tests, docs or plists", () => {
    expect(["src/fleet/view.ts", "web/styles.css", "src/fleet/scripts/check.sh", "package-lock.json", "tsconfig.json"].every(isSource)).toBe(true);
    expect(["test/fleet/view.test.ts", "README.md", "com.kinowo.nixos-dashboard.plist", "node_modules/x/index.ts", "src/.view.ts.swp"].some(isSource)).toBe(false);
  });
});

describe("autodeploy", () => {
  it("type-checks a merged change, then restarts onto it -- once for a whole burst", async () => {
    const { restarts, change, settle } = deployer();
    change("src/fleet/view.ts");
    await vi.advanceTimersByTimeAsync(1_500);
    change("web/fleet.ts");
    change("test/fleet/view.test.ts");
    await settle();
    expect(ran).toEqual(["tsc -p tsconfig.json"]);
    expect(restarts).toEqual(["autodeploy (src/fleet/view.ts, web/fleet.ts)"]);
  });

  it("keeps serving the old code when the change does not type-check", async () => {
    results.tsc = { code: 2, stdout: "src/fleet/view.ts(1,1): error TS1005" };
    const { restarts, logs, change, settle } = deployer();
    change("src/fleet/view.ts");
    await settle();
    expect(restarts).toEqual([]);
    expect(logs.join("\n")).toContain("NOT deploying src/fleet/view.ts -- it does not type-check");
    expect(logs.join("\n")).toContain("TS1005");
  });

  it("installs the new dependencies first when the lockfile moved", async () => {
    let lock = "lock-1";
    const { restarts, change, settle } = deployer(() => lock);
    lock = "lock-2";
    change("package-lock.json");
    await settle();
    expect(ran).toEqual(["npm ci --no-audit --no-fund", "tsc -p tsconfig.json"]);
    expect(restarts).toHaveLength(1);
  });

  it("does not restart onto new code whose dependencies failed to install", async () => {
    let lock = "lock-1";
    results.npm = { code: 1, stderr: "ERESOLVE" };
    const { restarts, logs, change, settle } = deployer(() => lock);
    lock = "lock-2";
    change("package.json");
    await settle();
    expect(ran).toEqual(["npm ci --no-audit --no-fund"]);
    expect(restarts).toEqual([]);
    expect(logs.join("\n")).toContain("npm ci failed");
  });

  it("ignores a change to anything the server is not built from", async () => {
    const { restarts, change, settle } = deployer();
    change("test/shell.test.ts");
    change("README.md");
    await settle();
    expect(ran).toEqual([]);
    expect(restarts).toEqual([]);
  });
});
