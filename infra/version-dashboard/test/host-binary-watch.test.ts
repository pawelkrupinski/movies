import { describe, expect, it } from "vitest";
import { BINARY_REPLACED_MESSAGE, explainUnlockFailure, watchOwnBinary } from "../src/host/binary-watch.js";

function harness(present: Set<string>) {
  const checks: Array<() => void> = [];
  const logs: string[] = [];
  let exits = 0;
  watchOwnBinary({
    executable: "/cellar/node/1/bin/node",
    exists: (path) => present.has(path),
    every: (_ms, check) => {
      checks.push(check);
      return () => undefined;
    },
    onGone: () => void (exits += 1),
    log: (line) => logs.push(line),
  });
  return { tick: () => checks.forEach((check) => check()), logs, exits: () => exits };
}

describe("watchOwnBinary", () => {
  it("keeps running while the node binary exists", () => {
    const present = new Set(["/cellar/node/1/bin/node"]);
    const { tick, logs, exits } = harness(present);
    tick();
    tick();
    expect(exits()).toBe(0);
    expect(logs).toEqual([]);
  });

  it("logs one line and requests the restart, once, when the binary is gone", () => {
    const { tick, logs, exits } = harness(new Set());
    tick();
    tick();
    expect(exits()).toBe(1);
    expect(logs).toHaveLength(1);
  });
});

describe("explainUnlockFailure", () => {
  it("names the replaced binary when the vault cannot identify the caller and the binary is gone", () => {
    expect(explainUnlockFailure("could not identify caller process", true)).toBe(BINARY_REPLACED_MESSAGE);
  });
  it("leaves any other failure as it was", () => {
    expect(explainUnlockFailure("could not identify caller process", false)).toBe("could not identify caller process");
    expect(explainUnlockFailure("vault is locked", true)).toBe("vault is locked");
  });
});
