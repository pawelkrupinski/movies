import { describe, expect, it } from "vitest";
import { compareVersions, decideVersion, nextPatch } from "../../src/mobile-release/version.js";

describe("version order", () => {
  it("compares numerically, missing components as 0", () => {
    expect(compareVersions("2.0.10", "2.0.9")).toBeGreaterThan(0);
    expect(compareVersions("2.0", "2.0.0")).toBe(0);
    expect(compareVersions("1.9.9", "2.0")).toBeLessThan(0);
  });

  it("bumps the patch, padding a short version", () => {
    expect(nextPatch("2.0.9")).toBe("2.0.10");
    expect(nextPatch("2.1")).toBe("2.1.1");
  });
});

describe("decideVersion", () => {
  it("ships main's version when neither store has released it", () => {
    expect(decideVersion({ onMain: "2.0.10", released: ["2.0.9", "2.0.9"], unreleased: [] })).toMatchObject({ version: "2.0.10", bump: false });
  });

  it("bumps past a version either store has released", () => {
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.9", "2.0.8"], unreleased: [] })).toMatchObject({ version: "2.0.10", bump: true });
    // Android alone having it is enough: one number must not mean two apps.
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.8", "2.0.9"], unreleased: [] })).toMatchObject({ version: "2.0.10", bump: true });
  });

  it("picks up an unreleased store version newer than everything released", () => {
    const decision = decideVersion({ onMain: "2.0.9", released: ["2.0.9"], unreleased: ["2.0.11"] });
    expect(decision).toMatchObject({ version: "2.0.11", bump: true });
    expect(decision.reason).toContain("picking up");
  });

  it("does not pick up an unreleased version older than a released one", () => {
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.9"], unreleased: ["2.0.3"] })).toMatchObject({ version: "2.0.10" });
  });

  it("bumps past the newest version anyone has named", () => {
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.12"], unreleased: [] })).toMatchObject({ version: "2.0.13", bump: true });
  });

  it("honours a requested version, and refuses one that cannot ship", () => {
    const evidence = { onMain: "2.0.9", released: ["2.0.9"], unreleased: [] };
    expect(decideVersion(evidence, "2.1.0")).toMatchObject({ version: "2.1.0", bump: true });
    expect(() => decideVersion(evidence, "2.0.9")).toThrow(/already released/);
    expect(() => decideVersion(evidence, "2.0.8")).toThrow(/not newer/);
    expect(() => decideVersion(evidence, "v2.1")).toThrow(/dotted numeric/);
    expect(() => decideVersion({ onMain: "2.0.12", released: ["2.0.9"], unreleased: [] }, "2.0.11")).toThrow(/older than main/);
  });
});
