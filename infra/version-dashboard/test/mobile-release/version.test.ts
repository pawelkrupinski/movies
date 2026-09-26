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
  it("ships main's version when it is newer than everything either store released", () => {
    expect(decideVersion({ onMain: "2.0.10", released: ["2.0.9", "2.0.9"] })).toMatchObject({ version: "2.0.10", bump: false });
  });

  it("bumps to the smallest version above BOTH stores' newest release", () => {
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.9", "2.0.8"] })).toMatchObject({ version: "2.0.10", bump: true });
    // Android alone having it is enough: one number must not mean two apps.
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.8", "2.0.9"] })).toMatchObject({ version: "2.0.10", bump: true });
    // Diverged stores converge on one number above the newer of the two.
    expect(decideVersion({ onMain: "2.0.7", released: ["2.0.9", "2.0.7"] })).toMatchObject({ version: "2.0.10", bump: true });
  });

  it("does not skip ahead to an unreleased draft's higher number -- the draft is renamed down instead", () => {
    // decideVersion no longer even sees drafts: only released versions and main's count.
    expect(decideVersion({ onMain: "2.0.9", released: ["2.0.9"] })).toMatchObject({ version: "2.0.10" });
  });

  it("honours a requested version, and refuses one that cannot ship", () => {
    const evidence = { onMain: "2.0.9", released: ["2.0.9"] };
    expect(decideVersion(evidence, "2.1.0")).toMatchObject({ version: "2.1.0", bump: true });
    expect(() => decideVersion(evidence, "2.0.9")).toThrow(/already released/);
    expect(() => decideVersion(evidence, "2.0.8")).toThrow(/not newer/);
    expect(() => decideVersion(evidence, "v2.1")).toThrow(/dotted numeric/);
    expect(() => decideVersion({ onMain: "2.0.12", released: ["2.0.9"] }, "2.0.11")).toThrow(/older than main/);
  });
});
