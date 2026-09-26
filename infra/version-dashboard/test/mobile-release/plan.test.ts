import { describe, expect, it } from "vitest";
import { androidStateFrom, type PlayTrack } from "../../src/mobile-release/android.js";
import { iosStateFrom } from "../../src/mobile-release/ios.js";
import { planRelease, type PlanInput } from "../../src/mobile-release/plan.js";
import { ascVersions } from "./fakes.js";

const RELEASED_SHA = "a".repeat(40);
const OLD_SHA = "b".repeat(40);

const production = (name: string): PlayTrack[] => [{ track: "production", releases: [{ name, status: "completed", versionCodes: ["1790075578"] }] }];

function input(overrides: Partial<PlanInput> = {}): PlanInput {
  return {
    onMain: "2.0.9",
    ios: iosStateFrom(ascVersions(["v9", "2.0.9", "READY_FOR_SALE", "2026-09-20"], ["v8", "2.0.8", "READY_FOR_SALE", "2026-09-16"])),
    android: androidStateFrom(production("2.0.9")),
    tags: new Map([
      ["mobile-ios-2.0.9", RELEASED_SHA],
      ["mobile-android-2.0.9", RELEASED_SHA],
    ]),
    appUnchangedSince: (sha) => sha === RELEASED_SHA,
    ...overrides,
  };
}

describe("planRelease", () => {
  it("has nothing to do when both stores carry main's app code", () => {
    expect(planRelease(input())).toMatchObject({ kind: "nothing" });
  });

  it("ships both under the next version once the app code changed", () => {
    const plan = planRelease(input({ appUnchangedSince: () => false }));
    expect(plan).toMatchObject({ kind: "ship", decision: { version: "2.0.10", bump: true } });
  });

  it("does not trust a store version with no tag naming its commit", () => {
    expect(planRelease(input({ tags: new Map([["mobile-ios-2.0.9", RELEASED_SHA]]) }))).toMatchObject({ kind: "ship", decision: { version: "2.0.10" } });
  });

  it("ships BOTH, under main's still-unreleased version, when iOS is already in review with it", () => {
    const plan = planRelease(input({
      onMain: "2.0.10",
      ios: iosStateFrom(ascVersions(["v10", "2.0.10", "WAITING_FOR_REVIEW", "2026-09-25"], ["v9", "2.0.9", "READY_FOR_SALE", "2026-09-20"])),
      tags: new Map([
        ["mobile-ios-2.0.10", RELEASED_SHA],
        ["mobile-android-2.0.9", OLD_SHA],
      ]),
    }));
    expect(plan).toMatchObject({ kind: "ship", decision: { version: "2.0.10", bump: false } });
  });

  it("ships BOTH one version up when Android already released main's version and iOS did not", () => {
    const plan = planRelease(input({
      onMain: "2.0.10",
      ios: iosStateFrom(ascVersions(["v10", "2.0.10", "PREPARE_FOR_SUBMISSION", "2026-09-25"], ["v9", "2.0.9", "READY_FOR_SALE", "2026-09-20"])),
      android: androidStateFrom(production("2.0.10")),
      tags: new Map([
        ["mobile-ios-2.0.10", RELEASED_SHA],
        ["mobile-android-2.0.10", RELEASED_SHA],
      ]),
    }));
    expect(plan).toMatchObject({ kind: "ship", decision: { version: "2.0.11", bump: true } });
  });

  it("ships both when the half-finished version was built from older code", () => {
    const plan = planRelease(input({
      onMain: "2.0.10",
      android: androidStateFrom(production("2.0.10")),
      tags: new Map([["mobile-android-2.0.10", OLD_SHA]]),
      appUnchangedSince: () => false,
    }));
    expect(plan).toMatchObject({ kind: "ship", decision: { version: "2.0.11" } });
  });

  it("ships anyway with --force or --version", () => {
    expect(planRelease(input({ force: true }))).toMatchObject({ kind: "ship", decision: { version: "2.0.10" } });
    expect(planRelease(input({ requested: "2.1.0" }))).toMatchObject({ kind: "ship", decision: { version: "2.1.0" } });
  });
});
