/**
 * What a run does, decided from main and both stores before anything is built.
 *
 * Three outcomes:
 *   - NOTHING: both stores already carry a build of main's current app code (ios/, android/ and
 *     mobile-version.txt unchanged since the commit each store's version was built from).
 *   - FINISH: one store carries main's version built from the current app code and the other does
 *     not -- a run that stopped half way (an App Store 409, a Play outage). Only the missing side is
 *     done, under the SAME version, so the two stores stay one version = one commit.
 *   - SHIP: both stores, under the version decideVersion picks.
 *
 * "Which commit a store's version was built from" is the mobile-<platform>-<version> tag the build
 * lanes push (tag-mobile-release.sh), so a store version with no tag is never treated as current.
 */
import { IN_REVIEW, type IosState } from "./ios.js";
import type { AndroidState } from "./android.js";
import { compareVersions, decideVersion, type VersionDecision } from "./version.js";

export type Platform = "ios" | "android";
export const PLATFORMS: readonly Platform[] = ["ios", "android"];

/** Paths that make up the shipped apps; a change anywhere else does not need a release. */
export const APP_PATHS = ["ios", "android", "mobile-version.txt"] as const;

export interface PlanInput {
  readonly onMain: string;
  readonly ios: IosState;
  readonly android: AndroidState;
  /** `mobile-<platform>-<version>` tag → the commit it points at. */
  readonly tags: ReadonlyMap<string, string>;
  /** Whether the app paths are identical between `sha` and main's head. */
  readonly appUnchangedSince: (sha: string) => boolean;
  readonly requested?: string;
  /** Ship even when nothing in the app paths changed. */
  readonly force?: boolean;
}

export type Plan =
  | { readonly kind: "nothing"; readonly reason: string }
  | { readonly kind: "ship"; readonly decision: VersionDecision; readonly platforms: readonly Platform[] };

export const tagName = (platform: Platform, version: string) => `mobile-${platform}-${version}`;

/** The version each store currently answers with: what it is serving or reviewing. */
export function storeVersion(input: Pick<PlanInput, "ios" | "android">, platform: Platform): string | null {
  if (platform === "ios") {
    const { pending } = input.ios;
    if (pending && IN_REVIEW.has(pending.state)) return pending.versionString;
    return input.ios.released[0] ?? null;
  }
  return [...input.android.released].sort(compareVersions).at(-1) ?? null;
}

/** Whether `platform`'s store already carries a build of main's current app code. */
function current(input: PlanInput, platform: Platform): string | null {
  const version = storeVersion(input, platform);
  const sha = version === null ? undefined : input.tags.get(tagName(platform, version));
  return sha !== undefined && input.appUnchangedSince(sha) ? version : null;
}

export function planRelease(input: PlanInput): Plan {
  if (input.requested === undefined && !input.force) {
    const [ios, android] = PLATFORMS.map((platform) => current(input, platform));
    if (ios !== null && android !== null) {
      return { kind: "nothing", reason: `both stores already carry main's app code (iOS ${ios}, Android ${android})` };
    }
    const finished = ios ?? android;
    const missing: Platform = ios === null ? "ios" : "android";
    // Only when the other store has NOT released it: released from code no tag vouches for, the
    // version is spent there, and shipping again under it would make one number two apps.
    if (finished !== null && finished === input.onMain && !input[missing].released.includes(finished)) {
      return {
        kind: "ship",
        decision: { version: finished, bump: false, reason: `finishing ${finished}: ${missing === "ios" ? "Android" : "iOS"} already has it from this code` },
        platforms: [missing],
      };
    }
  }
  const decision = decideVersion(
    {
      onMain: input.onMain,
      released: [...input.ios.released, ...input.android.released],
      unreleased: [...(input.ios.pending ? [input.ios.pending.versionString] : []), ...input.android.unreleased],
    },
    input.requested,
  );
  return { kind: "ship", decision, platforms: PLATFORMS };
}
