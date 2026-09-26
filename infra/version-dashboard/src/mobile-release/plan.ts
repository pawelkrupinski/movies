/**
 * What a run does, decided from main and both stores before anything is built.
 *
 *   - NOTHING: both stores already carry a build of main's current app code (ios/, android/ and
 *     mobile-version.txt unchanged since the commit each store's version was built from).
 *   - SHIP: BOTH stores, always, under the one version decideVersion picks. A run that stopped
 *     half way is not finished on one side only: if a store released the version, the version is
 *     spent and both move up to the next one; if neither did, both ship it (reusing whatever
 *     builds were already uploaded -- see release.ts).
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
  | { readonly kind: "ship"; readonly decision: VersionDecision };

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
  }
  const decision = decideVersion({ onMain: input.onMain, released: [...input.ios.released, ...input.android.released] }, input.requested);
  return { kind: "ship", decision };
}
