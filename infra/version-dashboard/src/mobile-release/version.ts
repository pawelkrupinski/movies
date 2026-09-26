/**
 * Which version number a release ships under -- ONE number for both stores, always.
 *
 * mobile-version.txt on main is only the number the last bump asked for. Once a version has
 * reached users (or Apple has approved it) on EITHER store it is spent: shipping new code under it
 * would make one number mean two apps. So:
 *
 *   1. main's version, when it is newer than everything either store has released -- a bump
 *      already committed, or a release that stopped before anything went public;
 *   2. otherwise the SMALLEST version above both stores' newest release (the next patch of the
 *      higher of the two), so the stores converge on one number and none is skipped.
 *
 * An unreleased App Store draft or in-review record does not steer the number: it is renamed to
 * whatever this picks (see ios.ts), so a stray draft at 2.0.12 cannot make the release skip 2.0.10.
 */

const VERSION = /^\d+(\.\d+){0,2}$/;

export function isVersion(value: string): boolean {
  return VERSION.test(value);
}

function parts(version: string): number[] {
  const numbers = version.split(".").map(Number);
  return [numbers[0] ?? 0, numbers[1] ?? 0, numbers[2] ?? 0];
}

/** Numeric order, missing components as 0: "2.0" == "2.0.0" < "2.0.10". */
export function compareVersions(a: string, b: string): number {
  const [x, y] = [parts(a), parts(b)];
  for (let index = 0; index < 3; index++) {
    const difference = (x[index] ?? 0) - (y[index] ?? 0);
    if (difference) return difference;
  }
  return 0;
}

export function nextPatch(version: string): string {
  const [major, minor, patch] = parts(version);
  return `${major}.${minor}.${(patch ?? 0) + 1}`;
}

function newest(versions: readonly string[]): string | null {
  return versions.filter(isVersion).reduce<string | null>((best, v) => (best === null || compareVersions(v, best) > 0 ? v : best), null);
}

/** What main and the two stores say, as version strings. */
export interface VersionEvidence {
  readonly onMain: string;
  /** Versions that reached users or were approved: iOS live/approved records, anything in Play production. */
  readonly released: readonly string[];
}

export interface VersionDecision {
  readonly version: string;
  /** Whether main has to be bumped to it (mobile-version.txt differs). */
  readonly bump: boolean;
  readonly reason: string;
}

/** Decide the version, or throw when a requested one cannot ship. */
export function decideVersion(evidence: VersionEvidence, requested?: string): VersionDecision {
  const released = new Set(evidence.released);
  const newestReleased = newest(evidence.released);
  const shippable = (version: string) => !released.has(version) && (newestReleased === null || compareVersions(version, newestReleased) > 0);
  const decision = (version: string, reason: string): VersionDecision => ({ version, bump: version !== evidence.onMain, reason });

  if (requested !== undefined) {
    if (!isVersion(requested)) throw new Error(`'${requested}' is not a dotted numeric version (e.g. 2.1.0)`);
    if (!shippable(requested)) throw new Error(`${requested} cannot ship: ${released.has(requested) ? "a store has already released it" : `it is not newer than the released ${newestReleased}`}`);
    if (compareVersions(requested, evidence.onMain) < 0) throw new Error(`${requested} is older than main's ${evidence.onMain}`);
    return decision(requested, "requested");
  }
  if (shippable(evidence.onMain)) return decision(evidence.onMain, `main's ${evidence.onMain} is newer than anything either store released`);
  const next = nextPatch(newestReleased ?? evidence.onMain);
  return decision(next, `${newestReleased} is the newest release on either store; bumping both to ${next}`);
}
