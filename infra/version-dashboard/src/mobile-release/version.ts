/**
 * Which version number a release ships under.
 *
 * mobile-version.txt on main is only the number the last bump asked for. Whether it is still
 * free to ship depends on the stores: once a version has reached users (or Apple has approved it)
 * on either store, shipping new code under it would make one number mean two apps. So the choice
 * is made from what both stores say, in this order:
 *
 *   1. main's version, when neither store has released it -- a bump already committed, or a
 *      release that stopped half way (built, uploaded, never submitted).
 *   2. a version a store holds but has not released, newer than everything released -- an iOS
 *      draft or a submission still in review, picked up rather than skipped past.
 *   3. otherwise the patch after the newest version either store or main has ever named.
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
  /** Versions a store holds that have NOT been released: an iOS draft or in-review record, a Play testing-track release. */
  readonly unreleased: readonly string[];
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
  if (shippable(evidence.onMain)) return decision(evidence.onMain, `main's ${evidence.onMain} has not been released by either store`);
  const pending = newest(evidence.unreleased.filter(shippable));
  if (pending !== null) return decision(pending, `picking up ${pending}, which a store holds but has not released`);
  const next = nextPatch(newest([evidence.onMain, ...evidence.released, ...evidence.unreleased]) ?? evidence.onMain);
  return decision(next, `${evidence.onMain} is already released; bumping to ${next}`);
}
