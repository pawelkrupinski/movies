/**
 * The mobile page's data, as plain JSON: what the server pushes, what the browser re-renders from.
 * Imports nothing from node: -- the browser bundles it.
 */

export interface Commit {
  readonly sha: string;
  readonly short: string;
  readonly date: string;
  readonly subject: string;
}

export interface Pending {
  readonly version: string | null;
  readonly state: string | null;
}

/**
 * One platform. `fetchFailed` IS ITS OWN FLAG, NOT INFERRED FROM `liveVersion` BEING NULL, because a
 * version that has genuinely never shipped ALSO has no live version -- and that is a different,
 * unremarkable state ("nothing released yet") from a JWT/network fault, which is the one thing on
 * this page actually worth an alarm-coloured box.
 */
export type Platform =
  | {
    readonly name: string;
    readonly fetchFailed: true;
    readonly error: string;
    /** The OS/network refused the round trip, as opposed to the store answering with an error. */
    readonly networkError: boolean;
  }
  | {
    readonly name: string;
    readonly fetchFailed: false;
    readonly liveVersion: string | null;
    readonly liveExtra: string | null;
    readonly pending: Pending | null;
    readonly baseline: string | null;
    /** null: git history could not be read (or there was no baseline to read it from). */
    readonly commits: readonly Commit[] | null;
    readonly error: string | null;
  };

/** A platform whose store answered. */
export type ReleasedPlatform = Extract<Platform, { readonly fetchFailed: false }>;

export interface MobileState {
  /** False until the first build has landed. */
  readonly ready: boolean;
  /** Epoch ms the newest build started. */
  readonly builtAt: number;
  /** Seconds that build took. */
  readonly took: number;
  readonly platforms: readonly Platform[];
  /** The newest build threw: the platforms are from the build before it, and the page says so. */
  readonly buildError: string | null;
}

export const emptyMobileState = (): MobileState => ({ ready: false, builtAt: 0, took: 0, platforms: [], buildError: null });

/**
 * Whether every platform failed for a network reason -- the signal the retry floor and the
 * self-restart key off. A single platform failing (Play fine, ASC 401s) is not this: it says
 * nothing about this machine's own network, so racing the retry floor for it would just hammer a
 * credential that is not coming back.
 */
export function allPlatformsNetworkFailed(state: Pick<MobileState, "platforms">): boolean {
  return state.platforms.length > 0 && state.platforms.every((platform) => platform.fetchFailed && platform.networkError);
}
