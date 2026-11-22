/**
 * What each store is actually serving.
 *
 * THE BASELINE FOR "NOT RELEASED" HAS TO BE EACH STORE'S OWN LIVE STATE, NOT mobile-version.txt AND
 * NOT THE NEWEST "Release mobile" COMMIT. The two stores do not ship every version in lockstep --
 * 2.0.7 went out iOS-only, so Android's store copy stayed 2.0.6 while `main` moved past the 2.0.7
 * release commit. A page diffing against the newest release commit would call Android's still-live
 * 2.0.6 changes "released" the moment iOS alone shipped past it. So each store is asked what it is
 * serving, and each platform is diffed from ITS OWN baseline (see build.ts).
 */
import type { Pending } from "./model.js";
import { IOS_APP_ID, VaultLockedError, type PlayApi, type Sleep } from "../app-stores/clients.js";
import { describeError, isTransientNetworkError, withNetworkRetries } from "../app-stores/network.js";

/** What one store says: live and pending, or why it could not be asked. */
export type StoreState =
  | { readonly error: string; readonly networkError: boolean; readonly vaultLocked?: true }
  | { readonly error: null; readonly liveVersion: string | null; readonly liveExtra: string | null; readonly pending: Pending | null };

const realSleep: Sleep = (ms) => new Promise((wake) => setTimeout(wake, ms));

function failed(error: unknown): StoreState {
  if (error instanceof VaultLockedError) {
    return { error: "the store key is in the Touch ID vault, which is locked for this dashboard -- unlock it with Touch ID", networkError: false, vaultLocked: true };
  }
  return { error: describeError(error), networkError: isTransientNetworkError(error) };
}

interface AscVersion {
  readonly versionString?: string;
  readonly appStoreState?: string;
  readonly createdDate?: string;
}

/** Sort order: the newest `createdDate` first, a version without one last. */
function newestCreatedFirst(a: AscVersion, b: AscVersion): number {
  const [x, y] = [a.createdDate ?? "", b.createdDate ?? ""];
  return x > y ? -1 : x < y ? 1 : 0;
}

/** Asks App Store Connect for the app's versions, and reads what is live and pending from the answer. */
export async function iosReleaseState(get: (path: string) => Promise<unknown>, sleep: Sleep = realSleep): Promise<StoreState> {
  let data: { data?: { attributes: AscVersion }[] };
  try {
    data = (await withNetworkRetries(() => get(`/v1/apps/${IOS_APP_ID}/appStoreVersions?limit=10`), sleep)) as typeof data;
  } catch (error) {
    return failed(error);
  }
  // An answer without the version list (an error document, a changed shape) is no answer: read as
  // empty it reported nothing live, and every change since the last release as unreleased.
  if (!Array.isArray(data?.data)) return { error: "App Store Connect answered without a version list", networkError: false };
  return iosStoreState(data.data.map((version) => version.attributes).sort(newestCreatedFirst));
}

/**
 * The newest iOS version Apple has actually put in front of users, and separately whatever is ahead
 * of it and still working its way there, from the versions newest first.
 *
 * `appStoreState == READY_FOR_SALE` IS "RELEASED"; WAITING_FOR_REVIEW IS NOT, even though it can sit
 * there for days -- folding it into the baseline would report changes as shipped before a single
 * user could have them. The `sort` query parameter 400s on this endpoint (PARAMETER_ERROR.ILLEGAL),
 * so the caller orders the versions rather than trusting Apple's order.
 */
function iosStoreState(versions: readonly AscVersion[]): StoreState {
  const live = versions.find((version) => version.appStoreState === "READY_FOR_SALE");
  const newest = versions[0];
  const pending = newest && (!live || newest.versionString !== live.versionString)
    ? { version: newest.versionString ?? null, state: newest.appStoreState ?? null }
    : null;
  return { error: null, liveVersion: live?.versionString ?? null, liveExtra: live?.appStoreState ?? null, pending };
}
interface PlayRelease {
  readonly name?: string;
  readonly status?: string;
  readonly versionCodes?: readonly string[];
}

/**
 * The production track's own view of what is live, read through a throwaway EDIT because the Play
 * Developer API has no track read outside one. THE EDIT IS NEVER COMMITTED -- it is left to expire
 * on Google's side, like every read-only edit this repo's release tooling opens -- so this can never
 * change what is live, however it fails. The whole token → edit → track sequence is retried as one.
 */
export async function androidReleaseState(play: PlayApi, sleep: Sleep = realSleep): Promise<StoreState> {
  let track: { releases?: PlayRelease[] };
  try {
    track = (await withNetworkRetries(async () => {
      const token = await play.token();
      const edit = (await play.send("POST", "/edits", token)) as { id: string };
      return play.get(`/edits/${edit.id}/tracks/production`, token);
    }, sleep)) as typeof track;
  } catch (error) {
    return failed(error);
  }
  return androidStoreState(track.releases ?? []);
}

/** The track's completed release is live; a newer one listed first (rolling out, halted, a draft) is pending. */
function androidStoreState(releases: readonly PlayRelease[]): StoreState {
  const completed = releases.find((release) => release.status === "completed");
  const newest = releases[0];
  const pending = newest && newest !== completed ? { version: newest.name ?? null, state: newest.status ?? null } : null;
  return {
    error: null,
    liveVersion: completed?.name ?? null,
    liveExtra: completed ? (completed.versionCodes ?? []).join(",") : null,
    pending,
  };
}
