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
import { createPrivateKey, sign } from "node:crypto";
import { readFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import { httpRequest, type HttpRequest } from "../http.js";
import type { Pending } from "./model.js";
import { describeError, HttpError, isTransientNetworkError, withNetworkRetries } from "./network.js";

export const IOS_APP_ID = "6792566321";
export const ANDROID_PACKAGE = "net.pawel.kinowo";
export const ASC_BASE = "https://api.appstoreconnect.apple.com";
export const PLAY_BASE = `https://androidpublisher.googleapis.com/androidpublisher/v3/applications/${ANDROID_PACKAGE}`;
export const ASC_KEY_DIR = join(homedir(), ".appstoreconnect", "private_keys");
const REQUEST_TIMEOUT_MS = 20_000;

/** What one store says: live and pending, or why it could not be asked. */
export type StoreState =
  | { readonly error: string; readonly networkError: boolean }
  | { readonly error: null; readonly liveVersion: string | null; readonly liveExtra: string | null; readonly pending: Pending | null };

export type Sleep = (ms: number) => Promise<void>;
const realSleep: Sleep = (ms) => new Promise((wake) => setTimeout(wake, ms));

function failed(error: unknown): StoreState {
  return { error: describeError(error), networkError: isTransientNetworkError(error) };
}

async function requestJson(url: string, init: Omit<HttpRequest, "timeoutMs">): Promise<unknown> {
  const response = await httpRequest(url, { ...init, timeoutMs: REQUEST_TIMEOUT_MS });
  if (response.status < 200 || response.status >= 300) throw new HttpError(url, response.status, response.body);
  return JSON.parse(response.body) as unknown;
}

/**
 * One value out of .env.local, by reading the file rather than `source`ing it -- a value in there
 * contains a bare `&`, which kills a zsh `source` outright. Null on anything missing: an absent key
 * is a normal, reportable state for this page (a laptop with no release credentials), not a bug.
 */
export async function envLocal(repoDir: string, key: string): Promise<string | null> {
  let text: string;
  try {
    text = await readFile(join(repoDir, ".env.local"), "utf8");
  } catch {
    return null;
  }
  const line = text.split("\n").find((candidate) => candidate.startsWith(`${key}=`));
  if (line === undefined) return null;
  return line.slice(key.length + 1).trim().replace(/^"+|"+$/g, "").replace(/^'+|'+$/g, "");
}

const b64u = (data: string | Buffer): string => Buffer.from(data).toString("base64url");

/**
 * A ~18-minute ES256 JWT for App Store Connect. `ieee-p1363` makes node emit the raw r||s (32 bytes
 * each) Apple requires -- the default DER encoding is rejected outright.
 */
export function ascToken(keyId: string, issuerId: string, pem: string, nowSeconds: number): string {
  const header = b64u(JSON.stringify({ alg: "ES256", kid: keyId, typ: "JWT" }));
  const claims = b64u(JSON.stringify({ iss: issuerId, iat: nowSeconds, exp: nowSeconds + 1100, aud: "appstoreconnect-v1" }));
  const input = `${header}.${claims}`;
  const signature = sign("sha256", Buffer.from(input), { key: createPrivateKey(pem), dsaEncoding: "ieee-p1363" });
  return `${input}.${b64u(signature)}`;
}

/** GET against App Store Connect, signing a fresh token per call from the key on this Mac. */
export function ascClient(repoDir: string, keyDir = ASC_KEY_DIR, now: () => number = Date.now): (path: string) => Promise<unknown> {
  return async (path) => {
    const keyId = await envLocal(repoDir, "APP_STORE_KEY_ID");
    const issuerId = await envLocal(repoDir, "APP_STORE_ISSUER_ID");
    if (!keyId || !issuerId) throw new Error("APP_STORE_KEY_ID/APP_STORE_ISSUER_ID missing from .env.local");
    const pem = await readFile(join(keyDir, `AuthKey_${keyId}.p8`), "utf8");
    const token = ascToken(keyId, issuerId, pem, Math.floor(now() / 1000));
    return requestJson(`${ASC_BASE}${path}`, { headers: { Authorization: `Bearer ${token}` } });
  };
}

interface AscVersion {
  readonly versionString?: string;
  readonly appStoreState?: string;
  readonly createdDate?: string;
}

/**
 * The newest iOS version Apple has actually put in front of users, and separately whatever is ahead
 * of it and still working its way there.
 *
 * `appStoreState == READY_FOR_SALE` IS "RELEASED"; WAITING_FOR_REVIEW IS NOT, even though it can sit
 * there for days -- folding it into the baseline would report changes as shipped before a single
 * user could have them. The `sort` query parameter 400s on this endpoint (PARAMETER_ERROR.ILLEGAL),
 * so the ordering is done here rather than trusted from Apple.
 */
export async function iosReleaseState(get: (path: string) => Promise<unknown>, sleep: Sleep = realSleep): Promise<StoreState> {
  let data: { data?: { attributes: AscVersion }[] };
  try {
    data = (await withNetworkRetries(() => get(`/v1/apps/${IOS_APP_ID}/appStoreVersions?limit=10`), sleep)) as typeof data;
  } catch (error) {
    return failed(error);
  }
  const versions = (data.data ?? [])
    .map((version) => version.attributes)
    .sort((a, b) => ((b.createdDate ?? "") < (a.createdDate ?? "") ? -1 : (b.createdDate ?? "") > (a.createdDate ?? "") ? 1 : 0));
  const live = versions.find((version) => version.appStoreState === "READY_FOR_SALE") ?? null;
  const newest = versions[0] ?? null;
  const pending = newest && (!live || newest.versionString !== live.versionString)
    ? { version: newest.versionString ?? null, state: newest.appStoreState ?? null }
    : null;
  return { error: null, liveVersion: live?.versionString ?? null, liveExtra: live?.appStoreState ?? null, pending };
}

export interface PlayApi {
  token(): Promise<string>;
  post(path: string, token: string): Promise<unknown>;
  get(path: string, token: string): Promise<unknown>;
}

interface PlayCredentials {
  readonly client_email: string;
  readonly private_key: string;
  readonly token_uri: string;
}

/** The service-account assertion Google exchanges for an access token (RS256). */
export function playAssertion(credentials: PlayCredentials, nowSeconds: number): string {
  const header = b64u(JSON.stringify({ alg: "RS256", typ: "JWT" }));
  const claims = b64u(JSON.stringify({
    iss: credentials.client_email,
    scope: "https://www.googleapis.com/auth/androidpublisher",
    aud: credentials.token_uri,
    iat: nowSeconds,
    exp: nowSeconds + 3600,
  }));
  const input = `${header}.${claims}`;
  return `${input}.${b64u(sign("sha256", Buffer.from(input), createPrivateKey(credentials.private_key)))}`;
}

/** The Play Developer API through the service account in android/play-credentials.json. */
export function playClient(repoDir: string, now: () => number = Date.now): PlayApi {
  const auth = (token: string) => ({ Authorization: `Bearer ${token}` });
  return {
    async token() {
      const credentials = JSON.parse(await readFile(join(repoDir, "android", "play-credentials.json"), "utf8")) as PlayCredentials;
      const assertion = playAssertion(credentials, Math.floor(now() / 1000));
      const answer = (await requestJson(credentials.token_uri, {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: `grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=${assertion}`,
      })) as { access_token?: string };
      if (!answer.access_token) throw new Error("the token endpoint answered without an access_token");
      return answer.access_token;
    },
    post: (path, token) => requestJson(`${PLAY_BASE}${path}`, { method: "POST", headers: auth(token), body: "" }),
    get: (path, token) => requestJson(`${PLAY_BASE}${path}`, { headers: auth(token) }),
  };
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
      const edit = (await play.post("/edits", token)) as { id: string };
      return play.get(`/edits/${edit.id}/tracks/production`, token);
    }, sleep)) as typeof track;
  } catch (error) {
    return failed(error);
  }
  const releases = track.releases ?? [];
  const completed = releases.find((release) => release.status === "completed") ?? null;
  const newest = releases[0] ?? null;
  const pending = newest && newest !== completed ? { version: newest.name ?? null, state: newest.status ?? null } : null;
  return {
    error: null,
    liveVersion: completed?.name ?? null,
    liveExtra: completed ? (completed.versionCodes ?? []).join(",") : null,
    pending,
  };
}
