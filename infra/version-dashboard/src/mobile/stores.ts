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
import { homedir } from "node:os";
import { join } from "node:path";
import { runCommand, type Executor } from "../exec.js";
import { httpRequest, type HttpRequest } from "../http.js";
import type { Pending } from "./model.js";
import { describeError, isTransientNetworkError, withNetworkRetries } from "./network.js";

export const IOS_APP_ID = "6792566321";
export const ANDROID_PACKAGE = "net.pawel.kinowo";
export const ASC_BASE = "https://api.appstoreconnect.apple.com";
export const PLAY_BASE = `https://androidpublisher.googleapis.com/androidpublisher/v3/applications/${ANDROID_PACKAGE}`;
export const SECRETS_BIN = join(homedir(), ".local", "bin", "secrets");
const REQUEST_TIMEOUT_MS = 20_000;

/** What one store says: live and pending, or why it could not be asked. */
export type StoreState =
  | { readonly error: string; readonly networkError: boolean; readonly vaultLocked?: true }
  | { readonly error: null; readonly liveVersion: string | null; readonly liveExtra: string | null; readonly pending: Pending | null };

export type Sleep = (ms: number) => Promise<void>;
const realSleep: Sleep = (ms) => new Promise((wake) => setTimeout(wake, ms));

function failed(error: unknown): StoreState {
  if (error instanceof VaultLockedError) {
    return { error: "the store key is in the Touch ID vault, which is locked for this dashboard -- unlock it with Touch ID", networkError: false, vaultLocked: true };
  }
  return { error: describeError(error), networkError: isTransientNetworkError(error) };
}

/** The parsed JSON answer; null for an empty 2xx body (a 204 relationship PATCH, a Play edit DELETE). */
async function requestJson(url: string, init: Omit<HttpRequest, "timeoutMs">): Promise<unknown> {
  const response = await httpRequest(url, { ...init, timeoutMs: REQUEST_TIMEOUT_MS });
  return response.body.trim() ? (JSON.parse(response.body) as unknown) : null;
}

/** The headers and body for a JSON write, or neither for a bodiless call. */
function jsonBody(body: unknown): Pick<HttpRequest, "headers" | "body"> {
  return body === undefined ? {} : { headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) };
}

/** One secret by name from the Touch ID vault; rejects with VaultLockedError while the vault is locked for this process. */
export type Secrets = (name: string) => Promise<string>;

/** The vault answered "locked": this process (its session anchor) has not been granted the namespace. */
export class VaultLockedError extends Error {
  constructor(name: string) {
    super(`${name}: the Touch ID vault is locked for this process`);
  }
}

export const VAULT_NAMESPACE = "movies";

/**
 * Every vault key the release lane reads (here, in src/mobile-release and in
 * scripts/app-store-credentials.sh; a test keeps the list whole). The /mobile page starts the lane
 * under `secrets run` with exactly these: the vault pins an unlock to the process that asked, so
 * only the dashboard, unlocked by its own Touch ID, can read them -- the lane's own `get` is locked.
 */
export const RELEASE_SECRET_NAMES = [
  "APP_STORE_KEY_ID",
  "APP_STORE_ISSUER_ID",
  "APP_STORE_KEY_P8",
  "PLAY_SERVICE_ACCOUNT_JSON",
  "KINOWO_RELEASE_KEYSTORE_BASE64",
  "KINOWO_RELEASE_STORE_PASSWORD",
  "KINOWO_RELEASE_KEY_ALIAS",
  "KINOWO_RELEASE_KEY_PASSWORD",
] as const;

/** A secret handed in through the environment (`secrets run`) wins; the vault is asked only for the rest. */
export function secretsFromEnvironmentFirst(environment: Readonly<Record<string, string | undefined>>, vault: Secrets): Secrets {
  return async (name) => environment[name] || vault(name);
}
const VAULT_TIMEOUT_MS = 10_000;
/** Long enough for a person to notice the Touch ID dialog and touch the sensor. */
const UNLOCK_TIMEOUT_MS = 120_000;

/**
 * The Touch ID vault (~/projects/secrets) through its CLI -- where the App Store Connect key lives
 * since .env.local was retired. The vault pins an unlock to the asking process, so the always-on
 * dashboard has an unlock of its own (`unlock`, behind the page's button) that lasts while it runs;
 * a release run from a terminal or Claude session uses that session's.
 */
export function vaultSecrets(exec: Executor = runCommand, bin: string = SECRETS_BIN): { readonly get: Secrets; readonly unlock: () => Promise<void> } {
  const get: Secrets = async (name) => {
    const result = await exec([bin, "get", VAULT_NAMESPACE, name], { timeoutMs: VAULT_TIMEOUT_MS });
    if (result.code === 0) return result.stdout.trim();
    if (/locked/i.test(result.stderr)) throw new VaultLockedError(name);
    throw new Error(`could not read ${name} from the vault: ${result.stderr.trim() || `exit ${String(result.code)}`}`);
  };
  const unlock = async () => {
    const result = await exec([bin, "unlock", VAULT_NAMESPACE], { timeoutMs: UNLOCK_TIMEOUT_MS });
    if (result.code !== 0) throw new Error(result.stderr.trim() || (result.timedOut ? "Touch ID prompt timed out" : `exit ${String(result.code)}`));
  };
  return { get, unlock };
}

/**
 * For a run with a person at the machine (`scripts/android-signed`, from a terminal or the DevPanel):
 * a locked vault asks for Touch ID once, then reads again. The unlock and every later read come from
 * this same process, so the grant covers them all, and ends when the process exits.
 */
export function unlockingWhenLocked(vault: { readonly get: Secrets; readonly unlock: () => Promise<void> }): Secrets {
  let unlocking: Promise<void> | undefined;
  return async (name) => {
    try {
      return await vault.get(name);
    } catch (error) {
      if (!(error instanceof VaultLockedError)) throw error;
      unlocking ??= vault.unlock();
      await unlocking;
      return vault.get(name);
    }
  };
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

export type HttpMethod = "GET" | "POST" | "PUT" | "PATCH" | "DELETE";

/** App Store Connect: reads for the dashboard, writes for the release lane (src/mobile-release). */
export interface AscApi {
  readonly get: (path: string) => Promise<unknown>;
  readonly send: (method: HttpMethod, path: string, body?: unknown) => Promise<unknown>;
}

/**
 * `asc` with its READS retried through a transient network failure: a release polls Apple for up to
 * half an hour while a build processes, and one DNS or ECONNRESET blip there aborted the whole
 * release after the upload. Writes are NOT retried -- a request that timed out may have landed, and
 * creating a version or a review submission twice is not a blip.
 */
export function retryingReads(asc: AscApi, sleep: Sleep): AscApi {
  return { get: (path) => withNetworkRetries(() => asc.get(path), sleep), send: asc.send };
}

/** App Store Connect, signing a fresh token per call from the key in the Touch ID vault. */
export function ascApi(secrets: Secrets = vaultSecrets().get, now: () => number = Date.now): AscApi {
  const send = async (method: HttpMethod, path: string, body?: unknown) => {
    const keyId = await secrets("APP_STORE_KEY_ID");
    const issuerId = await secrets("APP_STORE_ISSUER_ID");
    const pem = await secrets("APP_STORE_KEY_P8");
    const token = ascToken(keyId, issuerId, pem, Math.floor(now() / 1000));
    const { headers, body: payload } = jsonBody(body);
    return requestJson(`${ASC_BASE}${path}`, { method, headers: { Authorization: `Bearer ${token}`, ...headers }, body: payload });
  };
  return { get: (path) => send("GET", path), send };
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
  // An answer without the version list (an error document, a changed shape) is no answer: read as
  // empty it reported nothing live, and every change since the last release as unreleased.
  if (!Array.isArray(data?.data)) return { error: "App Store Connect answered without a version list", networkError: false };
  const versions = data.data
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
  get(path: string, token: string): Promise<unknown>;
  send(method: HttpMethod, path: string, token: string, body?: unknown): Promise<unknown>;
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

/** The Play Developer API through the service account in the Touch ID vault (PLAY_SERVICE_ACCOUNT_JSON). */
export function playClient(secrets: Secrets = vaultSecrets().get, now: () => number = Date.now): PlayApi {
  const auth = (token: string) => ({ Authorization: `Bearer ${token}` });
  return {
    async token() {
      const credentials = JSON.parse(await secrets("PLAY_SERVICE_ACCOUNT_JSON")) as PlayCredentials;
      const assertion = playAssertion(credentials, Math.floor(now() / 1000));
      const answer = (await requestJson(credentials.token_uri, {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: `grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=${assertion}`,
      })) as { access_token?: string };
      if (!answer.access_token) throw new Error("the token endpoint answered without an access_token");
      return answer.access_token;
    },
    get: (path, token) => requestJson(`${PLAY_BASE}${path}`, { headers: auth(token) }),
    send: (method, path, token, body) => {
      const { headers, body: payload } = jsonBody(body);
      // A bodiless POST (opening an edit, committing one) still sends "" -- Google 411s a POST
      // with no Content-Length.
      return requestJson(`${PLAY_BASE}${path}`, { method, headers: { ...auth(token), ...headers }, body: payload ?? (method === "POST" ? "" : undefined) });
    },
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
      const edit = (await play.send("POST", "/edits", token)) as { id: string };
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
