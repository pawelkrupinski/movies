import { generateKeyPairSync, verify } from "node:crypto";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { setHttpClient, type HttpClient, type HttpRequest } from "../../src/http.js";
import { HttpError } from "../../src/mobile/network.js";
import {
  androidReleaseState,
  ascClient,
  ascToken,
  envLocal,
  iosReleaseState,
  playAssertion,
  playClient,
  type PlayApi,
} from "../../src/mobile/stores.js";
import { dnsFailure } from "./errors.js";

const NOW_MS = 1_790_000_000_000;
const noSleep = async () => {};

function decode(part: string | undefined): Record<string, unknown> {
  return JSON.parse(Buffer.from(part ?? "", "base64url").toString("utf8")) as Record<string, unknown>;
}

describe("JWTs", () => {
  it("signs App Store Connect's ES256 as raw r||s, which Apple requires", () => {
    const { privateKey, publicKey } = generateKeyPairSync("ec", { namedCurve: "P-256" });
    const pem = privateKey.export({ type: "pkcs8", format: "pem" }).toString();
    const token = ascToken("KEY123", "issuer-1", pem, 1_790_000_000);
    const [header, claims, signature] = token.split(".");
    expect(decode(header)).toEqual({ alg: "ES256", kid: "KEY123", typ: "JWT" });
    expect(decode(claims)).toEqual({ iss: "issuer-1", iat: 1_790_000_000, exp: 1_790_001_100, aud: "appstoreconnect-v1" });
    const raw = Buffer.from(signature ?? "", "base64url");
    expect(raw).toHaveLength(64); // DER would be ~70-72 bytes and start 0x30
    expect(verify("sha256", Buffer.from(`${header}.${claims}`), { key: publicKey, dsaEncoding: "ieee-p1363" }, raw)).toBe(true);
  });

  it("signs the Play service-account assertion RS256", () => {
    const { privateKey, publicKey } = generateKeyPairSync("rsa", { modulusLength: 2048 });
    const pem = privateKey.export({ type: "pkcs8", format: "pem" }).toString();
    const assertion = playAssertion({ client_email: "sa@x.iam", private_key: pem, token_uri: "https://oauth2.example/token" }, 1_790_000_000);
    const [header, claims, signature] = assertion.split(".");
    expect(decode(header)).toEqual({ alg: "RS256", typ: "JWT" });
    expect(decode(claims)).toMatchObject({ iss: "sa@x.iam", aud: "https://oauth2.example/token", exp: 1_790_003_600 });
    expect(verify("sha256", Buffer.from(`${header}.${claims}`), publicKey, Buffer.from(signature ?? "", "base64url"))).toBe(true);
  });
});

describe("iOS release state", () => {
  const version = (versionString: string, appStoreState: string, createdDate: string) => ({ attributes: { versionString, appStoreState, createdDate } });

  it("takes READY_FOR_SALE as live and the newer submission as pending, ordering by date itself", async () => {
    const state = await iosReleaseState(async () => ({
      data: [version("2.0.6", "READY_FOR_SALE", "2026-09-01"), version("2.0.8", "WAITING_FOR_REVIEW", "2026-09-20"), version("2.0.5", "REPLACED_WITH_NEW_VERSION", "2026-08-01")],
    }), noSleep);
    expect(state).toEqual({ error: null, liveVersion: "2.0.6", liveExtra: "READY_FOR_SALE", pending: { version: "2.0.8", state: "WAITING_FOR_REVIEW" } });
  });

  it("retries a DNS failure and recovers", async () => {
    let calls = 0;
    const state = await iosReleaseState(async () => {
      if (++calls < 2) throw dnsFailure();
      return { data: [version("2.0.7", "READY_FOR_SALE", "2026-09-01")] };
    }, noSleep);
    expect(calls).toBe(2);
    expect(state).toMatchObject({ liveVersion: "2.0.7", pending: null });
  });

  it("reports a persistent DNS failure as a network error", async () => {
    const state = await iosReleaseState(async () => {
      throw dnsFailure();
    }, noSleep);
    expect(state).toMatchObject({ networkError: true });
    expect(state.error).toContain("ENOTFOUND");
  });

  it("does not retry an auth failure, nor call it a network error", async () => {
    let calls = 0;
    const state = await iosReleaseState(async () => {
      calls++;
      throw new HttpError("https://x", 401, "Unauthorized");
    }, noSleep);
    expect(calls).toBe(1);
    expect(state).toMatchObject({ networkError: false });
  });
});

describe("Android release state", () => {
  const play = (token: () => Promise<string>, releases: unknown[]): PlayApi => ({
    token,
    post: async () => ({ id: "edit-1" }),
    get: async () => ({ releases }),
  });

  it("retries across the whole token → edit → track sequence", async () => {
    const calls: string[] = [];
    const state = await androidReleaseState(play(async () => {
      calls.push("token");
      if (calls.length < 2) throw dnsFailure();
      return "tok";
    }, [{ status: "completed", name: "2.0.6", versionCodes: ["309"] }]), noSleep);
    expect(calls).toEqual(["token", "token"]);
    expect(state).toEqual({ error: null, liveVersion: "2.0.6", liveExtra: "309", pending: null });
  });

  it("names a newer release still rolling out as pending", async () => {
    const state = await androidReleaseState(play(async () => "tok", [
      { status: "inProgress", name: "2.0.7", versionCodes: ["310"] },
      { status: "completed", name: "2.0.6", versionCodes: ["309"] },
    ]), noSleep);
    expect(state).toMatchObject({ liveVersion: "2.0.6", pending: { version: "2.0.7", state: "inProgress" } });
  });

  it("reports a persistent DNS failure as a network error", async () => {
    const state = await androidReleaseState(play(async () => {
      throw dnsFailure();
    }, []), noSleep);
    expect(state).toMatchObject({ networkError: true });
  });
});

describe("the real clients, over a fake HTTP client", () => {
  let dir: string;
  let restore: HttpClient;
  let requests: { url: string; init: HttpRequest }[];
  beforeEach(() => {
    dir = mkdtempSync(join(tmpdir(), "mobile-stores-"));
    requests = [];
  });
  afterEach(() => {
    setHttpClient(restore);
    rmSync(dir, { recursive: true, force: true });
  });
  const answer = (routes: Record<string, [number, unknown]>) => {
    restore = setHttpClient(async (url, init) => {
      requests.push({ url, init });
      const [status, body] = routes[url] ?? [404, { error: "no route" }];
      return { status, headers: new Headers(), body: JSON.stringify(body) };
    });
  };

  it("reads .env.local without sourcing it, quotes and ampersands included", async () => {
    writeFileSync(join(dir, ".env.local"), 'OTHER=1\nAPP_STORE_KEY_ID="KEY&1"\nAPP_STORE_ISSUER_ID=\'iss\'\n');
    expect(await envLocal(dir, "APP_STORE_KEY_ID")).toBe("KEY&1");
    expect(await envLocal(dir, "APP_STORE_ISSUER_ID")).toBe("iss");
    expect(await envLocal(dir, "MISSING")).toBeNull();
    expect(await envLocal(join(dir, "nowhere"), "APP_STORE_KEY_ID")).toBeNull();
  });

  it("signs an App Store Connect request with the key named in .env.local", async () => {
    const { privateKey } = generateKeyPairSync("ec", { namedCurve: "P-256" });
    writeFileSync(join(dir, ".env.local"), "APP_STORE_KEY_ID=KEY1\nAPP_STORE_ISSUER_ID=iss\n");
    writeFileSync(join(dir, "AuthKey_KEY1.p8"), privateKey.export({ type: "pkcs8", format: "pem" }));
    answer({ "https://api.appstoreconnect.apple.com/v1/x": [200, { ok: 1 }] });
    expect(await ascClient(dir, dir, () => NOW_MS)("/v1/x")).toEqual({ ok: 1 });
    const token = requests[0]?.init.headers?.Authorization?.replace("Bearer ", "") ?? "";
    expect(decode(token.split(".")[1])).toMatchObject({ iss: "iss", iat: 1_790_000_000 });
  });

  it("a missing .p8 is a local fault, not a network error", async () => {
    writeFileSync(join(dir, ".env.local"), "APP_STORE_KEY_ID=KEY1\nAPP_STORE_ISSUER_ID=iss\n");
    const state = await iosReleaseState(ascClient(dir, dir, () => NOW_MS), noSleep);
    expect(state).toMatchObject({ networkError: false });
    expect(state.error).toContain("AuthKey_KEY1.p8");
  });

  it("a 401 from Apple is reported, not retried", async () => {
    const { privateKey } = generateKeyPairSync("ec", { namedCurve: "P-256" });
    writeFileSync(join(dir, ".env.local"), "APP_STORE_KEY_ID=KEY1\nAPP_STORE_ISSUER_ID=iss\n");
    writeFileSync(join(dir, "AuthKey_KEY1.p8"), privateKey.export({ type: "pkcs8", format: "pem" }));
    answer({ "https://api.appstoreconnect.apple.com/v1/apps/6792566321/appStoreVersions?limit=10": [401, { errors: ["NOT_AUTHORIZED"] }] });
    const state = await iosReleaseState(ascClient(dir, dir, () => NOW_MS), noSleep);
    expect(requests).toHaveLength(1);
    expect(state).toMatchObject({ networkError: false });
    expect(state.error).toContain("HTTP 401");
  });

  it("reads Play's production track through an edit it never commits", async () => {
    const { privateKey } = generateKeyPairSync("rsa", { modulusLength: 2048 });
    mkdirSync(join(dir, "android"));
    writeFileSync(join(dir, "android", "play-credentials.json"), JSON.stringify({
      client_email: "sa@x.iam", private_key: privateKey.export({ type: "pkcs8", format: "pem" }), token_uri: "https://oauth2.example/token",
    }));
    const base = "https://androidpublisher.googleapis.com/androidpublisher/v3/applications/net.pawel.kinowo";
    answer({
      "https://oauth2.example/token": [200, { access_token: "tok" }],
      [`${base}/edits`]: [200, { id: "e1" }],
      [`${base}/edits/e1/tracks/production`]: [200, { releases: [{ status: "completed", name: "2.0.9", versionCodes: ["312", "313"] }] }],
    });
    const state = await androidReleaseState(playClient(dir, () => NOW_MS), noSleep);
    expect(state).toEqual({ error: null, liveVersion: "2.0.9", liveExtra: "312,313", pending: null });
    expect(requests.map(({ url, init }) => `${init.method ?? "GET"} ${url}`)).toEqual([
      "POST https://oauth2.example/token",
      `POST ${base}/edits`,
      `GET ${base}/edits/e1/tracks/production`,
    ]);
    expect(requests[0]?.init.body).toMatch(/^grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=[\w-]+\.[\w-]+\.[\w-]+$/);
    expect(requests[1]?.init.headers).toEqual({ Authorization: "Bearer tok" });
    // Nothing ever commits the edit.
    expect(requests.some(({ url }) => url.includes(":commit"))).toBe(false);
  });
});
