import { describe, expect, it } from "vitest";
import { dnsFailure, fetchFailed } from "./errors.js";
import { describeError, HttpError, isTransientNetworkError, NETWORK_RETRY_DELAYS_MS, withNetworkRetries } from "../../src/mobile/network.js";


describe("transient network error classification", () => {
  it("counts a DNS failure as transient, wrapped or bare", () => {
    expect(isTransientNetworkError(dnsFailure())).toBe(true);
    expect(isTransientNetworkError(Object.assign(new Error("getaddrinfo EAI_AGAIN"), { code: "EAI_AGAIN" }))).toBe(true);
  });

  it("counts connection and timeout failures as transient", () => {
    expect(isTransientNetworkError(fetchFailed("ECONNREFUSED"))).toBe(true);
    expect(isTransientNetworkError(fetchFailed("ECONNRESET"))).toBe(true);
    expect(isTransientNetworkError(fetchFailed("UND_ERR_CONNECT_TIMEOUT"))).toBe(true);
    expect(isTransientNetworkError(new DOMException("The operation was aborted due to timeout", "TimeoutError"))).toBe(true);
  });

  it("does not count an HTTP status: the server answered", () => {
    expect(isTransientNetworkError(new HttpError("https://api.appstoreconnect.apple.com/x", 401, "Unauthorized"))).toBe(false);
  });

  it("does not count an unrelated exception", () => {
    expect(isTransientNetworkError(new Error("access_token"))).toBe(false);
    expect(isTransientNetworkError(new SyntaxError("bad json"))).toBe(false);
  });

  it("does not count a certificate or TLS-protocol refusal", () => {
    expect(isTransientNetworkError(fetchFailed("CERT_HAS_EXPIRED", "certificate has expired"))).toBe(false);
    expect(isTransientNetworkError(fetchFailed("UNABLE_TO_VERIFY_LEAF_SIGNATURE"))).toBe(false);
    expect(isTransientNetworkError(fetchFailed("ERR_SSL_WRONG_VERSION_NUMBER", "wrong version number"))).toBe(false);
    expect(isTransientNetworkError(fetchFailed("ERR_TLS_CERT_ALTNAME_INVALID"))).toBe(false);
  });

  it("still counts a TLS connection dropped mid-handshake", () => {
    expect(isTransientNetworkError(fetchFailed("ECONNRESET", "Client network socket disconnected before secure TLS connection was established"))).toBe(true);
  });

  it("does not count a local error outside the round trip", () => {
    expect(isTransientNetworkError(Object.assign(new Error("ENOENT: AuthKey_X.p8"), { code: "ENOENT" }))).toBe(false);
  });
});

describe("describeError", () => {
  it("names the cause Node's bare 'fetch failed' hides", () => {
    expect(describeError(dnsFailure())).toBe("TypeError: fetch failed (getaddrinfo ENOTFOUND api.appstoreconnect.apple.com)");
    expect(describeError(new HttpError("https://x", 401, "Unauthorized"))).toBe("HttpError: HTTP 401 from https://x: Unauthorized");
  });
});

describe("network retries", () => {
  const recorder = () => {
    const slept: number[] = [];
    return { slept, sleep: async (ms: number) => void slept.push(ms) };
  };

  it("does not retry when the first attempt works", async () => {
    const { slept, sleep } = recorder();
    let calls = 0;
    expect(await withNetworkRetries(async () => (calls++, "ok"), sleep)).toBe("ok");
    expect(calls).toBe(1);
    expect(slept).toEqual([]);
  });

  it("retries a transient failure and returns the eventual success", async () => {
    const { slept, sleep } = recorder();
    let attempts = 0;
    const result = await withNetworkRetries(async () => {
      if (++attempts < 3) throw dnsFailure();
      return "ok";
    }, sleep);
    expect(result).toBe("ok");
    expect(attempts).toBe(3);
    expect(slept).toEqual(NETWORK_RETRY_DELAYS_MS);
    expect(NETWORK_RETRY_DELAYS_MS).toEqual([1_000, 3_000]);
  });

  it("gives up after every attempt, with the last failure", async () => {
    const { slept, sleep } = recorder();
    await expect(withNetworkRetries(async () => {
      throw dnsFailure();
    }, sleep)).rejects.toThrow("fetch failed");
    expect(slept).toHaveLength(2);
  });

  it("lets an HTTP error through on the first attempt", async () => {
    const { slept, sleep } = recorder();
    let calls = 0;
    await expect(withNetworkRetries(async () => {
      calls++;
      throw new HttpError("https://x", 401, "Unauthorized");
    }, sleep)).rejects.toBeInstanceOf(HttpError);
    expect(calls).toBe(1);
    expect(slept).toEqual([]);
  });
});
