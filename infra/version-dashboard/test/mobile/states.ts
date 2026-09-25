import type { MobileState, Platform, ReleasedPlatform } from "../../src/mobile/model.js";

export const NOW = 1_790_000_000_000;

export const networkDown = (name: string): Platform => ({ name, fetchFailed: true, error: "TypeError: fetch failed (getaddrinfo ENOTFOUND)", networkError: true });

export const upToDate = (name: string, version = "2.0.7"): ReleasedPlatform => ({
  name, fetchFailed: false, liveVersion: version, liveExtra: null, pending: null, baseline: "abc1234def0", commits: [], error: null,
});

export const state = (platforms: Platform[], over: Partial<MobileState> = {}): MobileState => ({
  ready: true, builtAt: NOW, took: 0.8, platforms, buildError: null, ...over,
});

export const allFailed = (): MobileState => state([networkDown("iOS"), networkDown("Android")]);
/** iOS fine, Android 401s: NOT a network failure. */
export const oneOk = (): MobileState => state([upToDate("iOS"), { name: "Android", fetchFailed: true, error: "HttpError: HTTP 401", networkError: false }]);
