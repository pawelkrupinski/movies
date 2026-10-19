import { readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import type { AscApi, HttpMethod, PlayApi } from "../../src/mobile/stores.js";

export const fixture = (name: string): string => readFileSync(join(import.meta.dirname, "fixtures", name), "utf8");
export const fixtureJson = (name: string): unknown => JSON.parse(fixture(name)) as unknown;

/** A route answers with a value, or a `(body, hit) => value` computing one from the request body and
 * how often it was hit. (Typed `unknown`: a function type in a union with it is redundant.) */
export type Route = unknown;
export interface Call {
  readonly method: HttpMethod;
  readonly path: string;
  readonly body?: unknown;
}

/**
 * A store API answering from a "METHOD /path" table, recording every call. An unlisted request
 * throws, so a test states every call it expects the code to make.
 */
class RoutedApi {
  readonly calls: Call[] = [];
  private readonly hits = new Map<string, number>();
  /** `events`, when given, is shared with other fakes so a test can assert the order across them. */
  constructor(readonly routes: Record<string, Route>, private readonly events: string[] = []) {}

  async answer(method: HttpMethod, path: string, body?: unknown): Promise<unknown> {
    this.calls.push(body === undefined ? { method, path } : { method, path, body });
    this.events.push(`${this.constructor.name} ${method} ${path}`);
    const key = `${method} ${path}`;
    if (!(key in this.routes)) throw new Error(`unexpected request: ${key}`);
    const hit = (this.hits.get(key) ?? 0) + 1;
    this.hits.set(key, hit);
    const route = this.routes[key];
    return typeof route === "function" ? (route as (body: unknown, hit: number) => unknown)(body, hit) : structuredClone(route);
  }

  writes(): Call[] {
    return this.calls.filter((call) => call.method !== "GET");
  }
}

export class FakeAsc extends RoutedApi implements AscApi {
  readonly get = (path: string): Promise<unknown> => this.answer("GET", path);
  readonly send = (method: HttpMethod, path: string, body?: unknown): Promise<unknown> => this.answer(method, path, body);
}

export class FakePlay extends RoutedApi implements PlayApi {
  async token(): Promise<string> {
    return "token";
  }
  get(path: string): Promise<unknown> {
    return this.answer("GET", path);
  }
  send(method: HttpMethod, path: string, _token: string, body?: unknown): Promise<unknown> {
    return this.answer(method, path, body);
  }
}

/** An appStoreVersions list in App Store Connect's shape. */
export const ascVersions = (...records: [id: string, versionString: string, state: string, createdDate: string][]) => ({
  data: records.map(([id, versionString, appStoreState, createdDate]) => ({ type: "appStoreVersions", id, attributes: { versionString, appStoreState, createdDate } })),
});

export const noSleep = async () => {};

/** The release signing secrets as the Touch ID vault holds them. */
export const VAULT: Readonly<Record<string, string>> = {
  KINOWO_RELEASE_KEYSTORE_BASE64: Buffer.from("JKS-BYTES").toString("base64"),
  KINOWO_RELEASE_STORE_PASSWORD: "sp",
  KINOWO_RELEASE_KEY_ALIAS: "ka",
  KINOWO_RELEASE_KEY_PASSWORD: "kp",
  PLAY_SERVICE_ACCOUNT_JSON: '{"client_email":"claude@kinowo"}',
};

/** A `Secrets` answering from `VAULT`; a name it does not hold rejects, as the real vault does. */
export const fakeVault = async (name: string): Promise<string> => {
  const value = VAULT[name];
  if (value === undefined) throw new Error(`${name} is not in the vault`);
  return value;
};

/** What the keystore and Play key files a signing env names hold right now, with their modes. */
export function signingFilesIn(env: Readonly<Record<string, string>>): Record<string, string> {
  const files: Record<string, string> = {};
  for (const name of ["KINOWO_RELEASE_STORE_FILE", "KINOWO_PLAY_CREDENTIALS_FILE"]) {
    const path = env[name];
    if (path) files[name] = `${readFileSync(path, "utf8")} mode=${(statSync(path).mode & 0o777).toString(8)}`;
  }
  return files;
}
