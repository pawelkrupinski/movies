import { describe, expect, it } from "vitest";
import { isLocalHost, isOwnOriginWrite } from "../src/local-host.js";

describe("which Host headers the dashboard answers", () => {
  it("is only its own loopback address and port -- a DNS-rebound page names its own domain", () => {
    expect(isLocalHost("127.0.0.1:8788", 8788)).toBe(true);
    expect(isLocalHost("localhost:8788", 8788)).toBe(true);
    expect(isLocalHost("LOCALHOST:8788", 8788)).toBe(true);
    expect(isLocalHost("attacker.example:8788", 8788)).toBe(false);
    expect(isLocalHost("127.0.0.1:8789", 8788)).toBe(false);
    expect(isLocalHost("127.0.0.1", 8788)).toBe(false);
    expect(isLocalHost(undefined, 8788)).toBe(false);
  });
});

describe("which writes the dashboard accepts", () => {
  it("refuses a POST another site's page sends straight to the loopback address", () => {
    expect(isOwnOriginWrite("POST", "https://attacker.example", "cross-site", 8788)).toBe(false);
    expect(isOwnOriginWrite("POST", "https://attacker.example", undefined, 8788)).toBe(false);
    expect(isOwnOriginWrite("POST", undefined, "cross-site", 8788)).toBe(false);
    expect(isOwnOriginWrite("POST", "null", undefined, 8788)).toBe(false);
    expect(isOwnOriginWrite("POST", "http://127.0.0.1:8789", "same-site", 8788)).toBe(false);
  });

  it("accepts its own pages, a client that is not a browser, and every read", () => {
    expect(isOwnOriginWrite("POST", "http://127.0.0.1:8788", "same-origin", 8788)).toBe(true);
    expect(isOwnOriginWrite("POST", "http://localhost:8788", undefined, 8788)).toBe(true);
    expect(isOwnOriginWrite("POST", undefined, undefined, 8788)).toBe(true);
    expect(isOwnOriginWrite("GET", "https://attacker.example", "cross-site", 8788)).toBe(true);
  });
});
