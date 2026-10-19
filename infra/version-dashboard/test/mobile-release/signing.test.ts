import { existsSync, mkdtempSync, readFileSync, readdirSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { androidSigningEnv, runSigned, withAndroidSigning } from "../../src/mobile-release/signing.js";
import { fakeVault, signingFilesIn } from "./fakes.js";

let dir: string;
beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "android-signing-"));
});
afterEach(() => rmSync(dir, { recursive: true, force: true }));

const VAULT_FILES = {
  KINOWO_RELEASE_STORE_FILE: "JKS-BYTES mode=600",
  KINOWO_PLAY_CREDENTIALS_FILE: '{"client_email":"claude@kinowo"} mode=600',
};

describe("withAndroidSigning", () => {
  it("hands the vault's signing env to the command, its key files private and gone once it settles", async () => {
    const keyDir = join(dir, "signing");
    let seen: Record<string, string> = {};
    let env: Record<string, string> = {};
    const result = await withAndroidSigning(fakeVault, keyDir, async (signing) => {
      env = signing;
      seen = signingFilesIn(signing);
      return "built";
    });
    expect(result).toBe("built");
    expect(env).toMatchObject({ KINOWO_RELEASE_STORE_PASSWORD: "sp", KINOWO_RELEASE_KEY_ALIAS: "ka", KINOWO_RELEASE_KEY_PASSWORD: "kp" });
    expect(seen).toEqual(VAULT_FILES);
    expect(existsSync(keyDir)).toBe(false);
  });

  it("removes the keys when the command fails too", async () => {
    const keyDir = join(dir, "signing");
    await expect(withAndroidSigning(fakeVault, keyDir, () => Promise.reject(new Error("gradle failed")))).rejects.toThrow("gradle failed");
    expect(existsSync(keyDir)).toBe(false);
  });
});

describe("runSigned (scripts/android-signed)", () => {
  it("runs the command with the signing env and returns its exit code, leaving no keys behind", async () => {
    const ran: { argv: readonly string[]; files: Record<string, string> }[] = [];
    const code = await runSigned(fakeVault, ["./gradlew", "runOnDevice"], async (argv, env) => {
      ran.push({ argv, files: signingFilesIn(env) });
      return 3;
    }, dir);
    expect(code).toBe(3);
    expect(ran).toEqual([{ argv: ["./gradlew", "runOnDevice"], files: VAULT_FILES }]);
    expect(readdirSync(dir)).toEqual([]);
  });

  it("refuses an empty command before touching the vault", async () => {
    const asked: string[] = [];
    await expect(runSigned(async (name) => (asked.push(name), ""), [], async () => 0, dir)).rejects.toThrow("usage: scripts/android-signed");
    expect(asked).toEqual([]);
  });
});

/** build.gradle.kts reads the env vars the vault route exports by these very names (the file fallbacks it once had: gradle-signing.test.ts). */
describe("android/app/build.gradle.kts and the vault route", () => {
  it("reads every env var the vault route exports", async () => {
    const gradle = readFileSync(join(import.meta.dirname, "..", "..", "..", "..", "android", "app", "build.gradle.kts"), "utf8");
    const names = Object.keys(await androidSigningEnv(fakeVault, join(dir, "signing")));
    expect(names.filter((name) => !gradle.includes(`"${name}"`))).toEqual([]);
  });
});
