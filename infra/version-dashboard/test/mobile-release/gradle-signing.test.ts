import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

/**
 * Signing comes ONLY from the vault: the Gradle build reads the env vars `androidSigningEnv` exports
 * and nothing else -- no gitignored keystore or Play key file beside it to fall back on.
 */
describe("android/app/build.gradle.kts signs from the vault only", () => {
  const gradle = readFileSync(join(import.meta.dirname, "..", "..", "..", "..", "android", "app", "build.gradle.kts"), "utf8");
  const FILE_FALLBACK = /keystore\.properties|play-credentials\.json/;

  it("reads no signing or Play credentials file of its own", () => {
    const offenders = gradle.split("\n").map((text, index) => ({ line: index + 1, text: text.trim() })).filter(({ text }) => FILE_FALLBACK.test(text));
    expect(offenders).toEqual([]);
  });

  it("would catch a file fallback (positive control)", () => {
    expect(FILE_FALLBACK.test('val f = rootProject.file("keystore.properties")')).toBe(true);
    expect(FILE_FALLBACK.test('(System.getenv("KINOWO_PLAY_CREDENTIALS_FILE") ?: "play-credentials.json")')).toBe(true);
    expect(FILE_FALLBACK.test('System.getenv("KINOWO_PLAY_CREDENTIALS_FILE")')).toBe(false);
  });
});
