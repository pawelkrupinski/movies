/**
 * Android release signing out of the Touch ID vault -- the ONLY place it comes from. android/app/
 * build.gradle.kts reads nothing but the env vars `androidSigningEnv` returns; there is no file
 * fallback. Two callers: the release lane (release.ts's buildAndroid) and `scripts/android-signed`
 * (`runOnDevice` from a shell or the devpanel), so a local device run signs with the same key.
 */
import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import type { Secrets } from "../mobile/stores.js";

/** Runs `argv` with `env` added to this process's environment; resolves to its exit code. */
export type SignedRun = (argv: readonly string[], env: Readonly<Record<string, string>>) => Promise<number>;

/**
 * Signing for a Gradle run: the passwords as env, the upload keystore and the Play service account as
 * owner-only files in `keyDir`. The names match the GitHub secrets CI signs with.
 */
export async function androidSigningEnv(secrets: Secrets, keyDir: string): Promise<Record<string, string>> {
  await mkdir(keyDir, { recursive: true, mode: 0o700 });
  const storeFile = join(keyDir, "kinowo-release.jks");
  const playFile = join(keyDir, "play-service-account.json");
  await writeFile(storeFile, Buffer.from(await secrets("KINOWO_RELEASE_KEYSTORE_BASE64"), "base64"), { mode: 0o600 });
  await writeFile(playFile, await secrets("PLAY_SERVICE_ACCOUNT_JSON"), { mode: 0o600 });
  return {
    KINOWO_RELEASE_STORE_FILE: storeFile,
    KINOWO_RELEASE_STORE_PASSWORD: await secrets("KINOWO_RELEASE_STORE_PASSWORD"),
    KINOWO_RELEASE_KEY_ALIAS: await secrets("KINOWO_RELEASE_KEY_ALIAS"),
    KINOWO_RELEASE_KEY_PASSWORD: await secrets("KINOWO_RELEASE_KEY_PASSWORD"),
    KINOWO_PLAY_CREDENTIALS_FILE: playFile,
  };
}

/** `use` with the signing env, `keyDir` removed as soon as it settles -- whether it succeeded or not. */
export async function withAndroidSigning<T>(secrets: Secrets, keyDir: string, use: (env: Record<string, string>) => Promise<T>): Promise<T> {
  try {
    return await use(await androidSigningEnv(secrets, keyDir));
  } finally {
    await rm(keyDir, { recursive: true, force: true });
  }
}

/** `scripts/android-signed <command...>`: `argv` run with the signing env, its keys in a fresh dir under `tmpRoot`. */
export async function runSigned(secrets: Secrets, argv: readonly string[], run: SignedRun, tmpRoot: string): Promise<number> {
  if (argv.length === 0) throw new Error("usage: scripts/android-signed <command...>   e.g. scripts/android-signed ./gradlew runOnDevice");
  const keyDir = await mkdtemp(join(tmpRoot, "kinowo-android-signing-"));
  return withAndroidSigning(secrets, keyDir, (env) => run(argv, env));
}
