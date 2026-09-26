/**
 * The Play half of a release: read the tracks, and promote the build gradle-play-publisher put on
 * `internal` to `production`.
 *
 * Promotion is a direct track PUT in one edit rather than `:app:promoteReleaseArtifact`: it needs no
 * keystore or Gradle, and GPP's `-Ptrack=production` variant has already once promoted internal
 * onto ITSELF and reported BUILD SUCCESSFUL. Either way the proof is the same -- read production
 * back in a fresh edit, never trust the exit status.
 */
import type { PlayApi } from "../mobile/stores.js";
import { PLAY_NOTES_LIMIT, type Notes } from "./notes.js";
import { isVersion } from "./version.js";

export interface PlayRelease {
  readonly name?: string;
  readonly status?: string;
  readonly versionCodes?: readonly string[];
}

export interface PlayTrack {
  readonly track: string;
  readonly releases?: readonly PlayRelease[];
}

export interface AndroidState {
  readonly tracks: readonly PlayTrack[];
  /** Release names on production: what reached users. */
  readonly released: readonly string[];
  /** Version names only on testing tracks. */
  readonly unreleased: readonly string[];
  /** Every version code any track has ever held -- Play refuses one it has seen. */
  readonly versionCodes: readonly number[];
}

export function androidStateFrom(tracks: readonly PlayTrack[]): AndroidState {
  const names = (predicate: (track: PlayTrack) => boolean) =>
    tracks.filter(predicate).flatMap((track) => (track.releases ?? []).map((release) => release.name ?? "")).filter(Boolean);
  const released = names((track) => track.track === "production");
  return {
    tracks,
    released,
    // Only version-shaped names: the alpha track still holds a release called "Initial".
    unreleased: names((track) => track.track !== "production").filter((name) => isVersion(name) && !released.includes(name)),
    versionCodes: tracks.flatMap((track) => (track.releases ?? []).flatMap((release) => (release.versionCodes ?? []).map(Number))),
  };
}

/** Open an edit, run `work` in it, and throw the edit away -- a read changes nothing live. */
async function inEdit<T>(play: PlayApi, work: (editId: string, token: string) => Promise<T>): Promise<T> {
  const token = await play.token();
  const edit = (await play.send("POST", "/edits", token)) as { id: string };
  try {
    return await work(edit.id, token);
  } finally {
    await play.send("DELETE", `/edits/${edit.id}`, token).catch(() => {});
  }
}

export async function inspectAndroid(play: PlayApi): Promise<AndroidState> {
  return inEdit(play, async (editId, token) => androidStateFrom(((await play.get(`/edits/${editId}/tracks`, token)) as { tracks?: PlayTrack[] }).tracks ?? []));
}

/**
 * A code above every one Play has seen and never below the clock's seconds. Epoch seconds is what
 * the local lane has used since 2.0.9 (1790075578), so CI's run_number (~300) can no longer ship;
 * taking the max keeps it monotonic even if a code ever came from somewhere else.
 */
export function nextVersionCode(state: AndroidState, nowSeconds: number): number {
  return Math.max(nowSeconds, ...state.versionCodes.map((code) => code + 1));
}

/** The internal-track release a resumed run can promote instead of rebuilding: same name, newest code. */
export function internalBuildFor(state: AndroidState, version: string): number | null {
  const internal = state.tracks.find((track) => track.track === "internal");
  const codes = (internal?.releases ?? []).filter((release) => release.name === version).flatMap((release) => (release.versionCodes ?? []).map(Number));
  return codes.length ? Math.max(...codes) : null;
}

export interface AndroidPromotion {
  readonly version: string;
  readonly versionCode: number;
  readonly notes: Notes;
}

/** Promote `versionCode` from internal to production, fully rolled out, then prove it landed. */
export async function promoteAndroid(play: PlayApi, promotion: AndroidPromotion, log: (line: string) => void): Promise<void> {
  const code = String(promotion.versionCode);
  const token = await play.token();
  const edit = (await play.send("POST", "/edits", token)) as { id: string };
  try {
    const internal = (await play.get(`/edits/${edit.id}/tracks/internal`, token)) as PlayTrack;
    if (!(internal.releases ?? []).some((release) => (release.versionCodes ?? []).includes(code))) {
      throw new Error(`version code ${code} is not on the internal track -- the upload did not land`);
    }
    const listings = (await play.get(`/edits/${edit.id}/listings`, token)) as { listings?: { language: string }[] };
    const releaseNotes = (listings.listings ?? []).map(({ language }) => ({ language, text: promotion.notes(language) }));
    const tooLong = releaseNotes.find((note) => note.text.length > PLAY_NOTES_LIMIT);
    if (tooLong) throw new Error(`Play release notes for ${tooLong.language} are ${tooLong.text.length} chars; the limit is ${PLAY_NOTES_LIMIT}`);
    await play.send("PUT", `/edits/${edit.id}/tracks/production`, token, {
      track: "production",
      releases: [{ name: promotion.version, versionCodes: [code], status: "completed", releaseNotes }],
    });
    await play.send("POST", `/edits/${edit.id}:commit`, token);
  } catch (error) {
    // Uncommitted, the edit changed nothing live; drop it so it does not linger.
    await play.send("DELETE", `/edits/${edit.id}`, token).catch(() => {});
    throw error;
  }
  log(`committed production ${promotion.version} / ${code}`);

  const production = await inEdit(play, async (editId, fresh) => (await play.get(`/edits/${editId}/tracks/production`, fresh)) as PlayTrack);
  const live = (production.releases ?? []).find((release) => release.status === "completed" && (release.versionCodes ?? []).includes(code));
  if (!live) throw new Error(`production does not show ${promotion.version} / ${code} after the commit: ${JSON.stringify(production.releases ?? [])}`);
  log(`production reads ${live.name} / ${code} completed`);
}
