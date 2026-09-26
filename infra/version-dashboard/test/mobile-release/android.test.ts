import { describe, expect, it } from "vitest";
import { androidStateFrom, internalBuildFor, nextVersionCode, promoteAndroid, type PlayTrack } from "../../src/mobile-release/android.js";
import { notesFrom } from "../../src/mobile-release/notes.js";
import { FakePlay, fixtureJson } from "./fakes.js";

const tracks = () => (fixtureJson("play-tracks.json") as { tracks: PlayTrack[] }).tracks;

describe("reading the tracks", () => {
  it("reads Play's real tracks: production released, nothing version-named held back", () => {
    const state = androidStateFrom(tracks());
    expect(state.released).toEqual(["2.0.9"]);
    expect(state.unreleased).toEqual([]); // alpha's "Initial" is not a version
    expect(Math.max(...state.versionCodes)).toBe(1790075578);
  });

  it("picks a version code above every one Play has seen, and never below the clock", () => {
    const state = androidStateFrom(tracks());
    expect(nextVersionCode(state, 1_790_500_000)).toBe(1_790_500_000);
    expect(nextVersionCode(state, 1_700_000_000)).toBe(1_790_075_579);
  });

  it("finds the internal build a resumed run can promote", () => {
    expect(internalBuildFor(androidStateFrom(tracks()), "2.0.9")).toBe(1790075578);
    expect(internalBuildFor(androidStateFrom(tracks()), "2.0.10")).toBeNull();
  });
});

describe("promoteAndroid", () => {
  const CODE = 1_790_500_000;
  const internal = { track: "internal", releases: [{ name: "2.0.10", status: "completed", versionCodes: [String(CODE)] }] };
  const routes = (productionAfter: unknown) => ({
    "POST /edits": (_: unknown, hit: number) => ({ id: `edit-${hit}` }),
    "GET /edits/edit-1/tracks/internal": internal,
    "GET /edits/edit-1/listings": fixtureJson("play-listings.json"),
    "PUT /edits/edit-1/tracks/production": null,
    "POST /edits/edit-1:commit": { id: "edit-1" },
    "GET /edits/edit-2/tracks/production": productionAfter,
    "DELETE /edits/edit-2": null,
    "DELETE /edits/edit-1": null,
  });

  it("puts the internal build on production with notes per listing language, commits, and reads it back", async () => {
    const play = new FakePlay(routes({ track: "production", releases: [{ name: "2.0.10", status: "completed", versionCodes: [String(CODE)] }] }));
    await promoteAndroid(play, { version: "2.0.10", versionCode: CODE, notes: notesFrom(new Map([["en", "New cities."]])) }, () => {});
    expect(play.calls.find((call) => call.method === "PUT")?.body).toEqual({
      track: "production",
      releases: [{
        name: "2.0.10",
        versionCodes: [String(CODE)],
        status: "completed",
        releaseNotes: [
          { language: "pl-PL", text: "Poprawki błędów i ulepszenia." },
          { language: "en-GB", text: "New cities." },
          { language: "de-DE", text: "Fehlerbehebungen und Verbesserungen." },
          { language: "es-ES", text: "Corrección de errores y mejoras." },
        ],
      }],
    });
    expect(play.calls.map((call) => `${call.method} ${call.path}`)).toContain("POST /edits/edit-1:commit");
  });

  it("fails when production does not show the build after the commit -- a green commit is not proof", async () => {
    const play = new FakePlay(routes({ track: "production", releases: [{ name: "2.0.9", status: "completed", versionCodes: ["1790075578"] }] }));
    await expect(promoteAndroid(play, { version: "2.0.10", versionCode: CODE, notes: notesFrom(new Map()) }, () => {})).rejects.toThrow(/does not show 2.0.10/);
  });

  it("refuses a code that never reached internal, and drops the edit uncommitted", async () => {
    const play = new FakePlay(routes(null));
    await expect(promoteAndroid(play, { version: "2.0.10", versionCode: 42, notes: notesFrom(new Map()) }, () => {})).rejects.toThrow(/not on the internal track/);
    expect(play.writes().map((call) => `${call.method} ${call.path}`)).toEqual(["POST /edits", "DELETE /edits/edit-1"]);
  });

  it("refuses notes longer than Play allows, before touching production", async () => {
    const play = new FakePlay(routes(null));
    await expect(promoteAndroid(play, { version: "2.0.10", versionCode: CODE, notes: () => "x".repeat(501) }, () => {})).rejects.toThrow(/limit is 500/);
    expect(play.writes().some((call) => call.method === "PUT")).toBe(false);
  });
});
