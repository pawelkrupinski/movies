import { describe, expect, it } from "vitest";
import { HttpError } from "../../src/http.js";
import { deliveryUuid, existingBuild, iosStateFrom, planIosVersion, submitIos, syncPrivacyPolicyUrls } from "../../src/mobile-release/ios.js";
import { notesFrom } from "../../src/mobile-release/notes.js";
import { ascVersions, FakeAsc, fixture, fixtureJson, noSleep } from "./fakes.js";

const APP = "6792566321";
const BUILD = "e352007c-db06-4c71-9d33-a9a5aab21183";
const pace = { sleep: noSleep, log: () => {} };
const VERSIONS = `/v1/apps/${APP}/appStoreVersions?filter[platform]=IOS&limit=50`;
const DRAFTS = `/v1/reviewSubmissions?filter[app]=${APP}&filter[platform]=IOS&filter[state]=READY_FOR_REVIEW`;
const OPEN = `/v1/reviewSubmissions?filter[app]=${APP}&filter[platform]=IOS&filter[state]=WAITING_FOR_REVIEW,IN_REVIEW`;
const APP_INFOS = `/v1/apps/${APP}/appInfos?include=appInfoLocalizations`;
/** App Store Connect's real answer while 2.0.11 is live: one app info, READY_FOR_DISTRIBUTION, locked. */
const liveAppInfos = fixtureJson("asc-app-infos.json");

describe("reading the version records", () => {
  it("reads App Store Connect's real list: everything released, nothing pending", () => {
    const state = iosStateFrom(fixtureJson("asc-app-store-versions.json"));
    expect(state.pending).toBeNull();
    expect(state.released[0]).toBe("2.0.9");
    expect(state.released).toContain("2.0.5");
  });

  it("orders by createdDate itself and separates the one unreleased record", () => {
    const state = iosStateFrom(ascVersions(["old", "2.0.9", "READY_FOR_SALE", "2026-09-20"], ["new", "2.0.10", "PREPARE_FOR_SUBMISSION", "2026-09-25"]));
    expect(state.pending).toMatchObject({ id: "new", versionString: "2.0.10" });
    expect(state.released).toEqual(["2.0.9"]);
  });

  it("keeps a record left in an unsent review submission unreleased, so a rerun resumes it", () => {
    // 2.0.13: the run added it to a review submission, then timed out before submitting it.
    const state = iosStateFrom(ascVersions(["old", "2.0.12", "READY_FOR_SALE", "2026-10-15"], ["stuck", "2.0.13", "READY_FOR_REVIEW", "2026-10-21"]));
    expect(state.pending).toMatchObject({ id: "stuck", versionString: "2.0.13" });
    expect(state.released).toEqual(["2.0.12"]);
  });

  it("counts an approved-but-not-live record as released", () => {
    expect(iosStateFrom(ascVersions(["a", "2.0.10", "PENDING_DEVELOPER_RELEASE", "2026-09-25"])).released).toEqual(["2.0.10"]);
  });
});

describe("planIosVersion", () => {
  const withPending = (state: string) => iosStateFrom(ascVersions(["old", "2.0.9", "READY_FOR_SALE", "2026-09-20"], ["p", "2.0.10", state, "2026-09-25"]));

  it("creates a record when every one is released", () => {
    expect(planIosVersion(iosStateFrom(fixtureJson("asc-app-store-versions.json")), "2.0.10")).toEqual({ kind: "create" });
  });

  it("reuses a draft or a rejection, and cancels a review first", () => {
    expect(planIosVersion(withPending("PREPARE_FOR_SUBMISSION"), "2.0.11")).toMatchObject({ kind: "reuse", cancelReview: false, record: { id: "p" } });
    expect(planIosVersion(withPending("REJECTED"), "2.0.10")).toMatchObject({ kind: "reuse", cancelReview: false });
    expect(planIosVersion(withPending("WAITING_FOR_REVIEW"), "2.0.10")).toMatchObject({ kind: "reuse", cancelReview: true });
  });

  it("refuses to replace an approved version", () => {
    expect(() => planIosVersion(withPending("PENDING_DEVELOPER_RELEASE"), "2.0.11")).toThrow(/approved but not live/);
  });
});

describe("builds", () => {
  it("takes the build id from altool's real upload log", () => {
    expect(deliveryUuid(fixture("altool-upload.txt"))).toBe(BUILD);
    expect(deliveryUuid("UPLOAD FAILED")).toBeNull();
  });

  it("finds the newest usable build already uploaded for a version", async () => {
    const asc = new FakeAsc({
      [`GET /v1/builds?filter[app]=${APP}&filter[preReleaseVersion.version]=2.0.9&sort=-uploadedDate&limit=10`]: fixtureJson("asc-builds-2.0.9.json"),
    });
    expect(await existingBuild(asc, "2.0.9")).toBe(BUILD);
  });
});

describe("submitIos", () => {
  const localizations = { data: [
    { type: "appStoreVersionLocalizations", id: "loc-pl", attributes: { locale: "pl", whatsNew: null } },
    { type: "appStoreVersionLocalizations", id: "loc-en", attributes: { locale: "en-GB", whatsNew: "Kept." } },
  ] };
  const validBuild = fixtureJson("asc-build.json");

  it("waits out processing, renames the draft, attaches, fills empty notes and submits", async () => {
    const asc = new FakeAsc({
      [`GET /v1/builds/${BUILD}`]: (_: unknown, hit: number) => {
        if (hit === 1) throw new HttpError(`/v1/builds/${BUILD}`, 404, "NOT_FOUND");
        return hit === 2 ? { data: { attributes: { processingState: "PROCESSING" } } } : validBuild;
      },
      [`GET ${VERSIONS}`]: ascVersions(["old", "2.0.9", "READY_FOR_SALE", "2026-09-20"], ["draft", "2.0.10", "PREPARE_FOR_SUBMISSION", "2026-09-25"]),
      "PATCH /v1/appStoreVersions/draft": null,
      "PATCH /v1/appStoreVersions/draft/relationships/build": null,
      "GET /v1/appStoreVersions/draft/appStoreVersionLocalizations": localizations,
      "PATCH /v1/appStoreVersionLocalizations/loc-pl": null,
      [`GET ${APP_INFOS}`]: liveAppInfos,
      [`GET ${DRAFTS}`]: { data: [] },
      "POST /v1/reviewSubmissions": { data: { id: "sub-1" } },
      "GET /v1/reviewSubmissions/sub-1/items?include=appStoreVersion": { data: [] },
      "POST /v1/reviewSubmissionItems": { data: { id: "item-1" } },
      "PATCH /v1/reviewSubmissions/sub-1": null,
      "GET /v1/appStoreVersions/draft": { data: { id: "draft", attributes: { appStoreState: "WAITING_FOR_REVIEW" } } },
    });
    const lines: string[] = [];
    expect(await submitIos(asc, { version: "2.0.11", buildId: BUILD, notes: notesFrom(new Map()), overwriteNotes: false }, { ...pace, log: (line) => lines.push(line) })).toBe("sub-1");
    // Apple lists no build at all until processing starts (minutes); that wait must not be silent.
    expect(lines.slice(0, 2)).toEqual([
      `build ${BUILD} waiting for Apple to pick up the upload (not listed yet)`,
      `build ${BUILD} PROCESSING`,
    ]);
    expect(asc.writes()).toEqual([
      { method: "PATCH", path: "/v1/appStoreVersions/draft", body: { data: { type: "appStoreVersions", id: "draft", attributes: { versionString: "2.0.11" } } } },
      { method: "PATCH", path: "/v1/appStoreVersions/draft/relationships/build", body: { data: { type: "builds", id: BUILD } } },
      { method: "PATCH", path: "/v1/appStoreVersionLocalizations/loc-pl", body: { data: { type: "appStoreVersionLocalizations", id: "loc-pl", attributes: { whatsNew: "Poprawki błędów i ulepszenia." } } } },
      { method: "POST", path: "/v1/reviewSubmissions", body: { data: { type: "reviewSubmissions", attributes: { platform: "IOS" }, relationships: { app: { data: { type: "apps", id: APP } } } } } },
      { method: "POST", path: "/v1/reviewSubmissionItems", body: { data: { type: "reviewSubmissionItems", relationships: {
        reviewSubmission: { data: { type: "reviewSubmissions", id: "sub-1" } },
        appStoreVersion: { data: { type: "appStoreVersions", id: "draft" } },
      } } } },
      { method: "PATCH", path: "/v1/reviewSubmissions/sub-1", body: { data: { type: "reviewSubmissions", id: "sub-1", attributes: { submitted: true } } } },
    ]);
  });

  it("cancels a submission in review, then reuses the record and a draft submission", async () => {
    const asc = new FakeAsc({
      [`GET /v1/builds/${BUILD}`]: validBuild,
      [`GET ${VERSIONS}`]: ascVersions(["inreview", "2.0.10", "WAITING_FOR_REVIEW", "2026-09-25"]),
      "GET /v1/appStoreVersions/inreview/build": { data: { type: "builds", id: "an-older-build" } },
      [`GET ${OPEN}`]: { data: [{ id: "sub-old" }] },
      "PATCH /v1/reviewSubmissions/sub-old": null,
      "GET /v1/appStoreVersions/inreview": (_: unknown, hit: number) => ({ data: { id: "inreview", attributes: { appStoreState: hit === 1 ? "WAITING_FOR_REVIEW" : "DEVELOPER_REJECTED" } } }),
      "PATCH /v1/appStoreVersions/inreview/relationships/build": null,
      "GET /v1/appStoreVersions/inreview/appStoreVersionLocalizations": { data: [] },
      [`GET ${APP_INFOS}`]: liveAppInfos,
      [`GET ${DRAFTS}`]: { data: [{ id: "sub-draft" }] },
      "GET /v1/reviewSubmissions/sub-draft/items?include=appStoreVersion": { data: [{ id: "i", relationships: { appStoreVersion: { data: { id: "inreview" } } } }] },
      "PATCH /v1/reviewSubmissions/sub-draft": null,
    });
    await submitIos(asc, { version: "2.0.10", buildId: BUILD, notes: notesFrom(new Map()), overwriteNotes: false }, pace);
    expect(asc.writes().map((call) => `${call.method} ${call.path}`)).toEqual([
      "PATCH /v1/reviewSubmissions/sub-old",
      "PATCH /v1/appStoreVersions/inreview/relationships/build",
      "PATCH /v1/reviewSubmissions/sub-draft",
    ]);
  });

  it("leaves a review alone when it already holds this version and this build", async () => {
    const asc = new FakeAsc({
      [`GET /v1/builds/${BUILD}`]: validBuild,
      [`GET ${VERSIONS}`]: ascVersions(["inreview", "2.0.10", "WAITING_FOR_REVIEW", "2026-09-25"]),
      "GET /v1/appStoreVersions/inreview/build": { data: { type: "builds", id: BUILD } },
    });
    expect(await submitIos(asc, { version: "2.0.10", buildId: BUILD, notes: notesFrom(new Map()), overwriteNotes: false }, pace)).toBeNull();
    expect(asc.writes()).toEqual([]);
  });

  it("creates the record when every version is released, and passes Apple's readiness refusal on", async () => {
    const asc = new FakeAsc({
      [`GET /v1/builds/${BUILD}`]: validBuild,
      [`GET ${VERSIONS}`]: fixtureJson("asc-app-store-versions.json"),
      "POST /v1/appStoreVersions": { data: { id: "new" } },
      "PATCH /v1/appStoreVersions/new/relationships/build": null,
      "GET /v1/appStoreVersions/new/appStoreVersionLocalizations": { data: [] },
      [`GET ${APP_INFOS}`]: liveAppInfos,
      [`GET ${DRAFTS}`]: { data: [{ id: "sub-draft" }] },
      "GET /v1/reviewSubmissions/sub-draft/items?include=appStoreVersion": { data: [] },
      "POST /v1/reviewSubmissionItems": () => {
        throw new HttpError("/v1/reviewSubmissionItems", 409, '{"errors":[{"code":"STATE_ERROR.ENTITY_STATE_INVALID","meta":{"associatedErrors":{"copyright":["missing"]}}}]}');
      },
    });
    await expect(submitIos(asc, { version: "2.0.10", buildId: BUILD, notes: notesFrom(new Map()), overwriteNotes: false }, pace)).rejects.toThrow(/refused the version for review.*copyright/);
    expect(asc.calls.find((call) => call.path === "/v1/appStoreVersions")?.body).toEqual({
      data: { type: "appStoreVersions", attributes: { platform: "IOS", versionString: "2.0.10" }, relationships: { app: { data: { type: "apps", id: APP } } } },
    });
    expect(asc.writes().some((call) => call.path === "/v1/reviewSubmissions/sub-draft")).toBe(false);
  });

  it("stops on a build Apple rejected", async () => {
    const asc = new FakeAsc({ [`GET /v1/builds/${BUILD}`]: { data: { attributes: { processingState: "INVALID" } } } });
    await expect(submitIos(asc, { version: "2.0.10", buildId: BUILD, notes: notesFrom(new Map()), overwriteNotes: false }, pace)).rejects.toThrow(/INVALID/);
  });
});

describe("syncPrivacyPolicyUrls", () => {
  type Info = { id: string; attributes: Record<string, unknown>; relationships: { appInfoLocalizations: { data: { type: string; id: string }[] } } };
  type Localization = { id: string; attributes: Record<string, unknown> };
  /**
   * The real answer plus the draft app info Apple opens beside it once a new version exists: the
   * live record's localizations copied under new ids, the way App Store Connect carries them over.
   */
  const withDraft = (): unknown => {
    const live = structuredClone(liveAppInfos) as { data: Info[]; included: Localization[] };
    const draftLocalizations = live.included.map((localization) => ({ ...structuredClone(localization), id: `draft-${String(localization.attributes["locale"])}` }));
    draftLocalizations.push({ id: "draft-fr-FR", attributes: { locale: "fr-FR", privacyPolicyUrl: "https://example.test/fr" } });
    const draft = structuredClone(live.data[0]) as Info;
    draft.id = "draft-info";
    draft.attributes = { ...draft.attributes, appStoreState: "PREPARE_FOR_SUBMISSION", state: "PREPARE_FOR_SUBMISSION" };
    draft.relationships.appInfoLocalizations.data = draftLocalizations.map((localization) => ({ type: "appInfoLocalizations", id: localization.id }));
    // pl already right: it must not be rewritten.
    const pl = draftLocalizations.find((localization) => localization.attributes["locale"] === "pl");
    if (pl) pl.attributes["privacyPolicyUrl"] = "https://kinowo.net/privacy-policy?lang=pl";
    return { ...live, data: [...live.data, draft], included: [...live.included, ...draftLocalizations] };
  };

  it("points each locale of the editable app info at the policy in its own language", async () => {
    const asc = new FakeAsc({
      [`GET ${APP_INFOS}`]: withDraft(),
      "PATCH /v1/appInfoLocalizations/draft-es-ES": null,
      "PATCH /v1/appInfoLocalizations/draft-en-GB": null,
      "PATCH /v1/appInfoLocalizations/draft-de-DE": null,
    });
    await syncPrivacyPolicyUrls(asc, pace);
    const patch = (id: string, url: string) => ({
      method: "PATCH", path: `/v1/appInfoLocalizations/${id}`, body: { data: { type: "appInfoLocalizations", id, attributes: { privacyPolicyUrl: url } } },
    });
    expect(asc.writes()).toEqual([
      patch("draft-es-ES", "https://showtimes.cc/es/privacy-policy?lang=es"),
      patch("draft-en-GB", "https://showtimes.cc/uk/privacy-policy?lang=en"),
      patch("draft-de-DE", "https://showtimes.cc/de/privacy-policy?lang=de"),
    ]);
  });

  it("leaves the live app info alone: Apple locks it until a new version exists", async () => {
    const asc = new FakeAsc({ [`GET ${APP_INFOS}`]: liveAppInfos });
    const lines: string[] = [];
    await syncPrivacyPolicyUrls(asc, { ...pace, log: (line) => lines.push(line) });
    expect(asc.writes()).toEqual([]);
    expect(lines).toEqual(["privacy policy URLs: no editable app info, left as they are"]);
  });
});
