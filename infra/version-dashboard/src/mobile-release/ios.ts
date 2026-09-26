/**
 * The App Store half of a release, after ios-release.sh has uploaded the build: wait for Apple to
 * process it, put it on the one version record that is not yet released, and submit that for review.
 *
 * ONE UNRELEASED RECORD AT A TIME. Apple holds at most one iOS appStoreVersion that has not been
 * released, and it is always the one to use -- a draft (PREPARE_FOR_SUBMISSION), a rejection
 * (REJECTED / DEVELOPER_REJECTED / METADATA_REJECTED / INVALID_BINARY) or a submission still in
 * review. Creating another one next to it 409s, so it is renamed to the new version and gets the
 * new build. A submission in review is cancelled first: the build cannot be swapped while Apple
 * holds it, and cancelling leaves the record DEVELOPER_REJECTED, the same editable state as above.
 *
 * AN APPROVED RECORD STOPS THE LANE. PENDING_DEVELOPER_RELEASE and friends mean Apple has accepted
 * a version that has not gone live; replacing it would throw away an approval, so that is a
 * decision for a person, not for this script.
 */
import { HttpError } from "../mobile/network.js";
import { IOS_APP_ID, type AscApi, type Sleep } from "../mobile/stores.js";
import type { Notes } from "./notes.js";

export const EDITABLE = new Set(["PREPARE_FOR_SUBMISSION", "DEVELOPER_REJECTED", "REJECTED", "METADATA_REJECTED", "INVALID_BINARY"]);
export const IN_REVIEW = new Set(["WAITING_FOR_REVIEW", "IN_REVIEW"]);
export const APPROVED = new Set(["PENDING_DEVELOPER_RELEASE", "PENDING_APPLE_RELEASE", "PROCESSING_FOR_APP_STORE", "PROCESSING_FOR_DISTRIBUTION", "ACCEPTED"]);

export interface IosVersionRecord {
  readonly id: string;
  readonly versionString: string;
  readonly state: string;
  readonly createdDate: string;
}

export interface IosState {
  /** Newest first. */
  readonly records: readonly IosVersionRecord[];
  /** Version strings that reached users or were approved. */
  readonly released: readonly string[];
  /** The one record not yet released, if any. */
  readonly pending: IosVersionRecord | null;
}

interface JsonApiList {
  readonly data?: readonly { readonly id: string; readonly attributes?: Record<string, unknown>; readonly relationships?: Record<string, { data?: { id: string } | null }> }[];
}

const text = (value: unknown): string => (typeof value === "string" ? value : "");

export function iosStateFrom(json: unknown): IosState {
  const records = ((json as JsonApiList).data ?? [])
    .map((item) => ({
      id: item.id,
      versionString: text(item.attributes?.["versionString"]),
      // appStoreState is deprecated in favour of appVersionState, but they agree on every state
      // this lane branches on; read either so the rename cannot silently blank it.
      state: text(item.attributes?.["appStoreState"]) || text(item.attributes?.["appVersionState"]),
      createdDate: text(item.attributes?.["createdDate"]),
    }))
    .sort((a, b) => (a.createdDate < b.createdDate ? 1 : a.createdDate > b.createdDate ? -1 : 0));
  const pending = records.find((record) => EDITABLE.has(record.state) || IN_REVIEW.has(record.state) || APPROVED.has(record.state)) ?? null;
  const released = records.filter((record) => record !== pending || APPROVED.has(record.state)).map((record) => record.versionString);
  return { records, released, pending };
}

export async function inspectIos(asc: AscApi): Promise<IosState> {
  return iosStateFrom(await asc.get(`/v1/apps/${IOS_APP_ID}/appStoreVersions?filter[platform]=IOS&limit=50`));
}

export type IosVersionPlan =
  | { readonly kind: "create" }
  | { readonly kind: "reuse"; readonly record: IosVersionRecord; readonly cancelReview: boolean };

/** What to do with the version records to get `version` onto one that can be submitted. */
export function planIosVersion(state: IosState, version: string): IosVersionPlan {
  const { pending } = state;
  if (pending && APPROVED.has(pending.state)) {
    throw new Error(`iOS ${pending.versionString} is ${pending.state} -- approved but not live; release or reject it in App Store Connect first`);
  }
  if (pending) return { kind: "reuse", record: pending, cancelReview: IN_REVIEW.has(pending.state) };
  if (state.released.includes(version)) throw new Error(`iOS ${version} is already released`);
  return { kind: "create" };
}

/** Human-readable, for the plan printed before anything runs. */
export function describeIosPlan(plan: IosVersionPlan, version: string): string {
  if (plan.kind === "create") return `create App Store version ${version}`;
  const { record } = plan;
  const rename = record.versionString === version ? "" : `, renamed ${record.versionString} → ${version}`;
  return `${plan.cancelReview ? "cancel the review of" : "reuse"} ${record.state} record ${record.versionString}${rename}`;
}

/** The build id altool reports: the Delivery UUID IS the App Store Connect build id. */
export function deliveryUuid(uploadLog: string): string | null {
  return /Delivery UUID:\s*([0-9a-f-]{36})/i.exec(uploadLog)?.[1] ?? null;
}

export interface Pace {
  readonly sleep: Sleep;
  readonly log: (line: string) => void;
}

async function poll<T>(pace: Pace, what: string, intervalMs: number, attempts: number, check: () => Promise<T | null>): Promise<T> {
  for (let attempt = 1; attempt <= attempts; attempt++) {
    const done = await check();
    if (done !== null) return done;
    if (attempt < attempts) await pace.sleep(intervalMs);
  }
  throw new Error(`gave up waiting for ${what} after ${Math.round((intervalMs * (attempts - 1)) / 60_000)} min`);
}

/**
 * Wait until Apple has processed `buildId` into a VALID build. It is not in /v1/builds AT ALL until
 * processing starts, which has taken from one to ten minutes -- a 404 is "not yet", not a failure.
 */
export async function waitForBuild(asc: AscApi, buildId: string, pace: Pace): Promise<void> {
  await poll(pace, `build ${buildId} to finish processing`, 30_000, 61, async () => {
    let build: { data?: { attributes?: Record<string, unknown> } };
    try {
      build = (await asc.get(`/v1/builds/${buildId}`)) as typeof build;
    } catch (error) {
      if (error instanceof HttpError && error.status === 404) {
        pace.log(`build ${buildId} waiting for Apple to pick up the upload (not listed yet)`);
        return null;
      }
      throw error;
    }
    const state = text(build.data?.attributes?.["processingState"]);
    if (state === "VALID") {
      if (build.data?.attributes?.["expired"] === true) throw new Error(`build ${buildId} has expired`);
      return true;
    }
    if (state === "FAILED" || state === "INVALID") throw new Error(`build ${buildId} is ${state}`);
    pace.log(`build ${buildId} ${state || "listed, no processing state yet"}`);
    return null;
  });
}

/** The newest usable (processed, unexpired) build already uploaded for `version`, for a resumed run. */
export async function existingBuild(asc: AscApi, version: string): Promise<string | null> {
  const builds = (await asc.get(
    `/v1/builds?filter[app]=${IOS_APP_ID}&filter[preReleaseVersion.version]=${encodeURIComponent(version)}&sort=-uploadedDate&limit=10`,
  )) as JsonApiList;
  const usable = (builds.data ?? []).find((build) => build.attributes?.["expired"] !== true && !["FAILED", "INVALID"].includes(text(build.attributes?.["processingState"])));
  return usable?.id ?? null;
}

async function stateOf(asc: AscApi, versionId: string): Promise<string> {
  const record = (await asc.get(`/v1/appStoreVersions/${versionId}`)) as { data?: unknown };
  return iosStateFrom({ data: [record.data] }).records[0]?.state ?? "";
}

const OPEN_REVIEW_STATES = "WAITING_FOR_REVIEW,IN_REVIEW";

async function cancelReview(asc: AscApi, record: IosVersionRecord, pace: Pace): Promise<void> {
  const submissions = (await asc.get(`/v1/reviewSubmissions?filter[app]=${IOS_APP_ID}&filter[platform]=IOS&filter[state]=${OPEN_REVIEW_STATES}`)) as JsonApiList;
  for (const submission of submissions.data ?? []) {
    pace.log(`cancelling review submission ${submission.id}`);
    await asc.send("PATCH", `/v1/reviewSubmissions/${submission.id}`, { data: { type: "reviewSubmissions", id: submission.id, attributes: { canceled: true } } });
  }
  await poll(pace, `${record.versionString} to leave review`, 5_000, 37, async () => {
    return EDITABLE.has(await stateOf(asc, record.id)) ? true : null;
  });
}

async function versionRecordFor(asc: AscApi, plan: IosVersionPlan, version: string, pace: Pace): Promise<string> {
  if (plan.kind === "create") {
    const created = (await asc.send("POST", "/v1/appStoreVersions", {
      data: {
        type: "appStoreVersions",
        attributes: { platform: "IOS", versionString: version },
        relationships: { app: { data: { type: "apps", id: IOS_APP_ID } } },
      },
    })) as { data: { id: string } };
    pace.log(`created App Store version ${version} (${created.data.id})`);
    return created.data.id;
  }
  const { record } = plan;
  if (plan.cancelReview) await cancelReview(asc, record, pace);
  if (record.versionString !== version) {
    await asc.send("PATCH", `/v1/appStoreVersions/${record.id}`, { data: { type: "appStoreVersions", id: record.id, attributes: { versionString: version } } });
    pace.log(`renamed ${record.versionString} → ${version}`);
  }
  return record.id;
}

/** Fill every locale's whatsNew that is empty; an explicit --notes-dir overwrites. */
async function fillWhatsNew(asc: AscApi, versionId: string, notes: Notes, overwrite: boolean, pace: Pace): Promise<void> {
  const localizations = (await asc.get(`/v1/appStoreVersions/${versionId}/appStoreVersionLocalizations`)) as JsonApiList;
  for (const localization of localizations.data ?? []) {
    const locale = text(localization.attributes?.["locale"]);
    if (!overwrite && text(localization.attributes?.["whatsNew"]).trim()) continue;
    await asc.send("PATCH", `/v1/appStoreVersionLocalizations/${localization.id}`, {
      data: { type: "appStoreVersionLocalizations", id: localization.id, attributes: { whatsNew: notes(locale) } },
    });
    pace.log(`whatsNew[${locale}] set`);
  }
}

/**
 * Submit `versionId` for review, reusing a draft review submission if one exists -- a draft left by
 * a failed attempt cannot be deleted (DELETE 403s), only filled and submitted.
 */
async function submit(asc: AscApi, versionId: string, pace: Pace): Promise<string> {
  const drafts = (await asc.get(`/v1/reviewSubmissions?filter[app]=${IOS_APP_ID}&filter[platform]=IOS&filter[state]=READY_FOR_REVIEW`)) as JsonApiList;
  let submissionId = drafts.data?.[0]?.id;
  if (submissionId) pace.log(`reusing draft review submission ${submissionId}`);
  else {
    const created = (await asc.send("POST", "/v1/reviewSubmissions", {
      data: { type: "reviewSubmissions", attributes: { platform: "IOS" }, relationships: { app: { data: { type: "apps", id: IOS_APP_ID } } } },
    })) as { data: { id: string } };
    submissionId = created.data.id;
  }
  const items = (await asc.get(`/v1/reviewSubmissions/${submissionId}/items?include=appStoreVersion`)) as JsonApiList;
  const alreadyIn = (items.data ?? []).some((item) => item.relationships?.["appStoreVersion"]?.data?.id === versionId);
  if (!alreadyIn) {
    try {
      await asc.send("POST", "/v1/reviewSubmissionItems", {
        data: {
          type: "reviewSubmissionItems",
          relationships: {
            reviewSubmission: { data: { type: "reviewSubmissions", id: submissionId } },
            appStoreVersion: { data: { type: "appStoreVersions", id: versionId } },
          },
        },
      });
    } catch (error) {
      // A 409 here is Apple's readiness check: its body names every missing field (copyright,
      // supportUrl, DAC7, ...). It is the one place those surface, so it is passed on whole.
      if (error instanceof HttpError && error.status === 409) throw new Error(`App Store refused the version for review -- ${error.message}`);
      throw error;
    }
  }
  await asc.send("PATCH", `/v1/reviewSubmissions/${submissionId}`, { data: { type: "reviewSubmissions", id: submissionId, attributes: { submitted: true } } });
  return submissionId;
}

export interface IosSubmission {
  readonly version: string;
  readonly buildId: string;
  readonly notes: Notes;
  /** --notes-dir was given: replace whatsNew even where a locale already has one. */
  readonly overwriteNotes: boolean;
}

/**
 * Everything after the upload. Returns the review submission id, or null when Apple is already
 * reviewing this version with this build -- a rerun must not cancel that and lose its queue place.
 */
export async function submitIos(asc: AscApi, release: IosSubmission, pace: Pace): Promise<string | null> {
  await waitForBuild(asc, release.buildId, pace);
  pace.log(`build ${release.buildId} VALID`);
  // Re-read now, not at the start of the run: the build took minutes, and the plan has to act on
  // the records as they are, not as they were before the archive.
  const plan = planIosVersion(await inspectIos(asc), release.version);
  if (plan.kind === "reuse" && plan.cancelReview && plan.record.versionString === release.version) {
    const attached = (await asc.get(`/v1/appStoreVersions/${plan.record.id}/build`)) as { data?: { id?: string } | null };
    if (attached.data?.id === release.buildId) {
      pace.log(`${release.version} is already ${plan.record.state} with build ${release.buildId}; leaving it`);
      return null;
    }
  }
  const versionId = await versionRecordFor(asc, plan, release.version, pace);
  await asc.send("PATCH", `/v1/appStoreVersions/${versionId}/relationships/build`, { data: { type: "builds", id: release.buildId } });
  pace.log(`build attached to ${release.version}`);
  await fillWhatsNew(asc, versionId, release.notes, release.overwriteNotes, pace);
  const submissionId = await submit(asc, versionId, pace);
  pace.log(`submitted for review (submission ${submissionId}); version is ${await stateOf(asc, versionId)}`);
  return submissionId;
}
