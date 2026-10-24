/**
 * The mobile page's markup, as pure functions of the pushed state: the server renders the first
 * paint with these and the browser re-renders every snapshot with the SAME functions. Imports
 * nothing from node:.
 */
import { ago } from "../age.js";
import { html, type Raw } from "../html.js";
import { NOTES_LANGUAGES, PLAY_NOTES_LIMIT, plannedVersion, type Commit, type MobileState, type Platform, type ReleaseRun } from "./model.js";

/** Where each platform's pending version waits to go live. */
const STORE_OF: Readonly<Record<string, string>> = { iOS: "Apple", Android: "Google Play" };

function platformBlock(platform: Platform): Raw {
  const heading = html`<h2>${platform.name}</h2>`;
  if (platform.fetchFailed) {
    const unlock = platform.vaultLocked ? html` <button class=needs-live data-action=unlock-vault>Unlock with Touch ID</button>` : "";
    return html`${heading}<div class=err>${platform.error}${unlock}</div>`;
  }
  const submitted = platform.submitted;
  const pendingVersion = platform.pending?.version ?? "?";
  // Its own line, ABOVE live: the version on its way out is what this page is usually opened for.
  const pending = platform.pending
    ? html`<div class=sub><b>${pendingVersion}</b>: submitted to ${STORE_OF[platform.name] ?? "the store"}, will be live <span class=hint>(${platform.pending.state ?? "?"})</span>${submitted ? html` from <code>${submitted.commit.slice(0, 10)}</code>` : ""}</div>`
    : "";
  const released = platform.baseline ? html` · released from <code>${platform.baseline.slice(0, 10)}</code>` : "";
  const liveExtra = platform.liveExtra ? html` <span class=hint>(${platform.liveExtra})</span>` : "";
  const line = html`${heading}${pending}<div class=sub><b>${platform.liveVersion ?? "?"}</b>: live${liveExtra}${released}</div>`;
  if (platform.error) return html`${line}<div class=note>${platform.error}</div>`;
  const commits = platform.commits;
  if (commits === null) return html`${line}<div class=note>could not read git history for this platform</div>`;
  if (!commits.length) {
    return html`${line}<div class=note>up to date — nothing merged here since the release that shipped the live version</div>`;
  }
  const cells = (commit: Commit) => html`<td class=sv><span class=sha title='${commit.sha}'>${commit.short}</span></td><td class=mut>${commit.date}</td><td>${commit.subject}</td>`;
  if (!submitted) {
    const rows = commits.map((commit) => html`<tr>${cells(commit)}</tr>`);
    return html`${line}<table><tr><th>commit</th><th>date</th><th>subject</th></tr>${rows}</table><div class=sub>${commits.length} commit(s) not yet released</div>`;
  }
  const inBuild = new Set(submitted.includes);
  const rows = commits.map((commit) => html`<tr>${cells(commit)}${inBuild.has(commit.sha) ? html`<td>in ${pendingVersion}</td>` : html`<td class=mut>not submitted</td>`}</tr>`);
  const carried = commits.filter((commit) => inBuild.has(commit.sha)).length;
  return html`${line}<table><tr><th>commit</th><th>date</th><th>subject</th><th>submission</th></tr>${rows}</table><div class=sub>${commits.length} commit(s) not yet released: ${carried} in ${pendingVersion}, ${commits.length - carried} not submitted yet</div>`;
}

/**
 * The release button and, once pressed, its run: the dry run's plan and, under a plan with a version
 * to ship, the second press that ships it. Pressing again starts a fresh dry run.
 */
function releaseBlock(run: ReleaseRun | undefined, now: number): Raw {
  const running = !!run && !run.done;
  const planButton = html`<button class='applybtn needs-live' data-action=release-plan${running ? " disabled" : ""}>Release to the App Store and Google Play…</button>`;
  const intro = html`<span class=hint>runs <code>scripts/mobile-ship.sh --dry-run</code> first: it reads both stores and says what it would ship, changing nothing</span>`;
  if (!run) return html`<div class=actionrow>${planButton}${intro}</div>`;
  const what = run.mode === "plan" ? "dry run" : "release";
  const status = !run.done ? "running…" : run.exit === 0 ? "finished" : `failed (exit ${run.exit ?? "abnormal"})`;
  const version = plannedVersion(run);
  const notes = NOTES_LANGUAGES.map((language) => html`<label class=notes><span>${language}</span><textarea data-notes=${language} maxlength=${PLAY_NOTES_LIMIT} rows=2 placeholder="What's new — blank ships the generic line"></textarea></label>`);
  const ship = version
    ? html`<div class=notesrow>${notes}</div><div class=actionrow><button class='applybtn go needs-live' data-action=release>Ship ${version} to both stores</button><span class=hint>builds both apps from one commit, submits iOS for review, then promotes Android to production — a production release cannot be taken back</span></div>`
    : "";
  const output = html`<pre class=out>${run.lines.join("\n")}</pre>`;
  return html`<div class=actionrow>${planButton}</div><details class=cons open><summary>${what} started ${ago(run.startedAt / 1000, now / 1000)}: ${status}</summary>${output}</details>${ship}`;
}

/** The whole page body for one state. `now` in epoch ms. */
export function renderMobile(state: MobileState, now: number): Raw {
  const built = state.ready ? html` · built in ${state.took.toFixed(1)}s, ${ago(state.builtAt / 1000, now / 1000)}` : "";
  const header = html`<div class=toolbar><div class=sub>what's in <code>ios/</code> and <code>android/</code> that neither store has shipped yet, diffed from each platform's OWN last released version${built}</div><button class=needs-live data-action=refresh>Refresh</button><span id=refreshnote class=hint></span></div>`;
  if (!state.ready) {
    return html`<div id=mobile>${header}<div class=note>Asking the App Store, Play and git for the first time — it appears here by itself when it is done.</div></div>`;
  }
  const failure = state.buildError
    ? html`<div class=err>The last rebuild failed, so what is below is from the build before it: ${state.buildError}</div>`
    : "";
  return html`<div id=mobile>${header}${failure}<div class=release>${releaseBlock(state.release, now)}</div>${state.platforms.map(platformBlock)}</div>`;
}
