/**
 * The mobile page's markup, as pure functions of the pushed state: the server renders the first
 * paint with these and the browser re-renders every snapshot with the SAME functions. Imports
 * nothing from node:.
 */
import { ago } from "../age.js";
import { html, type Raw } from "../html.js";
import type { MobileState, Platform } from "./model.js";

function platformBlock(platform: Platform): Raw {
  const heading = html`<h2>${platform.name}</h2>`;
  if (platform.fetchFailed) return html`${heading}<div class=err>${platform.error}</div>`;
  const live = html`<b>${platform.liveVersion ?? "?"}${platform.liveExtra ? html` <span class=hint>(${platform.liveExtra})</span>` : ""}</b>`;
  const pending = platform.pending
    ? html` · already submitted, not yet live: <b>${platform.pending.version ?? "?"}</b> <span class=hint>(${platform.pending.state ?? "?"})</span>`
    : "";
  const released = platform.baseline ? html` · released from <code>${platform.baseline.slice(0, 10)}</code>` : "";
  const line = html`${heading}<div class=sub>live: ${live}${pending}${released}</div>`;
  if (platform.error) return html`${line}<div class=note>${platform.error}</div>`;
  const commits = platform.commits;
  if (commits === null) return html`${line}<div class=note>could not read git history for this platform</div>`;
  if (!commits.length) {
    return html`${line}<div class=note>up to date — nothing merged here since the release that shipped the live version</div>`;
  }
  const rows = commits.map((commit) => html`<tr><td class=sv><span class=sha title='${commit.sha}'>${commit.short}</span></td><td class=mut>${commit.date}</td><td>${commit.subject}</td></tr>`);
  return html`${line}<table><tr><th>commit</th><th>date</th><th>subject</th></tr>${rows}</table><div class=sub>${commits.length} commit(s) not yet released</div>`;
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
  return html`<div id=mobile>${header}${failure}${state.platforms.map(platformBlock)}</div>`;
}
