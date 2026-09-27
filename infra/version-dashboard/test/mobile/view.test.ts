import { describe, expect, it } from "vitest";
import { emptyMobileState } from "../../src/mobile/model.js";
import { renderMobile } from "../../src/mobile/view.js";
import { NOW, state, upToDate } from "./states.js";

const render = (...args: Parameters<typeof state>) => renderMobile(state(...args), NOW + 90_000).__raw;

describe("renderMobile", () => {
  it("renders a fetch failure as an err box", () => {
    const out = render([{ name: "iOS", fetchFailed: true, error: "HttpError: HTTP 401 from x", networkError: false }]);
    expect(out).toContain("<div class=err>HttpError: HTTP 401 from x</div>");
  });

  it("says up to date, with the version and where it was released from", () => {
    const out = render([upToDate("iOS", "2.0.7")]);
    expect(out).toContain("<h2>iOS</h2>");
    expect(out).toContain("<b>2.0.7</b>");
    expect(out).toContain("released from <code>abc1234def</code>");
    expect(out).toContain("up to date");
  });

  it("renders unreleased commits as escaped table rows with the full sha on hover", () => {
    const out = render([{
      ...upToDate("Android", "2.0.6"),
      liveExtra: "309",
      commits: [{ sha: "deadbeef".repeat(5), short: "deadbee", date: "2026-09-14", subject: "Flatten the city picker's <search>" }],
    }]);
    expect(out).toContain(`<span class=sha title='${"deadbeef".repeat(5)}'>deadbee</span>`);
    expect(out).toContain("Flatten the city picker&#39;s &lt;search&gt;");
    expect(out).toContain("(309)");
    expect(out).toContain("1 commit(s) not yet released");
  });

  it("shows a pending submission separately from live", () => {
    const out = render([{ ...upToDate("iOS", "2.0.6"), pending: { version: "2.0.7", state: "WAITING_FOR_REVIEW" } }]);
    expect(out).toContain("<h2>iOS</h2><div class=sub><b>2.0.7</b>: submitted to Apple, will be live <span class=hint>(WAITING_FOR_REVIEW)</span></div><div class=sub><b>2.0.6</b>: live · released from");
  });

  it("names Google Play as where an Android version was submitted", () => {
    const out = render([{ ...upToDate("Android", "2.0.10"), pending: { version: "2.0.11", state: "draft" } }]);
    expect(out).toContain("<h2>Android</h2><div class=sub><b>2.0.11</b>: submitted to Google Play, will be live");
  });

  it("names the commit a pending submission was built from, and splits the commits by it", () => {
    const commit = (sha: string, subject: string) => ({ sha, short: sha.slice(0, 9), date: "2026-09-27", subject });
    const out = render([{
      ...upToDate("iOS", "2.0.10"),
      pending: { version: "2.0.11", state: "WAITING_FOR_REVIEW" },
      submitted: { commit: "acbbd2a1a29f73f7", includes: ["aaaaaaaaaaaa"] },
      commits: [commit("bbbbbbbbbbbb", "after the submission"), commit("aaaaaaaaaaaa", "Release mobile 2.0.11")],
    }]);
    expect(out).toContain("<b>2.0.11</b>: submitted to Apple, will be live <span class=hint>(WAITING_FOR_REVIEW)</span> from <code>acbbd2a1a2</code></div><div class=sub><b>2.0.10</b>: live");
    expect(out).toMatch(/<td>after the submission<\/td><td class=mut>not submitted<\/td>/);
    expect(out).toMatch(/<td>Release mobile 2.0.11<\/td><td>in 2.0.11<\/td>/);
    expect(out).toContain("2 commit(s) not yet released: 1 in 2.0.11, 1 not submitted yet");
  });

  it("makes never-released a note, not an err box", () => {
    const out = render([{ ...upToDate("Android"), liveVersion: null, baseline: null, commits: null, error: "never released to this store yet" }]);
    expect(out).toContain("<div class=note>never released to this store yet</div>");
    expect(out).not.toContain("class=err");
  });

  it("says when git history could not be read rather than calling it up to date", () => {
    const out = render([{ ...upToDate("iOS"), commits: null }]);
    expect(out).toContain("could not read git history");
    expect(out).not.toContain("up to date");
  });

  it("dates the build from the injected clock", () => {
    expect(render([upToDate("iOS")])).toContain("built in 0.8s, 1m ago");
  });

  it("keeps the last platforms visible under a failed rebuild, and says so", () => {
    const out = render([upToDate("iOS")], { buildError: "Error: git exploded" });
    expect(out).toContain("The last rebuild failed, so what is below is from the build before it: Error: git exploded");
    expect(out).toContain("<h2>iOS</h2>");
  });

  it("has a first-load state and a live-gated Refresh button", () => {
    const out = renderMobile(emptyMobileState(), NOW).__raw;
    expect(out).toContain("for the first time");
    expect(out).not.toContain("built in");
    expect(out).toContain("<button class=needs-live data-action=refresh>Refresh</button>");
  });
});
