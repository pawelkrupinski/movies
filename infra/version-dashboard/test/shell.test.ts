import { describe, expect, it } from "vitest";
import type { Page } from "../src/page.js";
import { shell } from "../src/shell.js";
import { Store } from "../src/store.js";

const pageOf = (name: string, path: string, state: unknown): Page<unknown> => ({
  name, path, title: name.toUpperCase(), store: new Store(state, () => 0),
  start: () => {}, stop: () => {}, renderBody: () => "<p>body</p>",
});

describe("the page shell", () => {
  const fleet = pageOf("fleet", "/nixos", { note: "</script><script>alert(1)</script>" });
  const mobile = pageOf("mobile", "/mobile", {});
  const markup = shell(fleet, [fleet, mobile]);

  it("links every page and marks the current one", () => {
    expect(markup).toContain('<a href="/nixos" class=current>FLEET</a><a href="/mobile">MOBILE</a>');
    expect(markup).toContain("<title>kinowo — FLEET</title>");
  });

  it("embeds the first snapshot without letting its text close the script it sits in", () => {
    expect(markup).not.toContain("</script><script>alert(1)");
    expect(markup).toContain("\\u003c/script>");
    expect(markup).toContain('<script type=module src="/assets/fleet.js">');
  });
});
