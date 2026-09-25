import { describe, expect, it } from "vitest";
import type { MachineRow } from "../../src/fleet/model.js";
import { FLEET_COLUMNS, renderFleet } from "../../src/fleet/view.js";
import { CLOSURE, machine, NOW, silent, stateOf } from "./fixtures.js";

/**
 * The seven columns the sibling dashboard at :8787 renders, in its order. THE POINT OF PINNING THEM
 * IS THAT THE TWO SCREENS ARE READ BY THE SAME PERSON, minutes apart, and a column that moves
 * between them is read as a different fact rather than the same one in a different place.
 */
const REFERENCE_COLUMNS = ["machine", "role · env", "address", "closure", "nixpkgs", "auto-apply", "state"];

const page = (...rows: MachineRow[]) => renderFleet(stateOf(rows), NOW).__raw;

function headers(markup: string): string[] {
  const row = /<table class=fleet><tr>(.*?)<\/tr>/s.exec(markup)?.[1] ?? "";
  return [...row.matchAll(/<th>(.*?)<\/th>/gs)].map((cell) => (cell[1] ?? "").replace(/<[^>]+>/g, "").replace("&middot;", "·").trim());
}

describe("columns", () => {
  it("match the reference dashboard", () => {
    expect(headers(page(machine()))).toEqual(REFERENCE_COLUMNS);
  });

  it("carry the environment as a coloured pill in the role cell", () => {
    expect(page(machine({ role: "mongo", env: "prod" }))).toContain("<td>mongo <span class=env style='background:#e5484d'>prod</span></td>");
  });

  it("give an unrecognised environment the neutral colour, not an invented one", () => {
    expect(page(machine({ env: "staging-2" }))).toContain("background:#5b6472'>staging-2<");
  });

  it("show twelve characters of the closure with the whole store name on hover", () => {
    expect(page(machine())).toContain(`title='${CLOSURE}'>6idh361s36gw`);
  });

  it("still show a booted closure that differs", () => {
    expect(page(machine({ booted: "aaaaaaaaaaaabbbb2p0v4x8n1m7c3jt5-nixos-system-mongo-1-26.05" }))).toContain("booted aaaaaaaaaaaa");
  });

  it("render auto-apply as a badge and a last-pass line, keeping the raw verdict on hover", () => {
    const markup = page(machine({ autoApply: "applied" }));
    expect(markup).toContain(">on</span><span class=hint>last pass 10m ago</span>");
    expect(markup).toContain("Last verdict: applied.");
  });

  it("do not report dry-run as auto-apply being on", () => {
    const markup = page(machine({ autoApply: "blocked", blockedReason: "dry_run" }));
    expect(markup).toContain(">dry-run</span>");
    expect(markup).not.toContain(">on</span>");
  });

  it("tell a host excluded on purpose from a host nobody wired up", () => {
    const excluded = page(machine({ excludedReason: "k3s drains its own workloads" }));
    expect(excluded).toContain(">excluded</span>");
    expect(excluded).toContain("k3s drains its own workloads");
    expect(page(machine({ applyCovered: false }))).toContain(">not covered</span>");
  });
});

describe("the revision, folded into the state cell", () => {
  it("has no column of its own", () => {
    expect(headers(page(machine()))).not.toContain("revision");
  });

  it("names the revision and its distance from main under the state badge", () => {
    expect(page(machine({ revisionShort: "aaa92e6e2", behind: 0 }))).toContain("<span class=sha>aaa92e6e2</span> on main");
  });

  it("says how far behind main", () => {
    expect(page(machine({ behind: 3 }))).toContain("</span> 3 behind main");
  });

  it("never renders an unmeasurable distance as being on main", () => {
    const markup = page(machine({ behind: null }));
    expect(markup).toContain("distance from main unknown");
    expect(markup).not.toContain("on main");
  });

  it("says a closure with no commit is unmeasurable rather than saying nothing", () => {
    expect(page(machine({ revisionShort: "", revision: "" }))).toContain("unmeasurable");
  });

  it("still names a staged revision", () => {
    expect(page(machine({ stagedShort: "9446b50f", state: "staged, not activated", stateKey: "staged", severity: "warn" })))
      .toContain("staged <span class=sha>9446b50f</span>");
  });

  it("keeps built-dirty as a state badge", () => {
    expect(page(machine({ dirty: true, state: "built dirty", stateKey: "dirty", severity: "warn" }))).toContain(">built dirty</span>");
  });
});

describe("a silent host", () => {
  it("renders dashes, not blanks", () => {
    const markup = page(silent());
    expect(markup.split("<td class=none>&mdash;</td>")).toHaveLength(4);
    expect(markup).toContain("what it runs is UNKNOWN, which is not the same as behind");
  });

  it("is never phrased as a distance from main", () => {
    expect(page(silent())).not.toContain("behind main");
  });
});

describe("the page", () => {
  it("spans the console row across exactly the table", () => {
    const markup = page(machine({ actionable: true, state: "staged, not activated", stateKey: "staged", severity: "warn" }));
    expect(FLEET_COLUMNS).toBe(headers(markup).length);
    expect(markup).toContain(`colspan='${FLEET_COLUMNS}'`);
  });

  it("cannot be broken out of by a hostile label", () => {
    const markup = page(machine({ role: "<script>x</script>", excludedReason: "a'b" }));
    expect(markup).not.toContain("<script>x</script>");
    expect(markup).toContain("&lt;script&gt;");
    expect(markup).toContain("a&#39;b");
  });

  it("says a failed roster is not an empty fleet", () => {
    expect(page()).toContain("<strong>roster unavailable</strong>");
    expect(page(machine())).toContain("1 declared host(s): 1 current, 0 needing attention, 0 not reporting");
  });

  it("says out loud when every Prometheus read has been failing for minutes", () => {
    const stale = renderFleet(stateOf([machine()], { readAt: (NOW - 301) * 1000 }), NOW).__raw;
    expect(stale).toContain("last read 301s ago");
    expect(page(machine())).not.toContain("every read since has failed");
  });

  it("shows every error, the dirty checkout and undeclared scrape targets", () => {
    const markup = renderFleet(stateOf([machine()], { errors: ["nix eval failed: boom"], dirtyCheckout: true, undeclared: ["10.20.0.99"] }), NOW).__raw;
    expect(markup).toContain("<div class='err'>nix eval failed: boom</div>");
    expect(markup).toContain("uncommitted changes");
    expect(markup).toContain("not declared in the flake: 10.20.0.99");
  });

  it("says it is loading, not empty, before the first read lands", () => {
    expect(renderFleet({ ...stateOf([]), ready: false }, NOW).__raw).toContain("still being read for the first time");
  });

  it("says it knows nothing when the state could not be built", () => {
    expect(renderFleet({ ...stateOf([machine()]), buildError: "boom" }, NOW).__raw).toContain("telling you NOTHING about the fleet");
  });
});

describe("the bulk button", () => {
  it("is absent when nothing is staged anywhere", () => {
    expect(page(machine(), machine({ name: "monitoring-1" }))).not.toContain("id=fleetbulkbtn");
  });

  it("counts the machines with something staged", () => {
    expect(page(machine({ name: "mongo-1", actionable: true }), machine({ name: "monitoring-1", actionable: true }), machine({ name: "k3s-worker-1" })))
      .toContain("Bring all to latest… (2)</button>");
  });

  it("can never disagree with the buttons below it", () => {
    // BOTH READ THE SAME `actionable` FLAG. A second, independently derived count is exactly how
    // the header comes to promise a fleet needs three switches while the table offers two.
    const markup = page(...Array.from({ length: 6 }, (_, index) => machine({ name: `h${index}`, actionable: index % 2 === 0 })));
    expect(markup).toContain("Bring all to latest… (3)</button>");
    expect(markup.split("Bring to latest…</button>")).toHaveLength(4);
  });

  it("has a console of its own", () => {
    expect(page(machine({ actionable: true }))).toContain("<details id=fleetbulkcons");
  });
});

describe("the per-machine button", () => {
  it("is named the same as on the sibling dashboard", () => {
    const markup = page(machine({ actionable: true }));
    expect(markup).toContain("Bring to latest…</button>");
    expect(markup).not.toContain(">Activate the staged closure");
  });

  it("carries what the bulk run selects on", () => {
    const markup = page(machine({ actionable: true, public: "1.2.3.4", env: "prod" }));
    expect(markup).toContain("data-machine='mongo-1'");
    expect(markup).toContain("data-address='1.2.3.4'");
    expect(markup).toContain("data-env='prod'");
  });

  it("marks the production database dangerous", () => {
    const markup = page(machine({ name: "mongo-1", role: "mongo", actionable: true }));
    expect(markup).toContain("data-danger='1'");
    expect(markup).toContain("production database");
  });

  it("does not mark an ordinary host", () => {
    expect(page(machine({ role: "k3s-worker", actionable: true }))).toContain("data-danger=''");
  });
});
