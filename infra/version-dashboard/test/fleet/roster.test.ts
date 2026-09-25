import { mkdirSync, mkdtempSync, rmSync, utimesSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { flakeFingerprint, RosterKeeper, ROSTER_RETRY_MS, type Machines } from "../../src/fleet/roster.js";

const CLOCK = 1_790_000_000_000;
const MONGO: Machines = { "mongo-1": { hostName: "mongo-1", privateAddress: "10.20.0.13" } };

/**
 * The roster half is a `nix eval` over every NixOS configuration -- 86 seconds on an idle laptop,
 * measured, almost none of it CPU. Running it on every read meant one was always in flight, until
 * every one of them hit the timeout and the page announced "roster unavailable" while every machine
 * was healthy. These pin what stopped that: an unchanged flake is not read again, and a read that
 * fails is neither retried at once nor allowed to empty the table.
 */
describe("the roster is read only when the flake changes", () => {
  let dir: string;
  let reads: string[];
  let fingerprint: string;
  let answer: { machines: Machines } | { error: string };
  let now: number;
  let changes: number;

  const keeper = (over: Partial<ConstructorParameters<typeof RosterKeeper>[0]> = {}) => new RosterKeeper({
    infraDir: "/infra",
    cacheFile: join(dir, "roster.json"),
    fingerprint: () => fingerprint,
    evaluate: async () => {
      reads.push(fingerprint);
      return answer;
    },
    now: () => now,
    onChange: () => changes++,
    ...over,
  });

  /** Reads, then waits for any background re-read it started. */
  async function settled(roster: RosterKeeper) {
    const read = await roster.read();
    await roster.pending();
    return read;
  }

  beforeEach(() => {
    dir = mkdtempSync(join(tmpdir(), "roster-"));
    reads = [];
    fingerprint = "flake-as-committed";
    answer = { machines: MONGO };
    now = CLOCK;
    changes = 0;
  });
  afterEach(() => rmSync(dir, { recursive: true, force: true }));

  it("reads an unchanged flake once however many times it is asked", async () => {
    const roster = keeper();
    for (let i = 0; i < 3; i++) expect(await roster.read()).toEqual({ machines: MONGO, error: null });
    expect(reads).toHaveLength(1);
  });

  it("reads an edited flake again, behind the page, and says when it has landed", async () => {
    const roster = keeper();
    await roster.read();
    fingerprint = "flake-with-a-fourth-host";
    await settled(roster);
    expect(reads).toHaveLength(2);
    expect(changes).toBe(1);
  });

  it("keeps the machines the last read returned when a re-read fails, and says how old they are", async () => {
    const roster = keeper();
    await roster.read();
    now = CLOCK + 600_000;
    fingerprint = "edited";
    answer = { error: "nix eval failed: timed out after 900s" };
    await settled(roster);
    const { machines, error } = await roster.read();
    expect(machines).toEqual(MONGO);
    expect(error).toContain("timed out after 900s");
    expect(error).toContain("the roster read 10m ago, which is the last one that evaluated");
  });

  it("does not repeat a failed read on the next ask", async () => {
    answer = { error: "nix eval failed: timed out after 240s" };
    const roster = keeper();
    const first = await roster.read();
    const second = await roster.read();
    expect(reads).toHaveLength(1);
    expect([first.machines, second.machines]).toEqual([{}, {}]);
    expect(second.error).toBe(first.error);
  });

  it("lets the retry floor expire", async () => {
    answer = { error: "nix eval failed: timed out after 240s" };
    const roster = keeper();
    await roster.read();
    now = CLOCK + ROSTER_RETRY_MS + 1;
    answer = { machines: MONGO };
    expect(await roster.read()).toEqual({ machines: MONGO, error: null });
    expect(reads).toHaveLength(2);
  });

  it("reports the error and no machines when a roster never loaded", async () => {
    answer = { error: "nix eval failed: timed out after 240s" };
    expect(await keeper().read()).toEqual({ machines: {}, error: "nix eval failed: timed out after 240s" });
  });

  it("outlives the process that read it", async () => {
    // The restart case, which is most of them: launchd's KeepAlive, a laptop rebooting.
    await keeper().read();
    expect(await keeper().read()).toEqual({ machines: MONGO, error: null });
    expect(reads).toHaveLength(1);
  });

  it("recalls the roster the Python page wrote (the file survives the port)", async () => {
    writeFileSync(join(dir, "roster.json"), JSON.stringify({ fingerprint, machines: MONGO, evaluated_at: CLOCK / 1000 - 60 }));
    expect(await keeper().read()).toEqual({ machines: MONGO, error: null });
    expect(reads).toEqual([]);
  });

  it("re-reads a remembered roster when the flake has moved on since", async () => {
    await keeper().read();
    fingerprint = "edited-while-the-dashboard-was-down";
    const read = await settled(keeper());
    expect(read.machines).toEqual(MONGO);
    expect(read.error).toContain("roster evaluation is pending");
    expect(reads).toHaveLength(2);
  });

  it("treats an unreadable cache file as no cache, not an error", async () => {
    writeFileSync(join(dir, "roster.json"), "{not json");
    expect(await keeper().read()).toEqual({ machines: MONGO, error: null });
  });

  it("does not hold up the page for a re-read, and runs only one at a time", async () => {
    let release: () => void = () => {};
    const roster = keeper({
      evaluate: async () => {
        reads.push(fingerprint);
        if (reads.length > 1) await new Promise<void>((wake) => (release = wake));
        return answer;
      },
    });
    await roster.read();
    fingerprint = "edited";
    const during = await roster.read(); // returns while the re-read is still held
    expect(during.machines).toEqual(MONGO);
    expect(during.error).toContain("pending");
    await roster.read();
    expect(reads).toHaveLength(2);
    release();
    await roster.pending();
    expect(await roster.read()).toEqual({ machines: MONGO, error: null });
    expect(reads).toHaveLength(2);
  });
});

/**
 * What counts as "the flake changed". It has to notice an edit to anything the evaluation reads,
 * and -- the reason it is not simply the repository's HEAD -- ignore the ~18k application files that
 * share this checkout with infra/.
 */
describe("the flake fingerprint", () => {
  let root: string;
  const LATER = new Date(CLOCK + 10_000);

  beforeEach(() => {
    root = mkdtempSync(join(tmpdir(), "flake-"));
    mkdirSync(join(root, "nix", "hosts", "mongo-1"), { recursive: true });
    mkdirSync(join(root, "web", "src"), { recursive: true });
    writeFileSync(join(root, "flake.nix"), "{ outputs = _: {}; }");
    writeFileSync(join(root, "flake.lock"), "{}");
    writeFileSync(join(root, "nix/hosts/mongo-1/default.nix"), '{ fleet.role = "mongo"; }');
    writeFileSync(join(root, "web/src/Application.scala"), "object Application");
  });
  afterEach(() => rmSync(root, { recursive: true, force: true }));

  it("changes on an edit to a host", () => {
    const before = flakeFingerprint(root);
    expect(flakeFingerprint(root)).toBe(before);
    const host = join(root, "nix/hosts/mongo-1/default.nix");
    writeFileSync(host, '{ fleet.role = "mongo"; fleet.environment = "prod"; }');
    utimesSync(host, LATER, LATER);
    expect(flakeFingerprint(root)).not.toBe(before);
  });

  it("does not change on a commit to the application sharing the checkout", () => {
    const before = flakeFingerprint(root);
    const scala = join(root, "web/src/Application.scala");
    writeFileSync(scala, "object Application { val changed = true }");
    utimesSync(scala, LATER, LATER);
    expect(flakeFingerprint(root)).toBe(before);
  });

  it("changes when a file it read is removed", () => {
    const before = flakeFingerprint(root);
    rmSync(join(root, "flake.lock"));
    expect(flakeFingerprint(root)).not.toBe(before);
  });
});
