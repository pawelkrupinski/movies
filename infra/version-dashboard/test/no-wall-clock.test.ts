import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

/**
 * No test reads the wall clock (the movies repo's NoWallClockInTestsSpec, for the same reason): a
 * test that does passes or fails by the time of day it runs, and a fixture dated "now" is a time
 * bomb. Time logic takes an injected `now`, or the test pins it with vi.useFakeTimers +
 * vi.setSystemTime. oxlint has no no-restricted-syntax, so the rule lives here.
 */
const WALL_CLOCK = /\bDate\.now\(\)|new Date\(\s*\)|performance\.now\(\)/;

function testFiles(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true }).flatMap((entry) =>
    entry.isDirectory() ? testFiles(join(dir, entry.name)) : entry.name.endsWith(".ts") ? [join(dir, entry.name)] : [],
  );
}

describe("tests never read the wall clock", () => {
  it("finds no Date.now(), new Date() or performance.now() in any test", () => {
    const offenders = testFiles(import.meta.dirname)
      .filter((file) => !file.endsWith("no-wall-clock.test.ts"))
      .flatMap((file) =>
        readFileSync(file, "utf8")
          .split("\n")
          .map((line, index) => ({ file, line: index + 1, text: line.trim() }))
          .filter(({ text }) => WALL_CLOCK.test(text) && !text.startsWith("//") && !text.startsWith("*")),
      );
    expect(offenders).toEqual([]);
  });

  it("would catch one (positive control)", () => {
    expect(WALL_CLOCK.test("const t = Date.now();")).toBe(true);
    expect(WALL_CLOCK.test("vi.setSystemTime(new Date(0))")).toBe(false);
  });
});
