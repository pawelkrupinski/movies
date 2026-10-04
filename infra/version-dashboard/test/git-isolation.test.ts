import { execFileSync } from "node:child_process";
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import { isolateGitEnvironment } from "./git-isolation.js";
import { TempRepo } from "./mobile/repo.js";

// A decoy stands in for the repository a git hook would have exported as GIT_DIR; a ~/.gitconfig
// that would name and sign every commit stands in for the developer's.
describe("the isolated git environment", () => {
  const made: TempRepo[] = [];
  afterEach(() => made.splice(0).forEach((repo) => repo.cleanup()));

  it("keeps a throwaway repository's commits out of an inherited GIT_DIR and away from ~/.gitconfig", () => {
    const decoy = new TempRepo();
    const scratch = new TempRepo();
    made.push(decoy, scratch);
    const head = decoy.commit("a", "the decoy's own history");
    const config = readFileSync(join(decoy.root, ".git/config"), "utf8");
    const home = mkdtempSync(join(tmpdir(), "git-isolation-home-"));
    writeFileSync(join(home, ".gitconfig"), "[user]\n\tname = Leaked\n[commit]\n\tgpgsign = true\n");
    const env: NodeJS.ProcessEnv = {
      ...process.env,
      HOME: home,
      GIT_DIR: join(decoy.root, ".git"),
      GIT_INDEX_FILE: join(decoy.root, ".git/index"),
      GIT_WORK_TREE: decoy.root,
    };
    delete env.GIT_CONFIG_GLOBAL;
    isolateGitEnvironment(env);
    try {
      mkdirSync(join(scratch.root, "d"));
      writeFileSync(join(scratch.root, "d/f"), "x");
      const git = (...args: string[]) =>
        execFileSync("git", args, { cwd: scratch.root, env, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
      git("add", "d/f");
      git("commit", "-q", "-m", "scratch");
      expect(decoy.git("rev-parse", "HEAD")).toBe(head);
      expect(readFileSync(join(decoy.root, ".git/config"), "utf8")).toBe(config);
      expect(scratch.git("log", "-1", "--format=%s %an")).toBe("scratch Spec");
    } finally {
      rmSync(home, { recursive: true, force: true });
    }
  });
});
