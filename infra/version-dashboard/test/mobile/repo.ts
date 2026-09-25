import { execFileSync } from "node:child_process";
import { appendFileSync, mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

/**
 * A throwaway git repository, never this one: the mobile page shells out to real `git log`, and the
 * point of these tests is the exact-match anchoring and the per-directory scoping, which this
 * repository's own history cannot be relied on to exercise on demand.
 */
export class TempRepo {
  readonly root: string;

  constructor(root?: string) {
    this.root = root ?? mkdtempSync(join(tmpdir(), "mobile-dashboard-"));
    if (!root) {
      this.git("init", "-q");
      this.git("config", "user.email", "t@t");
      this.git("config", "user.name", "t");
      this.git("config", "commit.gpgsign", "false");
      this.git("config", "tag.gpgsign", "false");
    }
  }

  git(...args: string[]): string {
    return execFileSync("git", args, { cwd: this.root, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
  }

  commit(path: string, message: string): string {
    const full = join(this.root, path);
    mkdirSync(dirname(full), { recursive: true });
    appendFileSync(full, "x");
    this.git("add", path);
    this.git("commit", "-q", "-m", message);
    return this.git("rev-parse", "HEAD");
  }

  tag(name: string, sha: string): void {
    this.git("tag", "-f", name, sha);
  }

  currentBranch(): string {
    return this.git("rev-parse", "--abbrev-ref", "HEAD");
  }

  /** A commit on a branch that is never merged back; HEAD returns to where it was. */
  orphanCommit(path: string, message: string): string {
    const back = this.currentBranch();
    this.git("checkout", "-q", "-b", "never-merged");
    const sha = this.commit(path, message);
    this.git("checkout", "-q", back);
    return sha;
  }

  cleanup(): void {
    rmSync(this.root, { recursive: true, force: true });
  }
}
