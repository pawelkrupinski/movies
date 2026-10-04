/**
 * Strips `env` of the git environment a test process may have inherited, the TypeScript twin of the
 * repository's scripts/scratch-git.sh. Every worktree of this clone and every agent session commits
 * through ONE shared `.git`; a test started under a git hook (or from a shell that exported GIT_DIR)
 * hands its children that GIT_DIR, which beats the directory a `git` runs in, so a throwaway
 * repository's commits and `git config` would land in the shared repository (2026-10-04: a spec wrote
 * user.name=Spec and core.bare=true there). Applied to `process.env` by setup.ts, so the TempRepo
 * helpers and the code under test that shells out to real git both inherit the isolated environment.
 */
export function isolateGitEnvironment(env: NodeJS.ProcessEnv): void {
  for (const name of Object.keys(env)) if (name.startsWith("GIT_")) delete env[name];
  env.GIT_CONFIG_NOSYSTEM = "1";
  env.GIT_CONFIG_GLOBAL = "/dev/null";
  const config: [string, string][] = [
    ["user.name", "Spec"],
    ["user.email", "spec@example.test"],
    ["commit.gpgsign", "false"],
    ["tag.gpgsign", "false"],
    ["init.defaultBranch", "main"],
  ];
  env.GIT_CONFIG_COUNT = String(config.length);
  config.forEach(([key, value], index) => {
    env[`GIT_CONFIG_KEY_${index}`] = key;
    env[`GIT_CONFIG_VALUE_${index}`] = value;
  });
}
