// `mongosh --nodb --quiet [--eval <code>] --file <a.js> --file <b.js> …` for plain Node, so the pure rule specs run where
// no mongosh is installed (a CI runner). Every file and --eval runs in ONE context, in order, as mongosh loads them:
// their top-level declarations meet, and `print` / `quit` mean what they mean there.
//
//   node scripts/local-mirror/nodb.js [--eval <code>] --file <a.js> …
//
// rules-test.sh runs the specs through this when mongosh is absent.
const fs = require("fs");
const vm = require("vm");

const context = vm.createContext({
  print: (...values) => console.log(...values),
  quit: code => process.exit(code === undefined ? 0 : code),
});
const args = process.argv.slice(2);
for (let i = 0; i < args.length; i++) {
  const flag = args[i];
  if (flag === "--nodb" || flag === "--quiet") continue;
  const value = args[++i];
  if (value === undefined) { console.error(`nodb.js: ${flag} needs a value`); process.exit(2); }
  if (flag === "--eval") vm.runInContext(value, context, { filename: "--eval" });
  else if (flag === "--file") vm.runInContext(fs.readFileSync(value, "utf8"), context, { filename: value });
  else { console.error(`nodb.js: unknown argument ${flag}`); process.exit(2); }
}
