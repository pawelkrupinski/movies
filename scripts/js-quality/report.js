// THE JS/TS QUALITY REPORT: the Scala ratchets' measures (CLAUDE.md, "Code quality ratchets" and "Coupling is
// reviewed") taken over a checkout's JavaScript and TypeScript, per AREA. Every area is reported; the production ones
// are also gated (scripts/js-quality-report --check, THE JS/TS QUALITY RATCHET). Run it through
// scripts/js-quality-report, which installs the tools first; its header says what each measure is.
//
//   node report.js --root DIR [--areas FILE | --area NAME[:globals]=PATH[,PATH...] ...] --out DIR
//
// Writes report.md (the markdown), functions.tsv (every function's measures), coupling.tsv (every module's fan-out,
// every directory's metrics) and over-limit.tsv (`kind<TAB>key<TAB>value`: the limits, then one row for each item over a limit -- the
// rows scripts/ci/js_quality_ratchet.py holds to its baseline) into --out.
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { execFileSync } = require('node:child_process');
const { measureFunctions } = require('./functions');
const { findClones, MinTokens } = require('./clones');
const { importEdges, globalEdges, measureCoupling } = require('./coupling');

const Limits = { cognitive: 10, cyclomatic: 10, innerDefs: 2, lines: 50, fileLines: 500, fanOut: 9, distance: 0.2, distanceMinModules: 8 };
const Extensions = /\.(js|mjs|cjs|ts|mts|cts|jsx|tsx)$/;
const Excluded = /(^|\/)(node_modules|dist|build|coverage)\/|\.min\.js$|\.d\.ts$/;
// the pinned tools, installed outside the checkout by scripts/js-quality-report (JS_QUALITY_NODE_MODULES)
const bin = (name) => path.join(process.env.JS_QUALITY_NODE_MODULES || path.join(__dirname, 'node_modules'), '.bin', name);

function parseArgs(argv) {
  const options = { areas: [] };
  for (let i = 0; i < argv.length; i += 1) {
    const value = argv[i + 1];
    if (argv[i] === '--root') options.root = fs.realpathSync(path.resolve(value));
    else if (argv[i] === '--out') options.out = path.resolve(value);
    else if (argv[i] === '--areas') options.areasFile = value;
    else if (argv[i] === '--area') options.areas.push(parseArea(value));
    else throw new Error(`unknown argument ${argv[i]}`);
    i += 1;
  }
  if (!options.root || !options.out) throw new Error('--root and --out are required');
  return options;
}

// `name[:globals]=path,path`: an area, its files those under the paths, read as globals (classic scripts) or imports.
function parseArea(spec) {
  const match = /^([\w.-]+)(?::(globals|imports))?=(.+)$/.exec(spec);
  if (!match) throw new Error(`--area ${spec}: expected NAME[:globals]=PATH[,PATH...]`);
  return { name: match[1], kind: match[2] || 'imports', paths: match[3].split(',') };
}

// The areas file: `name<TAB>kind<TAB>path path ...`, `#` comments.
function readAreasFile(file) {
  return fs.readFileSync(file, 'utf8').split('\n')
    .filter((line) => line.trim() && !line.startsWith('#'))
    .map((line) => {
      const [name, kind, paths] = line.split('\t');
      return { name, kind, paths: paths.trim().split(/\s+/) };
    });
}

function walk(root, relative) {
  const full = path.join(root, relative);
  if (!fs.existsSync(full)) return [];
  if (!fs.statSync(full).isDirectory()) return [relative];
  return fs.readdirSync(full).flatMap((entry) => walk(root, path.join(relative, entry)));
}

// The source files under `paths`: what git tracks there, or every file when `root` is no git checkout.
function sourcesUnder(root, paths) {
  let files;
  try {
    files = execFileSync('git', ['-C', root, 'ls-files', '--', ...paths], { stdio: ['ignore', 'pipe', 'ignore'] })
      .toString().split('\n').filter(Boolean);
  } catch {
    files = paths.flatMap((relative) => walk(root, relative));
  }
  return files.filter((file) => Extensions.test(file) && !Excluded.test(file)).sort();
}

// Distance is held only for a directory of 8+ modules (as for a Scala package of 8+ types) that is coupled at all.
const isDistanceMeasured = (row) => row.modules >= Limits.distanceMinModules && row.distance !== null;

const lineCount = (text) => text.split('\n').length - (text.endsWith('\n') ? 1 : 0);

function overLimit(area, functions, files, clones, coupling) {
  const fns = functions.filter((row) => row.area === area);
  const packages = coupling.packages.filter(isDistanceMeasured);
  return {
    files: files.length,
    lines: files.reduce((sum, file) => sum + file.lines, 0),
    functions: fns.length,
    cognitive: fns.filter((row) => row.cognitive > Limits.cognitive).length,
    cyclomatic: fns.filter((row) => row.cyclomatic > Limits.cyclomatic).length,
    innerDefs: fns.filter((row) => row.innerDefs > Limits.innerDefs).length,
    lines50: fns.filter((row) => row.lines > Limits.lines).length,
    fileLines: files.filter((file) => file.lines > Limits.fileLines).length,
    clones: clones.filter((group) => group.areas.includes(area)).length,
    fanOut: coupling.modules.filter((row) => row.fanOut > Limits.fanOut).length,
    moduleCycles: coupling.moduleCycles.length,
    packageCycles: coupling.packageCycles.length,
    distance: `${packages.filter((row) => row.distance > Limits.distance).length}/${packages.length}`,
  };
}

const fixed = (value) => (value === null ? 'n/a' : value.toFixed(2));

const table = (header, rows) => [`| ${header.join(' | ')} |`, `|${header.map(() => '---').join('|')}|`, ...rows.map((row) => `| ${row.join(' | ')} |`)];

function worst(title, rows, limit, cells) {
  const over = rows.filter((row) => row.value > limit).length;
  const top = [...rows].sort((a, b) => b.value - a.value).slice(0, 10);
  if (top.length === 0) return [`### ${title}`, '', 'none', ''];
  return [`### ${title} (${over} over ${limit})`, '', ...table(cells.header, top.map(cells.row)), ''];
}

function markdown(areas, summaries, functions, files, clones, couplings) {
  const header = ['area', 'files', 'lines', 'functions', `cognitive > ${Limits.cognitive}`, `cyclomatic > ${Limits.cyclomatic}`,
    `inner defs > ${Limits.innerDefs}`, `function > ${Limits.lines} lines`, `file > ${Limits.fileLines} lines`,
    `clone groups (${MinTokens}+ tokens)`, `fan-out > ${Limits.fanOut}`, 'module cycles', 'directory cycles',
    `D > ${Limits.distance} (dirs of ${Limits.distanceMinModules}+)`];
  const summaryRows = areas.map((area) => {
    const s = summaries.get(area.name);
    return [`${area.name} (${area.kind})`, s.files, s.lines, s.functions, s.cognitive, s.cyclomatic, s.innerDefs, s.lines50,
      s.fileLines, s.clones, s.fanOut, s.moduleCycles, s.packageCycles, s.distance];
  });
  const fn = (field) => ({
    header: ['function', 'area', field],
    row: (row) => [`\`${row.file}:${row.line}\` ${row.name}`, row.area, row.value],
  });
  const byField = (field) => functions.map((row) => ({ ...row, value: row[field] }));
  const modules = couplings.flatMap(({ area, coupling }) => coupling.modules.map((row) => ({ ...row, area, value: row.fanOut })));
  const dirs = couplings.flatMap(({ area, coupling }) => coupling.packages
    .filter(isDistanceMeasured).map((row) => ({ ...row, area, value: row.distance })));
  const cycleLines = couplings.flatMap(({ area, coupling }) => [
    ...coupling.moduleCycles.map((members) => `- ${area}, modules: ${members.map((m) => `\`${m}\``).join(' ↔ ')}`),
    ...coupling.packageCycles.map((members) => `- ${area}, directories: ${members.map((m) => `\`${m}\``).join(' ↔ ')}`),
  ]);
  return [
    '# JS/TS quality report', '',
    ...table(header, summaryRows), '',
    'Each function is measured by itself (a callback is a function of its own; Scala folds lambdas into their method).',
    'Fan-out counts every local module used (no data/collaborator split); a directory stands for a package.', '',
    '## Worst 10', '',
    ...worst('Cognitive complexity', byField('cognitive'), Limits.cognitive, fn('cognitive')),
    ...worst('Cyclomatic complexity', byField('cyclomatic'), Limits.cyclomatic, fn('cyclomatic')),
    ...worst('Inner defs', byField('innerDefs'), Limits.innerDefs, fn('inner defs')),
    ...worst('Function length', byField('lines'), Limits.lines, fn('lines')),
    ...worst('File length', files.map((file) => ({ ...file, value: file.lines })), Limits.fileLines,
      { header: ['file', 'area', 'lines'], row: (row) => [`\`${row.file}\``, row.area, row.value] }),
    ...worst('Clone groups', clones.map((group) => ({ ...group, value: group.tokens })), MinTokens - 1,
      { header: ['tokens', 'lines', 'copies'], row: (row) => [row.tokens, row.lines, row.copies.map((c) => `\`${c.file}:${c.start}-${c.end}\``).join(' ')] }),
    ...worst('Fan-out', modules, Limits.fanOut,
      { header: ['module', 'area', 'fan-out', 'template globals'], row: (row) => [`\`${row.file}\``, row.area, row.value, row.fromTemplates] }),
    ...worst(`Distance (directories of ${Limits.distanceMinModules}+ modules)`, dirs, Limits.distance,
      { header: ['directory', 'area', 'modules', 'Ca', 'Ce', 'I', 'A', 'D'],
        row: (row) => [`\`${row.dir}\``, row.area, row.modules, row.ca, row.ce, row.instability.toFixed(2), row.abstractness.toFixed(2), row.distance.toFixed(2)] }),
    '### Cycles', '', ...(cycleLines.length ? cycleLines : ['none']), '',
  ].join('\n');
}

// The ratchet's limits (scripts/ci/js_quality_ratchet.py reads them from the rows, as the Scala ratchets do).
const LimitRows = [['cognitive', Limits.cognitive], ['cyclomatic', Limits.cyclomatic], ['inner-defs', Limits.innerDefs],
  ['method-lines', Limits.lines], ['file-lines', Limits.fileLines], ['copies', 0], ['clone-tokens', MinTokens],
  ['fanout', Limits.fanOut], ['distance', Limits.distance], ['cycle', 0]];

// A function's ratchet key: `<file>:<name>`, no line (an edit above it must not re-key it), `#2`, `#3`... for the later
// functions of one file sharing a name (two `anonymous` callbacks), in line order.
function functionKeys(functions) {
  const seen = new Map();
  const keys = new Map();
  for (const row of [...functions].sort((a, b) => a.file.localeCompare(b.file) || a.line - b.line)) {
    const base = `${row.file}:${row.name}`;
    const n = (seen.get(base) || 0) + 1;
    seen.set(base, n);
    keys.set(row, n === 1 ? base : `${base}#${n}`);
  }
  return keys;
}

// The rows a ratchet freezes, `kind<TAB>key<TAB>value`: the limits, then every item over one. A key naming several files
// joins them with `|`: a clone group's files (sorted, each once; the value the copies of every group spanning exactly
// them), a cycle's members.
function overLimitRows(functions, files, clones, couplings) {
  const rows = LimitRows.map(([kind, value]) => ['limit', kind, value]);
  const push = (kind, key, value, limit) => { if (value > limit) rows.push([kind, key, value]); };
  const keys = functionKeys(functions);
  for (const row of functions) {
    const key = keys.get(row);
    push('cognitive', key, row.cognitive, Limits.cognitive);
    push('cyclomatic', key, row.cyclomatic, Limits.cyclomatic);
    push('inner-defs', key, row.innerDefs, Limits.innerDefs);
    push('method-lines', key, row.lines, Limits.lines);
  }
  for (const file of files) push('file-lines', file.file, file.lines, Limits.fileLines);
  const copies = new Map();
  for (const group of clones) {
    const key = [...new Set(group.copies.map((c) => c.file))].sort().join('|');
    copies.set(key, (copies.get(key) || 0) + group.copies.length);
  }
  for (const [key, value] of copies) rows.push(['copies', key, value]);
  for (const { coupling } of couplings) {
    for (const row of coupling.modules) push('fanout', row.file, row.fanOut, Limits.fanOut);
    for (const row of coupling.packages.filter(isDistanceMeasured)) push('distance', row.dir, Number(row.distance.toFixed(2)), Limits.distance);
    for (const members of [...coupling.moduleCycles, ...coupling.packageCycles]) rows.push(['cycle', members.join('|'), members.length]);
  }
  return rows.map((row) => row.join('\t')).join('\n') + '\n';
}

function couplingOf(area, root, areaFiles, texts, globalsProviders) {
  if (area.kind === 'globals') {
    const { edges, templateNames } = globalEdges(areaFiles, globalsProviders, texts);
    return measureCoupling(edges, texts, templateNames);
  }
  return measureCoupling(importEdges(root, areaFiles, bin('depcruise')), texts);
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  const areas = options.areas.length ? options.areas : readAreasFile(options.areasFile || path.join(__dirname, 'areas.tsv'));
  const areaOf = new Map();
  for (const area of areas) for (const file of sourcesUnder(options.root, area.paths)) if (!areaOf.has(file)) areaOf.set(file, area.name);
  const all = [...areaOf.keys()];
  const texts = new Map(all.map((file) => [file, fs.readFileSync(path.join(options.root, file), 'utf8')]));
  const files = all.map((file) => ({ file, area: areaOf.get(file), lines: lineCount(texts.get(file)) }));

  const { functions, unparsed } = await measureFunctions(options.root, all, texts);
  functions.forEach((row) => { row.area = areaOf.get(row.file); });
  const clones = findClones(options.root, all, bin('jscpd'))
    .map((group) => ({ ...group, areas: [...new Set(group.copies.map((copy) => areaOf.get(copy.file)))] }));
  const globalsProviders = all.filter((file) => areas.find((area) => area.name === areaOf.get(file)).kind === 'globals');
  const couplings = areas.map((area) => ({
    area: area.name,
    coupling: couplingOf(area, options.root, all.filter((file) => areaOf.get(file) === area.name), texts, globalsProviders),
  }));
  const summaries = new Map(areas.map((area) => [area.name,
    overLimit(area.name, functions, files.filter((file) => file.area === area.name), clones, couplings.find((c) => c.area === area.name).coupling)]));

  fs.mkdirSync(options.out, { recursive: true });
  let report = markdown(areas, summaries, functions, files, clones, couplings);
  if (unparsed.length) report += `\n### Not parsed\n\n${unparsed.map((u) => `- \`${u.file}\`: ${u.why}`).join('\n')}\n`;
  fs.writeFileSync(path.join(options.out, 'report.md'), report);
  fs.writeFileSync(path.join(options.out, 'over-limit.tsv'), overLimitRows(functions, files, clones, couplings));
  fs.writeFileSync(path.join(options.out, 'functions.tsv'), ['file\tline\tname\tarea\tcognitive\tcyclomatic\tinner-defs\tlines',
    ...functions.map((r) => [r.file, r.line, r.name, r.area, r.cognitive, r.cyclomatic, r.innerDefs, r.lines].join('\t'))].join('\n') + '\n');
  fs.writeFileSync(path.join(options.out, 'coupling.tsv'), ['kind\tarea\tkey\tvalues',
    ...couplings.flatMap(({ area, coupling }) => [
      ...coupling.modules.map((m) => `module\t${area}\t${m.file}\tfan-out=${m.fanOut} template-globals=${m.fromTemplates}`),
      ...coupling.packages.map((p) => `directory\t${area}\t${p.dir}\tmodules=${p.modules} ca=${p.ca} ce=${p.ce} I=${fixed(p.instability)} A=${fixed(p.abstractness)} D=${fixed(p.distance)}`),
    ])].join('\n') + '\n');
  process.stdout.write(report);
}

main().catch((error) => {
  process.stderr.write(`js-quality-report: ${error.stack || error}\n`);
  process.exit(2);
});
