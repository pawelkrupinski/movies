import { test } from 'node:test';
import assert from 'node:assert/strict';
import { lint } from './lint-flake-patterns.mjs';

const rules = (src) => lint(src).map((p) => p.rule);

test('a blind sleep is refused', () => {
  assert.deepEqual(rules(`test('x', async ({ page }) => {\n  await page.waitForTimeout(300);\n});`), ['wait-for-timeout']);
});

test('clearing storage then reloading is refused: the context is already fresh', () => {
  const src = `test('x', async ({ page }) => {\n  await clearLocalStorage(page);\n  await reload(page);\n});`;
  assert.deepEqual(rules(src), ['clear-then-reload']);
});

test('clearing storage mid-test, without a reload next, is fine', () => {
  const src = `test('x', async ({ page }) => {\n  await clearLocalStorage(page);\n  await expect(x).toBeHidden();\n});`;
  assert.deepEqual(rules(src), []);
});

test('a widened assertion timeout is refused', () => {
  assert.deepEqual(rules(`test('x', async ({ page }) => {\n  await expect(b).toBeHidden({ timeout: 15_000 });\n});`),
    ['widened-timeout']);
});

test('more loads than fit the budget, counting beforeEach, are refused', () => {
  const src = [
    `test.describe('d', () => {`,
    `  test.beforeEach(async ({ page }) => {`,
    `    await page.goto('/a/');`,
    `  });`,
    `  test('three more', async ({ page }) => {`,
    `    await reload(page);`,
    `    await page.reload();`,
    `    await backdateAndReload(page, 1);`,
    `  });`,
    `  test('two more', async ({ page }) => {`,
    `    await reload(page);`,
    `    await reload(page);`,
    `  });`,
    `});`,
  ].join('\n');
  assert.deepEqual(lint(src).map((p) => [p.rule, p.line]), [['load-budget', 5]]);
});

test('a helper definition is not a load', () => {
  const src = [
    `async function backdateAndReload(page) {`,
    `  await reload(page);`,
    `}`,
    `test('one', async ({ page }) => {`,
    `  await page.goto('/a/');`,
    `});`,
  ].join('\n');
  assert.deepEqual(rules(src), []);
});

test('a justified line opts out, on the line or the one above', () => {
  const src = [
    `test('x', async ({ page }) => {`,
    `  // flake-lint: allow wait-for-timeout — measuring the timer itself`,
    `  await page.waitForTimeout(300);`,
    `  await expect(b).toBeHidden({ timeout: 9000 }); // flake-lint: allow widened-timeout — why`,
    `});`,
  ].join('\n');
  assert.deepEqual(rules(src), []);
});
