// Run by `npm run lint` (node --test) ahead of ESLint itself, so the rule the
// lint relies on is proven to fire before its silence is trusted.
'use strict';
const { RuleTester } = require('eslint');
const test = require('node:test');
const rule = require('./storage-access-in-try');

RuleTester.describe = test.describe;
RuleTester.it = test.it;
RuleTester.itOnly = test.it.only;

new RuleTester({ languageOptions: { ecmaVersion: 2022, sourceType: 'script' } }).run('storage-access-in-try', rule, {
  valid: [
    'function f() { try { return localStorage.getItem("k"); } catch { return null; } }',
    'try { sessionStorage.setItem("k", "1"); } catch (e) {}',
    'try { Object.keys(localStorage).forEach(k => localStorage.removeItem(k)); } catch {}',
    'try { window.localStorage.clear(); } catch {}',
    'const o = { localStorage: 1 }; o.localStorage;',
  ],
  invalid: [
    { code: 'localStorage.getItem("k");', errors: [{ messageId: 'unguarded' }] },
    { code: 'function f() { return parseInt(localStorage.getItem("k") || "0", 10); }', errors: [{ messageId: 'unguarded' }] },
    { code: 'try { p.then(() => localStorage.setItem("k", "1")); } catch {}', errors: [{ messageId: 'unguarded' }] },
    { code: 'try { function later() { sessionStorage.clear(); } } catch {}', errors: [{ messageId: 'unguarded' }] },
    { code: 'try {} catch { localStorage.clear(); }', errors: [{ messageId: 'unguarded' }] },
    { code: 'window.sessionStorage.getItem("k");', errors: [{ messageId: 'unguarded' }] },
    { code: 'globalThis.localStorage.getItem("k");', errors: [{ messageId: 'unguarded' }] },
  ],
});
