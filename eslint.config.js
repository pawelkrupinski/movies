// ESLint flat config. Lints `web/src/main/assets/js/**/*.js` — the production
// browser code that ships to users — and `web/src/page/resources/**/*.js`, the
// page specs' in-page helpers (CDP fetch stubs, harness APIs; see
// `tools.PageScripts`). The Playwright suite under `page-tests-playwright/tests/`
// has its own setup.
//
// The "globals" list below is what the browser code reads from the
// templates: the constants `_sharedJsConfig.scala.html` emits (ALL_CINEMAS,
// CINEMA_PILLS, …) and the functions the per-page inline `<script>` block
// defines (`applyFilters`, `buildIndex`, …), which run in the same global
// scope as the scripts loaded after them.
//
// Between the shared modules (`js/shared/*.js`, loaded in order by
// `_sharedScripts.scala.html`) each file says it itself: a `/* global … */`
// line per module it reads from, and `/* exported … */` for what it offers
// the others and the templates. ESLint checks both — a name read but not
// declared is `no-undef`, one declared and also defined is `no-redeclare` —
// so those lines stay the true import list of each file.
//
// `i18n.js` (loaded ahead of the shared modules — see `_sharedScripts`/
// `landing.scala.html`) is intentionally NOT added here: it's part of
// this same lint glob, so declaring `t` as a global here would collide
// with its own `function t(...)` and fire `no-redeclare`. The shared
// modules reach it via `window.t(...)` instead, which needs no declaration.

const js = require('@eslint/js');
const globals = require('globals');
const storageAccessInTry = require('./eslint-rules/storage-access-in-try');

module.exports = [
  js.configs.recommended,
  {
    // A disable directive that no longer suppresses anything hides nothing but
    // the next real problem on that line — fail lint on it (ESLint's default only warns).
    linterOptions: { reportUnusedDisableDirectives: 'error' },
  },
  {
    files: ['web/src/main/assets/js/**/*.js'],
    languageOptions: {
      // A syntax ceiling, deliberately not 'latest': this code ships unbundled and
      // untranspiled, so newer syntax (ES2024's regex `v` flag, ES2025's regex
      // modifiers) would be a parse error for every visitor on an older iOS Safari.
      ecmaVersion: 2022,
      sourceType: 'script',
      globals: {
        ...globals.browser,

        // From `_sharedJsConfig.scala.html`.
        HAS_OAUTH_PROVIDERS: 'readonly',
        ALL_CINEMAS: 'readonly',
        CINEMA_PILLS: 'readonly',
        CURRENT_CITY: 'readonly',
        CURRENT_COUNTRY: 'readonly',
        KINOWO_LOCALE: 'readonly',

        // From the per-page inline `<script>` block.
        IS_FAVOURITES_PAGE: 'readonly',
        applyFilters: 'readonly',
        applyFiltersForDay: 'readonly',
        buildIndex: 'readonly',

        // From `_geoDistance.scala.html`, the pure-helper inline `<script>`
        // rendered ahead of the shared scripts on every page — see its own doc
        // comment for why it's a separate partial from `_repertoireView`'s.
        nearestCityWithinKm: 'readonly',
      },
    },
    plugins: { kinowo: { rules: { 'storage-access-in-try': storageAccessInTry } } },
    rules: {
      // Storage throws where site data is blocked — see the rule's own header.
      'kinowo/storage-access-in-try': 'error',
      // Leading-underscore args are intentional placeholders (e.g.
      // DOM-event handlers that receive `e` but ignore it).
      'no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
      // Empty `catch {}` is the idiomatic localStorage / sessionStorage
      // try-catch when the only possible failures (blocked site data, a
      // full quota) are ones we accept silently.
      'no-empty': ['warn', { allowEmptyCatch: true }],
      // Many shared-module functions are exported by being defined at top
      // level — ESLint's flat config treats them as unused unless we
      // also disable the prefer-const / no-inner-declarations checks
      // that don't understand the template-stitching pattern.
      'no-inner-declarations': 'off',
      'no-prototype-builtins': 'off',
    },
  },
  {
    // The page specs' helpers run inside the rendered pages, so they reach the
    // page's own globals (`getHidden`, `applyFilters`, …). Each file names the
    // ones it uses in a `/* global … */` comment rather than here: the list is
    // per page, and a helper calling something its page lacks should fail lint.
    files: ['web/src/page/resources/**/*.js'],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: 'script',
      globals: { ...globals.browser },
    },
    rules: {
      'no-unused-vars': ['error', { argsIgnorePattern: '^_' }],
    },
  },
];
