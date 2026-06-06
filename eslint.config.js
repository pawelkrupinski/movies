// ESLint 9 flat config. Lints `web/src/main/assets/js/**/*.js` — the production
// browser code that ships to users. Test JS lives under
// `page-tests-playwright/tests/` and has its own setup; this config
// deliberately stays narrow.
//
// The "globals" list below is the contract between `shared.js` and
// the inline `<script>` blocks in `repertoire/film/kina.scala.html`.
// Twirl renders the inline block BEFORE the `<script src=
// "/assets/js/shared.js">` tag, so:
//
//  - constants from `_sharedJsConfig` (IS_LOGGED_IN, ALL_CINEMAS, …)
//    are in scope for shared.js at runtime
//  - shared.js exports functions (`applyFilters`, `buildIndex`, …)
//    that the inline `<script>` block calls back into
//
// ESLint can't see across files, so without this list every
// cross-file reference would fire `no-undef`. Keep this list in sync
// with the actual interface — a new constant in _sharedJsConfig or a
// new function called by the inline block needs to land here.

const js = require('@eslint/js');
const globals = require('globals');

module.exports = [
  js.configs.recommended,
  {
    files: ['web/src/main/assets/js/**/*.js'],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: 'script',
      globals: {
        ...globals.browser,

        // From `_sharedJsConfig.scala.html`.
        IS_LOGGED_IN: 'readonly',
        HAS_OAUTH_PROVIDERS: 'readonly',
        ALL_CINEMAS: 'readonly',
        CINEMA_PILLS: 'readonly',
        CURRENT_CITY: 'readonly',
        ALL_CITIES: 'readonly',

        // From the per-page inline `<script>` block.
        IS_FAVOURITES_PAGE: 'readonly',
        applyFilters: 'readonly',
        buildIndex: 'readonly',
      },
    },
    rules: {
      // Leading-underscore args are intentional placeholders (e.g.
      // DOM-event handlers that receive `e` but ignore it).
      'no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
      // Empty `catch {}` is the idiomatic localStorage / sessionStorage
      // try-catch when the only possible failure (quota) is one we
      // accept silently.
      'no-empty': ['warn', { allowEmptyCatch: true }],
      // Many shared.js functions are exported by being defined at top
      // level — ESLint's flat config treats them as unused unless we
      // also disable the prefer-const / no-inner-declarations checks
      // that don't understand the template-stitching pattern.
      'no-inner-declarations': 'off',
      'no-prototype-builtins': 'off',
    },
  },
];
