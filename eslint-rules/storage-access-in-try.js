// `localStorage` / `sessionStorage` must be touched only inside a `try` block.
//
// Reading the property itself throws (a SecurityError) wherever the browser
// blocks site data — Safari with "Block All Cookies", Firefox or Chrome with
// cookies disabled for the site, some embedded webviews — and `setItem`
// throws on a full quota. An unguarded access there aborts whatever handler
// it sits in: an anonymous visitor's ✕ once left the card on screen because
// the "only on this device" nag read `localStorage` bare after the hide had
// been written (guarded) and before the card was dropped.
//
// "Inside a try" is lexical: the access must sit in a `try { … }` block of the
// SAME function. A callback crossing that boundary does not count — it may run
// long after the try has exited (`.then(() => localStorage…)`) — except an
// arrow handed straight to a synchronous array method (`forEach`, `filter`,
// …), which runs while the try is still on the stack.
'use strict';

const SYNCHRONOUS_CALLBACK_METHODS = new Set(['forEach', 'filter', 'map', 'some', 'every', 'find', 'reduce']);
const STORAGES = new Set(['localStorage', 'sessionStorage']);

function runsSynchronously(fn) {
  const call = fn.parent;
  return call && call.type === 'CallExpression' && call.arguments.includes(fn)
    && call.callee.type === 'MemberExpression' && !call.callee.computed
    && SYNCHRONOUS_CALLBACK_METHODS.has(call.callee.property.name);
}

function guarded(node) {
  for (let child = node, parent = node.parent; parent; child = parent, parent = parent.parent) {
    if (parent.type === 'TryStatement' && parent.block === child) return true;
    const isFunction = parent.type === 'FunctionDeclaration' || parent.type === 'FunctionExpression'
      || parent.type === 'ArrowFunctionExpression';
    if (isFunction && !runsSynchronously(parent)) return false;
  }
  return false;
}

module.exports = {
  meta: {
    type: 'problem',
    docs: { description: 'require localStorage / sessionStorage access inside a try block' },
    messages: {
      unguarded: '`{{name}}` throws where site data is blocked (and setItem on a full quota): ' +
        'touch it only inside a try block of the same function.',
    },
    schema: [],
  },
  create(context) {
    function check(node, name) {
      if (!guarded(node)) context.report({ node, messageId: 'unguarded', data: { name } });
    }
    return {
      Identifier(node) {
        if (!STORAGES.has(node.name)) return;
        const parent = node.parent;
        // `window.localStorage` is reported on its MemberExpression below; a
        // property KEY named localStorage (`{ localStorage: 1 }`, `x.localStorage`
        // on some other object) is not storage access.
        if (parent.type === 'MemberExpression' && parent.property === node && !parent.computed) return;
        if (parent.type === 'Property' && parent.key === node && !parent.computed) return;
        check(node, node.name);
      },
      MemberExpression(node) {
        if (node.computed || node.property.type !== 'Identifier' || !STORAGES.has(node.property.name)) return;
        if (node.object.type === 'Identifier' && (node.object.name === 'window' || node.object.name === 'self'))
          check(node, node.property.name);
      },
    };
  },
};
