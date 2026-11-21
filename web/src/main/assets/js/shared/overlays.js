// Dropdown panels and modal dialogs: opening and closing them, and the Escape and click-outside
// dismissals every page shares.

/* global closeAuthMenu */  // auth.js
/* exported clampPanel, closeDialog, closeDialogOnBackdrop, closeOtherCitiesModal, closeOtherPanels,
   openDialog, openOtherCitiesModal, setPanelOpen */

// Hide every navbar dropdown except (optionally) one — used by each toggle handler
// so opening any panel closes all the others.
// Every open/close of a dropdown goes through here, so its trigger
// (`aria-controls` naming the panel) always says whether it is open.
function setPanelOpen(panel, open) {
  panel.style.display = open ? 'block' : 'none';
  const trigger = document.querySelector('[aria-controls="' + panel.id + '"]');
  if (trigger) trigger.setAttribute('aria-expanded', String(open));
}

function closeOtherPanels(except) {
  document.querySelectorAll('.dropdown-panel').forEach(p => {
    if (p !== except) setPanelOpen(p, false);
  });
}

// ── Modal dialogs ─────────────────────────────────────────────────────────
//
// Every modal is a `<dialog class="modal-dialog">` opened with `showModal()`:
// the browser puts it in the top layer, moves focus into it, makes the page
// behind it inert, closes it on Escape (its `cancel`) and hands focus back to
// the opener on close. The dialog fills the viewport and centres its card, so
// a click whose target is the dialog element itself landed outside the card.

function openDialog(id) {
  const dialog = document.getElementById(id);
  if (dialog && !dialog.open) dialog.showModal();
}
function closeDialog(id) {
  const dialog = document.getElementById(id);
  if (dialog && dialog.open) dialog.close();
}
// The dialogs' inline `onclick`: a direct handler, not a delegated one, so iOS
// Safari dispatches the tap on the (non-interactive) backdrop at all.
function closeDialogOnBackdrop(event) {
  if (event.target === event.currentTarget) event.currentTarget.close();
}
window.closeDialogOnBackdrop = closeDialogOnBackdrop;

function clampPanel(panel) {
  panel.style.right = '0px';
  const rect = panel.getBoundingClientRect();
  if (rect.left < 8) panel.style.right = (rect.left - 8) + 'px';
}

// ── Film page: "other cities" popup ──────────────────────────────────────
//
// The sibling-city links themselves are server-rendered and already in the
// DOM (`_filmDetailContent`) — this only toggles whether a visitor SEES
// them. The same modal dialog as the login modal.
function openOtherCitiesModal() {
  openDialog('other-cities-dialog');
}
function closeOtherCitiesModal() {
  closeDialog('other-cities-dialog');
}

window.openOtherCitiesModal = openOtherCitiesModal;
window.closeOtherCitiesModal = closeOtherCitiesModal;

// ESC closes any open dropdown / menu (a modal `<dialog>` closes itself).
// `closeOtherPanels(null)` collapses every `.dropdown-panel` (Filtry
// today; any future dropdown automatically) so this stays correct as
// the UI grows.
document.addEventListener('keydown', e => {
  if (e.key === 'Escape') {
    closeAuthMenu();
    closeOtherPanels(null);
  }
});

// ── Click outside an open dropdown: dismiss only, swallow everything else ──
//
// When a `.dropdown-panel` (Filtry) or the auth menu is open, a click
// anywhere outside it must do exactly ONE thing: close it. Without this the
// same click also bubbled on to the card-tap handler (which navigates to
// `/movie`) or followed whatever link sat under the cursor — so dismissing
// the filter accidentally opened a page.
//
// We run in the CAPTURE phase (the `true` below) so this fires before any
// bubble-phase handler and before a link's default navigation, then
// `stopImmediatePropagation` + `preventDefault` so nothing else acts on the
// click. Inside-clicks and trigger-clicks are left alone so the panel's own
// controls (radios, cinema toggles, the Filtry button itself) keep working.
// When nothing is open this is a no-op, so ordinary card taps and links
// behave normally.
//
// Routing the close through `closeOtherPanels(null)` keeps it robust against
// future additions/removals of dropdowns.
document.addEventListener('click', e => {
  if (!(e.target instanceof Element)) return;
  const panelOpen = [...document.querySelectorAll('.dropdown-panel')]
    .some(p => p.style.display !== 'none');
  const menuOpen = !!document.querySelector('.auth-menu.open');
  if (!panelOpen && !menuOpen) return;
  // Clicks on a panel/menu or its trigger drive their own toggle logic.
  if (e.target.closest('.dropdown-panel, #format-filter-btn, .auth-menu')) return;
  e.preventDefault();
  e.stopImmediatePropagation();
  closeOtherPanels(null);
  closeAuthMenu();
}, true);
