# shellcheck shell=bash
# THE FLEET'S FLAKE AS A REFERENCE NIX CAN READ FROM THIS CHECKOUT. Sourced by the infra tests that
# evaluate a host's configuration.
#
# A SHALLOW CLONE (CI's, this repository's since the GitHub suspension, and every agent worktree
# cut from one) cannot be read as `path/to/infra`: the git fetcher wants a revCount it does not
# have ("'<checkout>' is a shallow Git repository, so 'revCount' is not available"). Asking for it
# shallow reads the same tracked files.

# fleet_flake <infra dir> -- prints the flake reference of <infra dir>.
fleet_flake() {
  local infra="$1" root
  root="$(git -C "$infra" rev-parse --show-toplevel)"
  if [ "$(git -C "$root" rev-parse --is-shallow-repository)" = true ]; then
    printf '%s' "git+file://$root?dir=infra&shallow=1"
  else
    printf '%s' "$infra"
  fi
}
