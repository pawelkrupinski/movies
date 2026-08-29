variable "name" {
  type = string
}

variable "server_type" {
  type = string
}

# The image a host is BUILT FROM. Required, no default -- a default here is wrong in both
# directions at once (it silently selects a retired image for new hosts, and bumping it proposes
# DESTROYING every host that inherits it, because `image` is ForceNew).
#
# ON THIS FLEET THE IMAGE IS NOT WHAT THE MACHINE RUNS. Every host here is bootstrapped from a
# stock Hetzner Ubuntu image and then converted to NixOS in place, from the Hetzner rescue system,
# by `infra/nix/bin/convert-host` (nixos-anywhere + disko, which repartitions the disk). Hetzner
# keeps reporting the original image forever, so this value records HOW THE MACHINE WAS BOOTSTRAPPED
# and nothing more. Do not "correct" it to something NixOS-shaped: it is ForceNew, so an edit here
# destroys and rebuilds the machine to deliver a difference no running system would observe.
#
# bitcashier/infra points this at a prebuilt `role=nixos-base` snapshot instead. That is the nicer
# shape and it is deliberately NOT copied here: the snapshot lives in that project's Hetzner
# account, this project has none, and building one needs a Linux nix builder that this estate does
# not have. nixos-anywhere from rescue is bitcashier's own bare-metal path (see its
# RUNBOOK-borg-ovh-1-nixos.md) and needs no snapshot at all.
variable "image" {
  type = string
}

variable "location" {
  type = string
}

variable "network_ids" {
  type = list(string)
}

# THE FLEET'S REGISTER. Read as authoritative by more than Terraform:
# infra/ansible/inventory/hcloud.yml keys its Ansible groups on `role` and `env`, so a host with no
# `role` label is a host no playbook can target.
#
#   role   What the machine DOES -- `mongo`, `monitoring`, `k3s-worker`. Matches the directory name
#          under infra/nix/hosts/, which is what makes `role_mongo` in the dynamic inventory line up
#          with the flake attribute the deploy role rebuilds.
#   env    Which environment it belongs to. `prod` for everything here today.
variable "labels" {
  type = map(string)
}

variable "ssh_keys" {
  type    = list(string)
  default = []
}

// Null means "let Hetzner pick the lowest free address". Pin it whenever the number is named
// anywhere else -- and on this fleet every number IS named elsewhere, because mongod's bind list,
// the Prometheus scrape config and the k3s server URL are all written against private addresses.
variable "private_ip" {
  type    = string
  default = null
}

// The PUBLIC addresses this server is CREATED on, as ids of `hcloud_primary_ip` resources.
//
// READ THE `ignore_changes` NOTE IN server.tf BEFORE TOUCHING THESE. `public_net` is fenced off
// from updates because the hcloud provider implements a change to it as unassign-then-assign, and
// an unassign DESTROYS the address. Naming an id here affects the CREATE call and nothing else --
// which is exactly what makes a Terraform replacement come back on the address it already had,
// instead of on a fresh one out of Hetzner's pool.
//
// Pass both or neither; the precondition in server.tf enforces it, because passing one alone
// leaves the other family to a fresh allocation -- the same drift, but quieter.
variable "primary_ipv4_id" {
  type    = string
  default = null
}

variable "primary_ipv6_id" {
  type    = string
  default = null
}

# Hetzner's automatic daily backups: seven rotating slots, 20% of the server's price, taken without
# anything on the host participating. Default true so a host built from this module is backed up
# unless somebody argues otherwise in writing.
#
# WHAT IT DOES NOT COVER, which is where this fleet's only irreplaceable data lives: ATTACHED
# VOLUMES ARE EXCLUDED. mongo-1's database is on a volume (see server.mongo.tf), so it is outside
# these backups and needs its own path -- that is what the mongodump timer in
# infra/nix/modules/roles/mongodb.nix is for. Do not read `backups = true` on that host as covering
# the database, because it does not.
variable "backups" {
  type    = bool
  default = true
}

# Guards BOTH deletion and rebuild.
#
# Default false, unlike bitcashier's module, and only until a host is converted: Hetzner's
# rebuild protection also refuses `enable_rescue`, and rescue is how NixOS gets onto these machines
# in the first place. Flip it to true per host once `convert-host` has run -- that edit is an
# in-place API call, not a replacement.
variable "delete_protection" {
  type    = bool
  default = false
}

variable "volumes" {
  type = map(object({
    size     = number
    location = string

    # Leave null and Hetzner's cloud-init automount mounts it at /mnt/HC_Volume_<id>. That path
    # carries the volume id, so anything pointed at it bakes an id into a config path -- and a
    # service writing there falls back to the ROOT DISK SILENTLY if the automount never runs.
    #
    # Set it and the automount is switched off, leaving the mount to NixOS at a path we choose.
    # SETTING THIS MOUNTS NOTHING. It only turns the automount off; the `fileSystems` entry must be
    # declared in the host's nix config, keyed by /dev/disk/by-id/scsi-0HC_Volume_<id>. Name it here
    # and forget that half and the volume is guaranteed to stay unmounted, with the service happily
    # filling the root disk and looking healthy.
    mount_point = optional(string, null)

    # PER-VOLUME override of the server's delete_protection, so retiring one disk does not strip the
    # guard from every other disk on that machine. Null inherits the server-wide value.
    delete_protection = optional(bool, null)
  }))

  default = {}
}
