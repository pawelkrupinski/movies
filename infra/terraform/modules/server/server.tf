resource "hcloud_server" "default" {
  name        = var.name
  server_type = var.server_type
  image       = var.image
  location    = var.location
  labels      = var.labels

  # No user_data anywhere on this fleet. bitcashier's module renders a cloud-config that enrols the
  # host with Puppet; there is no Puppet here, and a NixOS host installed by nixos-anywhere never
  # reads cloud-init at all -- disko repartitions the disk out from under it. Configuration arrives
  # exclusively through the flake.
  ssh_keys           = var.ssh_keys
  keep_disk          = true
  delete_protection  = var.delete_protection
  rebuild_protection = var.delete_protection
  backups            = var.backups

  dynamic "public_net" {
    for_each = var.primary_ipv4_id == null && var.primary_ipv6_id == null ? [] : [1]

    content {
      ipv4_enabled = true
      ipv4         = var.primary_ipv4_id
      ipv6_enabled = true
      ipv6         = var.primary_ipv6_id
    }
  }

  lifecycle {
    precondition {
      condition     = (var.primary_ipv4_id == null) == (var.primary_ipv6_id == null)
      error_message = "primary_ipv4_id and primary_ipv6_id must be given together or not at all: naming one alone leaves the other family to a fresh Hetzner allocation, which is the drift this is here to stop."
    }

    // `ssh_keys` is ForceNew and keys only take effect at creation time, so any drift between this
    // repo's key list and what Hetzner recorded destroys and recreates the machine for no gain.
    //
    // `public_net` is here for a much sharper reason, inherited wholesale from bitcashier's module
    // and worth restating rather than cross-referencing: the hcloud provider implements a CHANGE to
    // public_net as an unassign followed by an assign, and an unassign of a Primary IP DESTROYS the
    // address on this provider. bitcashier measured that on 2026-08-20 -- a plan showing nineteen
    // in-place updates with `ipv4_address` identical before and after, whose single apply deleted
    // 159.69.243.72 outright and left the machine with no public IPv4. Hetzner additionally refuses
    // to assign a Primary IP to a RUNNING server, so there is no in-place route through this
    // attribute at all.
    //
    // WHAT THIS COSTS: Terraform manages nothing about any server's public addressing, ever. A host
    // built by hand onto the wrong address will never be corrected by a plan, and no plan will
    // report the drift. That is deliberate -- the only correction Terraform can offer here is one
    // that destroys the address it is correcting.
    //
    // `ignore_changes` binds UPDATES ONLY; a resource is always created with its configured values.
    // So the `public_net` block above still governs the CREATE path, which is a different API call
    // with no unassign in it, and is what puts a rebuilt host back on its old address.
    ignore_changes = [ssh_keys, public_net]
  }

  dynamic "network" {
    for_each = var.network_ids

    content {
      network_id = network.value
      ip         = var.private_ip
      alias_ips  = []
    }
  }
}
