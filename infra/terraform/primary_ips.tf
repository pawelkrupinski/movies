# Public addresses, brought under Terraform so a rebuild cannot destroy one.
#
# Nothing declared them until now: Hetzner allocated each pair at create time, and a Primary IP
# created that way carries `auto_delete = true` -- so destroying the server DELETES the address, and
# Hetzner offers no way to reclaim a released one. bitcashier lost 162.55.38.10 exactly this way
# during a fleet rebuild; it had been on a third party's IP allowlist, which is how production
# started answering with a block page.
#
# `auto_delete = false` is what stops that. It makes the address OUTLIVE its server, so a rebuild
# leaves it unassigned in the project rather than destroying it.
#
# WHAT THAT DOES NOT DO ON ITS OWN: an entry here does not make a rebuilt server pick its old
# address back up. It only makes the address SURVIVE, unassigned, so that something else can. That
# something else is `modules/server`'s `public_net` block, driven by the `primary_ipv4_id` /
# `primary_ipv6_id` passed by each host block below -- and EVERY host here passes both. If a future
# server.*.tf appears without them, that is the failure to look for.
#
# All six were adopted at the values they already had (see the import blocks at the foot of the
# file); each id was checked against the live API and resolves to an address Hetzner reports as held
# by that exact server, because a wrong key here silently pins a host to another machine's address
# and no comment is read by Terraform.
# LOCATION, NOT DATACENTER. The hcloud provider takes exactly one of `location` or `assignee_id`
# and rejects `datacenter` outright ("Exactly one of these attributes must be configured:
# [location, assignee_id]"). That is the better half of the choice to be forced into: a location is
# a fact this repository already states three times over in the server declarations, whereas the
# specific datacenter within it (nbg1-dc3 vs nbg1-dc1) is Hetzner's to choose and was NOT
# discoverable -- `GET /v1/primary_ips` returns `"datacenter": null` for all six of these.
locals {
  primary_ips = {
    mongo_1_ipv4      = { type = "ipv4", location = "nbg1" }
    mongo_1_ipv6      = { type = "ipv6", location = "nbg1" }
    mongo_2_ipv4      = { type = "ipv4", location = "fsn1" }
    mongo_2_ipv6      = { type = "ipv6", location = "fsn1" }
    monitoring_1_ipv4 = { type = "ipv4", location = "fsn1" }
    monitoring_1_ipv6 = { type = "ipv6", location = "fsn1" }
    k3s_worker_1_ipv4 = { type = "ipv4", location = "fsn1" }
    k3s_worker_1_ipv6 = { type = "ipv6", location = "fsn1" }
  }
}

resource "hcloud_primary_ip" "fleet" {
  for_each = local.primary_ips

  name     = each.key
  type     = each.value.type
  location = each.value.location

  # THE LINE THIS FILE EXISTS FOR. False makes the address outlive the server it is attached to.
  auto_delete = false

  # Belt and braces over auto_delete: Hetzner refuses to delete a protected address at all, so even
  # a `terraform destroy` aimed at the whole workspace leaves these behind for a human to release
  # deliberately.
  delete_protection = true

  labels = {
    fleet = "kinowo"
  }

  lifecycle {
    // THIS EXISTS TO STOP A PLAN PROPOSING TO UNASSIGN AN ADDRESS, which on this provider is the
    // operation that DESTROYS it -- the exact loss `auto_delete = false` above is here to prevent,
    // arriving by the other door. Hetzner has no reclaim path for a released address.
    //
    // `assignee_id` is not set in this configuration AT ALL, deliberately: which server an address
    // is attached to is decided by the `public_net` block on the server resource, at create time
    // (see modules/server/server.tf). Left unfenced, Terraform would read the live assignment,
    // compare it against the null here, and plan to UNASSIGN. Two resources must never both own one
    // attachment; the server owns it, and this line is how this resource declines to.
    ignore_changes = [assignee_id]
  }
}

# Adopted 2026-08-29 at the addresses Hetzner had already allocated. Import blocks rather than
# `terraform import` runs so the adoption is reviewable in the diff and reproducible from a clean
# state.
import {
  to = hcloud_primary_ip.fleet["mongo_1_ipv4"]
  id = "147084435" # 2.28.56.140
}

import {
  to = hcloud_primary_ip.fleet["mongo_1_ipv6"]
  id = "147084437" # 2a01:4f8:1c19:11e9::/64
}

import {
  to = hcloud_primary_ip.fleet["mongo_2_ipv4"]
  id = "147587790" # 178.105.221.61
}

import {
  to = hcloud_primary_ip.fleet["mongo_2_ipv6"]
  id = "147587791" # 2a01:4f8:c014:7518::/64
}

import {
  to = hcloud_primary_ip.fleet["monitoring_1_ipv4"]
  id = "147574248" # 128.140.49.167
}

import {
  to = hcloud_primary_ip.fleet["monitoring_1_ipv6"]
  id = "147574249" # 2a01:4f8:c013:c930::/64
}

import {
  to = hcloud_primary_ip.fleet["k3s_worker_1_ipv4"]
  id = "147574152" # 2.28.47.31
}

import {
  to = hcloud_primary_ip.fleet["k3s_worker_1_ipv6"]
  id = "147574153" # 2a01:4f8:c012:6976::/64
}
