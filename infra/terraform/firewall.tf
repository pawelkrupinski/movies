# Hetzner's edge firewall, ahead of every NIC on this fleet.
#
# BECAUSE IT SITS AHEAD OF THE NIC IT IS INVISIBLE TO THE HOST. A host attached here shows a correct
# nftables ruleset, a listening socket and a green systemd unit while dropping the traffic it exists
# to serve, and no `nixos-rebuild` and no in-host check can see it. That is the whole reason the
# in-host firewall (infra/nix/modules/fleet/firewall.nix) states the same intent independently
# rather than trusting this one.
#
# It is deliberately a DENY-BY-DEFAULT edge: a Hetzner firewall drops every inbound port not named
# in a rule, and a server attached to NO firewall is completely unfiltered. So attaching a host here
# narrows it; forgetting to attach one leaves it wide open. `hcloud_firewall_attachment` below
# names all three, and the check in infra/bin/tf-check refuses a plan that leaves a server out.
resource "hcloud_firewall" "fleet" {
  name = "fleet"

  # Port 22 from anywhere, deliberately and not as an oversight. These hosts are administered over
  # the public internet from whatever address the operator happens to be on, and narrowing it would
  # need a stable set of administrative source addresses this estate does not have. Key-only login
  # (`services.openssh.settings.PasswordAuthentication = false` in fleet/default.nix) is what
  # actually bounds it.
  rule {
    direction  = "in"
    protocol   = "tcp"
    port       = "22"
    source_ips = ["0.0.0.0/0", "::/0"]
  }

  rule {
    direction  = "in"
    protocol   = "icmp"
    source_ips = ["0.0.0.0/0", "::/0"]
  }

  # NO INBOUND WireGuard RULE, and the absence is deliberate rather than an omission.
  #
  # mongo-1 does join Fly's 6PN over WireGuard, but it is the side that DIALS OUT: `fly wireguard
  # create` hands back a peer configuration carrying Fly's gateway as the `Endpoint`, so the tunnel
  # is established from here outwards and the return traffic arrives on the ephemeral source port
  # of an already-established flow -- which a stateful firewall passes without any rule naming it.
  # Opening 51820 inbound would therefore grant nothing except a listener for the whole internet to
  # find. See infra/nix/modules/roles/wireguard-fly.nix, which sets no `listenPort` for exactly this
  # reason, and relies on `persistentKeepalive` to hold the flow open through NAT.
  #
  # IF THE DIRECTION EVER REVERSES -- Fly dialling in, which would mean a fixed `listenPort` on this
  # side -- then this is the rule that has to come back, and it must land in the same change as that
  # `listenPort`. Split across two commits, the tunnel simply never establishes and the symptom is a
  # database the Fly apps cannot reach.

  # THE PRIVATE NETWORK IS NOT COVERED BY THIS FIREWALL AT ALL and that is worth being explicit
  # about, because it is the thing that makes every rule above readable as the complete public
  # attack surface. Hetzner Cloud firewalls filter the PUBLIC interface only; traffic arriving on
  # ens10 from 10.20.0.0/24 is never evaluated here. That is why mongod (27017), the k3s apiserver
  # (6443), node_exporter (9100) and Prometheus (9090) need no rules here -- they bind the private
  # address or the WireGuard interface and are unreachable from outside regardless.
}

# Attachment is its own resource so that adding or removing a server does not rewrite the rules, and
# so the rules can be read without also reading the membership.
resource "hcloud_firewall_attachment" "fleet" {
  firewall_id = hcloud_firewall.fleet.id

  server_ids = [
    module.mongo_1.id,
    module.monitoring_1.id,
    module.k3s_worker_1.id,
  ]
}
