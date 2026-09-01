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
  # is established from here outwards and return traffic arrives on the ephemeral source port of an
  # already-established flow, which a stateful firewall passes without any rule naming it.
  #
  # AN OPERATOR VPN WAS BUILT HERE AND BACKED OUT THE SAME DAY, which is worth recording so it is
  # not proposed a second time. The problem it solved -- reaching Grafana, Prometheus and the k3s
  # apiserver, all of which bind the private subnet -- is already solved by the SSH access this
  # fleet is administered through: `ssh -N -L 3000:10.20.0.11:3000 root@<monitoring-1>` forwards any
  # one of them, and `ssh -D` covers the rest at once. A VPN would have added an open UDP port, a
  # role and a second set of keys to maintain, to make an existing capability slightly more
  # convenient.
  #
  # Public HTTPS for Grafana is a genuinely different requirement and is solved separately, by a
  # reverse proxy on 80/443 -- see the rules for those.

  # HTTPS, AND HTTP, FOR GRAFANA ONLY -- the fleet's one published service.
  #
  # 80 IS NOT AN OVERSIGHT. ACME's HTTP-01 challenge is served on it, so closing 80 does not harden
  # anything: it makes the certificate fail to RENEW, silently, about sixty days later, and the
  # symptom is a browser TLS warning on a service that was working yesterday. Caddy redirects
  # everything that is not a challenge to 443.
  #
  # WHAT IS BEHIND THIS AND WHAT IS NOT: Grafana, and nothing else. It is the only service on this
  # fleet that authenticates its own users, which is what makes it the only one that can stand
  # behind a proxy whose job is TLS rather than auth. Prometheus, Alertmanager, VictoriaLogs, the
  # k3s apiserver and mongod stay private and are reached with `ssh -L`. See
  # infra/nix/modules/roles/public-proxy.nix.
  rule {
    direction  = "in"
    protocol   = "tcp"
    port       = "80"
    source_ips = ["0.0.0.0/0", "::/0"]
  }

  rule {
    direction  = "in"
    protocol   = "tcp"
    port       = "443"
    source_ips = ["0.0.0.0/0", "::/0"]
  }

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
    module.mongo_2.id,
    module.monitoring_1.id,
    module.k3s_worker_1.id,
  ]
}
