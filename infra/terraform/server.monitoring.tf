# Monitoring and the k3s control plane, on one machine, as specified.
#
# THOSE TWO ROLES ON ONE BOX IS A REAL TRADE and it is being made knowingly. The upside is that it
# costs nothing extra and the control plane is watched by a Prometheus that does not depend on it.
# The downside is that a k3s control plane under load is the loudest neighbour on a 2-core box, and
# it is sharing with the one service whose whole job is to still be answering when everything else
# is not. `fleet.observability` therefore gives Prometheus and Alertmanager an IOSchedulingClass and
# a CPUWeight above k3s's, so metrics keep being written while the apiserver is starving -- see
# infra/nix/modules/roles/prometheus.nix. If this host ever needs to be split, the k3s server is the
# half that moves; monitoring's storage is the half with state.
#
# MOVED nbg1 -> fsn1 ON 2026-09-01, following k3s-worker-1 (see server.k3s-worker.tf for why that one
# moved). This host had no latency reason of its own to move -- Prometheus scrapes and Grafana are
# not on anyone's request path -- but it is the k3s SERVER, and leaving the control plane a location
# away from the only node it schedules would have kept a long link inside the cluster for nothing.
#
# THE VOLUME COULD NOT COME WITH IT. A Hetzner volume cannot leave its location, so the 40GB
# `monitoring-data` in nbg1 was replaced by a new one in fsn1 and the 854MB of Prometheus TSDB,
# VictoriaLogs and Grafana sqlite was rsynced across. The new filesystem was then given the OLD
# volume's UUID with `tune2fs -U`, which is what let the host mount it without noticing -- see the
# `volumeDevice` note in nix/hosts/monitoring-1/default.nix, which no longer names a Hetzner device
# path at all. THAT is the change to read before moving a stateful host on this fleet again.
module "monitoring_1" {
  # PROTECTED NOW THAT IT IS CONVERTED. The module defaults this to FALSE, unlike bitcashier's,
  # because Hetzner's rebuild protection also refuses `enable_rescue` -- and rescue is how NixOS gets
  # onto these machines. That default is right for a host being built; it is wrong for one that is
  # serving, and leaving it is how a `terraform destroy` typo or a bad `-target` takes a live host.
  #
  # THE COST IS DELIBERATE FRICTION: re-running `convert-host` against this machine now requires
  # setting this back to false and applying first. That is a second look before repartitioning a
  # host that is in service, which is the point.
  delete_protection = true

  source = "./modules/server"

  name        = "monitoring-1"
  server_type = "cx23"
  image       = "ubuntu-26.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "fsn1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.11"

  primary_ipv4_id = hcloud_primary_ip.fleet["monitoring_1_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["monitoring_1_ipv6"].id

  labels = {
    role = "monitoring"
    env  = "prod"
  }

  # Prometheus TSDB and Grafana's sqlite on their own disk, for the same reasons as mongo-1's -- and
  # one more that is specific to monitoring: the metrics that explain an incident are written during
  # the incident, and a monitoring host whose root disk filled is a host that stopped recording at
  # exactly the moment the recording mattered. 40GB holds a comfortable retention for three
  # node_exporters plus the application scrape; see the retention setting in roles/prometheus.nix,
  # which is what actually bounds it.
  volumes = {
    monitoring-data = {
      size              = 40
      location          = "fsn1"
      mount_point       = "/var/lib/monitoring"
      delete_protection = true
    }
  }
}

# Adopted 2026-09-01, when the nbg1 -> fsn1 move forced a new volume (a Hetzner volume cannot change
# location, so the data was rsynced onto this one and the old 40GB in nbg1 was retired). Import
# blocks rather than `terraform import` runs, for the same reason primary_ips.tf gives: the adoption
# is reviewable in the diff and reproducible from a clean state.
#
# THE ATTACHMENT IS IMPORTED BY THE VOLUME'S ID, not an id of its own -- that is what the hcloud
# provider keys `hcloud_volume_attachment` on.
import {
  to = module.monitoring_1.hcloud_volume.default["monitoring-data"]
  id = "106766834"
}

import {
  to = module.monitoring_1.hcloud_volume_attachment.default["monitoring-data"]
  id = "106766834"
}
