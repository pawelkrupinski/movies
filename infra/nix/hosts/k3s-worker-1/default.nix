{ ... }:
{
  imports = [
    ./disko.nix
    ../../modules/roles/k3s-agent.nix
    ../../modules/roles/public-proxy.nix

    # See the note on the same import in hosts/monitoring-1: this belongs in
    # modules/fleet/default.nix's `imports`, and is repeated per host only because that list could
    # not be edited in the change that added it.
    ../../modules/fleet/logs.nix
  ];

  networking.hostName = "k3s-worker-1";

  fleet = {
    role = "k3s-worker";
    privateAddress = "10.20.0.12";

    # THE ADDRESS CI COPIES A CLOSURE TO, and the reason it is stated rather than inferred: this
    # fleet has no jump host, so every path onto these machines -- colmena, nixos-anywhere, the
    # staging workflow -- arrives on the public NIC. bin/stage-nixos-closures treats an empty
    # `publicAddress` as `unreachable-by-declaration` and FAILS rather than skipping, which is the
    # right direction (a host nobody can stage to silently stops tracking main) and is exactly what
    # it did on the first run after this landed on main.
    #
    # Stable because terraform/primary_ips.tf pins it with `auto_delete = false`; it is
    # `k3s_worker_1_ipv4` there.
    publicAddress = "204.168.140.213";
  };

  # THE ONLY HOST IN hel1, and the only one with no `fileSystems` entry beyond disko's root -- see
  # ./disko.nix for why it holds no state. It is also the only host with Hetzner's daily backups
  # switched OFF (in ../../../terraform/server.k3s-worker.tf), which is the same decision stated in
  # the other direction.

  # JOINS THE CONTROL PLANE ON monitoring-1, over the private network. Both hosts are in Hetzner's
  # `eu-central` network zone despite sitting in different datacentres (hel1 here, nbg1 there), so
  # 10.20.0.11 is directly reachable with no peering and no routes -- see terraform/network.tf. The
  # ~20ms between Helsinki and Nuremberg is paid by kubelet heartbeats and image pulls, which is
  # why nothing latency-sensitive should be scheduled here without moving the machine first.
  fleet.k3sAgent = {
    enable = true;
    serverAddr = "https://10.20.0.11:6443";
  };

  fleet.firewall.k3sAgent = true;

  # SHIP THE JOURNAL *AND* THE POD LOGS TO monitoring-1. The second half is the point on this host:
  # containerd writes container stdout/stderr to /var/log/pods and NOT to the journal, so a shipper
  # with only the journal source would send k3s's own units and nothing the cluster runs -- which is
  # the whole reason this machine exists. bitcashier hit exactly that gap when its workloads moved
  # to Kubernetes and nothing replaced the old alloc-log scrape; nothing alerted, because a stream
  # that has stopped looks the same as one nobody has asked about.
  #
  # This host holds no state (see ./disko.nix) -- vector's checkpoint and buffer under /var/lib are
  # the one exception, and both are disposable: losing them costs a re-ship, not data.
  #
  # See the note in hosts/mongo-1 for why the address is a literal here rather than read off
  # monitoring-1's own declaration.
  fleet.logs = {
    enable = true;
    serverAddress = "10.20.0.11";
    kubernetesPodLogs.enable = true;
  };

  # THE PUBLIC FACE OF THE PRODUCT, and the reason this host — not monitoring-1 — terminates it: the
  # web pods run HERE (nodeSelector pins them to this node), so every vhost below proxies to a
  # NodePort on loopback. Putting these on monitoring-1's Caddy instead would send every user request
  # nbg1 → hel1 across the private network and make the monitoring box a single point of failure for
  # the product, which is precisely backwards.
  #
  # WHY NOT AN INGRESS CONTROLLER. k3s ships with traefik and servelb disabled here
  # (roles/k3s-server.nix), so a Kubernetes-native answer means adding an ingress controller AND
  # cert-manager AND a LoadBalancer story, to do what Caddy already does on this host with automatic
  # ACME and no renewal timer to forget.
  #
  # EVERY NAME BELOW MUST RESOLVE TO 204.168.140.213 BEFORE A DEPLOY, or Let's Encrypt's HTTP-01
  # challenge fails and Caddy serves a self-signed certificate — which browsers reject outright, so
  # the failure mode is a hard TLS error rather than a degraded page. The A records live at OVH.
  fleet.publicProxy = {
    enable = true;
    acmeEmail = "pawel@bitcashier.io";
    vhosts = {
      # Poland, on its own domain and its own brand.
      "kinowo.net".upstream = "127.0.0.1:30910";
      "www.kinowo.net".redirectTo = "kinowo.net";

      # The Showtimes countries, one subdomain each.
      "uk.showtimes.cc".upstream = "127.0.0.1:30912";
      "de.showtimes.cc".upstream = "127.0.0.1:30911";

      # THE BARE APEX IS THE BRAND FRONT DOOR, not a fourth deployment: the app renders a country
      # picker (Poland included) whenever the request Host is the apex, so any country's pods answer
      # it identically — see models.Country.servesApex. It is pointed at the UK deployment only
      # because the picker is English-language chrome; nothing about the page is UK-specific.
      #
      # ONLY `/` IS THE FRONT DOOR, and everything else redirects, because the app's host check
      # gates the LANDING alone. Without this, showtimes.cc/london/ would serve the UK repertoire
      # from a second hostname — and since the canonical link and og:url are built from the request
      # host, every UK page would self-canonicalise on two domains and the apex would advertise its
      # own sitemap. That is textbook duplicate content, and it splits the ranking of the site it
      # is supposed to be a door into.
      "showtimes.cc" = {
        upstream = "127.0.0.1:30912";
        extraConfig = ''
          @notRoot not path /
          redir @notRoot https://uk.showtimes.cc{uri} permanent
        '';
      };
      "www.showtimes.cc".redirectTo = "showtimes.cc";
    };
  };

  sops.defaultSopsFile = ../../secrets/k3s-worker-1.yaml;

  system.stateVersion = "26.05";
}
