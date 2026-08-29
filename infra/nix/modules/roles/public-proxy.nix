# THE ONE PUBLIC HTTP SURFACE ON THIS FLEET, and the only deliberate exception to a convention that
# is otherwise absolute here: every service binds the private subnet, and the only port open at the
# Hetzner edge is 22.
#
# WHAT IT PUBLISHES AND WHAT IT DOES NOT. Grafana, and nothing else. Prometheus, Alertmanager,
# VictoriaLogs, the k3s apiserver, node_exporter and mongod stay private, because of the three
# things a reverse proxy can do -- terminate TLS, route, and authenticate -- only the first two are
# useful to a service that has no login of its own. Grafana is the sole service on this fleet that
# authenticates its own users, so it is the sole service that can safely stand behind a proxy whose
# only job is TLS. Publishing the others would mean inventing an auth layer for them here, and a
# shared password in front of an unauthenticated admin API is a worse answer than a tunnel.
#
# The alternative for everything else remains `ssh -N -L <port>:10.20.0.11:<port> root@<host>`,
# which needs no open port at all.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.publicProxy;
in
{
  options.fleet.publicProxy = {
    enable = lib.mkEnableOption "a public HTTPS reverse proxy in front of Grafana";

    hostName = lib.mkOption {
      type = lib.types.str;
      description = ''
        The public name this serves, and the name the certificate is issued for.

        WHY IT IS AN sslip.io NAME. This project owns no domain -- kinowo.fly.dev belongs to Fly --
        and a certificate needs a hostname, not an address. sslip.io resolves `<dashed-ip>.sslip.io`
        to that IP with no registration and no DNS to run, which makes a working HTTPS URL possible
        today instead of after a purchase.

        WHAT THAT COSTS, because it is not free: sslip.io is a SHARED registered domain, and Let's
        Encrypt rate-limits per registered domain (50 certificates per week across everyone using
        it). An issuance can therefore fail for reasons that have nothing to do with this fleet, and
        the failure mode is a browser TLS warning rather than an outage. If that happens, or when a
        real domain exists, changing this one option and re-deploying is the whole migration -- the
        address is pinned by terraform/primary_ips.tf, so a DNS record can point at it whenever.
      '';
    };

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      description = ''
        Where Let's Encrypt sends expiry warnings. Required by the ACME terms; a wrong address here
        means the one notification that a renewal has been failing for weeks goes nowhere.
      '';
    };

    upstream = lib.mkOption {
      type = lib.types.str;
      description = "host:port to proxy to, on the private interface.";
    };
  };

  config = lib.mkIf cfg.enable {
    security.acme = {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    services.caddy = {
      enable = true;

      # Caddy rather than nginx, for one reason that matters on a fleet nobody watches daily: it
      # obtains and RENEWS the certificate itself, with no timer to forget, no reload hook to get
      # wrong, and no separate acme.sh state to go stale. The config below is the entire deployment.
      virtualHosts.${cfg.hostName}.extraConfig = ''
        reverse_proxy ${cfg.upstream}

        # HSTS. Deliberately modest -- one week, no preload, no includeSubDomains. A long max-age or
        # a preload submission on a SHARED domain like sslip.io would impose HTTPS-only on names
        # this fleet does not control, which is somebody else's problem to inherit.
        header Strict-Transport-Security "max-age=604800"
      '';
    };

    # 80 AND 443, AND 80 IS NOT OPTIONAL. ACME's HTTP-01 challenge is served on 80, so closing it
    # does not harden anything -- it just makes the certificate fail to renew, silently, sixty days
    # later. Caddy redirects 80 to 443 for everything that is not a challenge.
    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
