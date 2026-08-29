# THE PUBLIC HTTP SURFACE ON THIS FLEET, and the only deliberate exception to a convention that is
# otherwise absolute here: every other service binds the private subnet, and the only port open at
# the Hetzner edge is 22.
#
# WHAT IT PUBLISHES. Two different things on two different hosts, and the distinction is worth
# keeping straight:
#
#   monitoring-1   Grafana, and nothing else. Of the three things a reverse proxy can do --
#                  terminate TLS, route, and authenticate -- only the first two are useful to a
#                  service with no login of its own. Grafana authenticates its own users, so it is
#                  the sole internal service that can safely stand behind a proxy whose only job is
#                  TLS. Prometheus, Alertmanager, VictoriaLogs, the k3s apiserver, node_exporter and
#                  mongod stay private; publishing them would mean inventing an auth layer here, and
#                  a shared password in front of an unauthenticated admin API is a worse answer than
#                  a tunnel. For those the answer remains
#                  `ssh -N -L <port>:10.20.0.11:<port> root@<host>`, which needs no open port.
#
#   k3s-worker-1   the PRODUCT: kinowo.net, uk.showtimes.cc, de.showtimes.cc and the showtimes.cc
#                  apex. These are meant to be public, they authenticate their own admin pages, and
#                  they proxy to NodePorts on 127.0.0.1 because the pods run on that same host.
#
# WHY MULTIPLE VHOSTS RATHER THAN ONE. This module used to take a single `hostName`/`upstream`
# pair, which was right while the fleet published exactly one thing. The product needs four names
# on one host, so the option is an attrset keyed by public hostname. The alternative -- an ingress
# controller plus cert-manager inside k3s -- buys Kubernetes-native Ingress objects at the cost of
# two more controllers and a LoadBalancer story on a cluster that has servicelb disabled, to do
# what fifteen lines of Caddy already do here.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.publicProxy;
in
{
  options.fleet.publicProxy = {
    enable = lib.mkEnableOption "a public HTTPS reverse proxy";

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      description = ''
        Where Let's Encrypt sends expiry warnings. Required by the ACME terms; a wrong address here
        means the one notification that a renewal has been failing for weeks goes nowhere.
      '';
    };

    vhosts = lib.mkOption {
      default = { };
      description = ''
        Public hostnames this proxy answers for, keyed by the name itself. Each name is also the
        name a certificate is issued for, so every key must already resolve to this host's public
        address -- ACME's HTTP-01 challenge is served on port 80 of whatever the DNS says, and a
        name pointed elsewhere fails issuance rather than falling back to plain HTTP.
      '';
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          upstream = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = ''
              host:port to proxy to -- a private-subnet address for a service on another fleet host,
              or 127.0.0.1:<nodePort> for a workload on this host's own k3s node.
            '';
          };

          extraConfig = lib.mkOption {
            type = lib.types.lines;
            default = "";
            description = ''
              Extra Caddy directives for this vhost, emitted before the upstream or redirect.

              Caddy sorts directives into its own canonical order rather than the order written,
              so a `redir` here still takes effect ahead of the `reverse_proxy` below regardless
              of where it appears. That is what makes a partial redirect expressible: send some
              paths elsewhere and proxy the rest.
            '';
          };

          redirectTo = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = ''
              Public hostname to 301 to instead of proxying, path and query preserved. This is how
              the `www.` spelling of a name is served: it still needs its own vhost (and so its own
              certificate, because the redirect itself is delivered over TLS -- a browser that
              already knows the HSTS policy will not follow a redirect it cannot validate first),
              but it runs no upstream.
            '';
          };
        };
      });
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.mapAttrsToList (host: v: {
      # Catches the two ways a vhost is silently wrong: neither field set (Caddy would serve an
      # empty 200 for the site, which looks like the app returning a blank page) and both set
      # (the redirect wins and the upstream is dead config nobody notices).
      assertion = (v.upstream == null) != (v.redirectTo == null);
      message = "fleet.publicProxy.vhosts.\"${host}\" must set exactly one of `upstream` or `redirectTo`.";
    }) cfg.vhosts;

    security.acme = {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    services.caddy = {
      enable = true;

      # Caddy rather than nginx, for one reason that matters on a fleet nobody watches daily: it
      # obtains and RENEWS every certificate itself, with no timer to forget, no reload hook to get
      # wrong, and no separate acme.sh state to go stale. The config below is the entire deployment.
      virtualHosts = lib.mapAttrs (host: v: {
        extraConfig = ''
          ${v.extraConfig}
          ${if v.redirectTo != null
            then ''redir https://${v.redirectTo}{uri} permanent''
            else ''reverse_proxy ${v.upstream}''}

          # HSTS. Deliberately modest -- one week, no preload, no includeSubDomains. Preload is a
          # one-way door (removal takes months and ships with the browser), and includeSubDomains
          # would impose HTTPS-only on every future name under these domains, including ones that
          # do not exist yet.
          header Strict-Transport-Security "max-age=604800"
        '';
      }) cfg.vhosts;
    };

    # 80 AND 443, AND 80 IS NOT OPTIONAL. ACME's HTTP-01 challenge is served on 80, so closing it
    # does not harden anything -- it just makes every certificate fail to renew, silently, sixty
    # days later. Caddy redirects 80 to 443 for everything that is not a challenge.
    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
