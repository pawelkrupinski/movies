# THE LOGIN IN FRONT OF THE THINGS THAT DO NOT HAVE ONE, and in front of one that does.
#
# WHY IT EXISTS. roles/public-proxy.nix opens its header by saying a reverse proxy can do three
# things -- terminate TLS, route, and authenticate -- and that only a service which authenticates
# its OWN users belongs behind one that does the first two. VictoriaLogs authenticates nobody, so
# publishing it at logs.kinowo.net meant giving Caddy a shared password to check. A shared password
# is a weak boundary: it does not say WHO used it, it cannot be revoked for one person, it is the
# same secret in a password manager and in a shell history, and nothing about it expires.
#
# This replaces it with Google, so the question at the door is "is this Paweł's account" rather than
# "does this request know the string". That is checkable, revocable, logged by Google, and carries
# whatever second factor the account already has.
#
# ------------------------------------------------------------------------------------------------
# WHY oauth2-proxy AND NOT CADDY
# ------------------------------------------------------------------------------------------------
#
# Caddy has no OIDC of its own. The plugin that adds it (caddy-security) would mean building Caddy
# with xcaddy -- a custom derivation, a second Go toolchain in the closure, and a plugin whose
# release cadence is not nixpkgs's -- to get what a 12MB Go binary with a NixOS module already does.
# oauth2-proxy is also the piece Grafana's own `auth.proxy` documentation assumes.
#
# ------------------------------------------------------------------------------------------------
# ONE CALLBACK NAME FOR THE WHOLE FLEET
# ------------------------------------------------------------------------------------------------
#
# Google validates the redirect URI against a list registered on the OAuth client, and that list is
# edited in a web console by a human -- so every name we protect would otherwise be a console visit
# before it works, which is the kind of step that gets forgotten between "merged" and "why does this
# 400". `auth.kinowo.net` is the only URI registered: oauth2-proxy signs in there, sets its cookie
# on `.kinowo.net`, and every other name under the domain is admitted by that same cookie. Adding a
# protected name is then a Caddy vhost and nothing else.
#
# THE COOKIE IS THE THING THAT CROSSES NAMES, so `cookie.domain` and the whitelist below are what
# make grafana. and logs. one session rather than two. `whitelistDomains` is NOT decoration: without
# it oauth2-proxy refuses to redirect back to a host it was not started from, and the sign-in ends
# on its own error page having authenticated successfully.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.googleSso;
in
{
  options.fleet.googleSso = {
    enable = lib.mkEnableOption "Google sign-in in front of this host's published names";

    clientId = lib.mkOption {
      type = lib.types.str;
      description = ''
        The OAuth client. Not a secret -- a client id travels in every authorisation URL, which is
        why roles/k3s-server.nix writes the same one down in the clear.

        THE SAME CLIENT AS HEADLAMP, deliberately. A second client would be a second consent screen,
        a second test-user list to keep in step, and a second secret to rotate, for one more
        redirect URI on a client that already exists. It also means the Google project's TESTING
        mode -- whose test-user list is an allow-list Google enforces before a request ever reaches
        this fleet -- covers this door too.
      '';
    };

    allowedEmails = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = ''
        The accounts that may sign in, checked AFTER Google has proven who the visitor is.

        THIS LIST IS THE AUTHORISATION and Google is only the authentication, which is a distinction
        worth keeping sharp: `gmail.com` is a public domain, so an `email.domains` rule of the kind
        that works for a company would admit every Google account in the world. Addresses only.
      '';
      example = [ "someone@example.com" ];
    };

    clientSecretFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."google-sso/client-secret".path;
      defaultText = ''config.sops.secrets."google-sso/client-secret".path'';
      description = "The OAuth client secret, from sops-nix. Never a store path.";
    };

    cookieSecretFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."google-sso/cookie-secret".path;
      defaultText = ''config.sops.secrets."google-sso/cookie-secret".path'';
      description = ''
        What the session cookie is sealed with -- 32 random bytes, base64.

        A SECOND SECRET, NOT A REUSE OF THE FIRST. It is what stops a cookie being forged, so it
        must be unguessable, and it is rotated on a different schedule from the client secret:
        changing this one signs everybody out and costs nothing else, while changing the client
        secret is a console visit.
      '';
    };

    authHostName = lib.mkOption {
      type = lib.types.str;
      description = ''
        The public name that hosts the sign-in flow, and the ONLY redirect URI registered with
        Google. Must resolve to this host and be published by roles/public-proxy.nix.
      '';
      example = "auth.kinowo.net";
    };

    cookieDomain = lib.mkOption {
      type = lib.types.str;
      description = ''
        The parent domain the session cookie is set on, so one sign-in covers every protected name
        beneath it. A leading dot is what makes it cover subdomains.
      '';
      example = ".kinowo.net";
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1:4180";
      description = ''
        LOOPBACK, and this is the one service on this fleet that should NOT bind
        `fleet.privateAddress`. Its whole purpose is to be asked "is this request allowed" by Caddy
        on this same machine, and an answer of "yes" is a header any caller could then send onward.
        On the private network that would be reachable by every pod k3s schedules; on loopback the
        only thing that can ask is something already running here.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.allowedEmails != [ ];
        message = ''
          fleet.googleSso.allowedEmails is empty, which would admit NOBODY and lock this host's
          published names. An empty list is never what was meant, so it is refused here rather than
          discovered at the door.
        '';
      }
      {
        assertion = lib.hasPrefix "." cfg.cookieDomain;
        message = ''
          fleet.googleSso.cookieDomain ("${cfg.cookieDomain}") needs a leading dot, or the session
          cookie is set on the auth host alone and every other protected name bounces through a
          sign-in that appears to succeed and never sticks.
        '';
      }
    ];

    # THE SAME LESSON vector.service TAUGHT ON 2026-09-06, applied before it is learned twice. This
    # unit's command line carries every option below, so any edit to this role disturbs it -- and
    # `nixos-auto-apply` is default-deny, so without this each edit would refuse the whole closure
    # and hold back every unrelated change staged for the host, silently.
    #
    # THE SENTENCE: bouncing this costs the sign-ins that are mid-flight at that instant, and
    # nothing else. Sessions live in a cookie the browser holds, sealed with `cookieSecretFile`, so
    # a restart does not sign anybody out -- it is a redirect that has to be retried, and the
    # browser retries it.
    fleet.autoApply.restartableUnits = [ "oauth2-proxy.service" ];

    sops.secrets."google-sso/client-secret" = { owner = "oauth2-proxy"; mode = "0400"; };
    sops.secrets."google-sso/cookie-secret" = { owner = "oauth2-proxy"; mode = "0400"; };

    services.oauth2-proxy = {
      enable = true;
      provider = "google";
      clientID = cfg.clientId;
      clientSecretFile = cfg.clientSecretFile;
      cookie.secretFile = cfg.cookieSecretFile;

      # NEWLINE-JOINED, NOT A LIST. The upstream option is the CONTENT of
      # `--authenticated-emails-file`, one address per line, so a list here fails to type-check with
      # a message about "strings concatenated" that does not obviously mean "join them yourself".
      email.addresses = lib.concatStringsSep "\n" cfg.allowedEmails;

      httpAddress = "http://${cfg.listenAddress}";
      redirectURL = "https://${cfg.authHostName}/oauth2/callback";

      cookie.domain = cfg.cookieDomain;
      # THE COOKIE IS ONLY EVER SENT OVER TLS. Every name it covers is published by Caddy on 443
      # and nothing reaches this proxy except through it, so there is no plain-http case to keep
      # working -- and a session cookie that would travel in the clear is worth refusing outright.
      cookie.secure = true;

      # CADDY IS IN FRONT, so the client's real scheme, host and address arrive in X-Forwarded-*.
      # Without this oauth2-proxy believes it is being reached on loopback over http, and builds a
      # redirect back to `http://127.0.0.1:4180`, which is the sign-in "succeeding" into nowhere.
      reverseProxy = true;

      # WHO MAY CLAIM TO BE SPEAKING FOR A CLIENT. `reverseProxy` above makes X-Forwarded-* load
      # bearing -- the scheme and host in them are what the redirect back from Google is built
      # from -- and upstream's default when this is unset is to trust EVERY source, which it warns
      # about at evaluation for good reason: anything that can reach this port could then name its
      # own redirect target. Caddy is on this same machine and speaks over loopback, so loopback is
      # the whole of the list.
      trustedProxyIP = [ "127.0.0.1/32" "::1/128" ];

      # WHAT CADDY IS TOLD ABOUT THE VISITOR. `forward_auth` copies these onto the request it then
      # forwards, and Grafana reads the email one to know who signed in.
      setXauthrequest = true;

      extraConfig = {
        # NOTHING TO PROXY TO. This process only ever answers "is this request allowed" for Caddy
        # and serves the sign-in flow; the upstream it would otherwise need does not exist, and a
        # static 202 is what upstream documents for exactly this shape.
        upstream = "static://202";

        # SEE THE HEADER: without these two, a sign-in that starts on grafana. or logs. authenticates
        # correctly and then refuses to return there.
        whitelist-domain = cfg.cookieDomain;
        cookie-csrf-per-request = "true";
        cookie-csrf-expire = "5m";

        # STRAIGHT TO GOOGLE. The interstitial "Sign in with Google" button page is a click that
        # asks nothing -- there is one provider and one allowed account.
        skip-provider-button = "true";

        # ONLY CADDY ON THIS MACHINE MAY CLAIM TO BE SPEAKING FOR SOMEONE ELSE. `reverseProxy` above
        # makes X-Forwarded-* trusted, and this is what bounds who may set them.
        real-client-ip-header = "X-Forwarded-For";
      };
    };

    # NO systemd OVERRIDE HERE, DELIBERATELY. The upstream module already orders this unit after
    # the network and sets `Restart = "always"` -- which is stronger than the `on-failure` this
    # role used to ask for, and asking for the weaker one is a CONFLICT rather than a preference:
    # NixOS refuses two definitions of the same serviceConfig key and the host stops evaluating.
  };
}
