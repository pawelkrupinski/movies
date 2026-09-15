package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * CI'S DEPLOY-MARKER TOKEN IS A BEARER TOKEN, NOT A GOOGLE SESSION — and
 * `roles/google-sso.nix` gates every request to `grafana.kinowo.net` on having
 * the latter. `forward_auth` asks oauth2-proxy WHO is signed in; a request
 * carrying only an `Authorization` header has no session to ask about, so it
 * always got the 401-turned-302-to-login. `curl -fsS` reads a 302 as success
 * (`-f` only trips on >= 400), so this shipped for weeks with CI logging
 * "Posted deploy marker" while nothing was ever written — confirmed live
 * 2026-09-15 by curling the annotations API with a valid token and getting the
 * Google login redirect back.
 *
 * `ssoExemptRequests` (`roles/public-proxy.nix`) is the fix: a named Caddy
 * matcher + terminal `handle` for one exact method+path, emitted BEFORE
 * `forward_auth` in the `route` block so it wins without ever asking Google.
 * Grafana's own token check is the real gate on that path — this only removes
 * the SSO hop a CI job could never complete. The two things that must stay
 * true, and the reason this is a spec rather than a comment: the exemption has
 * to come before `forward_auth` in the emitted Caddyfile (order is the only
 * thing making it an exemption rather than dead config), and it has to name
 * the exact path CI actually posts to, since a typo here fails open into "back
 * to no annotations" rather than loud.
 */
class GrafanaAnnotationSsoBypassSpec extends AnyFlatSpec with Matchers {

  private lazy val publicProxyNix = RepoFile.read("infra/nix/modules/roles/public-proxy.nix")
  private lazy val monitoringHost = RepoFile.read("infra/nix/hosts/monitoring-1/default.nix")
  private lazy val grafanaAction  = RepoFile.read(".github/actions/mark-grafana-deploy/action.yml")

  /** The `"key" = { ... };` attribute block, matched by brace depth rather than
   *  a line shape — `RepoFile.block` assumes a YAML `key:` line, which a nix
   *  `"key" = {` is not. */
  private def nixAttrBlock(text: String, key: String): String = {
    val marker = s"$key = {"
    val markerAt = text.indexOf(marker)
    require(markerAt >= 0, s"no `$marker` in the file")
    var depth = 0
    var i = markerAt + marker.length - 1
    var closeAt = -1
    while (i < text.length && closeAt < 0) {
      text.charAt(i) match {
        case '{' => depth += 1
        case '}' =>
          depth -= 1
          if (depth == 0) closeAt = i
        case _ => ()
      }
      i += 1
    }
    require(closeAt >= 0, s"unbalanced braces after `$marker`")
    text.substring(markerAt, closeAt + 1)
  }

  private lazy val grafanaVhost = nixAttrBlock(monitoringHost, "\"grafana.kinowo.net\"")

  /** The path CI's marker actually posts to, read out of the composite action
   *  rather than hard-coded here — so the two cannot drift silently. */
  private lazy val postPath: String = {
    val marker = "-X POST \"${GRAFANA_URL%/}"
    val at = grafanaAction.indexOf(marker)
    require(at >= 0, "could not find the annotation POST url in mark-grafana-deploy/action.yml")
    val restStart = at + marker.length
    val endQuote = grafanaAction.indexOf('"', restStart)
    grafanaAction.substring(restStart, endQuote)
  }

  "public-proxy.nix" should "offer a way to exempt one method+path from the Google-login gate" in {
    publicProxyNix should include("ssoExemptRequests")
  }

  it should "emit the exemption before forward_auth, or it never takes effect" in {
    val emission = "googleLoginBlock = lib.optionalString v.requireGoogleLogin"
    val body = publicProxyNix.substring(publicProxyNix.indexOf(emission))
    val exemptAt      = body.indexOf("ssoExemptBlock")
    val forwardAuthAt = body.indexOf("forward_auth ")
    withClue("ssoExemptBlock must be interpolated ahead of forward_auth in googleLoginBlock: ") {
      (exemptAt >= 0 && exemptAt < forwardAuthAt) shouldBe true
    }
  }

  "the grafana vhost" should "still require a Google session for everything else" in {
    // The whole point is a narrow hole, not a wider door — this line is the
    // difference between the two.
    grafanaVhost should include("requireGoogleLogin = true")
  }

  it should "exempt exactly the request CI's deploy marker actually sends" in {
    withClue(s"ssoExemptRequests must name the same path CI posts to ($postPath): ") {
      grafanaVhost should include(s"path = \"$postPath\"")
    }
    grafanaVhost should include("method = \"POST\"")
  }
}
