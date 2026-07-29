package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TunnelTunedUriSpec extends AnyFlatSpec with Matchers {

  "a tunnel-tuned URI" should "add fail-fast options to a bare URI" in {
    val tuned = TunnelTunedUri("mongodb://user:pw@127.0.0.1:27018/")
    tuned should include ("serverSelectionTimeoutMS=5000")
    tuned should include ("heartbeatFrequencyMS=2000")
    tuned should startWith ("mongodb://user:pw@127.0.0.1:27018/?")
  }

  it should "keep options the URI already carries" in {
    val tuned = TunnelTunedUri("mongodb://h/?authSource=admin&directConnection=true")
    tuned should include ("authSource=admin")
    tuned should include ("directConnection=true")
    tuned should include ("serverSelectionTimeoutMS=5000")
  }

  // An explicit value in the secret is a deliberate choice and must win.
  it should "never override a value the caller set itself" in {
    val tuned = TunnelTunedUri("mongodb://h/?serverSelectionTimeoutMS=60000")
    tuned should include ("serverSelectionTimeoutMS=60000")
    tuned should not include "serverSelectionTimeoutMS=5000"
  }

  it should "match case-insensitively, since Mongo options are" in {
    TunnelTunedUri("mongodb://h/?SERVERSELECTIONTIMEOUTMS=60000") should not include "serverSelectionTimeoutMS=5000"
  }
}
