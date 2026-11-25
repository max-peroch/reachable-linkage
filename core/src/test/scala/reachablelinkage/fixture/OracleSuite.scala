package reachablelinkage.fixture

import scala.concurrent.duration.*

/**
 * Base for suites that run missinglink-core as an oracle. Its first use builds a model of the whole
 * JDK (about 12 s and 2 GB on a healthy machine, several times that when the machine is short of
 * memory), so munit's 30 second default turns a slow host into a failure. Nothing here asserts on
 * time; speed is measured separately, by the benchmark tools.
 */
abstract class OracleSuite extends munit.FunSuite:
  override def munitTimeout: Duration = 5.minutes
