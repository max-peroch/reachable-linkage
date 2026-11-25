package reachablelinkage.fixture

/** The Scala versions fixtures are built with. */
enum ScalaVersion(val key: String, val isScala3: Boolean):
  case V212    extends ScalaVersion("2.12", false)
  case V213    extends ScalaVersion("2.13", false)
  case V3Lts   extends ScalaVersion("3-lts", true)
  case V3Next  extends ScalaVersion("3-next", true)

  override def toString: String = key
