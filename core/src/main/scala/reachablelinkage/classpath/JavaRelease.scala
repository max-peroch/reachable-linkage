package reachablelinkage.classpath

/** The Java release the running JVM implements, from `java.specification.version` (`1.8` on Java 8, `17` on Java 17), because `Runtime.version` does not exist
  * before Java 9.
  */
private[reachablelinkage] object JavaRelease:

  /** The release of the running JVM: 8, 11, 17... */
  val running: Int = ofSpecification(System.getProperty("java.specification.version", "17"))

  /** `1.8` is 8 and `17` is 17; anything unreadable is taken as 17, the oldest release this was written for before Java 8 support. */
  def ofSpecification(specification: String): Int =
    val digits = if specification.startsWith("1.") then specification.drop(2) else specification
    digits.takeWhile(_.isDigit).toIntOption.getOrElse(17)
