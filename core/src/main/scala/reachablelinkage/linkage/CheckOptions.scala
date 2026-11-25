package reachablelinkage.linkage

/**
 * What a check is asked to do besides the classpath and scope.
 *
 * @param precision how finely reachability is tracked ([[Precision.Method]] by default)
 * @param keep      what frameworks call by reflection ([[Keep.standard]], the Guice and Jackson packs, by default)
 * @param loaded    internal names of classes a real run loaded; each one on the classpath is a root
 * @param exportedPackages dotted names of JDK packages the application's JVM flags export (`--add-exports`), so naming them is not an error
 */
final case class CheckOptions(
    precision: Precision = Precision.Method,
    keep: Seq[Keep] = Keep.standard,
    loaded: Set[String] = Set.empty,
    exportedPackages: Set[String] = Set.empty
)
