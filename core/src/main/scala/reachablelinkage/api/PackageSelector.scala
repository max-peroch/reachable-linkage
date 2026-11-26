package reachablelinkage.api

/** A package, with or without its subpackages: `com.acme` matches `com.acme` and, by default, `com.acme.util`, but never `com.acmeco`. Built by
  * [[PackageSelector.of]], which checks the name.
  */
final case class PackageSelector private (name: String, subpackages: Boolean) {

  /** Does `packageName` fall under this selector? The default package is the empty string. */
  def matches(packageName: String): Boolean =
    packageName == name || (subpackages && packageName.startsWith(name + "."))
}

object PackageSelector {
  def of(name: String, subpackages: Boolean = true): Either[ConflictFilterError, PackageSelector] =
    Either.cond(JavaNames.isQualified(name), new PackageSelector(name, subpackages), ConflictFilterError.InvalidPackageName(name))
}
