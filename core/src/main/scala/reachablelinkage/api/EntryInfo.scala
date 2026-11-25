package reachablelinkage.api

/**
 * A classpath entry as a check saw it. `fingerprint` changes exactly when the entry's classes do;
 * it is empty for a class directory (the project itself), which is never tracked as a dependency.
 */
final case class EntryInfo(name: String, fingerprint: String)
