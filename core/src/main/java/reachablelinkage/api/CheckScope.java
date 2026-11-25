package reachablelinkage.api;

/**
 * `App`: start from the project's own classes and check everything they can reach, dependencies
 * included (sbt-missinglink's default). `Full`: every class of every checked entry is a root
 * (`missinglinkScanDependencies := true`).
 */
public enum CheckScope {
  App,
  Full;
}
