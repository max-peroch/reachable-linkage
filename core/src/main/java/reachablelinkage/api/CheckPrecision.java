package reachablelinkage.api;

/**
 * `METHOD` (the default): track which methods are reachable and label each conflict. `CLASS`:
 * sbt-missinglink's granularity, every conflict reported as reachable. `METHOD_RTA`: as `METHOD`, but a virtual call only lands in classes something instantiates, so
 * more conflicts are `LATENT`; it misses an instance made in a way it cannot see (see `Dispatch.Rta`).
 */
public enum CheckPrecision {
  CLASS,
  METHOD,
  METHOD_RTA;
}
