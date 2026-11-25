package reachablelinkage.api;

/**
 * Which conflicts should fail a build. `REACHABLE` is the default (`LinkageResult.failing()`): the `REACHABLE` and
 * `REFLECTIVE` ones, so nothing a framework could really hit is missed. `DIRECT` is stricter: only `REACHABLE`, the ones
 * the application's own code calls, so a conflict that rests on a reflection or keep rule only warns. `ANY` counts every
 * label, `LATENT`, `GUARDED`, `PROBED` and `DESERIALIZED` included.
 */
public enum FailOn {
  REACHABLE,
  DIRECT,
  ANY;
}
