package reachablelinkage.api;

/**
 * How likely a conflict is to be hit. At `CLASS` precision every conflict is `REACHABLE`. At `METHOD`
 * precision, `REACHABLE` means the method holding the broken reference can be reached from the
 * application through the call graph; `LATENT` means the class is on the classpath and referenced,
 * but nothing the application can reach calls into that method.
 */
public enum ConflictLabel {
  REACHABLE,
  LATENT,
  /** An enclosing `catch` handles the error this conflict would throw, or one of its superclasses. */
  GUARDED,
  /**
   * The missing class is named by a string constant in reached code (`Class.forName`, an availability
   * check, a message), so the code probably checks for it before use. A heuristic; `FailOn.ANY` counts it.
   */
  PROBED,
  /**
   * Reachable only through a reflection rule, a keep rule (`@Inject`, `@JsonCreator`, your own) or the class-load log: no
   * call path from the application's own code reaches it. Fails under the default `FailOn.REACHABLE`, not under `FailOn.DIRECT`.
   */
  REFLECTIVE,
  /**
   * Reachable only through `$deserializeLambda$`, which the JDK calls to rebuild a serializable lambda (every Scala function literal)
   * from bytes. Real only for closures that are really sent over the wire; the default `FailOn.REACHABLE` does not fail on it,
   * `FailOn.ANY` does.
   */
  DESERIALIZED;

  /** How likely a label is to break something, for ordering a report and for choosing one label among several: 5 for `REACHABLE` down to 0 for `LATENT`. */
  public int severity() {
    switch (this) {
      case REACHABLE:    return 5;
      case REFLECTIVE:   return 4;
      case PROBED:       return 3;
      case DESERIALIZED: return 2;
      case GUARDED:      return 1;
      default:           return 0;
    }
  }

  /** Labels the default `FailOn.REACHABLE` fails on: what the code calls, and what a reflection or keep rule makes it call. */
  public static boolean counts(ConflictLabel label) {
    return label == REACHABLE || label == REFLECTIVE;
  }
}
