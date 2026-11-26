package reachablelinkage.api;

/** Same names as missinglink-core's categories, so existing report code keeps working. */
public enum ConflictCategory {
  CLASS_NOT_FOUND,
  METHOD_SIGNATURE_NOT_FOUND,
  FIELD_NOT_FOUND,
  /** Not in missinglink: a class file we could not read. */
  UNREADABLE_CLASS,
  /**
   * Not in missinglink: a `META-INF/services` file lists a provider that is not on the classpath, so
   * iterating that service throws `ServiceConfigurationError`.
   */
  MISSING_PROVIDER,
  /** Not in missinglink: a class that exists but may not be named from where it is used (`IllegalAccessError`). */
  INACCESSIBLE_CLASS,
  /** Not in missinglink: a method or field that exists but became inaccessible, such as `public` to `private`. */
  INACCESSIBLE_MEMBER,
  /**
   * Not in missinglink: a concrete class does not implement an abstract method it inherits, so calling
   * it throws `AbstractMethodError` (for instance after an interface gained a method).
   */
  ABSTRACT_METHOD_NOT_IMPLEMENTED,
  /**
   * Not in missinglink: a class extends something that became `final` or an interface, or implements
   * something that became a class, so loading it throws `IncompatibleClassChangeError`.
   */
  INCOMPATIBLE_HIERARCHY,
  /** Not in missinglink: a class overrides a method its superclass made `final`, so loading it throws `VerifyError`. */
  OVERRIDES_FINAL_METHOD,
  /**
   * Not in missinglink: a call is linked as an interface method but the owner became a class, or the
   * reverse, so it throws `IncompatibleClassChangeError`.
   */
  INTERFACE_CLASS_MISMATCH,
  /** Not in missinglink: `new` of a class that became abstract or an interface, so it throws `InstantiationError`. */
  ABSTRACT_INSTANTIATION,
  /** Not in missinglink: a write to a field that became `final`, from another class, so it throws `IllegalAccessError`. */
  FINAL_FIELD_WRITE,
  /**
   * Not in missinglink: a class a framework reflects on (an injected or serialized class) has a member whose
   * parameter, return or field type is missing, so reflecting over the class throws `NoClassDefFoundError`.
   */
  MISSING_SIGNATURE_TYPE,
  /**
   * Not in missinglink: a reference to a JDK class whose module does not export its package to the classpath, such as
   * `sun.security.x509`, so the JVM throws `IllegalAccessError` unless started with `--add-exports`.
   */
  NOT_EXPORTED;

  /**
   * Is this one of the three categories missinglink-core itself reports (class, method and field not found)? A plugin that keeps the
   * old `missinglinkScanDependencies := true` behaviour (class precision, fail on any) can restrict itself to these, so a build that
   * passes today does not start failing on a kind of conflict missinglink never looked for (an unimplemented abstract method, an
   * access flag, a `final` change, ...).
   */
  public boolean reportedByMissinglink() {
    return this == CLASS_NOT_FOUND || this == METHOD_SIGNATURE_NOT_FOUND || this == FIELD_NOT_FOUND;
  }
}
