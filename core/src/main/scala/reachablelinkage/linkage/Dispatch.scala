package reachablelinkage.linkage

/** Which classes a virtual or interface call can land in. */
enum Dispatch:
  /** Class hierarchy analysis: the override in any class that has been reached. */
  case Cha
  /**
   * Rapid type analysis: only classes something has instantiated (`new` in reached code, a constructor
   * reference, a class the application, a service file, a keep rule or reflection by constant name builds),
   * as the target is looked up from the instantiated class, so an inherited method counts. Fewer
   * targets than CHA, so more conflicts are Latent; it cannot see an instance made some way not listed
   * (`Unsafe.allocateInstance`, a class built by a name computed at run time).
   */
  case Rta
