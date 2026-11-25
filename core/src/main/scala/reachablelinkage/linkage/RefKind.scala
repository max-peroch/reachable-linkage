package reachablelinkage.linkage

/** Which reference array of a class a finding points into. */
enum RefKind:
  case Method
  case Field

  /** A class used by a type instruction, or by a constructor call. */
  case Type

  /** The superclass or an interface: declared by the class, not by an instruction. */
  case Hierarchy

  /** A provider named by a `META-INF/services` file: declared by a resource, not by an instruction. */
  case Service
