package reachablelinkage.classfile

/**
 * How `ClassInfo.annotations` is laid out: four ints per entry, `(kind, member, annotation, value)`. The kind says what the entry is, `member` is the
 * method or field index (or -1 for the class), `annotation` the annotation type, and `value` the class a value inside it names (or -1 for an entry that
 * only says the annotation is there). Every reader goes through these names, so a new kind is a constant here and not a number in six files.
 */
private[reachablelinkage] object AnnotationEntries:
  /** Ints per entry. */
  final val Width = 4

  /** An annotation on the class. */
  final val OnClass = 0
  /** An annotation on a method or constructor. */
  final val OnMethod = 1
  /** A class value inside an annotation (`@ImplementedBy(Impl.class)`), nested annotations included. */
  final val ClassValue = 2
  /** An annotation on a field. */
  final val OnField = 3
  /** A class named by a string inside an annotation (`@ConditionalOnClass(name = "a.B")`). */
  final val NamedClassValue = 4

  extension (entries: Array[Int])
    inline def kindAt(i: Int): Int       = entries(i)
    inline def memberAt(i: Int): Int     = entries(i + 1)
    inline def annotationAt(i: Int): Int = entries(i + 2)
    inline def valueAt(i: Int): Int      = entries(i + 3)

    /** Is entry `i` an annotation on the class itself? */
    inline def isOnClass(i: Int): Boolean = entries(i) == OnClass
    /** Is entry `i` an annotation on a method? */
    inline def isOnMethod(i: Int): Boolean = entries(i) == OnMethod
    /** Is entry `i` an annotation on a field? */
    inline def isOnField(i: Int): Boolean = entries(i) == OnField
    /** Is entry `i` an annotation on the class, a method or a field (and not a value inside one)? */
    inline def isAnnotationUse(i: Int): Boolean = entries(i) != ClassValue && entries(i) != NamedClassValue
    /** Does entry `i` name a class inside an annotation, by class value or by string? */
    inline def isClassNamedInside(i: Int): Boolean = entries(i) == ClassValue || entries(i) == NamedClassValue
    /** Is entry `i` a class value (not a name) inside an annotation? */
    inline def isClassValue(i: Int): Boolean = entries(i) == ClassValue
