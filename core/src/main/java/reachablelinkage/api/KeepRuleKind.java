package reachablelinkage.api;

/** What a [[KeepRule]] keeps, for a class that has been reached. */
public enum KeepRuleKind {
  /** A method, constructor or field with one of the annotations is used by a framework (`@Inject`, `@Provides`). */
  KEEP_MEMBERS,
  /** A class with one of the annotations on it or on a method is constructed by a framework through its no-argument constructor. */
  KEEP_NO_ARG_CONSTRUCTOR,
  /** A class named inside one of the annotations is used by a framework (`@ImplementedBy(Impl.class)`). */
  FOLLOW_CLASS_VALUES,
  /**
   * A class that extends or implements one of the named types is constructed by a framework through its no-argument constructor, and the
   * framework calls its overrides of the type's methods (`HttpServlet.doGet`). The names are classes, not annotations.
   */
  KEEP_OVERRIDES,
  /**
   * A class literal passed to one of the named calls (`com.fasterxml.jackson.databind.ObjectMapper.readValue`: a class, a dot, a method)
   * is a class a framework is handed: it is built through its no-argument constructor and its bean accessors (`getX`, `isX`, `setX`) are
   * called.
   */
  KEEP_BEAN_ARGUMENTS,
  /**
   * A class literal passed to one of the named calls is built through its no-argument constructor and its fields are read and written
   * (Gson): [[KEEP_BEAN_ARGUMENTS]] without the accessors.
   */
  KEEP_BUILT_ARGUMENTS,
  /**
   * Every class that a file of one of the named names (`reference.conf`, `META-INF/persistence.xml`: a file name or the end of a path) names
   * is loaded by the framework that reads the file, and is a root. The names are files, not classes.
   */
  KEEP_CLASSES_NAMED_IN,
  /**
   * A class literal passed to one of the named calls is built by the framework through whichever of its constructors fits the arguments it is
   * given (`Props.create(Worker.class, "x")`): every constructor is a root.
   */
  KEEP_CONSTRUCTED_ARGUMENTS,
  /**
   * A class with one of the named annotations on it is built by a framework through whichever of its constructors fits (Spring's `@Component`,
   * `@Configuration`): it is instantiated and every constructor is a root.
   */
  KEEP_CONSTRUCTORS,
  /**
   * A framework ignores a class or method with one of the named annotations unless every class the annotation names is on the classpath
   * (`@ConditionalOnClass`): it is not rooted by any rule.
   */
  REQUIRES_CLASSES,
  /**
   * A class that is reached, with a class on the classpath named after it plus one of the named suffixes (`Foo` and `Foo_$logger`), makes that class a root:
   * a framework handed `Foo` loads its generated companion by name. The names are suffixes, not classes.
   */
  KEEP_GENERATED_CLASSES;
}
