package reachablelinkage.linkage

import reachablelinkage.linkage.RuleNames.*

/**
 * A keep rule: what a framework calls by reflection on a class that has been reached, so no
 * instruction names it ("when instantiated, keep"). Annotation types are internal names
 * (`javax/inject/Inject`); matching is by annotation type only, so it over-approximates.
 */
enum Keep:
  /**
   * A method or constructor with one of these annotations is a root (`@Inject`, `@Provides`), and a field with one is
   * read and written by the framework (field injection): either makes its class one a framework reflects on.
   */
  case Members(annotations: Set[ClassName])
  /**
   * A class with one of these annotations on it is built by a framework through whichever of its constructors fits the beans it has (Spring's
   * `@Component`, `@Service`, `@Configuration`): it is instantiated and every constructor is a root, and a framework that reads the class
   * loads the types of all its members.
   */
  case Constructors(annotations: Set[ClassName])
  /**
   * A class reached, with a class on the classpath named after it plus one of these suffixes (`Foo` and `Foo_$logger`), makes that class a root built through its
   * constructors: a framework that is handed `Foo` loads its generated companion by name (jboss-logging, annotation processors).
   */
  case GeneratedClasses(suffixes: Set[NameSuffix])
  /**
   * A framework ignores a class (or a method) with one of these annotations unless every class the annotation names, by class value or by
   * name, is on the classpath (Spring Boot's `@ConditionalOnClass`): no other rule roots it, and a class that a configuration file names
   * is not a root. Spring reads these annotations without loading the classes, so a class for a library that is absent is never run.
   */
  case RequiresClasses(annotations: Set[ClassName])
  /** A class with one of these annotations on it or on a method gets its public no-argument constructor as a root. */
  case NoArgConstructor(annotations: Set[ClassName])
  /** A class value inside one of these annotations is a reference to that class (`@ImplementedBy(Impl.class)`). */
  case ClassValues(annotations: Set[ClassName])
  /**
   * A class that extends or implements one of these types (internal names) is constructed by a framework through its no-argument
   * constructor, which calls its overrides of the type's methods (`HttpServlet.doGet`, `Filter.doFilter`): no instruction in the
   * classpath mentions either. Methods that override nothing the type declares are not kept.
   */
  case Overrides(types: Set[ClassName])
  /**
   * A class literal passed to one of these calls (`owner/Name.method`, internal owner) is a class a framework is handed
   * (`mapper.readValue(json, Dto.class)`): it is built through its no-argument constructor and its bean accessors (`getX`, `isX`,
   * `setX`, public, in it and in its classpath ancestors) are called, though nothing in the classpath calls them.
   */
  case BeanArguments(calls: Set[CallName])
  /**
   * A class literal passed to one of these calls (`owner/Name.method`) is built through its no-argument constructor and its fields are read
   * and written (Gson): like [[BeanArguments]] but no accessor is called.
   */
  case BuiltArguments(calls: Set[CallName])
  /**
   * A class literal passed to one of these calls (`owner/Name.method`) is built through whichever of its constructors fits the arguments the
   * framework is given (`Props.create(Worker.class, "x")` in akka and pekko): every constructor is a root, and nothing else of the class is.
   */
  case ConstructedArguments(calls: Set[CallName])
  /**
   * Every class that a file of one of these names (`reference.conf`, `META-INF/persistence.xml`) names, in any entry of the classpath, is loaded
   * by the framework that reads the file, and built through its no-argument constructor: it is a root, as a class from a class-load log is.
   */
  case ClassesNamedIn(files: Set[FileName])

object Keep:
  // Names of each kind, from strings: the packs and the tests write them as text.
  def classNames(names: String*): Set[ClassName] = names.map(ClassName(_)).toSet
  def callNames(names: String*): Set[CallName]   = names.map(CallName(_)).toSet
  def fileNames(names: String*): Set[FileName]   = names.map(FileName(_)).toSet
  def suffixes(names: String*): Set[NameSuffix]  = names.map(NameSuffix(_)).toSet

  /** Guice: injected constructors and methods, `@Provides`, and the classes `@ImplementedBy`/`@ProvidedBy` name. */
  val Guice: List[Keep] = List(
    Members(classNames(
      "javax/inject/Inject", "jakarta/inject/Inject", "com/google/inject/Inject", "com/google/inject/Provides",
      "com/google/inject/assistedinject/AssistedInject", "com/google/inject/multibindings/ProvidesIntoSet",
      "com/google/inject/multibindings/ProvidesIntoMap", "com/google/inject/multibindings/ProvidesIntoOptional")),
    ClassValues(classNames("com/google/inject/ImplementedBy", "com/google/inject/ProvidedBy")))

  // `JsonSerialize`, `JsonDeserialize` and `JsonTypeIdResolver` live in databind, the rest in jackson-annotations.
  private def plain(names: String*): Set[ClassName]    = classNames(names.map("com/fasterxml/jackson/annotation/" + _)*)
  private def databind(names: String*): Set[ClassName] = classNames(names.map("com/fasterxml/jackson/databind/annotation/" + _)*)

  private val jacksonKept = plain("JsonCreator", "JsonProperty", "JsonGetter", "JsonSetter", "JsonAnyGetter", "JsonAnySetter", "JsonValue") ++
    databind("JsonSerialize", "JsonDeserialize")

  /** Jackson: creators, properties, accessors and custom (de)serializers, the classes annotations name, and the default constructor of any annotated class. */
  val Jackson: List[Keep] = List(
    BeanArguments(callNames(List("readValue", "convertValue", "treeToValue", "readerFor", "writerFor").map("com/fasterxml/jackson/databind/ObjectMapper." + _)*)),
    Members(jacksonKept),
    ClassValues(plain("JsonSubTypes", "JsonTypeInfo", "JsonIdentityInfo", "JsonIdentityReference") ++
      databind("JsonSerialize", "JsonDeserialize", "JsonTypeIdResolver", "JsonTypeResolver", "JsonNaming", "JsonPOJOBuilder")),
    NoArgConstructor(jacksonKept ++ plain("JsonIgnoreProperties", "JsonIgnoreType", "JsonInclude", "JsonRootName", "JsonPropertyOrder",
      "JsonAutoDetect", "JsonFormat", "JsonTypeName", "JsonIgnore", "JsonUnwrapped", "JsonView", "JsonManagedReference",
      "JsonBackReference", "JsonSubTypes", "JsonTypeInfo") ++ databind("JsonNaming", "JsonPOJOBuilder")))

  /** Gson: a class handed to `fromJson` or `getAdapter` is built through its default constructor and its fields are read; `@JsonAdapter` names an adapter class. */
  val Gson: List[Keep] = List(
    BuiltArguments(callNames(List("fromJson", "getAdapter").map("com/google/gson/Gson." + _)*)),
    ClassValues(classNames("com/google/gson/annotations/JsonAdapter")))

  /**
   * Servlet containers (Tomcat, Jetty, Undertow): the container builds a servlet, filter or listener and calls what it overrides
   * (`doGet`, `doFilter`, `init`, `contextInitialized`), though no code in the classpath does. `javax` and `jakarta` both.
   */
  val Servlet: List[Keep] =
    val types = List("Servlet", "Filter", "ServletContextListener", "ServletContextAttributeListener", "ServletRequestListener",
      "ServletRequestAttributeListener", "AsyncListener", "http/HttpSessionListener", "http/HttpSessionAttributeListener",
      "http/HttpSessionActivationListener", "http/HttpSessionBindingListener", "http/HttpSessionIdListener")
    List(Overrides(classNames((for namespace <- List("javax", "jakarta"); t <- types yield s"$namespace/servlet/$t")*)))

  /** Akka and pekko actors: a class given to `Props` is built by the actor system, through the constructor that fits the arguments. */
  private def props(namespace: String): ConstructedArguments =
    val owners = List("Props", "Props$", "AbstractProps")
    ConstructedArguments(callNames((for owner <- owners; method <- List("create", "apply") yield s"$namespace/actor/$owner.$method")*))

  val Akka: List[Keep]  = List(props("akka"))
  val Pekko: List[Keep] = List(props("org/apache/pekko"))

  /** jboss-logging: `Logger.getMessageLogger(Foo.class)` loads the generated `Foo_$logger`, and `getMessageBundle` the `Foo_$bundle`. */
  val JBossLogging: List[Keep] = List(GeneratedClasses(suffixes("_$logger", "_$bundle")))

  private def spring(names: String*): Set[ClassName] = classNames(names.map("org/springframework/" + _)*)

  /**
   * Spring and Spring Boot: the container builds the stereotype classes and calls what it finds on them (`@Bean`, `@Autowired`, `@PostConstruct`,
   * request mappings, listeners), follows `@Import`, and the autoconfiguration classes Boot lists in `META-INF/spring.factories` and
   * the `AutoConfiguration.imports` file under `META-INF/spring` are loaded by name. Entities are built by the JPA provider.
   */
  val Spring: List[Keep] = List(
    Constructors(spring("stereotype/Component", "stereotype/Service", "stereotype/Repository", "stereotype/Controller",
      "web/bind/annotation/RestController", "web/bind/annotation/ControllerAdvice", "web/bind/annotation/RestControllerAdvice",
      "context/annotation/Configuration", "boot/autoconfigure/AutoConfiguration", "boot/SpringBootConfiguration",
      "boot/context/properties/ConfigurationProperties") ++
      classNames("jakarta/persistence/Entity", "jakarta/persistence/MappedSuperclass", "jakarta/persistence/Embeddable",
        "javax/persistence/Entity", "javax/persistence/MappedSuperclass", "javax/persistence/Embeddable")),
    Members(spring("context/annotation/Bean", "beans/factory/annotation/Autowired", "beans/factory/annotation/Value",
      "context/event/EventListener", "scheduling/annotation/Scheduled", "scheduling/annotation/Async", "transaction/annotation/Transactional",
      "web/bind/annotation/RequestMapping", "web/bind/annotation/GetMapping", "web/bind/annotation/PostMapping",
      "web/bind/annotation/PutMapping", "web/bind/annotation/DeleteMapping", "web/bind/annotation/PatchMapping",
      "web/bind/annotation/ExceptionHandler", "web/bind/annotation/InitBinder", "web/bind/annotation/ModelAttribute") ++
      classNames("jakarta/annotation/PostConstruct", "jakarta/annotation/PreDestroy", "jakarta/annotation/Resource",
        "javax/annotation/PostConstruct", "javax/annotation/PreDestroy", "javax/annotation/Resource")),
    ClassValues(spring("context/annotation/Import", "context/annotation/Conditional", "boot/autoconfigure/ImportAutoConfiguration",
      "boot/context/properties/EnableConfigurationProperties", "context/annotation/ComponentScan")),
    RequiresClasses(spring("boot/autoconfigure/condition/ConditionalOnClass")),
    ClassesNamedIn(fileNames("META-INF/spring.factories", "META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports")))

  /** What a check uses unless told otherwise: every pack. */
  val standard: List[Keep] = Guice ++ Jackson ++ Gson ++ Servlet ++ Akka ++ Pekko ++ Spring ++ JBossLogging
