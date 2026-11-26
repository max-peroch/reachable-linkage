# reachable-linkage

A linkage checker for the JVM that only reports the breaks your app can actually hit.

Upgrade a library and some other jar still calls a method that is gone: you get a
`NoSuchMethodError` at run time, maybe weeks later. Checkers find these by comparing every
reference with the classpath, and report all of them, including the ones in code nobody calls.
`reachable-linkage` follows the call graph from your code, so each finding says whether it can run.

```
 your code ──▶ lib-a 2.0 ──▶ lib-b.parse()      lib-b 2.0 removed parse()
     │                                           
     └──────▶ lib-c ──▶ lib-b.render()           lib-b 2.0 removed render()

   lib-a → parse():    REACHABLE   (your code calls lib-a)        ← fix this
   lib-c → render():   LATENT      (nothing you call gets there)  ← ignore
```

## What it catches

```java
// app, built against lib 1.0, run with lib 2.0
lib.Gone.x();          // method removed      → REACHABLE   NoSuchMethodError
new lib.Vanished();    // class removed       → REACHABLE   NoClassDefFoundError
try { lib.Gone.x(); } catch (NoSuchMethodError e) { }   // → GUARDED  (expected to fail)
Class.forName("x.Optional");                    // missing → PROBED   (an availability check)
```

Every kind of link it checks, as the smallest code that breaks (built against lib 1.0, run with 2.0):

```java
new lib.Gone();                      // class removed                          NoClassDefFoundError
lib.A.m();                           // method removed                         NoSuchMethodError
int n = lib.A.f;                     // field removed                          NoSuchFieldError
lib.A.m();                           // m() became static, or an instance one  IncompatibleClassChangeError
shape.area();                        // Shape was an interface, now a class    IncompatibleClassChangeError
new lib.Helper();                    // Helper became package-private          IllegalAccessError
lib.A.m();                           // m() became private                     IllegalAccessError
new sun.security.x509.X500Name(s);   // package not exported by its JDK module IllegalAccessError
lib.Config.size = 3;                 // size became final                      IllegalAccessError
class App extends lib.Base {}        // Base became final or an interface      IncompatibleClassChangeError
class App extends lib.Base { void m() {} }  // Base.m() became final           VerifyError
new lib.Shape();                     // Shape became abstract                  InstantiationError
class Impl implements lib.Api {}     // Api gained a method Impl lacks         AbstractMethodError
class App { @Inject App(lib.Dep d) {} }  // Dep removed, a framework reflects   NoClassDefFoundError
META-INF/services/lib.Plugin         // lists com.acme.P, which is not there   ServiceConfigurationError
```

A class file that cannot be read is reported too.

It also follows code the frameworks call for you (Guice `@Inject`/`@Provides`, Spring beans and
autoconfiguration, Jackson `@JsonCreator`, reflection by constant name).

| label | meaning |
| --- | --- |
| `REACHABLE` | your code can get there |
| `REFLECTIVE` | only a framework or reflection rule reaches it, not a call (still fails by default) |
| `DESERIALIZED` | only a lambda rebuilt from bytes (Spark, remoting) reaches it; does not fail by default |
| `LATENT` | the broken code is never called |
| `GUARDED` | wrapped in a `catch` for that error |
| `PROBED` | the code checks for the class by name first |

## A real run as evidence

```
java -Xlog:class+load:file=run.log -jar app.jar     # Java 8: -verbose:class
```

Parse the log with `ClassLoadLog.parse(path)` and pass its `classes` to `LinkageRequest.withLoadedClasses` (the bridge key is `classLoadLog`; the sbt plugin does not expose it). Every class the run loaded counts as used, which covers what no static analysis can see (classes loaded by a name from a config file).

## Precision

`METHOD` is the default (follows the call graph and labels each conflict); `CLASS` is what missinglink does; `METHOD_RTA` also only follows calls into classes that are built. A build fails on `REACHABLE` and `REFLECTIVE` (reached only through a framework or reflection rule) conflicts by default; `FailOn.DIRECT` fails on `REACHABLE` only.

## Your own frameworks

Tell it what your framework calls by reflection, in a rules file (Guice, Spring and Spring Boot, Jackson, Gson, servlet containers, Akka or Pekko actors and jboss-logging are built in):

```
keep-members            com.acme.Wired            # a method or field the container uses
keep-no-arg-constructor com.acme.Entity           # built through its default constructor
follow-class-values     com.acme.Uses             # @Uses(Impl.class) means Impl is used
keep-overrides          javax.servlet.Filter      # subtypes are built by the container, which calls what they override
keep-bean-arguments     com.acme.Mapper.read      # a class passed to this call is built, and its getters and setters called
keep-built-arguments    com.google.gson.Gson.fromJson   # a class passed to this call is built, and its fields read
keep-constructed-arguments com.acme.Spawner.spawn   # a class passed to this call is built through whichever constructor fits
keep-constructors       com.acme.Component        # a class with this annotation is built through whichever constructor fits
requires-classes        com.acme.OnClass          # the container ignores a class whose annotation names a class that is not on the classpath
keep-generated-classes  Impl                      # a framework handed UserMapper loads the generated UserMapperImpl by name
keep-classes-named-in   reference.conf            # a framework loads the classes this file names by name (opt-in: a default config names unused ones too)
```

## Use

The core is a JVM library: `Linkage.check(LinkageRequest)` returns the conflicts. It runs on Java 8 or later. The sbt plugin (sbt 1 and sbt 2) keeps
sbt-missinglink's tasks and settings and adds `missinglinkPrecision` (`Method` follows the call graph, `Class` is the old behaviour).

## Prior work

- [spotify/missinglink](https://github.com/spotify/missinglink) (Apache 2.0): the checker this
  builds on and is tested against, and the model for the report and settings.
- [twitter-incubator/classpath-verifier](https://github.com/twitter-incubator/classpath-verifier)
  (Apache 2.0): studied for its approach to checking Scala classpaths.
- [GoogleCloudPlatform/cloud-opensource-java](https://github.com/GoogleCloudPlatform/cloud-opensource-java)
  (Apache 2.0): its Linkage Checker, studied for how it reports linkage errors across a classpath.

Not a fork of any of them.

## License

Apache 2.0 (see `LICENSE` and `NOTICE`).
