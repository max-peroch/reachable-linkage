ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "io.github.max-peroch"
ThisBuild / licenses     := List(License.Apache2)
ThisBuild / homepage     := Some(uri("https://github.com/max-peroch/reachable-linkage"))
ThisBuild / developers   := List(Developer("max-peroch", "Maxime Perocheau", "max.peroch@hotmail.fr", uri("https://perocheau.com")))
ThisBuild / description  := "A linkage checker for the JVM that reports only the breaks an application can actually hit"
// Binary compatibility is promised from 1.0: until then a minor version may break it.
ThisBuild / versionScheme     := Some("early-semver")
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")

// Test-only: Scala compilers used to build fixtures, each resolved into its own configuration so
// they never share a classpath (2.12 and 2.13 libraries cannot coexist). Tests load each one in an
// isolated classloader.
val Scalac212   = config("scalac212").hide
val Scalac213   = config("scalac213").hide
val Scalac3Lts  = config("scalac3lts").hide
val Scalac3Next = config("scalac3next").hide
// Test-only: real framework jars the framework-pack fixtures are built and run against.
val GuiceJars   = config("guicejars").hide
val JacksonJars = config("jacksonjars").hide
val GsonJars    = config("gsonjars").hide
val JettyJars   = config("jettyjars").hide
val PekkoJars   = config("pekkojars").hide

// The published module is checked, in binary compatibility, against the version released before the one being built (`mimaReportBinaryIssues`).
// Without a tag for an earlier release there is nothing to compare with.
lazy val againstLastRelease = Seq(
  mimaPreviousArtifacts := previousStableVersion.value.map(version => organization.value %% moduleName.value % version).toSet
)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "reachable-linkage-core",
    againstLastRelease,
    // The suites share heavy global state (missinglink's full-JDK model, the Scala compilers), and run
    // in one JVM: sequential keeps every test inside its time limit.
    Test / parallelExecution := false,
    // The core runs on Java 8 and later: Scala 3.3 LTS is the newest line that can target Java 8 (3.8 and later need 17),
    // and -release:8 makes the compilers reject any API newer than Java 8. The facade enums are Java sources
    // (a Scala 3.3 enum that extends java.lang.Enum hands Java callers null if Scala code touches it first).
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("-Ywarn-unused")
      case _            => Seq("-Wunused:all")
    }),
    Compile / scalacOptions += "-release:8",
    // The core is also built for Scala 2.12, the Scala of an sbt 1 plugin: its sources are the syntax both compilers read, and what only Scala 3 has
    // (opaque ids) is in src/main/scala-3, with its 2.12 counterpart in src/main/scala-2.12. The suites are Scala 3; the 2.12 build has its own small one.
    crossScalaVersions := Seq("3.3.7", "2.12.21"),
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0")
      case _            => Nil
    }),
    // The Scala 3 suites cannot run on 2.12: there, only the sources in src/test/scala-2.12 are tests.
    Test / unmanagedSources / excludeFilter := (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => new SimpleFileFilter(f => !f.getPath.contains("scala-2.12"))
      case _            => NothingFilter
    }),
    Compile / javacOptions ++= Seq("--release", "8", "-Xlint:-options"),
    Compile / doc / javacOptions += "-Xdoclint:none",
    // The tests run on a newer JVM than the core promises (their compilers and child JVMs need 17), so they may use newer APIs.
    Test / scalacOptions := (Test / scalacOptions).value.filterNot(_.startsWith("-release")) :+ "-release:17",
    ivyConfigurations ++= Seq(
      Scalac212,
      Scalac213,
      Scalac3Lts,
      Scalac3Next,
      GuiceJars,
      JacksonJars,
      GsonJars,
      JettyJars,
      PekkoJars
    ),
    libraryDependencies ++= Seq(
      "org.ow2.asm"    % "asm"   % "9.11.1",
      "org.scalameta" %% "munit" % "1.2.3" % Test,
      // Test-only: the parity oracle. Never a runtime dependency.
      "com.spotify"                % "missinglink-core"  % "0.2.11"  % Test,
      "org.scala-lang"             % "scala-compiler"    % "2.12.21" % Scalac212,
      "org.scala-lang"             % "scala-compiler"    % "2.13.18" % Scalac213,
      "org.scala-lang"             % "scala3-compiler_3" % "3.3.9"   % Scalac3Lts,
      "org.scala-lang"             % "scala3-compiler_3" % "3.10.1"  % Scalac3Next,
      "com.google.inject"          % "guice"             % "4.2.3"   % GuiceJars,
      "com.fasterxml.jackson.core" % "jackson-databind"  % "2.14.0"  % JacksonJars,
      "com.google.code.gson"       % "gson"              % "2.10.1"  % GsonJars,
      // A real servlet container, to run servlets in.
      "org.eclipse.jetty" % "jetty-servlet"    % "9.2.21.v20170120" % JettyJars,
      "org.apache.pekko"  % "pekko-actor_2.13" % "1.6.0"            % PekkoJars
    ),
    Test / resourceGenerators += Def.task {
      val conv                           = fileConverter.value
      val types                          = classpathTypes.value
      val up                             = update.value
      def jars(c: Configuration): String =
        Classpaths.managedJars(c, types, up, conv).map(a => conv.toPath(a.data).toString).mkString(java.io.File.pathSeparator)
      val props = new java.util.Properties()
      props.setProperty("2.12", jars(Scalac212))
      props.setProperty("2.13", jars(Scalac213))
      props.setProperty("3-lts", jars(Scalac3Lts))
      props.setProperty("3-next", jars(Scalac3Next))
      props.setProperty("guice", jars(GuiceJars))
      props.setProperty("jackson", jars(JacksonJars))
      props.setProperty("gson", jars(GsonJars))
      props.setProperty("jetty", jars(JettyJars))
      props.setProperty("pekko", jars(PekkoJars))
      val out = (Test / resourceManaged).value / "scala-compilers.properties"
      IO.createDirectory(out.getParentFile)
      val w = new java.io.FileWriter(out)
      try props.store(w, "resolved Scala compiler classpaths for fixtures")
      finally w.close()
      Seq(out)
    }.taskValue
  )

lazy val root = rootProject.aggregate(core).settings(name := "reachable-linkage", publish / skip := true)
