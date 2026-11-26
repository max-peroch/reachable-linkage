ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "io.github.max-peroch"
ThisBuild / licenses     := List(License.Apache2)
ThisBuild / homepage     := Some(uri("https://github.com/max-peroch/reachable-linkage"))
ThisBuild / developers   := List(Developer("max-peroch", "Maxime Perocheau", "max.peroch@hotmail.fr", uri("https://perocheau.com")))
ThisBuild / description  := "A linkage checker for the JVM that reports only the breaks an application can actually hit"
// Binary compatibility is promised from 1.0: until then a minor version may break it.
ThisBuild / versionScheme     := Some("early-semver")
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all")

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
// sttp and tapir with a circe JSON body, and the same with an older circe.
val SttpTapirJars    = config("sttptapirjars").hide
val SttpTapirOldJars = config("sttptapiroldjars").hide
// logback with the optional dependencies it leaves out (Janino, Jakarta Servlet and Mail).
val LogbackJars = config("logbackjars").hide

// What the sttp and tapir control test needs: tapir with a circe JSON body and its sttp client, and the same set with circe 0.14.1 in place of circe.
val sttpTapirDependencies: Seq[ModuleID] = {
  val tapir  = Seq("tapir-core", "tapir-json-circe", "tapir-sttp-client4").map("com.softwaremill.sttp.tapir" %% _ % "1.14.1")
  val client = Seq("com.softwaremill.sttp.client4" %% "core" % "4.1.3")
  val circe  = Seq("io.circe" %% "circe-core" % "0.14.10", "io.circe" %% "circe-parser" % "0.14.10")
  val older  = Seq("circe-core", "circe-parser", "circe-jawn", "circe-numbers").map("io.circe" %% _ % "0.14.1")
  (tapir ++ client ++ circe).map(_ % SttpTapirJars) ++
    (tapir ++ client).map(_.excludeAll(ExclusionRule("io.circe")) % SttpTapirOldJars) ++ older.map(_ % SttpTapirOldJars)
}

lazy val core = project
  .in(file("core"))
  .settings(
    name := "reachable-linkage-core",
    // The suites share heavy global state (missinglink's full-JDK model, the Scala compilers), and run
    // in one JVM: sequential keeps every test inside its time limit.
    Test / parallelExecution := false,
    // The core runs on Java 8 and later: Scala 3.3 LTS is the newest line that can target Java 8 (3.8 and later need 17),
    // and -release:8 makes the compilers reject any API newer than Java 8. The facade enums are Java sources
    // (a Scala 3.3 enum that extends java.lang.Enum hands Java callers null if Scala code touches it first).
    Compile / scalacOptions += "-release:8",
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
      PekkoJars,
      SttpTapirJars,
      SttpTapirOldJars,
      LogbackJars
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
    ) ++ sttpTapirDependencies ++ Seq(
      "ch.qos.logback" % "logback-classic" % "1.5.18" % LogbackJars,
      "org.slf4j"      % "slf4j-api"       % "2.0.17" % LogbackJars
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
      props.setProperty("logback", jars(LogbackJars))
      props.setProperty("sttp-tapir", jars(SttpTapirJars))
      props.setProperty("sttp-tapir-old-circe", jars(SttpTapirOldJars))
      val out = (Test / resourceManaged).value / "scala-compilers.properties"
      IO.createDirectory(out.getParentFile)
      val w = new java.io.FileWriter(out)
      try props.store(w, "resolved Scala compiler classpaths for fixtures")
      finally w.close()
      Seq(out)
    }.taskValue
  )

lazy val root = rootProject.aggregate(core).settings(name := "reachable-linkage", publish / skip := true)
