# reachable-linkage

A Scala linkage checker for sbt that tracks reachability per method, so it reports only the `NoSuchMethodError`/`NoClassDefFoundError`-style breaks the app can actually hit.

Plan (goals, architecture, risks): https://claude.ai/artifact/R6cVAKiYNK5KLZLXDbERFY

The code is in `core/`; the sbt plugin is a separate clone (see "The sbt plugin"). Where this file and the code or tests disagree, the code and tests win.

## Decisions

- **Name:** `reachable-linkage` (core `reachable-linkage-core`, plugin `sbt-reachable-linkage`). If scalacenter adopts the work, the plugin keeps the `sbt-missinglink` name and contract. Do not rename.
- **Contract:** keep sbt-missinglink's task names, settings and report format. `missinglinkScanDependencies := true` means `Full` scope + `Class` precision + `Any` fail level.
- **Language:** idiomatic Scala 3 core: opaque types for ids, enums/sealed ADTs, `Either` for errors. Plain `while` loops only in private hot paths; otherwise `@tailrec` (see Rules).
- **Dependencies:** ASM is the only runtime dependency. No Jackson, no missinglink-core at runtime (test-only, as the parity oracle), no cats-effect/fs2/circe at runtime. Typelevel is fine in test and build (munit, ScalaCheck).
- **JVM floor:** the core runs on **Java 8 and later**: Scala 3.3 LTS with `-release:8`, `javac --release 8`, so the compilers reject newer JDK APIs. Tests are compiled for 17. The nine facade enums are
  **Java sources** (`core/src/main/java/reachablelinkage/api`), not Scala enums: a Scala 3.3 enum extending `java.lang.Enum` has a static-init cycle that hands a Java caller `null`.
  `ClassSource.runningJdk` reads the module image on 9+ and the boot jars on 8. `Java8Suite` needs `JAVA_8_HOME` (skipped without it).
- **Isolation:** the sbt plugin loads the core in its own classloader behind a bridge that takes and returns only JDK types, because an sbt 1 plugin has Scala 2.12 and the core needs Scala 3 and 2.13.
- **Precision and labels:** the default is `Method` (CHA call graph); `Class` is the compatibility view; `MethodRta` is opt-in (it can silently miss an instance made some way it cannot see). `Method` labels exactly
  the conflicts `Class` reports, so Method ⊆ Class holds by construction.
- **No command-line entry point.** A plugin calls `Linkage.check` (or the bridge) directly.
- **Prior work:** the README credits spotify/missinglink, twitter-incubator/classpath-verifier and Google's Linkage Checker (cloud-opensource-java). NOTICE entries only for files actually adapted. No fork of any of them.
- **Nothing goes upstream (scalacenter) without an explicit go-ahead.** The plugin clone's push URL is disabled.

## Package layout (`reachable-linkage-core`)

Concept-named packages under `reachablelinkage`, never `core`/`util`/`common`; one top-level type per file; internals `private[pkg]`. Dependencies point one way: `symbols` <- `classfile` <- `classpath` <- `linkage`.

- `symbols`: `ClassId`/`NameId`/`DescId` (opaque ints), `Interner`, `Symbols`.
- `classfile`: ASM visitor `ClassParser` (`SKIP_DEBUG | SKIP_FRAMES`, no `ClassNode`), `ClassInfo`, `Member`, `Probe`, `AccessFlags`.
- `classpath`: `ClassSource` (jar/dir/JDK table of contents), `ClassIndex` (lazy, first entry wins), `JarSummary` (per-jar cache).
- `linkage`: `Checker`, `Scope`, `Conflict`, `Finding`, `Locator`, `Keep`/`KeepRules`, `FrameworkTargets`; internal `Reachability` (class walk), `MethodGraph` (call graph), `Resolver`.
- `api`: the Java-friendly facade (`Linkage.check(LinkageRequest): LinkageResult`, `KeepRule`, `ConflictFilter`) and `LinkageBridge` (maps in, maps out).
- Tests: `reachablelinkage.fixture` holds the fixture builders and the missinglink oracle.

## Rules (from the user; they override defaults)

- **Errors are values.** Every failure the code can describe is a case of a small sealed enum per layer with a `message`, returned in an `Either`. No `Either[String, _]`, no `Try`, no `throw` anywhere in `main`
  (JDK exceptions at an I/O boundary are caught there and turned into a value). No throwing wrappers for Java callers. Tests assert on the variants, not on message text.
- **Functional style wherever it costs no measured performance;** benchmark before giving it up. Loops over indices, arrays and bit sets are `@tailrec` local methods carrying primitives (the compiler checks
  they are real tail calls, and they cost the same as a `while`); never allocate per step in one.
- **Small specialized methods with explicit names.** Extract a named method rather than a long body or a comment.
- Access flags are defined once (`AccessFlags`). Names in rules have their own opaque types (`RuleNames`). A sealed match is preferred over `Int` kinds at function boundaries.
- Public `def`s and `val`s have explicit return types; case classes are `final`; ADTs are sealed.
- The README stays very succinct.
- **No project history in the repo** (it is unreleased): docs and comments describe what is, not what changed or why it used to be different.

## Invariants that bite when forgotten

- **Bump `ClassParser.Version` whenever what `parse` produces changes**, so old per-jar summaries are not read. `JarSummary.FormatVersion` is for the file format. A test fails if `ClassInfo` gains or loses a field.
- **Label precedence** (weakest first): `Latent`, `Guarded`, `Probed`, `Deserialized`, `Reflective`, `Reachable`; report severity is the reverse. `FailOn.REACHABLE` (default) fails on `Reachable` and `Reflective`;
  `DIRECT` on `Reachable` only; `ANY` on every label. `Class` precision has no call graph, so no `Reflective` label. The call-graph phases (`Direct`, `Rule`, `Deserialization`) must only add to earlier ones.
- **Name rules never invent a class:** a constant, fragment or config name that is not on the classpath is ignored (a missing one is the `Probed` label's business). Rules over-approximate by design, which errs towards reporting.
- **A scenario is evidence only with a control run:** the same program must work against the old library, and the failure must be the specific error. Every scenario that really fails on the JVM must be `Reachable` or
  `Reflective` at `Method` and `MethodRta`. Check new checks for false positives on the on-demand real classpaths (below): rows there must stay identical unless the change is meant to move them.
- **Adding a kind of conflict is one place:** the `Conflict` case, its `Conflict.describe` entry, the producer in `Checker`, and if the category is new, one case each in `linkage.Category` and the Java `ConflictCategory`.
  `ConflictDescriptionSuite` keeps this honest. Adding a kind of keep rule: the `Keep` case, its bits in `KeepRules`, the `KeepRuleKind` case and its `RuleKinds.describe` line.
- **The facade stays Java-callable** and the bridge crosses only `java.*` types (`LinkageApiSuite`, `JavaCallerSuite`, `LinkageBridgeSuite` pin it). New request settings go on the defaulted case class and the builder, not on another constructor.
- **Java 8 floor:** no JDK API newer than 8 in `main`; `Java8Suite` and runs on JDK 17, 21 and 25 catch regressions. javac 17 merges identical lambda bodies, so fixtures must not depend on javac's layout.
- Compare timings only **interleaved or back to back** on the same machine; absolute numbers move by a factor of two between days.

## Running things

- `sbt core/test` is the suite (CI runs it on JDK 17 and 25). Suites that use missinglink-core as an oracle extend `OracleSuite` (5-minute timeout; its first use builds a JDK model, about 12 s and 2 GB).
- On demand, never in `sbt test` (test sources, plain `main`): `reachablelinkage.bench.Bench` (`app`, `ours`, `ml`), `Profile`, `LoopBench`, and in `linkage`: `Soundness`, `BlindSpots`. Real classpaths
  (Play, pekko-http, http4s, armeria, Spring Boot, sbt) are resolved offline into a scratch sbt project and passed as a file with one jar per line.
- `tools/coverage-agent/build.sh` builds a small java agent (ASM; jar gitignored) that records the methods that ran; `Soundness` then checks that the call graph reaches them (worked example in its README). It is a dev tool, not shipped.
- `sbt core/publishLocal` publishes `reachable-linkage-core_3` to `~/.ivy2/local`, where the plugin fetches it.

## The sbt plugin

A **patched local clone of sbt-missinglink** at `~/IdeaProjects/sbt-missinglink`, branch `reachable-linkage-engine`. One source file builds for **sbt 1 (Scala 2.12, Java 8) and sbt 2 (Scala 3.8.4)** via `sbt2-compat`;
both are supported. Licence is Apache 2.0 (keep its LICENSE/NOTICE).

- **Engine in its own classloader.** The plugin fetches `reachable-linkage-core_3` with the **Coursier API** (not sbt's resolution, which forces the project's Scala library onto the engine and breaks it), loads it in a
  `URLClassLoader` whose parent is the platform loader, and calls the one static method `reachablelinkage.api.LinkageBridge.check(Map): Map` by reflection (`EngineBridge.scala`).
- **Settings kept from upstream** (names, defaults, report format at `Class` precision). **New:** `missinglinkPrecision` (`Class`|`Method`), `missinglinkKeepRulesFile`, `missinglinkCacheDirectory` (default under `target`).
  The engine version is a constant in `EngineBridge`, fetched from Maven Central and `~/.ivy2/local`. `missinglinkScanDependencies` is the one switch for scope: false (default) starts from the project's classes and fails
  on the conflicts they can reach; true starts from every class and fails on every conflict, and implies `Class` precision unless `missinglinkPrecision` is set. `Class` precision is the old sbt-missinglink behaviour, with the scan switch choosing the scope as it always did (off: its default; on: its full scan).
  At `Class` precision the plugin shows only the three conflict kinds missinglink reports, in its own layout.
- **Label-aware summary** (`ConflictSummary`, pure and unit tested): at `Method` precision it counts reachable apart from unreachable conflicts, suggests excluding only dependencies whose conflicts are all unreachable,
  lists reachable ones as "to fix", and shows method and line for conflicts in the project's own code. **Eviction note:** names a version dependency resolution replaced (and who asked for it) when a conflict involves that
  module, read from sbt's `evicted` task (the `update` report's `details` are empty with Coursier).
- **Not exposed by the plugin:** a class-load-log setting. The core has `LinkageRequest.loadedClasses` and the bridge key `classLoadLog`.
- **No sbt 2 task caching.** `missinglinkCheck` is `@transient`. Caching it would key only on declared inputs, and the project's class directory and the rules file are plain paths whose contents are not hashed, so an
  edit could return a stale "no conflicts"; whether log output is replayed on a hit is undocumented; and the gain is a warm engine run of seconds. The per-jar summary cache makes warm runs fast. To add it: hash the project
  classes and the rules file into the key, return plain data, and test that each invalidates.
- **Pitfalls:** `key := Def.task { ... }` without `.value` compiles and silently never runs the body; `+:` and `++` cannot be mixed in one expression (Scala 3 and 2.12 both reject it); use `.asJava`, not `: _*`;
  on sbt 2 `target` is under `target/out/jvm/...`, so fixtures use `baseDirectory`.
- **Tests:** `sbt test` (munit, both Scala versions) for `ConflictSummary`; `sbt scripted` on sbt 1 and `sbt ++3.8.4 scripted` on sbt 2, 12 fixtures under `src/sbt-test/missinglink` (the 10 upstream ones plus
  `latent-conflict` and `engine-settings`); they assert that tasks pass or fail, not the text. Republish the core (`sbt core/publishLocal`) before a plugin run that needs a core change.
- **Not yet:** the engine is not published to a repository (the plugin reads `~/.ivy2/local`); a PR to scalacenter only on an explicit go-ahead.

## Known limits

Computed class names in general; a class literal returned by one method and built by another (Hibernate persisters: pass a class-load log through `loadedClasses`); `MethodRta` can miss an instance made by a route it
cannot see (so it is opt-in); Windows and arm64 are untested; the GitHub workflow has not run.

## Testing notes

- Upstream issues (spotify/missinglink) are covered by `UpstreamIssuesSuite`: #39 static method in a superclass, #23 shadowed duplicate classes, #55 `MethodHandle` signature polymorphism, #25 missing supertype,
  #22 interface gained a method (`AbstractMethodError`), #11 access and static flags, #7 guarded by catch, #8 reachability, #6 DI containers.
- Scala fixtures (`ScalaFixturesSuite` and friends) build in-process with real scalac 2.12, 2.13, 3 LTS and 3 current, each in its own classloader (test-only sbt configurations supply the compiler classpaths).
- Mutation testing is the habit: break one engine part and require the matching test to fail.
