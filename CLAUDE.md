# reachable-linkage

A linkage checker for the JVM that tracks reachability per method, so it reports only the `NoSuchMethodError`/`NoClassDefFoundError`-style breaks an application can actually hit.

The code is in `core/`; the sbt plugin lives in its own repository (see "The sbt plugin"). Where this file and the code or tests disagree, the code and tests win.

## Decisions

- **Name:** `reachable-linkage` (core `reachable-linkage-core`). The plugin keeps the `sbt-missinglink` name and contract. Do not rename.
- **Language:** idiomatic Scala 3: opaque types for ids, enums/sealed ADTs, `Either` for errors.
- **Dependencies:** ASM is the only runtime dependency. missinglink-core is test-only (the parity oracle). Typelevel is fine in test and build.
- **JVM floor:** the core runs on **Java 8 and later** (Scala 3.3 LTS, `-release:8`, `javac --release 8`); tests are compiled for 17. The facade enums are **Java sources**, because a Scala 3.3 enum extending
  `java.lang.Enum` hands a Java caller `null`. `OtherJavaSuite` runs the checker in a child JVM of the Java at `CHECK_JAVA_HOME` (skipped without it; CI sets it for 8 to 16).
- **Precision and labels:** `Method` (CHA call graph) is the default, `Class` the compatibility view, `MethodRta` opt-in (it can silently miss an instance made by a route it cannot see). `Method` labels exactly
  the conflicts `Class` reports.
- **No command-line entry point.** A caller uses `Linkage.check` or the bridge.
- **Prior work:** the README credits spotify/missinglink, twitter-incubator/classpath-verifier and Google's Linkage Checker. NOTICE entries only for files actually adapted. Not a fork of any.

## Package layout

Concept-named packages under `reachablelinkage`, never `core`/`util`/`common`; one top-level type per file; internals `private[pkg]`. Dependencies point one way:
`symbols` <- `classfile` <- `classpath` <- `linkage` <- `api` (the Java-friendly facade `Linkage.check` and `LinkageBridge`, maps in and out). Tests: `reachablelinkage.fixture` holds the fixture builders and the oracle.

## Rules (from the user; they override defaults)

- **Errors are values.** Every failure is a case of a small sealed enum per layer with a `message`, returned in an `Either`. No `Either[String, _]` (except the bridge decoder), no `Try`, no `throw` in `main` (JDK exceptions
  at an I/O boundary are caught there and turned into a value). Tests assert on the variants, not on message text.
- **Functional style wherever it costs no measured performance;** benchmark before giving it up. Loops are `@tailrec` local methods carrying primitives; plain `while` only in private hot paths; never allocate per step.
- **Small specialized methods with explicit names.** Extract a named method rather than a long body or a comment.
- Public `def`s and `val`s have explicit return types; case classes are `final`; ADTs are sealed.
- The README stays very succinct.
- **No project history in the repo** (it is unreleased): docs and comments describe what is, not what changed.
- Format with scalafmt (`scalafmtOnCompile`; CI checks it).

## Invariants that bite when forgotten

- **Bump `ClassParser.Version` whenever what `parse` produces changes**, so old per-jar summaries are not read. `JarSummary.FormatVersion` is for the file format.
- **Label precedence** (weakest first): `Latent`, `Guarded`, `Probed`, `Deserialized`, `Reflective`, `Reachable`; report severity is the reverse. `FailOn.REACHABLE` (default) fails on `Reachable` and `Reflective`;
  `DIRECT` on `Reachable` only; `ANY` on all. `Class` precision has no `Reflective` label. The call-graph phases must only add to earlier ones.
- **Name rules never invent a class:** a name that is not on the classpath is ignored (a missing one is the `Probed` label's business). Rules over-approximate by design.
- **A scenario is evidence only with a control run:** the same program must work against the old library, and fail with the specific error. Every scenario that really fails must be `Reachable` or `Reflective` at
  `Method` and `MethodRta`. Check new checks for false positives on the real classpaths: rows there must stay identical unless the change is meant to move them.
- **Adding a kind of conflict:** the `Conflict` case, its `Conflict.describe` entry, the producer in `Checker`, and for a new category one case each in `linkage.Category` and the Java `ConflictCategory`.
  Adding a kind of keep rule: the `Keep` case, its bits in `KeepRules`, the `KeepRuleKind` case and its `RuleKinds.describe` line.
- **The facade stays Java-callable** and the bridge crosses only `java.*` types. New request settings go on the defaulted case class and the builder, not on another constructor.
- **Java 8 floor:** no JDK API newer than 8 in `main`. javac 17 merges identical lambda bodies, so fixtures must not depend on javac's layout.
- Compare timings only **interleaved or back to back** on the same machine.

## Running things

- `sbt core/test` is the suite (CI: JDK 17 to 27, and the checker on 8 to 16). Suites that use the oracle extend `OracleSuite` (first use builds a JDK model, about 12 s and 2 GB).
- On demand, never in `sbt test`: `reachablelinkage.bench.Bench`, `Profile`, `LoopBench`, and in `linkage`: `Soundness`, `BlindSpots`. Real classpaths (Play, pekko-http, http4s, armeria, Spring Boot, sbt, sttp with tapir) are resolved
  offline into a scratch sbt project and passed as a file with one jar per line. `tools/coverage-agent` records the methods a run executes for `Soundness` (see its README).
- `sbt core/publishLocal` publishes to `~/.ivy2/local`, where the plugin fetches it.
- **Releases:** the version comes from the git tag (`sbt-ci-release`); there is no `version` setting. Pushing `vX.Y.Z` runs the CI `publish` job (`sbt ci-release`; secrets `PGP_SECRET`, `PGP_PASSPHRASE`,
  `SONATYPE_USERNAME`, `SONATYPE_PASSWORD`). Without a tag, `publishLocal` makes `0.0.0+N-<sha>-SNAPSHOT`. MiMa (`core/mimaReportBinaryIssues`) compares with the last tagged release; before 1.0 CI reports it and does not fail.

## The sbt plugin

The plugin is a patched sbt-missinglink in its own repository (branch `reachable-linkage-engine`), for **sbt 1 (Scala 2.12) and sbt 2 (Scala 3)**; keep its LICENSE/NOTICE.

- **Engine in its own classloader:** fetched with the Coursier API (sbt's resolution would force the project's Scala library onto it), loaded in a `URLClassLoader` whose parent is the platform loader, and called by
  reflection on `LinkageBridge.check(Map): Map`. The engine version is a constant in `EngineBridge`.
- **Settings:** sbt-missinglink's names and defaults, plus `missinglinkPrecision`, `missinglinkKeepRulesFile`, `missinglinkCacheDirectory`. `missinglinkScanDependencies` is the one switch for scope: false (default) starts
  from the project's classes and fails on conflicts they reach; true starts from every class, fails on every conflict, and implies `Class` precision unless `missinglinkPrecision` is set. `Class` precision is the old
  sbt-missinglink behaviour and shows only the three kinds it reports. There is no class-load-log setting.
- **No sbt 2 task caching:** `missinglinkCheck` is `@transient`; the class directory and rules file are unhashed paths, so a cache hit could return a stale "no conflicts".
- **Pitfalls:** `key := Def.task { ... }` without `.value` silently never runs; `+:` and `++` cannot mix in one expression; use `.asJava`, not `: _*`; on sbt 2 `target` is under `target/out/jvm/...`, so fixtures use `baseDirectory`.
- **Tests:** `sbt test` (both Scala versions) and `sbt scripted` on sbt 1 and `sbt ++<Scala 3 version of sbt 2> scripted` on sbt 2. Republish the core before a plugin run that needs a core change.

## Known limits

Computed class names; a class literal returned by one method and built by another (pass a class-load log through `loadedClasses`); `MethodRta` can miss an instance made by a route it cannot see; Windows and arm64
are not tested.

## Testing notes

- The issues of spotify/missinglink are covered by `UpstreamIssuesSuite`.
- Scala fixtures build in-process with real scalac 2.12, 2.13, 3 LTS and 3 current, each in its own classloader.
- Mutation testing is the habit: break one engine part and require the matching test to fail.
