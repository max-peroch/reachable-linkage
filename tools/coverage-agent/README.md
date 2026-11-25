# coverage-agent

A development tool, not part of reachable-linkage: it is not in the build, not published and not run by `sbt test` or CI.

It is a small Java agent that records which methods of non-JDK classes a real run executes, and writes them to a file at exit.
`Soundness` (in the core's test sources, run by hand) compares that list with the call graph, to check that code which really
ran was not labelled `LATENT`.

```
tools/coverage-agent/build.sh                                # builds coverage-agent.jar (needs the ASM jar; see ASM_JAR)
java -javaagent:tools/coverage-agent/coverage-agent.jar=executed.txt -cp ... Main
# then run reachablelinkage.linkage.Soundness <classpath-file> <app-dir> executed.txt [cha|rta] [-v] [-config]
```

The jar is built locally and gitignored.
