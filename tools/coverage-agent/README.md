# coverage-agent

A development tool, not part of reachable-linkage: it is not in the build, not published and not run by `sbt test` or CI.

It answers one question about the call graph: **does it reach every method a real run executes?** A method that ran but was not reached is a hole in "`LATENT` means no run gets there": a broken reference in it would be labelled `LATENT` although the program executes it. Run it after changing the call graph (`MethodGraph`) or the framework rules, on an application you can really start.

It has two parts:

- the **agent** (this folder), a small Java agent that records every method of every non-JDK class that runs and writes the list to a file when the JVM exits;
- **`Soundness`** (`reachablelinkage.linkage.Soundness`, in the core's test sources), which compares that list with the call graph.

## Build the agent

```
tools/coverage-agent/build.sh
```

This writes `tools/coverage-agent/coverage-agent.jar` (gitignored). It needs an ASM jar: it looks in the Coursier cache on macOS and Linux, or set `ASM_JAR=/path/to/asm-9.x.jar`.

## A worked example

An application that loads a class by a name only known at run time, so nothing in its bytecode names it:

```java
// demo/app/Main.java
package demo.app;
import demo.lib.Greeter;

public class Main {
  public static void main(String[] args) throws Exception {
    System.out.println(new Greeter().hello());
    String name = "demo.lib." + System.getProperty("plugin", "Hidden");
    ((Runnable) Class.forName(name).getDeclaredConstructor().newInstance()).run();
  }
}
```

`demo.lib.Greeter` has a `hello()` method, and `demo.lib.Hidden implements Runnable`. Compile the library into `lib.jar` and the application into `app-out/`, then:

```
DEMO=/path/to/demo        # the folder with lib.jar and app-out/
AGENT=$PWD/tools/coverage-agent/coverage-agent.jar      # from the repository root

# 1. Run the application under the agent. The list is written when the JVM exits.
java -javaagent:$AGENT=$DEMO/executed.txt -cp $DEMO/app-out:$DEMO/lib.jar demo.app.Main

# 2. A file with the library jars of the application's classpath, one per line (not the application itself).
echo $DEMO/lib.jar > $DEMO/classpath.txt

# 3. Ask the call graph, still from the repository root.
sbt "core/Test/runMain reachablelinkage.linkage.Soundness $DEMO/classpath.txt $DEMO/app-out $DEMO/executed.txt cha"
```

```
Cha: 5 methods ran, 5 of them in classes of the classpath; the graph reaches 3 and misses 2
      2  class not reached at all
    [class missed]  demo/lib/Hidden.<init>()V
    [class missed]  demo/lib/Hidden.run()V
```

The graph starts from the application's classes, so it reaches `Main` and `Greeter` but never learns about `Hidden`. A conflict inside `Hidden` would be labelled `LATENT`, although it runs.

## Reading the result

`Soundness <classpath-file> <app-dir> <executed-methods> [cha|rta] [-v] [-config]`

- `<classpath-file>`: the application's dependency jars, one path per line.
- `<app-dir>`: the application's own classes. Every class there is a root, as in a check at `App` scope.
- `<executed-methods>`: the file the agent wrote.
- `cha` (default) or `rta`: how virtual calls are resolved.
- `-v`: list every method that was not reached (without it, the first 12).
- `-config`: also treat the classes `reference.conf` and `application.conf` name as roots (the opt-in `keep-classes-named-in` rule).

Only methods of classes on the classpath count; the JDK is not walked. Misses are grouped by why: the class was not reached at all (as above), a lambda body, a static initialiser, a constructor, or another method of a class that was reached. `[class missed]` means the whole class is unknown to the graph, `[class reached]` that the class is known and one of its methods is not.

A miss is not always a bug: a class chosen by a computed name, or built by a framework the rules do not know, is a documented limit. It tells you where a rule (`keep-members` and the others in the main README) or a class-load log would be needed.

## On a real application

Start the application with the agent, exercise it (ask it for a route, run its jobs), and stop it so the JVM exits normally (a signal such as SIGTERM is fine) and writes the file; a process killed with SIGKILL writes nothing. Resolve the application's dependency jars into the classpath file and pass its compiled classes as `<app-dir>`.
