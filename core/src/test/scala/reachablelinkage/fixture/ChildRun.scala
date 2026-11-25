package reachablelinkage.fixture

import java.nio.file.{Path, Paths}

/**
 * Runs a static method of a fixture app in a child JVM, so a run can use JVM options the test JVM was not
 * started with (`--add-opens`, which frameworks such as Guice 4 need on a modern JDK) and a failure cannot
 * disturb the tests. Returns what went wrong, as the chain of (exception class, message), outermost first.
 */
object ChildRun:
  private val launcher: Path = JavaFixture.compile(Map("rl/ChildMain.java" ->
    """package rl;
      |public class ChildMain {
      |  public static void main(String[] a) throws Exception {
      |    try {
      |      Class.forName(a[0]).getMethod(a[1]).invoke(null);
      |    } catch (Throwable t) {
      |      Throwable c = t;
      |      while (c instanceof java.lang.reflect.InvocationTargetException && c.getCause() != null) c = c.getCause();
      |      // Never trust getMessage(): Guice 4's builds a message with a bundled ASM that cannot read this JDK's class files.
      |      for (; c != null; c = c.getCause()) {
      |        String m;
      |        try { m = String.valueOf(c.getMessage()).replace('\n', ' '); } catch (Throwable x) { m = "<message unavailable>"; }
      |        System.out.println("CAUSE\t" + c.getClass().getName() + "\t" + m);
      |      }
      |      System.exit(3);
      |    }
      |  }
      |}""".stripMargin))

  /** Opens what Guice 4's bundled cglib needs to define classes on JDK 17 and later. */
  val AddOpens: Seq[String] = Seq("--add-opens", "java.base/java.lang=ALL-UNNAMED")

  /** None if `main.method()` ran to the end; else the causes of its failure. */
  def run(classpath: Seq[Path], main: String = "app.Main", method: String = "run", jvmArgs: Seq[String] = AddOpens): Option[List[(String, String)]] =
    val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val cp      = (launcher +: classpath).mkString(java.io.File.pathSeparator)
    val proc    = new ProcessBuilder((Seq(javaBin) ++ jvmArgs ++ Seq("-cp", cp, "rl.ChildMain", main, method))*).redirectErrorStream(true).start()
    val output  = new String(proc.getInputStream.readAllBytes())
    val code    = proc.waitFor()
    val causes  = output.linesIterator.filter(_.startsWith("CAUSE\t")).map(_.split("\t", 3)).collect { case Array(_, c, m) => (c, m) }.toList
    if code == 0 then None
    else if causes.nonEmpty then Some(causes)
    else Some(List(("the child JVM exited with code " + code, output.takeRight(1800))))
