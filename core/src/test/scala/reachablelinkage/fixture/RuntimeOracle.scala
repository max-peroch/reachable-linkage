package reachablelinkage.fixture

import java.net.URLClassLoader
import java.nio.file.Path

/** Actually runs a fixture app, so reachability claims can be checked against what the JVM does. */
object RuntimeOracle:

  /** Calls `main.method()` (static, no arguments) with `classpath` in an isolated class loader.
    * @return
    *   the linkage error or exception it threw, if any
    */
  def run(classpath: Seq[Path], main: String = "app.Main", method: String = "run"): Option[Throwable] =
    val loader = new URLClassLoader(classpath.map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
    try
      loader.loadClass(main).getMethod(method).invoke(null)
      None
    catch
      case e: java.lang.reflect.InvocationTargetException => Some(e.getCause)
      case e: LinkageError                                => Some(e)
    finally loader.close()
